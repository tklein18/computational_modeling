;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;



patches-own [

]


turtles-own[
  investor-type ; this will determine how the turtle updates their price target
  price-target ; this is the price for each turtle that will determine whether the turtle buys or sells the stock
  shares-owned ; this is the current number of shares that turtles own
  cash-available ; this is the amount of cash each turtle has to spend on shares
  order-action ; buy or sell
  order-quantity ; this will be used to store the amount of shares each turtle would like to buy or sell
  prev-value ; this is the previous days' value of shares owned for each turtle
  influencer; turtles are randomly selected at the beginning to be influencers, these turtles will influence the price target of irrational turtles
]


globals[
  current-price ; this is the current price of the stock
  current-ni ; this is the current net income of the company
  buy-list ; this contains the buy orders for every tick
  sell-list ; this contains the sell orders for every tick
  prev-price ; the price from the end of the most recent day
  price-log ; log of prices from last 100 days (used for plotting)
]





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; procedures ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; set up procedures ;;;;;;;;;;;;;;;;;;;;;;

to setup

  ca

  set current-price 1
  set current-ni 50
  set prev-price 1
  set price-log (list 1)

  ask patches [sprout 1]

  ; setting turtle investment type and color-coding the turtles
  ; also giving the turtles an equal amount of the shares outstanding
  ; also giving turtles cash to spend on purchasing shares
  ask turtles [
    set investor-type (ifelse-value (who <= (num-rational - 1)) ["rational"] ["irrational"])
    ifelse investor-type = "rational" [set color green] [set color orange]
    ifelse investor-type = "irrational" [set price-target (random 10 + .01)] [set price-target 1]
    set shares-owned 10
    set cash-available 50
    set influencer 0
  ]

  ; making random turtles influencers
  ask n-of num-influencer turtles [
    set influencer 1
  ]

  ; making influencers stars so that we can identify them
  ask turtles with [influencer = 1][

    set shape "star"

  ]









  reset-ticks

end



;;; go procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  if current-price = 0 [stop]
  report-ni
  update-price-target
  place-orders
  color-patches
  settle-orders
  log-prices
  tick

end






;;;;; report-ni ;;;;;;;;;;;;;;;;;;;;;;;;
; net income is adjusted every 65 periods
; it is increased by a random normal percentage
; the mean and stdv of the random normal number
; are set by the user

to report-ni

  if remainder ticks 65 = 0[
  set current-ni current-ni * (1 + random-normal mean-ni-growth sd-ni-growth)
  ]

end







; update price-target procedure ;;;;;;;;;;;;;;;;;;;;;;;;;

to update-price-target

  ask turtles[


    if investor-type = "rational" [
      ; rational turtles update their price target whenever the net income is updated
      ; they do this by estimating the net income in 4 reports from the latest one
      ; and then using the number of shares outstanding (1,000) and their P/E target to calculate a fair value share price
      ; they then discount that fair value share price by their target return (.05) to calculate their target price
      if remainder ticks 65 = 0[
        set price-target max (list ((((current-ni * (1 + (random-normal (4 * mean-ni-growth) (4 * sd-ni-growth)))) / 1000) * 20) / 1.05) .01)
      ]

    ]


    if investor-type = "irrational" and influencer = 0
    [
      ; irrational agents will update their price-target positively 1%
      ; if they're share appreciated in value from the last day
      ; they will update their price-target negatively 1% if they lost value from the last day

      ifelse current-price > prev-price [set price-target (price-target + (current-price * .01))] [set price-target (price-target - (current-price * .01))]




    ]



    if investor-type = "irrational" and influencer = 0[

    ; irrational investors also change their target price
    ; based on the target price of their neighbors
    ; by moving their target price 10% between
     ; their current target price and the average of their neighbors'
      ; target prices

      set price-target (price-target + ((([price-target] of min-one-of turtles with [influencer = 1] [xcor + ycor]) - price-target) / 10))




    ]




    ; irrational influencers have a random chance of updating their price-target
    ; by a random number
    if investor-type = "irrational" and influencer = 1[

      if 1 = random 65 [set price-target (current-price * (random-float 10))]

    ]




  ]



end




;;; place buy/sell orders procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; if turtles have a price target that is above the current price
; they put in a buy order
; if they have price target below the current price, they put in a sell order
; the price of the buy order (bid/ask) is the target price
; each buy order is for a quantity of 1 share
; turtles can submit multiple orders
; the quantity of orders is calculated slightly differently for buy/sell orders
; the quantity of a buy order is between 1 and
; the total amount the turtle could purchase given its cash and the current price
; the quantity scales up to the max amount as the current price approaches 50% of the target price
; the quantity for sell orders is similar, its between 1 and the shares on hand
; the quantity scales up to the shares on hand as the current price approaches 200% of the target price

to place-orders

  set buy-list []
  set sell-list []

  ask turtles [

    ifelse price-target >= current-price [

      set order-action "buy"
      set order-quantity  (ifelse-value (cash-available - current-price) > 0 [1] [0])
      repeat order-quantity [set buy-list lput (list 1 price-target who) buy-list]

    ] [

      set order-action "sell"
      set order-quantity  (ifelse-value shares-owned > 0 [1] [0])
      repeat order-quantity [set sell-list lput (list -1 price-target who) sell-list]

    ]

  ]




end






; color patches procedures ================================================



to color-patches


  ask patches [

     if count (turtles-here with [order-action = "buy"]) > 0 [set pcolor 87]

    if count (turtles-here with [order-action = "sell"]) > 0 and (count (turtles-here with [shares-owned > 0]) > 0) [set pcolor 37]


    if count (turtles-here with [order-action = "sell"]) > 0 and (count (turtles-here with [shares-owned = 0]) > 0) [set pcolor 0]





  ]


end













;;;;;;;; settle orders procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to settle-orders


  ; if there are more buy orders than sell orders
  ; it automatically executes all the sell orders
  ; randomly executes a number of buy orders = sell orders
  ; sets the new market price = median of the top x buy orders, sorted by descending target price
  ; where x is the difference between the sell orders and buy orders

  if ((sum map first sell-list) + (sum map first buy-list)) > 0 [

    ask turtles with [order-action = "sell"] [
    ; if there are more buy orders than sell orders, then all of the sell orders are filled
      set shares-owned (shares-owned - order-quantity)
      set cash-available (cash-available + (order-quantity * current-price))

    ]

    ; randomly selects the buy orders equal to the number of sell orders
    ; then settles those orders for each turtle

    foreach (n-of (- sum map first sell-list) buy-list)  [

      x -> ask turtles with [who = (last x)] [

        set shares-owned (shares-owned + 1)
        set cash-available (cash-available - current-price)

      ]

    ]


     ; setting previous price equal to the current price

    set prev-price current-price




    ; setting the current price equal to the current price plus a change
    ; the change is equal to the ratio of the difference in buy and sell orders over the total number of outstanding shares * 5
    ; times the current price
    ; so if agents demanded 1000 more shares than were available to sell, the price would increase 20%

    set current-price (current-price + (current-price * min (list (((sum map first sell-list) + (sum map first buy-list)) / 500) .2)))




  ]




  if ((sum map first sell-list) + (sum map first buy-list)) < 0 [

    ask turtles with [order-action = "buy"] [
    ; if there are more sell orders than buy orders, then all of the buy orders are filled
      set shares-owned (shares-owned + order-quantity)
      set cash-available (cash-available - (order-quantity * current-price))

    ]

    ; randomly selects the sell orders equal to the number of buy orders
    ; then settles those orders for each turtle

    foreach (n-of (sum map first buy-list) sell-list)  [

      x -> ask turtles with [who = (last x)] [

        set shares-owned (shares-owned - 1)
        set cash-available (cash-available + current-price)

      ]

    ]



    ; setting previous price equal to the current price

    set prev-price current-price

   ; setting the current price equal to the current price plus a change
    ; the change is equal to the ratio of the difference in buy and sell orders over the total number of outstanding shares * 5
    ; times the current price
    ; so if agents wanted to sell 1000 more shares than were available to buy, the price would decrease 20%

    set current-price (current-price - (current-price * min (list (((- sum map first sell-list) + (- sum map first buy-list)) / 500) .2)))




  ]





  ; if the market is in perfect balance,
  ; or if there is no demand or supply
  ; then the price remains the same
  if ((sum map first sell-list) + (sum map first buy-list)) = 0 [

    if length buy-list > 0 [

        ask turtles with [order-action = "sell"] [
    ; if there are equal buy orders and sell orders, then all of the sell orders are filled
      set shares-owned (shares-owned - order-quantity)
      set cash-available (cash-available + (order-quantity * current-price))

    ]


          ask turtles with [order-action = "buy"] [
    ; if there are equal sell orders and buy orders, then all of the buy orders are filled
      set shares-owned (shares-owned + order-quantity)
      set cash-available (cash-available - (order-quantity * current-price))

    ]


    ]

  set prev-price current-price

  ]


end






; log prices command =====================================
; this logs the last 100 prices
; used to adjust the graph of prices

to log-prices


  set price-log fput current-price (sublist price-log 0 (min (list 99 (length price-log))))

end









@#$#@#$#@
GRAPHICS-WINDOW
210
10
618
419
-1
-1
40.0
1
10
1
1
1
0
1
1
1
0
9
0
9
0
0
1
ticks
30.0

BUTTON
10
46
76
79
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
12
95
75
128
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
93
97
174
130
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
9
153
181
186
mean-ni-growth
mean-ni-growth
-.1
.1
0.018
.001
1
NIL
HORIZONTAL

SLIDER
10
198
182
231
sd-ni-growth
sd-ni-growth
.005
.3
0.075
.005
1
NIL
HORIZONTAL

PLOT
644
27
1132
198
price plot
ticks
price
0.0
1.1
0.0
2.0
true
true
"" "if (ticks > 100) [set-plot-x-range (ticks - 100) (ticks)]\nset-plot-y-range 0 (ceiling (1.1 * (max price-log)))"
PENS
"Price" 1.0 0 -16777216 true "" "plot current-price"
"Net Income" 1.0 0 -13840069 true "" "plot (current-ni / 1000) * 20 "

SLIDER
10
241
182
274
num-rational
num-rational
0
100
50.0
1
1
NIL
HORIZONTAL

MONITOR
646
229
740
274
Irration Cash
sum [cash-available] of (turtles with [investor-type = \"irrational\"])
0
1
11

MONITOR
756
230
870
275
irrational shares
sum [shares-owned] of (turtles with [investor-type = \"irrational\"])
0
1
11

MONITOR
896
233
1044
278
irrational price-target
mean [price-target] of (turtles with [investor-type = \"irrational\"])
2
1
11

MONITOR
1068
236
1165
281
NIL
current-price
2
1
11

CHOOSER
26
290
164
335
num-influencer
num-influencer
1 2 3 4 5 6 7 8 9 10
9

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
