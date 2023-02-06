#lang racket/gui
(require (planet jphelps/guiml))





(define northernEdgwareMordenCX (list "Edgware" "Burnt Oak" "Colindale" "Hendon Central" "Brent Cross" "Golders Green" "Hampstead" "Belsize Park" "Chalk Farm" "Camden Town" "Mornington Crescent" "Euston" "Warren Street" "Goodge Street" "Tottenham Court Road" "Leicester Square" "Charing Cross" "Embankment" "Waterloo" "Kennington" "Oval" "Stockwell" "Clapham North" "Clapham Common" "Clapham South" "Balham" "Tooting Bec" "Tooting Broadway" "Colliers Wood" "South Wimbledon" "Morden"))
(define northernHighBarnetMordenCX (list "High Barnet" "Totteridge and Whetstone" "Woodside Park" "West Finchley" "Finchley Central" "East Finchley" "Highgate" "Archway" "Tufnell Park" "Kentish Town" "Camden Town" "Mornington Crescent" "Euston" "Warren Street" "Goodge Street" "Tottenham Court Road" "Leicester Square" "Charing Cross" "Embankment" "Waterloo" "Kennington" "Oval" "Stockwell" "Clapham North" "Clapham Common" "Clapham South" "Balham" "Tooting Bec" "Tooting Broadway" "Colliers Wood" "South Wimbledon" "Morden"))
(define northernEdgwareHighBarnet (list "Edgware" "Burnt Oak" "Colindale" "Hendon Central" "Brent Cross" "Golders Green" "Hampstead" "Belsize Park" "Chalk Farm" "Camden Town" "SWITCH TO NORTHBOUND TO HIGH BARNET" "Kentish Town" "Tufnell Park" "Archway" "Highgate" "East Finchley" "Finchley Central" "West Finchley" "Woodside Park" "Totteridge and Whetstone" "High Barnet"))



(define frame (new frame%
                   [label "Journey Planner"]
                   [width 350]
                   [height 350]))
(define inputPanel (new vertical-panel%
                        [parent frame]
                        [border 10]
                        [spacing 10]
                        [style (list 'border)]))
(define input (new text-field%
                   [label "Starting Location"]
                   [parent inputPanel]
))

(define input1 (new text-field%
                   [label "Destination          "]
                   [parent inputPanel]))


(define button (new button% [parent inputPanel]
                    [label "Find Route"]
                    [callback (lambda (button event)
                                (delete-children panel)
                                (send frame resize 350 350)
                                (newMessages (findBranch (send input get-value) (send input1 get-value))))]))
(define noStart (new dialog%
                    [label "Error"]
                    [parent frame]))
(define noStartMsg (new message%
                        [label "Invalid station(s)"]
                        [parent noStart]))

(new message%
     [label "START"]
     [parent frame])

(define panel (new vertical-panel%
                   [parent frame]
                   [spacing 5]
                   [style (list 'border)]
                   ))


(new message%
     [label "FINISH"]
     [parent frame])



(define findRoute (λ (start finish stations)
                    (cond
                      ((< (index-of stations start) (index-of stations finish))
                       (for/list ([x (in-range (index-of stations start) (+ 1 (index-of stations finish)))])
                         (list-ref stations x)))
                      ((> (index-of stations start) (index-of stations finish))
                       (for/list ([x (in-range (index-of stations start)  (- (index-of stations finish) 1) -1)])
                         (list-ref stations x))))))

(define newMessages (λ (x)
                      (cond
                        ((equal? x "Invalid station(s)") (send noStart show #t))
                        ((empty? x) "")
                        (#t (new message%
                                 [parent panel]
                                 [label (string-append "⦿ - " (first x))]) (newMessages (rest x))))))

(define findBranch (λ (start finish)
                     (cond
                       ((and (list? (member start northernEdgwareMordenCX)) (list? (member finish northernEdgwareMordenCX)))(findRoute start finish northernEdgwareMordenCX))
                       ((and (list? (member start northernHighBarnetMordenCX)) (list? (member finish northernHighBarnetMordenCX)))(findRoute start finish northernHighBarnetMordenCX))
                       ((and (list? (member start northernEdgwareHighBarnet)) (list? (member finish northernEdgwareHighBarnet)))(findRoute start finish northernEdgwareHighBarnet))
                       (#t "Invalid station(s)"))))



(send frame show #t)



