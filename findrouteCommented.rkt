#lang racket/gui
(require (planet jphelps/guiml))




;define Nothern lines' branches as well as their respective stations
(define northernEdgwareMordenCX (list "Edgware" "Burnt Oak" "Colindale" "Hendon Central" "Brent Cross" "Golders Green" "Hampstead" "Belsize Park" "Chalk Farm" "Camden Town" "Mornington Crescent" "Euston" "Warren Street" "Goodge Street" "Tottenham Court Road" "Leicester Square" "Charing Cross" "Embankment" "Waterloo" "Kennington" "Oval" "Stockwell" "Clapham North" "Clapham Common" "Clapham South" "Balham" "Tooting Bec" "Tooting Broadway" "Colliers Wood" "South Wimbledon" "Morden"))
(define northernHighBarnetMordenCX (list "High Barnet" "Totteridge and Whetstone" "Woodside Park" "West Finchley" "Finchley Central" "East Finchley" "Highgate" "Archway" "Tufnell Park" "Kentish Town" "Camden Town" "Mornington Crescent" "Euston" "Warren Street" "Goodge Street" "Tottenham Court Road" "Leicester Square" "Charing Cross" "Embankment" "Waterloo" "Kennington" "Oval" "Stockwell" "Clapham North" "Clapham Common" "Clapham South" "Balham" "Tooting Bec" "Tooting Broadway" "Colliers Wood" "South Wimbledon" "Morden"))
(define northernEdgwareHighBarnet (list "Edgware" "Burnt Oak" "Colindale" "Hendon Central" "Brent Cross" "Golders Green" "Hampstead" "Belsize Park" "Chalk Farm" "Camden Town" "SWITCH TO NORTHBOUND TO HIGH BARNET" "Kentish Town" "Tufnell Park" "Archway" "Highgate" "East Finchley" "Finchley Central" "West Finchley" "Woodside Park" "Totteridge and Whetstone" "High Barnet"))


;Creates a window(frame) with the label "Journey Planner"
(define frame (new frame%
                   [label "Journey Planner"]
                   [width 350]
                   [height 350]))
;Creates a panel inside of the frame. A panel is an object that allows us to pack objects such as messages or input fields inside. It is helpful for manipulating the overall design of the program.
(define inputPanel (new vertical-panel%
                        [parent frame] ;Puts it inside frame
                        [border 10] ;Padding
                        [spacing 10] ;Padding between objects
                        [style (list 'border)])) ;'border makes it so that the panel has a visible outline

(define input (new text-field% ;Defines a new text field. Text fields allow us to input information; in this programs case the starting or destination station
                   [label "Starting Location"]
                   [parent inputPanel] ;Packs this instance inside the inputPanel frame we defined earlier
))

(define input1 (new text-field%
                   [label "Destination          "]
                   [parent inputPanel])) ;Packs this inside the inputPanel frame we defined earlier


(define button (new button% [parent inputPanel] ;Defined a new button inside the inputPanel we defined earlier
                    [label "Find Route"]
                    [callback (lambda (button event) ;Upon pressing the button execute the code beneath
                                (delete-children panel) ;Delete children gets rid of all the objects packed inside 'panel'. On the first query this does nothing however, on any subsequent queries it deletes the previous query's output (the route)
                                (send frame resize 350 350);Does nothing on the first query as the window is already that size however on subsequent queries where the query is so large that it does not fit in the original 350 x 350 it is responsible for resizing the window back to its original size
                                (newMessages (findBranch (send input get-value) (send input1 get-value))))])) ;Calls the newMessages function with the value that findBranch returns.
                                              ;findBranch is called with two values. Those two values are extracted from the input fields the user interacts with in the GUI.

(define noStart (new dialog% ;Defines a new dialog. Dialogs are popup windows that are often used to return Errors.
                    [label "Error"]
                    [parent frame])) ;Despite being a seperate window it still has to be given a parent to pop-up over; in this case it is frame.
(define noStartMsg (new message% ;Defines a new message
                        [label "Invalid station(s)"]
                        [parent noStart])) ;Packs it inside of noStart (the dialog popup window)

(new message%
     [label "START"]
     [parent frame])

(define panel (new vertical-panel% ;Defines a new panel. This panel will be used to hold the output of a specific query.
                   [parent frame];Packed inside of the previously defined 'frame'
                   [spacing 5];Padding
                   [style (list 'border)] ;Gives a visible outline to the panel
                   ))


(new message%
     [label "FINISH"]
     [parent frame])



(define findRoute (λ (start finish stations) ;The findRoute function takes in two strings and a list of stations. It is responsible for returning a list of stations that the user will have to pass through to get to their destination.
                    (cond
                      ;index-of takes in a list and a string. We can use it to find the location ("index") of a specific item within a list e.g (index-of (list 1 2 3) 2) will be 1 as the index starts from 0
                      ((< (index-of stations start) (index-of stations finish)) ;if the index of the starting station is smaller than of finish that means we have to query the list from front to back.
                       (for/list ([x (in-range (index-of stations start) (+ 1 (index-of stations finish)))]) ;This loop will go through a range on x e.g: 3-10 so on this case x will go from 3,4,5...9. The range we give this loop is the index of the starting station and the index of the starting station. 
                         (list-ref stations x))) ;for every iteration of the above loop we return the data stored behind index x
                      ((> (index-of stations start) (index-of stations finish));if the index of the starting station is greater than of finish that means we have to query the list from back to front.
                       (for/list ([x (in-range (index-of stations start)  (- (index-of stations finish) 1) -1)]);this for loop does the exact same as the one above however, it queries a list from back to front. So given a range 10-3 x will be 10,9,8 ... 4
                         (list-ref stations x)))))) ;for every iteration of the above loop we return the data stored behind index x

(define newMessages (λ (x) ;newMessages function takes in a list of stations and packs in every element as a message in the GUI
                      (cond
                        ((equal? x "Invalid station(s)") (send noStart show #t)) ;If it gets "Invalid station(s)" then make the error popup window show up
                        ((empty? x) "") ;Base case. If the list is empty we have no more elements to work through
                        (#t (new message% ;Else create a new message
                                 [parent panel] ;Inside of 'panel' panel
                                 [label (string-append "⦿ - " (first x))]) ;Makes the message say ⦿ + station
                            (newMessages (rest x)))))) ;Recursively calls itself again with the (rest x)

(define findBranch (λ (start finish) ;findBranch finds the correct branch for the specific user input, then calls findRoute with the appropriate list.
                     (cond
                       ;(member x y) returns a list if x exists in list y, otherwise it returns #f
                       ;list? returns #t if x is a list otherwise it returns false
                       ;(and x y) will only result in #t if both x and y are #t
                       ((and (list? (member start northernEdgwareMordenCX)) (list? (member finish northernEdgwareMordenCX)))(findRoute start finish northernEdgwareMordenCX))
                       ;If the starting station is a member of northernEdgwareMordenCX AND destination also. Then call findRoute with northernEdgwareMordenCX
                       ((and (list? (member start northernHighBarnetMordenCX)) (list? (member finish northernHighBarnetMordenCX)))(findRoute start finish northernHighBarnetMordenCX))
                       ;If the starting station is a member of northernHighBarnetMordenCX AND destination also. Then call findRoute with northernHighBarnetMordenCX
                       ((and (list? (member start northernEdgwareHighBarnet)) (list? (member finish northernEdgwareHighBarnet)))(findRoute start finish northernEdgwareHighBarnet))
                       ;;If the starting station is a member of northernEdgwareHighBarnet AND destination also. Then call findRoute with northernEdgwareHighBarnet
                       (#t "Invalid station(s)"))));If all the above fail return "Invalid station(s)"




(send frame show #t)



