
(define sum_1 ; soma 1 à todos os itens da lista
    (lambda (my_list)
        (cond
            ((null? (cdr my_list))
                (list(+ (car my_list) 1))
            )
            (else
                (cons
                    (+ (car my_list) 1)
                    (sum_1 (cdr my_list))
                )
            )
        )
    )
)

(define append_item ; inclui um item no fim da lista
    (lambda (item my_list)
        (cond
            ((null? my_list)
                (list item)
            )
            (else
                (cons
                    (car my_list)
                    (append_item item (cdr my_list))
                )
            )
        )
    )
)

(define index ; pega um item na posição 'position'
    (lambda (position my_list)
        (cond
            ((< position 0)
                #f
            )
            ((null? my_list)
                #f
            )
            ((equal? position 0)
                (car my_list)
            )
            (else
                (index (- position 1) (cdr my_list))
            )
        )
    )
)

(define sum ; soma toda a lista
    (lambda (my_list)
        (cond
            ((null? my_list)
                0
            )
            (else
                (+ (car my_list) (sum (cdr my_list)))
            )
        )
    )
)

(define len ; descobre o tamanho da lista
    (lambda (my_list)
        (cond
            ((null? my_list)
                0
            )
            (else
                (+ 1 (len (cdr my_list)))
            )
        )
    )
)

(define avg ; calcula a média da lista
    (lambda (my_list)
        (/ (sum my_list) (len my_list))
    )
)

(define create_list ; cria uma lista de ordem crescente de 'start' à 'end'
    (lambda (start end)
        (cond
            ((> start end)
                #f
            )
            ((equal? start end)
                (list end)
            )
            (else
                (cons
                    start
                    (create_list (+ start 1) end)
                )
            )
        )
    )
)

(define create_list_1_to_n ; cria uma lista de ordem crescente de 1 à 'n'
    (lambda (n)
        (create_list 1 n)
    )
)

(define append_list ; adiciona uma lista2 ao final da lista1.
    (lambda (list1 list2)
        (cond
            ((null? list1)
                list2
            )
            (else
                (cons
                    (car list1)
                    (append_list (cdr list1) list2)
                )
            )
        )
    )
)

(define append_1_n ; adiciona de 1 a n no fim da lista
    (lambda (my_list n)
        (cond
            ((<= n 0)
                #f
            )
            (else
                (append_list my_list (create_list 1 n))
            )
        )
    )
)

(define sum_to_all ; Soma um número em cada um dos itens da lista
    (lambda (number my_list)
        (cond
            ((null? (cdr my_list))
                (list(+ (car my_list) number))
            )
            (else
                (cons
                    (+ (car my_list) number)
                    (sum_to_all number (cdr my_list))
                )
            )
        )
    )
)

(define sub_list_start ; pega a sublista da posição 'start' até o fim da lista
    (lambda (start my_list)
        (cond
            ((< start 0)
                #f
            )
            ((>= start (len my_list))
                #f
            )
            ((equal? start 0)
                my_list
            )
            (else
                (sub_list_start (- start 1) (cdr my_list))
            )
        )
    )
)

(define remove_last ; remove o último item da lista
    (lambda (my_list)
        (cond
            ((null? my_list)
                (list)
            )
            ((null? (cdr my_list))
                (list)
            )
            (else
                (cons
                    (car my_list)
                    (remove_last (cdr my_list))
                )
            )
        )
    )
)
	
(define sub_list_end ; pega a sublista do inicio da lista até a posição 'end' (incluso).
    (lambda (my_list end)
        (cond
            ((< end 0)
                #f
            )
            ((>= end (len my_list))
                #f
            )
            ((equal? end (- (len my_list) 1))
                my_list
            )
            (else
                (sub_list_end (remove_last my_list) end)
            )
        )
    )
)

(define sub_list ; pega a sublista da posição 'start' da lista até a posição 'end' (incluso)
    (lambda (my_list start end)
        (cond
            ((< start 0)
                #f
            )
            ((null? my_list)
                (list)
            )
            ((> start end)
                (list)
            )
            (else
                (sub_list_start start (sub_list_end my_list end))
            )
        )
    )
)

(define include_item_on_position ; Inclui o número 'number' na posição 'position' da lista
    (lambda (item position my_list)
        (cond
            ((equal? position 0)
                (cons item my_list)
            )
            (else
                (cons
                    (car my_list)
                    (include_item_on_position item (- position 1) (cdr my_list))
                )
            )
        )
    )
)

(define remove_even_numbers ; remove os números pares
    (lambda (my_list)
        (cond
            ((null? my_list)
                (list)
            )
            ((even? (car my_list))
                (remove_even_numbers (cdr my_list))
            )
            (else
                (cons
                    (car my_list)
                    (remove_even_numbers (cdr my_list))
                )
            )
        )
    )
)

(define remove_all ; remove todas as ocorrencias do numero
    (lambda (my_list number)
        (cond
            ((null? my_list)
                (list)
            )
            ((equal? number (car my_list))
                (remove_all(cdr my_list) number)
            )
            (else
                (cons
                    (car my_list)
                    (remove_all(cdr my_list) number)
                )
            )
        )
    )
)

(define remove_all_duplicated ; remove todos os numeros repetidos
    (lambda(my_list)
        (cond
            ((null? my_list)
                (list)
            )
            (else
                (cons 
                    (car my_list)
                    (remove_all_duplicated (remove_all(cdr my_list) (car my_list)))
                )
            )
        )
    )
)

