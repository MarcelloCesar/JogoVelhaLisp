(defun novoJogo (X)
	(cons X 
		(cons X
			(cons X
				(cons X
					(cons X
						(cons X
							(cons X
								(cons X
									(cons X nil)
								)
							)
						)
					)
				)
			)
		)							
	)
)

(defun printaTabuleiro(X)
	(format t " ~D ~D ~D ~C ~C ~D ~D ~D ~C ~C ~D ~D ~D ~C ~C ~C ~C" 
		(car X) 
		(car (cdr X))
		(car (cdr (cdr X))) #\return #\linefeed 
		(car (cdr (cdr (cdr X)))) 
		(car (cdr (cdr (cdr (cdr X)))))
		(car (cdr (cdr (cdr (cdr (cdr X)))))) #\return #\linefeed 
		(car (cdr (cdr (cdr (cdr (cdr (cdr X)))))))
		(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr X))))))))
		(car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr X))))))))) #\return #\linefeed #\return #\linefeed 
	)
)

(defun setaValor (tabuleiro casa valor)
	(if (= casa 0)
		(cons valor (cdr tabuleiro))
		(cons (car tabuleiro) (setaValor (cdr tabuleiro) (- casa 1) valor))
	)
)

(defun venceu(tabuleiro X)
	(if 
		(or
			(and 
				(eq X (car tabuleiro)) 
				(eq X (car (cdr tabuleiro)))
				(eq X (car (cdr (cdr tabuleiro))))
			)
			(and 
				(eq X (car (cdr (cdr (cdr tabuleiro)))))
				(eq X (car (cdr (cdr (cdr (cdr tabuleiro))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr tabuleiro)))))))
			)
			(and 
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro))))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro)))))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro))))))))))
			)
			(and 
				(eq X (car tabuleiro))
				(eq X (car (cdr (cdr (cdr tabuleiro)))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro))))))))
			)
			(and 
				(eq X (car (cdr tabuleiro)))
				(eq X (car (cdr (cdr (cdr (cdr tabuleiro))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro)))))))))
			)
			(and 
				(eq X (car (cdr (cdr tabuleiro))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr tabuleiro)))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro))))))))))
			)
			(and
				(eq X (car tabuleiro)) 
				(eq X (car (cdr (cdr (cdr (cdr tabuleiro))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro))))))))))
			)
			(and
				(eq X (car (cdr (cdr tabuleiro))))
				(eq X (car (cdr (cdr (cdr (cdr tabuleiro))))))
				(eq X (car (cdr (cdr (cdr (cdr (cdr (cdr tabuleiro))))))))
			)				
		)
		't
		nil
	)
)

(defun jogar(tabuleiro peca)
	(printaTabuleiro tabuleiro)
	(if (venceu tabuleiro 'X)		
		(format t "Vitória do X")
		(if (venceu tabuleiro 'O)
			(format t "Vitória do O")							
			(jogar 
				(setaValor tabuleiro (read) peca)
				(if (eq peca 'X) 'O 'X)
			)			
		)
	)
)

(jogar (novoJogo '-) 'X)