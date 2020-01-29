(setq buffer '())
(setq tokens '())

(defun main (filename)
	(lexicalAnalyzer filename)
	(setq tokens (reverse tokens))
	(loop for i from 0 to (- (list-length tokens) 1) do
		(format t "~a ~%" (nth i tokens))
	)
)

(defun lexicalAnalyzer (filename)
	(setq code (codeReader filename))
	(tokenization code)
)

(defun tokenization (L)
	(setq token_map '())
	(loop for i in L
	    when (isOperator i)
	      	do (addOperator i)
	    else
	    	do (progn
	    			(if (and (not (char= #\Space i )) (not (char= #\Tab i)) (not (char= #\newline i)) ) 
	    				(setq buffer (append buffer (list i)))
	    				(if (and (not (equal nil buffer)) (not (char= (first (last buffer)) #\Space)))
	    					(progn 
	    						(setq buffer (makeTheWord buffer))
	    						(cond 
	    							((isKeyword (list buffer))
	    								(addKeyword (list buffer))
	    							)
	    							((isValue (coerce buffer 'string))	
	    								(setq tokens (push "VALUE" tokens))
	    								(setf buffer '())
	    							)
	    							(T    								
	    								(setq tokens (push "IDENTIFIER" tokens))
	    								(setf buffer '())
	    							)
	    						)
	    					)
							nil
	    				)
	    			)
	    		)
	    end)
)

(defun makeTheWord (Liste)
	(if(not (eq nil Liste))
		(coerce Liste 'string)
		nil
	)
)


(defun isValue (i)
	(isNumber i)
)

(defun isNumber (token)
  	(let ((*read-eval* nil))
    	(ignore-errors (numberp (read-from-string token)))
    )
)

(defun addKeyword (word)
	(if (equal '("deffun") word)
		(progn 
			(setq tokens (push "KW_DEFFUN" tokens))
	    	(setf buffer '())
		)
	)
	(if (equal '("and") word)
		(progn
			(setq tokens (push "KW_AND" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("or") word)
		(progn
			(setq tokens (push "KW_OR" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("not") word)
		(progn 
			(setq tokens (push "KW_NOT" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("equal") word)
		(progn 
			(setq tokens (push "KW_EQUAL" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("append") word)
		(progn 
			(setq tokens (push "KW_APPEND" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("concat") word)
		(progn 
			(setq tokens (push "KW_CONCAT" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("set") word)
		(progn 
			(setq tokens (push "KW_SET" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("for") word)
		(progn 
			(setq tokens (push "KW_FOR" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("less") word)
		(progn 
			(setq tokens (push "KW_LESS" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("if") word)
		(progn 
			(setq tokens (push "KW_IF" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("exit") word)
		(progn 
			(setq tokens (push "KW_EXIT" tokens)) 
	    	(setf buffer '())
	    )
	)
	(if (equal '("list") word)
		(progn 
			(setq tokens (push "KW_LIST" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("true") word)
		(progn 
			(setq tokens (push "KW_TRUE" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("false") word)
		(progn 
			(setq tokens (push "KW_FALSE" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("nil") word)
		(progn 
			(setq tokens (push "KW_NIL" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("load") word)
		(progn 
			(setq tokens (push "KW_LOAD" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("disp") word)
		(progn 
			(setq tokens (push "KW_DISP" tokens)) 
	    	(setf buffer '())
		)
	)
	(if (equal '("**") word)
		(progn 
			(setq tokens (push "OP_DBLMULT" tokens)) 
	    	(setf buffer '())
		)
	)
)

(defun isKeyword (token)
	(cond 
		( (equal nil token) nil)
		( (equal '("and") token) T)
		( (equal '("or") token) T)
		( (equal '("not") token) T)
		( (equal '("equal") token) T)
		( (equal '("append") token) T)
		( (equal '("concat") token) T)
		( (equal '("set") token) T)
		( (equal '("deffun") token) T)
		( (equal '("for") token) T)
		( (equal '("less") token) T)
		( (equal '("if") token) T)
		( (equal '("exit") token) T)
		( (equal '("list") token) T)
		( (equal '("true") token) T)
		( (equal '("false") token) T)
		( (equal '("nil") token) T)
		( (equal '("load") token) T)
		( (equal '("disp") token) T)
		( (equal '("**") token) T)
		(nil)
	)	
)

(defun isOperator (token)
	(cond 
		( (char= #\( token) T)
		( (char= #\) token) T)
		( (char= #\+ token) T)
		( (char= #\- token) T)
		( (char= #\* token) T)
		( (char= #\/ token) T)
		( (char= #\; token) T)
		( (char= #\, token) T)
		( (char= #\“ token) T)
		( (char= #\” token) T)
		(nil)
	)
)

(defun addOperator (token)
	(if (char= #\( token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_OP" tokens))
			)
		T
	)
	(if (char= #\) token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_CP" tokens))
			)
		T
	)
	(if (char= #\+ token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_PLUS" tokens))
			)
		T
	)
	(if (char= #\- token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_MINUS" tokens))
			)
		T
	)
	(if (char= #\* token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_MULT" tokens))
			)
		T
	)
	(if (char= #\/ token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_DIV" tokens))
			)
		T
	)
	(if (char= #\; token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_COMMENT" tokens))
			)
		T
	)
	(if (char= #\, token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_COMMA" tokens))
			)
		T
	)
	(if (char= #\“ token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_OC" tokens))
			)
		T
	)
	(if (char= #\” token)
		(progn
			(setq c (string token))
	    	(setq tokens (push "OP_CC" tokens))
			)
		T
	)
	(setf buffer '())
)

(defun codeReader (filename)
    (with-open-file (stream filename)
      	(loop while (peek-char nil stream nil nil)
           	collect (read-char stream)
        )
    )
)

(main "test.txt")


