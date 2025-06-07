#lang racket

;; --- Definiciones del Lenguaje Python (Simplificado) ---
(define python-keywords
  '("False" "None" "True" "and" "as" "assert" "async" "await" "break"
    "class" "continue" "def" "del" "elif" "else" "except" "finally"
    "for" "from" "global" "if" "import" "in" "is" "lambda" "nonlocal"
    "not" "or" "pass" "raise" "return" "try" "while" "with" "yield"))

; Operadores ordenados de más largo a más corto para "longest match"
(define python-operators
  '("**=" "//=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" ">>=" "<<=" ; Asignación compuesta
    "**" "//" "==" "!=" "<=" ">=" "->" ; Otros multi-caracter
    "+" "-" "*" "/" "%" "&" "|" "^" "~" "<" ">" "=" "@" ":" )) ; Un solo caracter

(define python-delimiters
  '(";" "(" ")" "[" "]" "{" "}" "," ".")) ; Punto también puede ser parte de float

;; --- Funciones Auxiliares ---

(define (char-letter? c)
  (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))

(define (char-digit? c)
  (char<=? #\0 c #\9))

(define (char-alphanumeric? c)
  (or (char-letter? c) (char-digit? c)))

(define (char-underscore? c)
  (char=? c #\_))

(define (char-identifier-start? c)
  (or (char-letter? c) (char-underscore? c)))

(define (char-identifier-part? c)
  (or (char-alphanumeric? c) (char-underscore? c)))

(define (char-whitespace? c)
  (memv c '(#\space #\tab #\newline #\return)))

; Consume caracteres mientras el predicado sea verdadero
; Retorna: (values string-consumido lista-restante)
(define (consume-while pred char-list)
  (let loop ((ls char-list) (acc '()))
    (if (or (empty? ls) (not (pred (first ls))))
        (values (list->string (reverse acc)) ls)
        (loop (rest ls) (cons (first ls) acc)))))

;; --- Reconocedores de Tokens ---

; Ignora espacios en blanco, pero retorna un tipo para consistencia si se quisiera tokenizar
(define (recognize-whitespace char-list)
  (if (and (not (empty? char-list)) (char-whitespace? (first char-list)))
      (let-values (((token rest-chars) (consume-while char-whitespace? char-list)))
        (values token 'ESPACIOBLANCO rest-chars))
      (values #f #f char-list))) ; No es whitespace

(define (recognize-comment char-list)
  (if (and (not (empty? char-list)) (char=? (first char-list) #\#))
      (let loop ((ls (rest char-list)) (acc '(#\#)))
        (if (or (empty? ls) (char=? (first ls) #\newline))
            (values (list->string (reverse acc)) 'COMENTARIO ls)
            (loop (rest ls) (cons (first ls) acc))))
      (values #f #f char-list))) ; No es comentario

(define (recognize-identifier-or-keyword char-list)
  (if (and (not (empty? char-list)) (char-identifier-start? (first char-list)))
      (let-values (((token rest-c) (consume-while char-identifier-part? (rest char-list))))
        (let ((full-token (string-append (string (first char-list)) token)))
          (if (member full-token python-keywords)
              (values full-token 'PALABRA_CLAVE rest-c)
              (values full-token 'IDENTIFICADOR rest-c))))
      (values #f #f char-list)))

; Motor de AFD para números (basado en Actividad 3.2)
(define (recognize-number input-chars)
  ; Define all states first
  (define (state-q0 current-char-list acc type) ; initial
    (if (empty? current-char-list)
        (if (null? acc) 
            (values #f #f input-chars) 
            (values (list->string (reverse acc)) type current-char-list))
        (let ((char (first current-char-list)))
          (cond
            ((char=? char #\-) (state-q1 (rest current-char-list) (cons char acc) type))
            ((char-digit? char) (state-q2 (rest current-char-list) (cons char acc) type))
            ((char=? char #\.) (state-q3 (rest current-char-list) (cons char acc) 'FLOAT))
            (else (if (null? acc) 
                     (values #f #f input-chars) 
                     (values (list->string (reverse acc)) type current-char-list)))))))

  (define (state-q1 current-char-list acc type) ; after '-'
    (if (empty? current-char-list)
        (values #f #f input-chars)
        (let ((char (first current-char-list)))
          (cond
            ((char-digit? char) (state-q2 (rest current-char-list) (cons char acc) type))
            ((char=? char #\.) (state-q3 (rest current-char-list) (cons char acc) 'FLOAT))
            (else (values #f #f input-chars))))))

  (define (state-q2 current-char-list acc type) ; integer part
    (if (empty? current-char-list)
        (values (list->string (reverse acc)) type current-char-list)
        (let ((char (first current-char-list)))
          (cond
            ((char-digit? char) (state-q2 (rest current-char-list) (cons char acc) type))
            ((char=? char #\.) (state-q3 (rest current-char-list) (cons char acc) 'FLOAT))
            ((or (char=? char #\E) (char=? char #\e)) (state-q5 (rest current-char-list) (cons char acc) 'FLOAT))
            (else (values (list->string (reverse acc)) type current-char-list))))))

  (define (state-q3 current-char-list acc type) ; after '.'
    (if (empty? current-char-list)
        (if (or (string=? "." (list->string (reverse acc))) 
                (string=? "-." (list->string (reverse acc))))
            (values #f #f input-chars)
            (values (list->string (reverse acc)) type current-char-list))
        (let ((char (first current-char-list)))
          (cond
            ((char-digit? char) (state-q4 (rest current-char-list) (cons char acc) type))
            ((or (char=? char #\E) (char=? char #\e)) (state-q5 (rest current-char-list) (cons char acc) 'FLOAT))
            (else (if (or (string=? "." (list->string (reverse acc))) 
                         (string=? "-." (list->string (reverse acc))))
                     (values #f #f input-chars)
                     (values (list->string (reverse acc)) type current-char-list)))))))

  (define (state-q4 current-char-list acc type) ; decimal part
    (if (empty? current-char-list)
        (values (list->string (reverse acc)) type current-char-list)
        (let ((char (first current-char-list)))
          (cond
            ((char-digit? char) (state-q4 (rest current-char-list) (cons char acc) type))
            ((or (char=? char #\E) (char=? char #\e)) (state-q5 (rest current-char-list) (cons char acc) 'FLOAT))
            (else (values (list->string (reverse acc)) type current-char-list))))))

  (define (state-q5 current-char-list acc type) ; after 'E' or 'e'
    (if (empty? current-char-list)
        (values #f #f input-chars)
        (let ((char (first current-char-list)))
          (cond
            ((or (char=? char #\+) (char=? char #\-)) (state-q6 (rest current-char-list) (cons char acc) type))
            ((char-digit? char) (state-q7 (rest current-char-list) (cons char acc) type))
            (else (values #f #f input-chars))))))

  (define (state-q6 current-char-list acc type) ; after E+ or E-
    (if (empty? current-char-list)
        (values #f #f input-chars)
        (let ((char (first current-char-list)))
          (if (char-digit? char)
              (state-q7 (rest current-char-list) (cons char acc) type)
              (values #f #f input-chars)))))

  (define (state-q7 current-char-list acc type) ; exponent digits
    (if (empty? current-char-list)
        (values (list->string (reverse acc)) type current-char-list)
        (let ((char (first current-char-list)))
          (if (char-digit? char)
              (state-q7 (rest current-char-list) (cons char acc) type)
              (values (list->string (reverse acc)) type current-char-list)))))

  ; Start the DFA with initial state
  (state-q0 input-chars '() 'INTEGER))


(define (string-starts-with? str prefix)
  (let ((n (string-length prefix))
        (m (string-length str)))
    (and (>= m n)
         (string=? prefix (substring str 0 n)))))

(define (recognize-operator char-list operators-sorted)
  (if (empty? char-list)
      (values #f #f char-list)
      (let loop ((ops operators-sorted))
        (if (empty? ops)
            (values #f #f char-list) ; Ningún operador coincidió
            (let* ((op-str (first ops))
                   (op-len (string-length op-str)))
              (if (>= (length char-list) op-len)
                  (let ((candidate-str (list->string (take char-list op-len))))
                    (if (string=? candidate-str op-str)
                        (values op-str 'OPERADOR (drop char-list op-len))
                        (loop (rest ops))))
                  (loop (rest ops)))))))) ; No hay suficientes caracteres para este operador

(define (recognize-delimiter char-list delimiters-list)
    (if (empty? char-list)
        (values #f #f char-list)
        (let ((c-str (string (first char-list))))
          (if (member c-str delimiters-list)
              (values c-str 'DELIMITADOR (rest char-list))
              (values #f #f char-list)))))

(define (recognize-string-literal char-list)
  (if (empty? char-list)
      (values #f #f char-list)
      (let ((quote-char (first char-list)))
        (if (or (char=? quote-char #\") (char=? quote-char #\'))
            (let loop ((ls (rest char-list)) (acc (list quote-char)) (escape #f))
              (cond
                ((empty? ls) (values #f #f char-list)) ; String no terminado
                (escape (loop (rest ls) (cons (first ls) acc) #f))
                ((char=? (first ls) #\\) (loop (rest ls) (cons (first ls) acc) #t))
                ((char=? (first ls) quote-char)
                 (values (list->string (reverse (cons (first ls) acc))) 'CADENA (rest ls)))
                (else (loop (rest ls) (cons (first ls) acc) #f))))
            (values #f #f char-list))))) ; No es un string literal

;; --- Motor Léxico Principal ---
(define (lexer-python-scanner char-list original-text-length)
  (if (empty? char-list)
      '()
      (let ((current-pos (- original-text-length (length char-list))))
        ;; Primero, intentar consumir whitespace (que se ignora para la lista de tokens)
        (let-values (((ws-tok ws-type rest-after-ws) (recognize-whitespace char-list)))
          (if ws-tok
              (lexer-python-scanner rest-after-ws original-text-length) ; Recursión sin añadir token
              ;; Si no es whitespace, intentar otros tokens
              (let-values (((comm-tok comm-type rest-after-comm) (recognize-comment char-list)))
                (if comm-tok
                    (cons (list comm-tok comm-type) (lexer-python-scanner rest-after-comm original-text-length))
                (let-values (((str-tok str-type rest-after-str) (recognize-string-literal char-list)))
                  (if str-tok
                      (cons (list str-tok str-type) (lexer-python-scanner rest-after-str original-text-length))
                (let-values (((id-kw-tok id-kw-type rest-after-idkw) (recognize-identifier-or-keyword char-list)))
                  (if id-kw-tok
                      (cons (list id-kw-tok id-kw-type) (lexer-python-scanner rest-after-idkw original-text-length))
                  (let-values (((num-tok num-type rest-after-num) (recognize-number char-list))) ; Antes de operador para el '-'
                    (if num-tok
                        (cons (list num-tok num-type) (lexer-python-scanner rest-after-num original-text-length))
                    (let-values (((op-tok op-type rest-after-op) (recognize-operator char-list python-operators)))
                      (if op-tok
                          (cons (list op-tok op-type) (lexer-python-scanner rest-after-op original-text-length))
                      (let-values (((del-tok del-type rest-after-del) (recognize-delimiter char-list python-delimiters)))
                        (if del-tok
                            (cons (list del-tok del-type) (lexer-python-scanner rest-after-del original-text-length))
                            ;; Error: Caracter no reconocido
                            (begin
                              (eprintf "Error Léxico: Caracter '~a' no reconocido en la posición ~a.~n"
                                       (first char-list) current-pos)
                              (cons (list (string (first char-list)) 'ERROR)
                                    (lexer-python-scanner (rest char-list) original-text-length))))))))))))))))))))

(define (lexer-python file-path)
  (if (not (file-exists? file-path))
      (eprintf "Error: El archivo '~a' no existe.~n" file-path)
      (let* ((file-content (file->string file-path))
             (char-list (string->list file-content))
             (tokens (lexer-python-scanner char-list (string-length file-content)))
             (html-path "resultado_lexer.html"))
        (call-with-output-file html-path
          (lambda (out)
            (fprintf out "<!DOCTYPE html>\n<html lang='es'>\n<head>\n")
            (fprintf out "<meta charset='UTF-8'>\n<title>Resultado del Lexer</title>\n")
            (fprintf out "<style>\n")
            ;; Aquí insertamos el CSS proporcionado
            (fprintf out "body {\n  font-family: \"Segoe UI\", Tahoma, Geneva, Verdana, sans-serif;\n  margin: 0;\n  padding: 20px;\n  background-color: #f5f5f5;\n}\n")
            (fprintf out ".container {\n  max-width: 1000px;\n  margin: 0 auto;\n  background-color: white;\n  padding: 20px;\n  border-radius: 8px;\n  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);\n}\n")
            (fprintf out "h1 {\n  color: #2c3e50;\n  text-align: center;\n  margin-bottom: 30px;\n}\n")
            (fprintf out ".file-info {\n  background-color: #edf2f7;\n  padding: 15px;\n  border-radius: 6px;\n  margin-bottom: 20px;\n}\n")
            (fprintf out ".file-info h2 {\n  margin: 0;\n  font-size: 1.2em;\n  color: #4a5568;\n}\n")
            (fprintf out "table {\n  width: 100%;\n  border-collapse: collapse;\n  margin-top: 20px;\n}\n")
            (fprintf out "th, td {\n  padding: 12px;\n  text-align: left;\n  border-bottom: 1px solid #e2e8f0;\n}\n")
            (fprintf out "th {\n  background-color: #2c3e50;\n  color: white;\n  font-weight: 600;\n}\n")
            (fprintf out "tr:nth-child(even) {\n  background-color: #f8fafc;\n}\n")
            (fprintf out "tr:hover {\n  background-color: #edf2f7;\n}\n")
            (fprintf out ".token-PALABRA_CLAVE { color: #8e44ad; }\n")
            (fprintf out ".token-IDENTIFICADOR { color: #2980b9; }\n")
            (fprintf out ".token-OPERADOR { color: #c0392b; }\n")
            (fprintf out ".token-CADENA { color: #27ae60; }\n")
            (fprintf out ".token-ENTERO { color: #d35400; }\n")
            (fprintf out ".token-DECIMAL { color: #e67e22; }\n")
            (fprintf out ".token-COMENTARIO { color: #7f8c8d; font-style: italic; }\n")
            (fprintf out ".token-DELIMITADOR { color: #2c3e50; }\n")
            (fprintf out ".token-ERROR { color: #e74c3c; font-weight: bold; }\n")
            (fprintf out "</style>\n</head>\n<body>\n")
            (fprintf out "<div class='container'>\n")
            (fprintf out "<h1>Tokens del archivo analizado</h1>\n")
            (fprintf out "<div class='file-info'><h2>Archivo: ~a</h2></div>\n" file-path)
            (fprintf out "<table>\n<tr><th>Token</th><th>Tipo</th></tr>\n")
            (for-each
             (lambda (token-info)
               (let* ((token (string-replace (first token-info) "<" "&lt;"))
                      (type (symbol->string (second token-info))))
                 (fprintf out "<tr><td class='token-~a'>~a</td><td>~a</td></tr>\n"
                          type token type)))
             tokens)
            (fprintf out "</table>\n</div>\n</body>\n</html>"))
          #:exists 'replace))))
;; --- Pruebas ---
; Crear un archivo de prueba "test_python.py" con el siguiente contenido:
;
; def my_func(x, y): # Esto es una funcion
;   res = x + y * 2.0 - 3.14E-2
;   name = "Python_Lexer_Test"
;   if x > y and (x != 0):
;     return res + 10 # Retorno
;   # Fin
;   numbers = -10.5
;   another = .5
;   integer = 100
;   num_only_dot_after = 23.
;   num_only_dot_before = .007
;   negative_int = -50

(lexer-python "test_python.py")