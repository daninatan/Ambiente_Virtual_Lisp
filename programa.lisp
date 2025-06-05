#|
 ======================================================
 SISTEMA DE SIMULAÇÃO DE EVOLUÇÃO EM AMBIENTES VIRTUAIS
 Linguagens e paradigmas de programação - Trabalho1 - Lisp
 Autores: 
 Tema 14: Simulação de Evolução em Ambientes Virtuais
 ======================================================
|#

#|
Notas: 
  Vou ter que adicionar um menu com as seguintes opções:
1 - Inicializar Ambiente
  Tamanho do ambiente, sua capacidade total, a temperatura e a umidade
2 - Adicionar Organismos
  Adicionar a quantidade e o tipo
3 - Adicionar Recursos
  Adicionar a quantidade e o tipo de recurso
4 - Executar Simulação
  Explicitar por quantos ciclos
5 - Ver estado atual
6 - Ver estatísticas
7 - Resetar simulação
8 - Sair

|#

;Tamanho, Capacidade máxima, Temperatura e Umidade
(setf ambiente (list 0 0 0 0))

(defun main()
  (menu_principal)
)

(defun menu_principal()
  (let ((escolha 0))
    (loop while(not (equal escolha 8))
      do
        (format t "1 - Inicializar ambiente~%")
        (format t "2 - Adicionar Organismo~%")
        (format t "3 - Adicionar Recursos~%")
        (format t "4 - Executar Simulação~%")
        (format t "5 - Ver Estado atual~%")
        (format t "6 - Ver Estatísticas~%")
        (format t "7 - Resetar Simulação~%")
        (format t "8 - Sair~%~%")
        (format t "Escolha uma opção: ")
        (setf escolha (read))
        (setf a 1)
        (processar_escolha escolha)
    )
  )
)



(defun processar_escolha(escolha)
  (cond 
    (
      (equal escolha 1) (format t "1~%")
    )
    (
      (equal escolha 2) (format t "2~%") 
    )
    (
      (equal escolha 3) (format t "3~%") 
    )
    (
      (equal escolha 4) (format t "4~%") 
    )
    (
      (equal escolha 5) (format t "5~%") 
    )
    (
      (equal escolha 6) (format t "6~%") 
    )
    (
      (equal escolha 7) (format t "7~%") 
    )
    (
      (equal escolha 8) ()
    )
  )
)







