#|
 ======================================================
 SISTEMA DE SIMULAÇÃO DE EVOLUÇÃO EM AMBIENTES VIRTUAIS
 Linguagens e paradigmas de programação - Trabalho1 - Lisp
 Autores: 
 Tema 14: Simulação de Evolução em Ambientes Virtuais
 ======================================================
|#

#|
NOTE
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

;Tamanho, Capacidade máxima 

;IdOrganismo, Posição, Genes, Energia, Idade 
;Genes - Guardar informações em % da probabilidade dele achar comida, probabilidade de achar comida mais proxima, probabilidade de reproduzir

;IdRecurso, Posição

;Função Principal do programa

(defun MAIN()
  (let ((ambiente (list 0 0)) (organismos nil) (recursos nil))
  (menu_principal ambiente organismos recursos))
)

(defun menu_principal(ambiente organismos recursos)
  (let ((escolha 0))
    (loop while(not (equal escolha 8))
      do
        (format t "~%~%~%1 - Inicializar ambiente~%")
        (format t "2 - Adicionar Organismo~%")
        (format t "3 - Adicionar Recursos~%")
        (format t "4 - Executar Simulação~%")
        (format t "5 - Ver Estado atual~%")
        (format t "6 - Ver Estatísticas~%")
        (format t "7 - Resetar Simulação~%")
        (format t "8 - Sair~%~%")
        (format t "Escolha uma opção: ")
        (setf escolha (read))
        (setf (values ambiente organismos recursos)
        (processar_escolha escolha ambiente organismos recursos))
    )
  )
)



(defun processar_escolha(escolha ambiente organismos recursos)
  (cond 
    (
      (equal escolha 1) (inicializar_ambiente ambiente)
    )
    (
      (equal escolha 2) (adicionar_organismos organismos) 
    )
    (
      (equal escolha 3) (adicionar_recursos recursos ambiente) 
    )
    (
      (equal escolha 4) (format t "4~%") 
    )
    (
      (equal escolha 5) (format t "5~%") 
    )
    (
      (equal escolha 6) (estatisticas organismos recursos ambiente) 
    )
    (
      (equal escolha 7) (resetar) 
    )
    (
      (equal escolha 8) (format t "Saindo do programa...")
    )
  )
)

(
  defun inicializar_ambiente(ambiente)
    (format t "~%~%Qual o tamanho? (ex: 100)  ")
    (setf (nth 0 ambiente) (read))
    (format t "~%Qual a capacidade máxima? (ex: 50) ")
    (setf (nth 1 ambiente) (read))
 )


(
  defun adicionar_organismos(organismos)
    (format t "~%~%Qual a quantidade?  ")
    (setf quantidade (read))
    (dotimes (i quantidade)
      (gerar_genes)
    ) 
 )

(
  defun gerar_genes()
    (setf gene1 (random 100))
    (print gene1)
)

;Todas as variáveis se tornam NIL para resetar a simulação
(defun resetar(organismos ambiente recursos)
  (setf organismos nil)
  (setf recursos nil)
  (setf ambiente nil)
)

(
  defun adicionar_recursos(recursos ambiente)
  (format t "~%~%Qual a quantidade? ")
  (setf quantidade (read))
  (dotimes (i quantidade)
  (setf x (random (nth 0 ambiente)))
  (setf y (random (nth 0 ambiente)))
  (push (list x y) recursos))
)

(
  defun estatisticas(organismos recursos ambiente)
    (format t "~%~%Ambiente: ~%")
    (format t "Tamanho:~A  Capacidade:~A~%~%" (nth 0 ambiente) (nth 1 ambiente))
    (format t "~%~%Organismos: ~%")
    (dolist (organismo organismos)
      (format t "~A~%" organismo)
    )
    (format t "~%~%Recursos: ~%")
    (dolist (recurso recursos)
      (format t "~A~%" recurso)
    )
)







