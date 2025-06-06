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
    (multiple-value-setq (ambiente organismos recursos)
      (menu_principal ambiente organismos recursos))))

(defun menu_principal(ambiente organismos recursos)
  (let ((escolha 0))
    (loop while (not (equal escolha 8))
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
        (multiple-value-setq (ambiente organismos recursos)
              (processar_escolha escolha ambiente organismos recursos)))
    (values ambiente organismos recursos)))

(defun processar_escolha(escolha ambiente organismos recursos)
  (cond 
    ((equal escolha 1) 
     (inicializar_ambiente ambiente organismos recursos))
    ((equal escolha 2) 
     (adicionar_organismos ambiente organismos recursos))
    ((equal escolha 3) 
     (adicionar_recursos ambiente organismos recursos))
    ((equal escolha 4) 
     (format t "4~%") 
     (values ambiente organismos recursos))
    ((equal escolha 5) 
     (format t "5~%")
     (values ambiente organismos recursos))
    ((equal escolha 6) 
     (estatisticas ambiente organismos recursos)
     (values ambiente organismos recursos))
    ((equal escolha 7) 
     (resetar))
    ((equal escolha 8) 
     (format t "Saindo do programa...")
     (values ambiente organismos recursos))
    (t 
     (format t "Opção inválida!")
     (values ambiente organismos recursos))))

(defun inicializar_ambiente(ambiente organismos recursos)
  (format t "~%~%Qual o tamanho? (ex: 100)  ")
  (setf (first ambiente) (read))
  (format t "~%Qual a capacidade máxima? (ex: 50) ")
  (setf (second ambiente) (read))
  (format t "~%Ambiente inicializado com sucesso!~%")
  (values ambiente organismos recursos))

(defun adicionar_organismos(ambiente organismos recursos)
  (format t "~%~%Qual a quantidade?  ")
  (let ((quantidade (read)))
    (dotimes (i quantidade)
      (let ((novo-organismo (gerar_genes)))
        (setf organismos (append organismos (list novo-organismo)))))
    (format t "~A organismos adicionados!~%" quantidade)
    (values ambiente organismos recursos)))

(defun gerar_genes()
  (let ((gene1 (random 100)))
    (format t "Gene gerado: ~A~%" gene1)
    gene1))

(defun resetar()
  (let ((ambiente (list 0 0))
        (organismos nil)
        (recursos nil))
    (format t "Simulação resetada!~%")
    (values ambiente organismos recursos)))

(defun adicionar_recursos(ambiente organismos recursos)
  (if (= (first ambiente) 0)
      (progn
        (format t "~%Erro: Ambiente não foi inicializado!~%")
        (values ambiente organismos recursos))
      (progn
        (format t "~%~%Qual a quantidade? ")
        (let ((quantidade (read)))
          (dotimes (i quantidade)
            (let ((x (random (first ambiente)))
                  (y (random (first ambiente))))
              (setf recursos (append recursos (list (list x y))))))
          (format t "~A recursos adicionados!~%" quantidade)
                  (if (>= (length recursos) 3)
                      (subseq recursos 0 3)
                      recursos))
          (values ambiente organismos recursos)))))

(defun estatisticas(ambiente organismos recursos)
  (format t "~%~%=== ESTATÍSTICAS ===~%")
  (format t "~%Ambiente:~%")
  (format t "  Tamanho: ~A~%" (first ambiente))
  (format t "  Capacidade: ~A~%" (second ambiente))
  (format t "~%Organismos (~A total):~%" (length organismos))
  (if organismos
      (dolist (organismo organismos)
        (format t "  Gene: ~A~%" organismo))
      (format t "  Nenhum organismo adicionado~%"))
  (format t "~%Recursos (~A total):~%" (length recursos))
  (if recursos
      (dolist (recurso recursos)
        (format t "  Posição: (~A, ~A)~%" (first recurso) (second recurso)))
      (format t "  Nenhum recurso adicionado~%")))
