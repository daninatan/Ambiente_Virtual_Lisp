#|
 ======================================================
 SISTEMA DE SIMULAÇÃO DE EVOLUÇÃO EM AMBIENTES VIRTUAIS
 Linguagens e paradigmas de programação - Trabalho1 - Lisp
 Autores: 
 Tema 14: Simulação de Evolução em Ambientes Virtuais
 ======================================================
|#

;Ambiente
;Tamanho do ambiente, Ultimo id de recursos, Ultimo id organismo

;Organismo
;IdOrganismo, Genes, Posição, Energia 
;Genes - Força (Para competir por comida), Eficiencia para achar alimento, Sexo 

;Recurso
;IdRecurso, Posição

;Função Principal do programa
(defun MAIN()
  (let ((ambiente (list 0 0 0 0)) (organismos nil) (recursos nil))
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
     (executar_simulacao ambiente organismos recursos)
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
      (let ((novo-organismo (gerar_organismo ambiente)))
        (setf organismos (append organismos (list novo-organismo))))
        (setf (nth 3 ambiente) (+ 1 (nth 3 ambiente))))
    (format t "~A organismos adicionados!~%" quantidade)
    (values ambiente organismos recursos)))

(defun gerar_organismo(ambiente)
  (let ((organismo nil) (id (+ 1 (nth 3 ambiente))) (genes nil) (posicao (list (random (nth 0 ambiente)) (random (nth 1 ambiente)))))
    (setf genes (gerar_genes))
    (setf organismo (list id genes posicao 50)) ;50 seria a energia base, que todos os indivíduos nascem
    organismo
  )
)

(defun gerar_genes()
  (let ((genes (list 0 0 0)) (forca 0) (sexo 0) (eficiencia 0))
    (setf forca (random 100))
    (setf sexo (random 2))
    (setf eficiencia (random 100))
    (setf genes (list forca eficiencia sexo))
    genes
  )
) 

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
        (let ((quantidade (read)) (id (nth 2 ambiente)))
          (dotimes (i quantidade)
            (let ((x (random (first ambiente)))
                  (y (random (first ambiente))))
              (setf recursos (append recursos (list (list (+ 1 id) x y))))
              (setf id (+ 1 id))))
          (format t "~A recursos adicionados!~%" quantidade)
                  (if (>= (length recursos) 3)
                      (subseq recursos 0 3)
                      recursos)
                  (setf (nth 2 ambiente) (+ quantidade (nth 2 ambiente))))
          (values ambiente organismos recursos))))

(defun estatisticas(ambiente organismos recursos)
  (format t "~%~%=== ESTATÍSTICAS ===~%")
  (format t "~%Ambiente:~%")
  (format t "  Tamanho: ~A~%" (first ambiente))
  (format t "  Capacidade: ~A~%" (second ambiente))
  (format t "~%Organismos (~A total):~%" (length organismos))
  (if organismos
      (dolist (organismo organismos)
        (format t "ID: ~A  Genes: ~A Posição: ~A Energia: ~A ~%" (first organismo) (second organismo) (third organismo) (fourth organismo)))
      (format t "  Nenhum organismo adicionado~%"))
  (format t "~%Recursos (~A total):~%" (length recursos))
  (if recursos
      (dolist (recurso recursos)
        (format t "ID: ~A  Posição: (~A, ~A)~%" (first recurso) (second recurso) (third recurso)))
      (format t "  Nenhum recurso adicionado~%")))

(defun executar_simulacao (ambiente organismos recursos)
  (let ((rodadas 0))
    (format t "~%~%Quantas rodadas? ")
    (setf rodadas (read))
    (dotimes (i rodadas)
      (format t "~%Rodada ~A" (+ i 1))
    )
  ) 
)
