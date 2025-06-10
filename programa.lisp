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
;Genes - Força (Para competir por comida), Eficiencia para achar alimento, Sexo(1 - Masculino, 0 - Feminino)

;Recurso
;IdRecurso, Posição

;Função Principal do programa
(defun MAIN()
  (let ((ambiente (list 0 0 0 0)) (organismos nil) (recursos nil))
    (multiple-value-setq (ambiente organismos recursos)
      (menu_principal ambiente organismos recursos))))

(defun menu_principal(ambiente organismos recursos)
  (let ((escolha 0))
    (loop while (not (equal escolha 7))
      do
        (format t "~%~%~%1 - Inicializar ambiente~%")
        (format t "2 - Adicionar Organismo~%")
        (format t "3 - Adicionar Recursos~%")
        (format t "4 - Executar Simulação~%")
        (format t "5 - Ver Estatísticas~%")
        (format t "6 - Resetar Simulação~%")
        (format t "7 - Sair~%~%")
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
     (executar_simulacao ambiente organismos recursos))  
    ((equal escolha 5) 
     (estatisticas ambiente organismos recursos)
     (values ambiente organismos recursos))
    ((equal escolha 6) 
     (resetar))
    ((equal escolha 7) 
     (format t "Saindo do programa...")
     (values ambiente organismos recursos))
    (t 
     (format t "Opção inválida!")
     (values ambiente organismos recursos))))

(defun inicializar_ambiente(ambiente organismos recursos)
  (format t "~%~%Qual o tamanho? (ex: 100)  ")
  (setf (first ambiente) (read))
  (format t "~%Ambiente inicializado com sucesso!~%")
  (values ambiente organismos recursos))

(defun adicionar_organismos(ambiente organismos recursos)
  (format t "~%~%Qual a quantidade?  ")
  (let ((quantidade (read)))
    (dotimes (i quantidade)
      (let ((novo-organismo (gerar_organismo ambiente)))
        (setf organismos (append organismos (list novo-organismo))))
        (setf (nth 2 ambiente) (+ 1 (nth 2 ambiente))))
    (format t "~A organismos adicionados!~%" quantidade)
    (values ambiente organismos recursos)))

(defun gerar_organismo(ambiente)
  (let ((organismo nil) (id (+ 1 (nth 2 ambiente))) (genes nil) (posicao (list (random (nth 0 ambiente)) (random (nth 0 ambiente)))))
    (setf genes (gerar_genes))
    (setf organismo (list id genes posicao 30)) ;50 seria a energia base, que todos os indivíduos nascem
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
  (let ((ambiente (list 0 0 0))
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
        (let ((quantidade (read)) (id (nth 1 ambiente)))
          (dotimes (i quantidade)
            (let ((x (random (first ambiente)))
                  (y (random (first ambiente))))
              (setf recursos (append recursos (list (list (+ 1 id) (list x y)))))
              (setf id (+ 1 id))))
          (format t "~A recursos adicionados!~%" quantidade)
                  (if (>= (length recursos) 3)
                      (subseq recursos 0 3)
                      recursos)
                  (setf (nth 1 ambiente) (+ quantidade (nth 1 ambiente))))
          (values ambiente organismos recursos))))


(defun adicionar_recursos_rodada(ambiente recursos)
  (let ((quantidade (round (/ (first ambiente) 10)))
      (id (second ambiente)))
            (dotimes (i quantidade)
              (let ((x (random (first ambiente)))
                    (y (random (first ambiente))))
                (setf recursos (append recursos (list (list (+ 1 id) (list x y)))))
                (setf id (+ 1 id))))
                    (if (>= (length recursos) 3)
                        (subseq recursos 0 3)
                        recursos)
                    (setf (nth 1 ambiente) (+ quantidade (nth 1 ambiente)))
    (values ambiente recursos)))

(defun estatisticas(ambiente organismos recursos)
  (format t "~%~%=== ESTATÍSTICAS ===~%")
  (format t "~%Ambiente:~%")
  (format t "  Tamanho: ~A~%" (first ambiente))
  (format t "~%Organismos (~A total):~%" (length organismos))
  (if organismos
      (dolist (organismo organismos)
        (format t "ID: ~A  Genes: ~A Posição: ~A Energia: ~A ~%" (first organismo) (second organismo) (third organismo) (fourth organismo)))
      (format t "  Nenhum organismo adicionado~%"))
  (format t "~%Recursos (~A total):~%" (length recursos))
  (if recursos
      (dolist (recurso recursos)
        (format t "ID: ~A  Posição: (~A, ~A)~%" (first recurso) (first (second recurso)) (second (second recurso))))
      (format t "  Nenhum recurso adicionado~%")))

;Função de reprodução entre organismos
(defun reproduzir (ambiente organismo1 organismo2)
  (let ((organismo3 (list 0 0 0 0)) 
        (id (+ 1 (nth 2 ambiente))) 
        (genes1 (second organismo1))  ; Pega os genes do organismo1
        (genes2 (second organismo2))  ; Pega os genes do organismo2
        (pos1 (third organismo1))     ; Posição do organismo1
        (pos2 (third organismo2))     ; Posição do organismo2
        (forca 0) 
        (eficiencia 0) 
        (sexo 0)
        (taxa_mutacao 3)
        (quantidade_mutacao 8)
        (mutacao (random 10))
        (gene_mutacao (random 2))
        (sinal_mutacao (random 2))
        (nova_quantidade_mutacao 0))
    
    ; Atualiza o último ID no ambiente
    (setf (nth 2 ambiente) (+ 1 (nth 2 ambiente)))
    
    ; Define sexo aleatoriamente
    (setf sexo (random 2))
    
    ; Calcula genes do filho (média dos pais)
    (setf forca (round (/ (+ (first genes1) (first genes2)) 2)))
    (setf eficiencia (round (/ (+ (second genes1) (second genes2)) 2)))
    
    ; Cria lista de genes do filho
    (setf organismo3 (list forca eficiencia sexo))
    
    ; Define sinal da mutação
    (if (equal sinal_mutacao 1)
        (setf sinal_mutacao 1)
        (setf sinal_mutacao -1))
    
    ; Aplica mutação se sorteada
    (if (< mutacao taxa_mutacao)
        (progn
          (setf nova_quantidade_mutacao (* quantidade_mutacao sinal_mutacao))
          (setf (nth gene_mutacao organismo3) 
                (+ nova_quantidade_mutacao (nth gene_mutacao organismo3)))
          ; Garante que os valores não sejam negativos
          (when (< (nth gene_mutacao organismo3) 0)
            (setf (nth gene_mutacao organismo3) 0))
          ; Garante que eficiência não passe de 100
          (when (and (= gene_mutacao 1) (> (nth gene_mutacao organismo3) 100))
            (setf (nth gene_mutacao organismo3) 100))))
    
    ; Calcula posição do filho (média das posições dos pais)
    (let ((pos_x (/ (+ (first pos1) (first pos2)) 2))
          (pos_y (/ (+ (second pos1) (second pos2)) 2)))
      
      ; Retorna o organismo filho completo: (ID, genes, posição, energia)
      (list id organismo3 (list pos_x pos_y) 30)))) ; Energia inicial de 50




;; Calcula distância Euclidiana
(defun distancia (p1 p2)
    (sqrt (+ (expt (- (first p1) (first p2)) 2)
             (expt (- (second p1) (second p2)) 2))))

;Função para executar simulação
;Em cada iteração, o organismo pode procurar por recursos ou reproduzir | 0 = Reproducao, 1 = Procurar recurso
;Para que ele reproduza, a femea que ele encontrou precisa querer reproduzir também, se não ele vai para proxima
;Caso dois ou mais machos querem reproduzir com a mesma fêmea, deverá haver competição
;Reproduzir gasta 10 de energia
;Procurar por recurso gasta 
;   10 - Se eficiencia <= 40
;   7 - Se eficiencia <= 80 && > 40
;   5 - Se eficiencia <= 100 && > 80
;O organismo sempre vai no recurso mais proximo
;Se dois ou mais individuos buscarem pelo mesmo recurso, eles devem competir, o com a maior força ganha o recurso
;Cada recurso aumenta em 8 a energia 
;Se um individuo tiver energia <= 10, obrigatoriamente deve procurar recurso 
;; Função auxiliar para calcular a distância entre dois pontos

(defun executar_simulacao (ambiente organismos recursos)
  (format t "~%Digite o número de rodadas: ")
  (let ((rodadas (read))
        (org-atuais organismos)
        (rec-atuais recursos))
    (loop for r from 1 to rodadas do
      (format t "~%=== Rodada ~A ===~%" r)
      
      (let ((novos-org nil))
        ;; Para cada organismo
        (dolist (org org-atuais)
          (let* ((energia    (fourth org))
                 (genes      (second org))
                 (sexo       (nth 2 genes))
                 (eficiencia (second genes))
                 (decisao    (if (<= energia 10) 1 (random 2))))
            
            (if (= decisao 1)
                ;; Buscar recurso
                (let ((melhor nil) (dmin nil))
                  (dolist (rc rec-atuais)
                    (let* ((pos-org (third org))
                           (pos-rc  (second rc))
                           (d       (distancia pos-org pos-rc)))
                      (when (or (not dmin) (< d dmin))
                        (setf dmin d
                              melhor rc))))
                  
                  ;; Gasta energia pela tentativa de buscar (independente de encontrar)
                  (decf energia (cond ((<= eficiencia 40) 10)
                                     ((<= eficiencia 80) 7)
                                     (t 5)))
                  
                  ;; Se encontrou recurso, consome e ganha energia
                  (when melhor
                    (setf rec-atuais (remove melhor rec-atuais :test #'equal))
                    (incf energia 8))
                  
                  (setf (fourth org) energia))
              ;; Reproduzir
              (when (= sexo 1)
                (let ((fem  nil) (dmin2 nil))
                  (dolist (pot org-atuais)
                    (let* ((g2  (second pot))
                           (s2  (nth 2 g2))
                           (e2  (fourth pot)))
                      (when (and (= s2 0) (>= e2 20))
                        (let ((d2 (distancia (third org) (third pot))))
                          (when (or (not dmin2) (< d2 dmin2))
                            (setf dmin2 d2
                                  fem pot))))))
                  (when fem
                    (let ((filho (reproduzir ambiente org fem)))
                      (push filho novos-org)
                      (decf (fourth org) 10)
                      (decf (fourth fem) 10))))))))
        
        ;; Atualiza organismos e recursos no fim da rodada
        (setf org-atuais (append
                           (remove-if (lambda (o) (<= (fourth o) 0))
                                      org-atuais)
                           novos-org))
        
        ;; Atualiza recursos adicionando novos (chamando sua função)
        (multiple-value-setq (ambiente rec-atuais)
          (adicionar_recursos_rodada ambiente rec-atuais)))
      
      (format t "~%Fim da rodada ~A. Organismos vivos: ~A, Recursos restantes: ~A~%" 
              r (length org-atuais) (length rec-atuais)))
    
    ;; Retorna os valores atualizados após todas as rodadas
    (values ambiente org-atuais rec-atuais)))
