;;; ------------------------------------------------------------------------------------------------
;;; SBCL ソケットを用いたクライアント
;;; ------------------------------------------------------------------------------------------------

;;; SBCLソケットパッケージ
(require :sb-bsd-sockets)

;;; SBCLソケットを用いたクライアントパッケージを定義
(defpackage "SBCL-SOCKET-CLIENT"
  (:use "COMMON-LISP" "SB-BSD-SOCKETS"))

;;; パッケージに入る
(in-package SBCL-SOCKET-CLIENT)

;;; サーバ側のURIまたはIPアドレス
(defparameter host "localhost")
;(defparameter host "127.0.0.2")

(defun resolve-hostname (hostname)
  "URIまたはIPアドレスの文字列をIPアドレスの配列に解決する
   hostname: ホスト名(URIまたはIPアドレスの文字列)
   ret: IPアドレスの配列"
  (car (host-ent-addresses
         (get-host-by-name hostname))))

(defun start (host port)
  "クライアント側としてソケットを作成しサーバへアクセスする
   host: サーバ側のホスト名(URIまたはIPアドレスの文字列)
   port: ポート番号"
  ;; ソケットオブジェクト
  ;; - tcp接続によるinetドメインソケット
  ;; - サーバと同じ接続方式を指定すること
  (let ((soc (make-instance 'inet-socket
                            :type :stream
                            :protocol :tcp)))
    (unwind-protect
      (progn 
        ;; 標準入力の文字列("test"など)をサーバ側へ送信する
        (let ((str (read)))
          ;; 指定したアドレスおよびポートにソケットをバインドして接続
          (socket-connect soc (resolve-hostname host) port)
          ;; 接続先にメッセージを送信
          (socket-send soc str (1+ (length str)))))
      ;; 最後に必ずソケットをクローズ
      (socket-close soc))))

;;; クライアント側開始
;;; !! サーバ側起動後に実行すること !!
(start "localhost" 8080)

