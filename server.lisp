;;; ------------------------------------------------------------------------------------------------
;;; SBCL ソケットを用いたサーバ
;;; ------------------------------------------------------------------------------------------------

;;; SBCLソケットパッケージ
(require :sb-bsd-sockets)

;;; SBCLソケットを用いたサーバパッケージを定義
(defpackage "SBCL-SOCKET-SERVER"
  (:use "COMMON-LISP" "SB-BSD-SOCKETS")
  (:export "START-LISTEN"))

;;; パッケージに入る
(in-package SBCL-SOCKET-SERVER)

;;; サーバのURL
(defparameter host '(127 0 0 1))

(defun start-listen(address port)
  "サーバ側としてソケットを作成する
   address: サーバのURL or IPアドレス
   port: サーバのポート番号"
  (let
    ;; ソケットオブジェクト
    ;; - tcp接続によるinetドメインソケット
    ;; - 他にはUNIXドメインソケットが使用可能
    ((socket (make-instance 'inet-socket     ; inetドメインソケット
                            :type :stream    ; 他にはデータグラムも選択可
                            :protocol :tcp)) ; tcp通信
     ;; ソケットがアクセプトしていないセッションの最大保持数
     (backlog-max 100))

    ;; アドレスを使い回す
    (setf (sockopt-reuse-address socket) t)

    ;; 指定したアドレスおよびポートにソケットをバインド
    (socket-bind socket address port)
    (princ "-*- socket bound -*-") (princ #\newline)
    (princ "object:socket : ") (princ socket) (princ #\newline)

    ;; ブロッキングコール : 通信接続を待つための準備を行う
    (socket-listen socket backlog-max)
    (princ "-*- socket ready to listen -*-") (princ #\newline)
    (princ "object:socket : ") (princ socket) (princ #\newline)

    ;; ブロッキングコール : クライアント側から通信接続が来るまで待つ
    ;; - connection: 接続情報を持つオブジェクト
    ;; - client-addr: 接続先のアドレスの配列
    (multiple-value-bind (connection client-addr) (socket-accept socket)
      (unwind-protect
        (progn
          (princ "-*- socket establish a connection now -*-") (princ #\newline)
          (princ "object:connection : ") (princ connection) (princ #\newline)
          (princ "client-address    : ") (princ client-addr) (princ #\newline)
          (princ "------------------------------------------------------") (princ #\newline)
          (multiple-value-bind (addr port) (socket-name connection)
            (format t "local info        : address : ~A~%" addr)
            (format t "                  : port    : ~A~%" port))
          (multiple-value-bind (addr port) (socket-peername connection)
            (format t "peer info         : address : ~A~%" addr)
            (format t "                  : port    : ~A~%" port))
          (princ "------------------------------------------------------") (princ #\newline)
          (let* ((buffer (make-array backlog-max
                                     ;; 各要素のデータ型は文字(他に'integerが指定可能)
                                     :element-type 'character
                                     ;; 各要素の初期値はスペース「 」
                                     :initial-element #\ ))
                 ;; 受信可能なバッファサイズ
                 (message-length (length buffer)))
            ;; コネクションから送られてきたデータを受け取る
            (multiple-value-bind (buffer message-length end-point port)
              ;; buffer         : メッセージ受信済みのバッファ
              ;; message-length : 受信メッセージ長
              ;; end-point      : エンドポイント情報(接続先情報)
              ;; port           ; ポート番号
              (socket-receive connection buffer message-length)
              (progn
                (princ "-*- message received -*-") (princ #\newline)
                (princ "------------------------------------------------------") (princ #\newline)
                (format t "data in buffer    : ~A~%" buffer)
                (format t "message length    : ~A~%" message-length)
                (format t "end point         : ~A~%" end-point)
                (format t "client port       : ~A~%" port)
                (princ "------------------------------------------------------") (princ #\newline)
                ))))
        (progn
          (format t "-*- disconnecting the socket -*-~%")
          (socket-close connection))))))

;;; サーバ稼働
(start-listen host 8080)
