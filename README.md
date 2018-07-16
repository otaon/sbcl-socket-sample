# sbcl-socket-sample
sbclでsocket通信のサンプル

## ソケット通信の手順
大抵のソケット通信用パッケージおよびライブラリは、下記のステップごとに関数などが用意されている。  
これらの関数にわたすパラメータを適切に設定してやることで、INETドメインソケットやUNIXドメインソケットが使用できる。

```
はじめ
|
v
socket()        (1)ソケット生成
|
v
bind()          (2)ソケット登録
|
v
listen()        (3)ソケット接続準備
|
v
accept()        (4)ソケット接続待機	<--- 接続要求
|
v
read()/write()  (5)受信/送信 <---(データ)---> クライアント側プログラム
|
v
close()         (6)ソケット切断
|
v
終わり
```

## 動作例
### server側
```bash
$ ros run
* (load "server")
-*- socket bound -*-
object:socket : #<INET-SOCKET 127.0.0.1:8080, fd: 5 {100310DF03}>
-*- socket ready to listen -*-
object:socket : #<INET-SOCKET 127.0.0.1:8080, fd: 5 {100310DF03}>

# ここでクライアント側からの接続要求を待つ

-*- socket establish a connection now -*-
object:connection : #<INET-SOCKET 127.0.0.1:8080, peer: 127.0.0.1:49847, fd: 6 {10031F6673}>
client-address    : #(127 0 0 1)
------------------------------------------------------
local info        : address : #(127 0 0 1)
                  : port    : 8080
peer info         : address : #(127 0 0 1)
                  : port    : 49847
------------------------------------------------------
-*- message received -*-
------------------------------------------------------
data in buffer    : how do you do?                                                                                     
message length    : 15
end point         : #(0 0 0 0)
client port       : 0
------------------------------------------------------
-*- disconnecting the socket -*-
T
* 
```

### client側
```bash
$ ros run
* (load "client")
"how do you do?"

T
* 
```

## 参考文献
- 国立情報学研究所 [コネクション型通信：サーバプログラムの作成](http://research.nii.ac.jp/~ichiro/syspro98/server.html)
- Implementation Notes for GNU CLISP [32.4. Socket Streams](https://clisp.sourceforge.io/impnotes/socket.html)
- SBCL 1.4.9 User Manual [14 Networking](http://www.sbcl.org/manual/#Networking)
- Github [sbcl/contrib/sb-bsd-sockets/sockets.lisp](https://github.com/sbcl/sbcl/blob/master/contrib/sb-bsd-sockets/sockets.lisp)
- Qiita [Common Lispでsocketプログラミング(前編)](https://qiita.com/t-cool/items/e0eef9678bbb197e36d2)
- Qiita [Common Lispでsocketプログラミング(後編)](https://qiita.com/t-cool/items/1d9f6ec37c4b00153eab)
- Shammerism [2011-05-23 CLISP echo-server & echo-client](http://d.hatena.ne.jp/shammer/20110523/p1)
- Shammerism [2011-06-13 sbcl で Socket を使う](http://d.hatena.ne.jp/shammer/20110613/p1)
- Shammerism [2011-06-17 sbcl で socket からアドレスとポートの情報を取得する](http://d.hatena.ne.jp/shammer/20110617/p1)
- Shammerism [2011-07-25 sbcl http GET client](http://d.hatena.ne.jp/shammer/20110725/p1)
- ほんとのこと知りたいだけなのに。 [SBCL のマニュアル 14 Networking を俯瞰してみる。](http://yanqirenshi.hatenablog.com/entry/2016/12/05/SBCL_%E3%81%AE%E3%83%9E%E3%83%8B%E3%83%A5%E3%82%A2%E3%83%AB_14_Networking_%E3%82%92%E4%BF%AF%E7%9E%B0%E3%81%97%E3%81%A6%E3%81%BF%E3%82%8B%E3%80%82)
