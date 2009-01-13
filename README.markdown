RxTx
====

I run a headless version of [Transmission](http://www.transmissionbt.com/) 
on my [router](http://oleg.wl500g.info/) -- RxTx aims to implement basic 
functionality needed to operate the Transmission daemon remotely with 
[its RPC mechanism](http://trac.transmissionbt.com/browser/trunk/doc/rpc-spec.txt).


Tx
--
Tx sends a request to start download of a torrent. When downloading .torrent 
files, I use Tx to open them and then they're downloaded on the remote machine.


Rx
--
The idea is to implement some simple callback functionality in (headless) 
Transmission that sends an HTTP request when a download has finished. Rx will 
be a simple server to receive notification when a download is completed. One use
case is to forward the message to Growl.

