otrainer: otrainer.lisp rule-checker.lisp repertoires.lisp
	sbcl --load otrainer.lisp \
		--eval "(sb-ext:save-lisp-and-die #p\"otrainer\" :toplevel #'otrainer:main :executable t)"

clean:
	rm -f otrainer
