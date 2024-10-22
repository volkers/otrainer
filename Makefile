otrainer: otrainer.lisp
	sbcl --load otrainer.lisp \
		--eval "(sb-ext:save-lisp-and-die #p\"otrainer\" :toplevel #'otrainer:main :executable t)"

install: otrainer
	mv otrainer ~/bin/otrainer

clean:
	rm -f otrainer
