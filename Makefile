develop:
	clojure -A:fig:build
.PHONY: develop

release:
	clojure -m cljs.main --optimizations advanced -c colorpicker.core
.PHONY: release
