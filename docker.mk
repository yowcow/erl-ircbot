IMAGE := densuke/ngircd
CIDFILE := ngircd.cid

all:
	docker pull $(IMAGE)

start: $(CIDFILE)

$(CIDFILE):
	docker run --rm -d \
		--cidfile $@ \
		-p 6667:6667 \
		$(IMAGE)

stop:
	-[ -f $(CIDFILE) ] && docker stop $$(cat $(CIDFILE))
	rm -f $(CIDFILE)

.PHONY: all start stop clean
