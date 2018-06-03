
##### Settings #####
OUT="../out"
EXEC=$(OUT)/brianweb

##### Objects #####
##### Flags #####
##### Dependencies #####
##### Static targets #####

all:
	$(call compiling_msg,"")
	@mkdir -p $(OUT)
	ghc --make -threaded brianweb.hs -o $(EXEC) -odir $(OUT) -hidir $(OUT)

clean:
	@pkill brianweb || true
	$(call redecho,"cleaning\n")
	rm -rf $(OUT)

run: all
	$(EXEC) &

##### Utilities #####

define compiling_msg
	@printf "["
	$(call cyanecho,"Compiling")
	@printf "]: $1\n"
endef

define redecho
	$(call colorecho,1,$1)
endef

define greenecho
	$(call colorecho,2,$1)
endef

define yellowecho
	$(call colorecho,3,$1)
endef

define blueecho
	$(call colorecho,4,$1)
endef

define magentaecho
	$(call colorecho,5,$1)
endef

define cyanecho
	$(call colorecho,6,$1)
endef

define colorecho
      @tput setaf $1
      @printf $2
      @tput sgr0
endef

