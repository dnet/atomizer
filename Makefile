SRC_DIR=src
OUT_DIR=ebin

all: clean prepare
	erlc -o $(OUT_DIR) $(SRC_DIR)/*.erl

prepare:
	mkdir $(OUT_DIR)
clean:
	rm -rf $(OUT_DIR)

