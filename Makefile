SRC_DIR=src
OUT_DIR=ebin

all: prepare
	erlc -o $(OUT_DIR) $(SRC_DIR)/*.erl

prepare:
	mkdir -p $(OUT_DIR)
clean:
	rm -f $(OUT_DIR)/*.beam

