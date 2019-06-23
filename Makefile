## Makefile

# This program is free software: you can redistribute it and/or modify
# it under the terms of the Affero GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
# GNU General Public License for more details.

# You should have received a copy of the Affero GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

all:

include Makefunc.mk

DIRS := json

EMACS := conao3/emacs:alpine-min-26.2

##################################################

.PHONY: all make-json
all: make-json

##############################

$(DIRS):
	mkdir -p $@

make-json: json json/shingeta-layout.json

json/shingeta-layout.json: karabiner-json.el
	docker run --rm -it -v $$(pwd)/:/karabiner $(EMACS) sh -c "emacs --batch -l /karabiner/$< --eval='(karabiner-rule-print-json shingeta-rule)' > /karabiner/$@"

##############################

docker-pull:
	docker pull $(EMACS)

clean:
	rm -rf el-get elpa $(DIRS)
