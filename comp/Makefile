SHELL     := /usr/bin/env bash
TOP       := ..
DEPS      := vendor/botocore
MODEL_DIR := model
OUT_DIR   := ..

CABAL_INSTALL_DEFARGS ?= -j \
 --disable-documentation \
 --disable-coverage \
 --enable-profiling

define version =
$(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(1))
endef

VERSION        = $(call version,amazonka-compiler.cabal)
CLIENT_VERSION = $(call version,$(TOP)/amazonka/amazonka.cabal)
CORE_VERSION   = $(call version,$(TOP)/core/amazonka-core.cabal)

# + dynamodb
MODELS ?= \
 autoscaling \
 cloudformation \
 cloudfront \
 cloudhsm \
 cloudsearch \
 cloudsearchdomain \
 cloudtrail \
 cloudwatch \
 codedeploy \
 cognito-identity \
 cognito-sync \
 config \
 datapipeline \
 directconnect \
 ds \
 ec2 \
 ecs \
 efs \
 elasticache \
 elasticbeanstalk \
 elastictranscoder \
 elb \
 emr \
 glacier \
 iam \
 importexport \
 kinesis \
 kms \
 lambda \
 logs \
 machinelearning \
 opsworks \
 rds \
 redshift \
 route53 \
 route53domains \
 s3 \
 sdb \
 ses \
 sns \
 sqs \
 ssm \
 storagegateway \
 sts \
 support \
 swf \
 workspaces

.PHONY: format compile

default: compile # temporary to avoid formatting

format: compile
	@echo -e '\nFormatting...'
	@find $(wildcard $(OUT_DIR)/amazonka-*/gen) \
 -type f \
 -name '*.hs' \
 -printf ' -> %p\n' \
 -exec stylish-haskell -i {} \;

compile: build
	dist/build/compile/compile \
 --out=$(OUT_DIR) \
 --library-version=$(VERSION) \
 --client-version=$(VERSION) \
 --core-version=$(CORE_VERSION) \
 --annexes=annex \
 --configs=config \
 --templates=template \
 --static=static \
 --retry=$(MODEL_DIR)/_retry.json \
 $(addprefix --model=,$(addprefix $(MODEL_DIR)/,$(MODELS)))

build: $(MODEL_DIR)
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

include ../share/stackage.mk

deps: $(DEPS) cabal.sandbox.config # temporary cabal.config
	cabal install $(CABAL_INSTALL_DEFARGS) --only-dependencies

clean:
	cabal clean
	rm -rf $(MODEL_DIR) cabal.sandbox.config .cabal-sandbox cabal.config

full-clean: clean
	rm -rf $(DEPS)

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

$(MODEL_DIR):
	cp -rf vendor/botocore/botocore/data $@
