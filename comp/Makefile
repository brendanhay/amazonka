SHELL     := /usr/bin/env bash
TOP       := ..
DEPS      := vendor/botocore
MODEL_DIR := model
OUT_DIR   := ..

define version =
$(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(1))
endef

VERSION        = $(call version,amazonka-compiler.cabal)
CLIENT_VERSION = $(call version,$(TOP)/amazonka/amazonka.cabal)
CORE_VERSION   = $(call version,$(TOP)/core/amazonka-core.cabal)

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
 dynamodb \
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

.PHONY: compile format

release: compile format

compile: install
	~/.local/bin/amazonka-compiler \
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

format:
	@echo -e '\nFormatting...'
	@find $(wildcard $(OUT_DIR)/amazonka-*/gen) \
 -type f \
 -name '*.hs' \
 -printf ' -> %p\n' \
 -exec stylish-haskell -i {} \;

build: $(MODEL_DIR)
	stack build amazonka-compiler

install: $(MODEL_DIR) build
	stack install

clean:
	stack clean
	rm -rf $(MODEL_DIR)

full-clean: clean
	rm -rf $(DEPS)

vendor/botocore:
	git clone git@github.com:boto/botocore $@

$(MODEL_DIR):
	cp -rf vendor/botocore/botocore/data $@
