SHELL     := /usr/bin/env bash
TOP       := ..
DEPS      := vendor/botocore vendor/jason
MODEL_DIR := model

CABAL_INSTALL_DEFARGS ?= -j --disable-documentation --disable-coverage

XML := \
 cloudfront \
 route53 \
 s3

QUERY := \
 autoscaling \
 cloudformation \
 cloudsearch \
 cloudwatch \
 ec2 \
 ecs \
 elasticache \
 elasticbeanstalk \
 elb \
 iam \
 importexport \
 rds \
 redshift \
 sdb \
 ses \
 sns \
 sqs \
 sts

JSON := \
 cloudhsm \
 cloudsearchdomain \
 cloudtrail \
 codedeploy \
 cognito-identity \
 cognito-sync \
 config \
 datapipeline \
 directconnect \
 dynamodb \
 elastictranscoder \
 emr \
 glacier \
 kinesis \
 kms \
 lambda \
 logs \
 machinelearning \
 opsworks \
 route53domains \
 ssm \
 storagegateway \
 support \
 swf \
 workspaces

MODELS ?= $(QUERY) $(XML) $(JSON)

define version =
$(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(1))
endef

VERSION        = $(call version,amazonka-compiler.cabal)
CLIENT_VERSION = $(call version,$(TOP)/amazonka/amazonka.cabal)
CORE_VERSION  = $(call version,$(TOP)/core/amazonka-core.cabal)

.PHONY: compile gen

compile: build gen
	dist/build/compile/compile \
 --out=gen \
 --library-version=$(VERSION) \
 --client-version=$(VERSION) \
 --core-version=$(CORE_VERSION) \
 --annexes=annex \
 --configs=config \
 --templates=template \
 --assets=asset \
 --retry=$(MODEL_DIR)/_retry.json \
 $(addprefix --model=,$(addprefix $(MODEL_DIR)/,$(MODELS)))

gen:
	rm -rf $@ && mkdir $@

build: $(MODEL_DIR)
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

deps: add-sources
	cabal install $(CABAL_INSTALL_DEFARGS) --only-dependencies

clean:
	cabal clean
	rm -rf $(MODEL_DIR) cabal.sandbox.config .cabal-sandbox cabal.config

full-clean: clean
	rm -rf $(DEPS)

include ../share/stackage.mk

add-sources: $(DEPS) cabal.sandbox.config # cabal.config - removing until stackage supports 7.10
	cabal sandbox add-source vendor/jason

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

$(MODEL_DIR):
	cp -rf vendor/botocore/botocore/data/aws $@
