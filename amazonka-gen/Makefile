SHELL    := /usr/bin/env bash
FLAGS    := -j --disable-documentation --disable-library-coverage
BOTO     := vendor/botocore
DEPS     := $(BOTO) vendor/ede
SERVICES := $(BOTO)/services
DEST     := model/gen
OVERRIDE := model/extra
GEN      := ../amazonka
VERSIONS := 1

RESTS3 := \
 $(DEST)/s3

RESTXML := \
 $(DEST)/cloudfront \
 $(DEST)/route53

RESTJSON := \
 $(DEST)/elastictranscoder

QUERY := \
 $(DEST)/autoscaling \
 $(DEST)/cloudformation \
 $(DEST)/cloudsearch \
 $(DEST)/cloudwatch \
 $(DEST)/ec2 \
 $(DEST)/elasticache \
 $(DEST)/elasticbeanstalk \
 $(DEST)/elb \
 $(DEST)/iam \
 $(DEST)/importexport \
 $(DEST)/rds \
 $(DEST)/redshift \
 $(DEST)/sdb \
 $(DEST)/ses \
 $(DEST)/sns \
 $(DEST)/sqs \
 $(DEST)/sts

JSON := \
 $(DEST)/cloudtrail \
 $(DEST)/datapipeline \
 $(DEST)/directconnect \
 $(DEST)/dynamodb \
 $(DEST)/emr \
 $(DEST)/kinesis \
 $(DEST)/opsworks \
 $(DEST)/storagegateway \
 $(DEST)/support \
 $(DEST)/swf

MODELS ?= $(QUERY)

.PHONY: gen model

gen:
	rm -rf $(GEN)/gen && make $(GEN)/gen

$(GEN)/gen: build
	dist/build/amazonka-gen/amazonka-gen \
	 --dir=$(realpath $(GEN)) \
	 --max=$(VERSIONS) \
	 $(addprefix --model=,$(realpath $(MODELS)))

model:
	rm -rf $(DEST) && make $(DEST)

$(DEST):
	script/build \
	 --services $(SERVICES) \
	 --destination $(DEST) \
	 --overrides $(OVERRIDE) \
	 --script script/model

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

clean:
	cabal clean
	rm -rf $(GEN)/gen
	rm -rf $(DEST)

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
