SHELL    := /usr/bin/env bash
FLAGS    := -j --disable-documentation --disable-library-coverage
DEPS     := vendor/botocore vendor/ede
BOTO     := vendor/botocore/botocore/data/aws
GEN      := ../amazonka
VERSIONS := 1

RESTS3   := $(BOTO)/s3
RESTXML  := $(BOTO)/cloudfront $(BOTO)/route53
RESTJSON := $(BOTO)/elastictranscoder

QUERY := \
 $(BOTO)/autoscaling \
 $(BOTO)/cloudformation \
 $(BOTO)/cloudsearch \
 $(BOTO)/cloudwatch \
 $(BOTO)/ec2 \
 $(BOTO)/elasticache \
 $(BOTO)/elasticbeanstalk \
 $(BOTO)/elb \
 $(BOTO)/iam \
 $(BOTO)/importexport \
 $(BOTO)/rds \
 $(BOTO)/redshift \
 $(BOTO)/sdb \
 $(BOTO)/ses \
 $(BOTO)/sns \
 $(BOTO)/sqs \
 $(BOTO)/sts

JSON := \
 $(BOTO)/cloudtrail \
 $(BOTO)/datapipeline \
 $(BOTO)/directconnect \
 $(BOTO)/dynamodb \
 $(BOTO)/emr \
 $(BOTO)/kinesis \
 $(BOTO)/opsworks \
 $(BOTO)/storagegateway \
 $(BOTO)/support \
 $(BOTO)/swf \
 $(BOTO)/cognito-identity

# Omitted for the time being - boto's models don't reflect the documentation
# $(BOTO)/cognito-sync

MODELS ?= $(RESTS3) $(RESTXML) $(RESTJSON)

.PHONY: gen

gen:
	rm -rf $(GEN)/gen && make $(GEN)/gen

$(GEN)/gen: build
	dist/build/amazonka-gen/amazonka-gen \
	 --output=$(realpath $(GEN)) \
	 --override=$(realpath ./model) \
	 --versions=$(VERSIONS) \
	 $(addprefix --model=,$(realpath $(MODELS)))

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

clean:
	cabal clean
	rm -rf $(GEN)/gen cabal.sandbox.config .cabal-sandbox vendor

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
