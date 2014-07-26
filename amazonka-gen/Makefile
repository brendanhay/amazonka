SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
BIN   := dist/build/amazonka-gen/amazonka-gen
DEPS  := vendor/botocore vendor/ede
BOTO  := vendor/botocore/botocore/data/aws
GEN   := ../amazonka

VERSIONS := 1
MODELS   := $(BOTO)/ec2

# MODELS := \
 # $(BOTO)/autoscaling \
 # $(BOTO)/cloudformation \
 # $(BOTO)/cloudfront \
 # $(BOTO)/cloudsearch \
 # $(BOTO)/cloudtrail \
 # $(BOTO)/cloudwatch \
 # $(BOTO)/datapipeline \
 # $(BOTO)/directconnect \
 # $(BOTO)/dynamodb \
 # $(BOTO)/ec2 \
 # $(BOTO)/elasticache \
 # $(BOTO)/elasticbeanstalk \
 # $(BOTO)/elastictranscoder \
 # $(BOTO)/elb \
 # $(BOTO)/emr \
 # $(BOTO)/iam \
 # $(BOTO)/importexport \
 # $(BOTO)/kinesis \
 # $(BOTO)/opsworks \
 # $(BOTO)/rds \
 # $(BOTO)/redshift \
 # $(BOTO)/route53 \
 # $(BOTO)/s3 \
 # $(BOTO)/sdb \
 # $(BOTO)/ses \
 # $(BOTO)/sns \
 # $(BOTO)/sqs \
 # $(BOTO)/storagegateway \
 # $(BOTO)/sts \
 # $(BOTO)/support \
 # $(BOTO)/swf

.PHONY: gen

gen:
	rm -rf $(GEN)/gen && make $(GEN)/gen

$(GEN)/gen: build
	$(BIN) \
	 --dir=$(realpath $(GEN)) \
	 --max=$(VERSIONS) \
	 $(addprefix --model=,$(realpath $(MODELS)))

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

clean:
	cabal clean

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
