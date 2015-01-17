SHELL := /usr/bin/env bash
DEPS  := vendor/botocore vendor/jason

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
 kinesis \
 kms \
 lambda \
 logs \
 opsworks \
 route53domains \
 storagegateway \
 support \
 swf

MODELS ?= $(QUERY) $(XML) $(JSON)

.PHONY: gen

gen: build
	@dist/build/amazonka-gen/amazonka-gen \
 --out=../ \
 --services=output \
 --overrides=overrides \
 --templates=templates \
 --assets=assets \
 --retry=input/_retry.json \
 $(addprefix --model=,$(addprefix input/,$(MODELS)))

build: input output
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(CABAL_INSTALL_DEFARGS) --only-dependencies

clean:
	cabal clean
	rm -rf input output cabal.sandbox.config .cabal-sandbox vendor

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/jason

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

input:
	cp -rf vendor/botocore/botocore/data/aws $@

output:
	@mkdir $@
