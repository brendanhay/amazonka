SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
DEPS  := vendor/botocore vendor/jason

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
 --services=stage2 \
 --overrides=overrides \
 --templates=templates \
 --assets=assets \
 $(addprefix --model=,$(addprefix stage1/,$(MODELS)))

build: stage1 stage2
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

clean:
	cabal clean
	rm -rf stage1 stage2 cabal.sandbox.config .cabal-sandbox vendor

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/jason

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

stage1:
	cp -rf vendor/botocore/botocore/data/aws $@

stage2:
	@mkdir $@
