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

.PHONY: gen

gen: build
	dist/build/amazonka-gen/amazonka-gen \
 --out=../ \
 --services=output \
 --overrides=overrides \
 --templates=templates \
 --assets=assets \
 --retry=input/_retry.json \
 $(addprefix --model=,$(addprefix input/,$(MODELS)))

build: input output
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

deps: add-sources
	cabal install $(CABAL_INSTALL_DEFARGS) --only-dependencies

full-clean: clean
	rm -rf input vendor

clean:
	cabal clean
	rm -rf output cabal.sandbox.config .cabal-sandbox

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
