SHELL    := /usr/bin/env bash
FLAGS    := -j --disable-documentation --disable-library-coverage
DEPS     := vendor/botocore vendor/jason

RESTS3 := \
 s3

RESTXML := \
 cloudfront \
 route53
# Omitted - boto's models seem to only speciy a small subset
# of the available calls in the documentation
# cloudsearchdomain

RESTJSON := \
 elastictranscoder \
 cognito-sync
# Omitted - boto's models don't reflect the documentation
# and my understanding of the service is limited.

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
 cloudtrail \
 route53domains \
 datapipeline \
 directconnect \
 dynamodb \
 emr \
 kinesis \
 opsworks \
 storagegateway \
 support \
 swf \
 logs \
 cognito-identity

MODELS ?= $(RESTS3) $(RESTXML) $(RESTJSON) $(QUERY) $(JSON)

.PHONY: gen

gen: build stage1 stage2
	@dist/build/amazonka-gen/amazonka-gen \
--out=../ \
 --services=stage2 \
 --overrides=overrides \
 --templates=templates \
 --assets=assets \
 $(addprefix --model=,$(addprefix stage1/,$(MODELS)))

build:
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
	git clone git@github.com:boto/botocore $@ && \
 cp -rf $@/botocore/data/aws stage1

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

stage1:
	@mkdir $@

stage2:
	@mkdir $@
