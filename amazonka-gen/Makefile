SHELL    := /usr/bin/env bash
FLAGS    := -j --disable-documentation --disable-library-coverage
DEPS     := vendor/botocore vendor/ede
BOTO     := vendor/botocore/botocore/data/aws
VERSIONS := 1

RESTS3 := \
 s3

RESTXML := \
 cloudfront \
 route53

RESTJSON := \
 elastictranscoder

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
 datapipeline \
 directconnect \
 dynamodb \
 emr \
 kinesis \
 opsworks \
 storagegateway \
 support \
 swf \
 cognito-identity

# Omitted for the time being - boto's models don't reflect the documentation
# and my understanding of the service is too limited.
# cognito-sync

MODELS ?= $(RESTS3) $(RESTXML) $(RESTJSON) $(QUERY) $(JSON)

.PHONY: gen

gen: build
	dist/build/amazonka-gen/amazonka-gen \
 --output=$(realpath ../lib) \
 --override=$(realpath ./model) \
 --assets=$(realpath ./asset) \
 --versions=$(VERSIONS) \
 $(addprefix --model=,$(realpath $(addprefix $(BOTO)/,$(MODELS))))

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

clean:
	cabal clean
	rm -rf cabal.sandbox.config .cabal-sandbox vendor

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
