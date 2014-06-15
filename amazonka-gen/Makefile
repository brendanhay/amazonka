TOP  := ..
BIN  := dist/build/amazonka-gen/amazonka-gen
DEPS := vendor/ede vendor/botocore
BOTO := vendor/botocore/botocore/data/aws
GEN  := $(TOP)/amazonka

include $(TOP)/include/common.mk
include $(TOP)/include/targets.mk

RESTS3 := \
 $(BOTO)/s3.json

RESTXML := \
 $(BOTO)/route53.json \
 $(BOTO)/cloudfront.json

# Issues need to be investigated regarding optional/required values,
# and Textual Booleans, and generally incorrect type info.
RESTJSON := \
 $(BOTO)/elastictranscoder.json

QUERY := \
 $(BOTO)/ses.json \
 $(BOTO)/elasticache.json \
 $(BOTO)/autoscaling.json \
 $(BOTO)/cloudformation.json \
 $(BOTO)/cloudsearch.json \
 $(BOTO)/cloudwatch.json \
 $(BOTO)/elasticbeanstalk.json \
 $(BOTO)/elb.json \
 $(BOTO)/rds.json \
 $(BOTO)/redshift.json \
 $(BOTO)/sns.json \
 $(BOTO)/sqs.json \
 $(BOTO)/sts.json \
 $(BOTO)/ec2.json \
 $(BOTO)/iam.json

JSON := \
 $(BOTO)/cloudtrail.json \
 $(BOTO)/datapipeline.json \
 $(BOTO)/directconnect.json \
 $(BOTO)/dynamodb.json \
 $(BOTO)/emr.json \
 $(BOTO)/kinesis.json \
 $(BOTO)/opsworks.json \
 $(BOTO)/storagegateway.json \
 $(BOTO)/support.json \
 $(BOTO)/swf.json

# MODELS := $(RESTXML) $(RESTS3) $(JSON) $(QUERY)
MODELS := $(RESTS3)

gen: $(GEN)/src $(GEN)/lib

$(GEN):
	mkdir -p $@

$(GEN)/src: $(GEN)
	cd $(GEN) && ln -fs $(TOP)/amazonka-types/src src

$(GEN)/lib: $(GEN) build
	$(BIN) --dir=$@ $(addprefix --model=,$(MODELS))

build: def-build

install: add-sources def-install

clean: def-clean
	rm -rf vendor/botocore $(GEN)

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/ede

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
