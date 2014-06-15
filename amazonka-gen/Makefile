TOP  := ..
BIN  := dist/build/amazonka-gen/amazonka-gen
DEPS := vendor/botocore
BOTO := vendor/botocore/botocore/data/aws
GEN  := $(TOP)/amazonka

include $(TOP)/include/common.mk
include $(TOP)/include/targets.mk

MODELS := $(BOTO)/s3
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

gen: $(GEN)/src $(GEN)/lib

$(GEN):
	rm -rf $@; mkdir -p $@

$(GEN)/src: $(GEN)
	cd $(GEN) && ln -fs $(TOP)/amazonka-types/src

$(GEN)/lib: $(GEN) build
	$(BIN) --dir=$(realpath $(GEN)) $(addprefix --model=,$(realpath $(MODELS)))

build: def-build

install: add-sources def-install

clean: def-clean

add-sources: cabal.sandbox.config $(DEPS)

vendor/botocore:
	git clone git@github.com:boto/botocore $@
