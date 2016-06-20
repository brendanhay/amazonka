# Change Log

## [1.4.3](https://github.com/brendanhay/amazonka/tree/1.4.3)
Released: **09 June, 2016**, Compare: [1.4.2](https://github.com/brendanhay/amazonka/compare/1.4.2...1.4.3)

### Fixed

- Additional fixes for APIGateway timestamps. [\#291](https://github.com/brendanhay/amazonka/issues/291)
- CloudWatchLogs `FilterLogEvents` pagination now correctly returns all results. [\#296](https://github.com/brendanhay/amazonka/issues/296)
- Documentation code samples for IoT, Lambda, and Discovery are now correctly Haddock formatted.

### Changed

- Documentation is now formatted more consistently at the expense of longer line columns.
- `POSIX` timestamps no longer have unecessary (and misleading) Text/XML instances.


## [1.4.2](https://github.com/brendanhay/amazonka/tree/1.4.2)
Released: **03 June, 2016**, Compare: [1.4.1](https://github.com/brendanhay/amazonka/compare/1.4.1...1.4.2)

### Fixed

- GHC 8 support. [\#294](https://github.com/brendanhay/amazonka/issues/294)
- APIGateway now correctly parses and encodes `ISO8601` formatted timestamps. [\#291](https://github.com/brendanhay/amazonka/issues/291)
- `~/.aws/credentials` now correctly parses with leading newlines. [\#290](https://github.com/brendanhay/amazonka/issues/290)

### Changed

- `SerializeError` now contains the unparsed response body. [\#293](https://github.com/brendanhay/amazonka/pull/293)

### Updated Service Definitions

- [WorkSpaces](https://github.com/brendanhay/amazonka/commit/b869bf0)
- [StorageGateway](https://github.com/brendanhay/amazonka/commit/9af316)
- [SSM](https://github.com/brendanhay/amazonka/commit/542f50c)
- [S3](https://github.com/brendanhay/amazonka/commit/84d66a6)
- [RDS](https://github.com/brendanhay/amazonka/commit/98edec9)
- [EC2](https://github.com/brendanhay/amazonka/commit/93b6b72)

### New Libraries

- `amazonka-discovery`: Discover on-premises application inventory and dependencies. [Overview](https://aws.amazon.com/application-discovery/)
- `amazonka-application-autoscaling`: General purpose scaling of AWS resources. [API Reference](http://docs.aws.amazon.com/ApplicationAutoScaling/latest/APIReference/Welcome.html)


## [1.4.1](https://github.com/brendanhay/amazonka/tree/1.4.1)
Released: **09 May, 2016**, Compare: [1.4.0](https://github.com/brendanhay/amazonka/compare/1.4.0...1.4.1)

### Fixed

- AutoScaling `DescribeAutoScalingInstances` response field `LaunchConfigurationName` is now optional. [\#281](https://github.com/brendanhay/amazonka/issues/281)
- SWF `PollForDecisionTask` and `PollForActivityTask` response fields are now optional. [\#285](https://github.com/brendanhay/amazonka/issues/285)

### Changed

- `NFData` instances generated for all eligible types. [\#283](https://github.com/brendanhay/amazonka/issues/283)
- Additional retry cases for HTTP `5XX` response codes. [c5e494e](https://github.com/brendanhay/amazonka/commit/c5e494e2a97fcf2e9210527ed5e8547f1be898de)

### Updated Service Definitions

> The following services contain a large number of definition updates.
Please review the linked commit for each library for specific changes:

- [APIGateway](https://github.com/brendanhay/amazonka/commit/e57d291)
- [CertificateManager](https://github.com/brendanhay/amazonka/commit/513fab6)
- [CloudFormation](https://github.com/brendanhay/amazonka/commit/b2e70e3)
- [CloudHSM](https://github.com/brendanhay/amazonka/commit/c091447)
- [CodeCommit](https://github.com/brendanhay/amazonka/commit/bffa050)
- [CodeDeploy](https://github.com/brendanhay/amazonka/commit/ef50a4c)
- [CodePipeline](https://github.com/brendanhay/amazonka/commit/f2bf49d)
- [Cognito Identity](https://github.com/brendanhay/amazonka/commit/6287585)
- [DMS](https://github.com/brendanhay/amazonka/commit/eab3c87)
- [DeviceFarm](https://github.com/brendanhay/amazonka/commit/452ef52)
- [DirectoryService](https://github.com/brendanhay/amazonka/commit/35fb573)
- [EC2](https://github.com/brendanhay/amazonka/commit/6b097cb)
- [ECR](https://github.com/brendanhay/amazonka/commit/8cf3929)
- [EMR](https://github.com/brendanhay/amazonka/commit/74a44ae)
- [ElastiCache](https://github.com/brendanhay/amazonka/commit/bb570ae)
- [ElasticBeanstalk](https://github.com/brendanhay/amazonka/commit/7f098ad)
- [Inspector](https://github.com/brendanhay/amazonka/commit/71dc844)
- [IoT](https://github.com/brendanhay/amazonka/commit/4c9276d)
- [KMS](https://github.com/brendanhay/amazonka/commit/fedd472)
- [Kineses](https://github.com/brendanhay/amazonka/commit/e2a1c4e)
- [Kinesis Firehose](https://github.com/brendanhay/amazonka/commit/cf4d8a9)
- [Lambda](https://github.com/brendanhay/amazonka/commit/53ec890)
- [OpsWorks](https://github.com/brendanhay/amazonka/commit/7672d9f)
- [RDS](https://github.com/brendanhay/amazonka/commit/236d9df)
- [Redshift](https://github.com/brendanhay/amazonka/commit/baffabf)
- [Route53Domains](https://github.com/brendanhay/amazonka/commit/9ac880f)
- [Route53](https://github.com/brendanhay/amazonka/commit/6a9bd83)
- [S3](https://github.com/brendanhay/amazonka/commit/3e65f86)
- [STS](https://github.com/brendanhay/amazonka/commit/cbc0625)
- [StorageGateway](https://github.com/brendanhay/amazonka/commit/f970f0e)
- [WAF](https://github.com/brendanhay/amazonka/commit/3981bfa)

### New Libraries

- `amazonka-coginito-idp`: Cognito Identity Provider.


## [1.4.0](https://github.com/brendanhay/amazonka/tree/1.4.0)
Released: **21 March, 2016**, Compare: [1.3.7](https://github.com/brendanhay/amazonka/compare/1.3.7...1.4.0)

### Fixed

- Host header missing for presigned URLs. [\#264](https://github.com/brendanhay/amazonka/pull/264)
- Set all API Gateway communication to `Accept: application/json`. [\#266](https://github.com/brendanhay/amazonka/pull/266)
- Override EC2 `AttachmentStatus` to add `"available"`. [\#273](https://github.com/brendanhay/amazonka/pull/273) [\#275](https://github.com/brendanhay/amazonka/pull/275)
- Allow EC2 `DeleteTags` to omit the tag value. [\#270](https://github.com/brendanhay/amazonka/pull/270)
- Add `Hashable` instances for non-streaming types. [\#267](https://github.com/brendanhay/amazonka/pull/270)

### Updated Service Definitions

> The following services contain a large number (3 months worth) of definition updates.
Please see the relevant libraries for specific changes:

- API Gateway
- AutoScaling
- CloudFormation
- CloudFront
- CloudHSM
- CloudSearchDomain
- CloudWatch
- CloudWatchLogs
- CodeCommit
- CodeDeploy
- Config
- DeviceFarm
- DirectConnect
- DirectoryService
- DynamoDB
- EC2
- ECS
- EMR
- IAM
- IoT
- Lambda
- Marketplace Analytics
- OpsWorks
- RDS
- Redshift
- Route53
- S3
- SES
- SSM
- STS
- StorageGateway
- Web Application Firewall

### New Libraries

- `amazonka-certificatemanager`: AWS Certificate Manager.
- `amazonka-dms`: Database Migration Service.
- `amazonka-ecr`: Elastic Container Registry.
- `amazonka-cloudwatch-events`: CloudWatch Events.
- `amazonka-gamelift`: Amazon GameLift.
- `amazonka-marketplace-metering`: AWS Markpletplace Metering.


## [1.3.7](https://github.com/brendanhay/amazonka/tree/1.3.7)
Released: **18 December, 2015**, Compare: [1.3.6](https://github.com/brendanhay/amazonka/compare/1.3.6...1.3.7)

### Fixed

- Fix SWF `PollFor{Activity,Decision}Task` response deserialisation. [\#257](https://github.com/brendanhay/amazonka/issues/257)

### Changed

- The `ErrorCode` type constructor is now exported. [\#258](https://github.com/brendanhay/amazonka/issues/258)
- Add `Bounded` instances for all enumerations with stable values. [\#255](https://github.com/brendanhay/amazonka/issues/255)
- The `AWS_PROFILE` environment variable can now be used to override the default
  file credentials location. [\#254](https://github.com/brendanhay/amazonka/issues/254)

### Updated Service Definitions

- AutoScaling: Updated service definition.
- CloudFront: GZip support.
- CloudTrail: Add `isMultiRegion` flag to various operations.
- Config: Updated service definition.
- DirectConnect: Documentation updates.
- DirectoryService: Updated service definition.
- EC2: NAT Gateway updates and an updated service definition.
- IAM: Additional resource types, documentation updates.
- IoT: Added `RegisterCertificate` operation.
- KMS: Updated service definition.
- RDS: Add enchanced monitoring support, documentation updates.
- Route53: Updated service definition.
- SSM: Updated service definition.


## [1.3.6](https://github.com/brendanhay/amazonka/tree/1.3.6)
Released: **18 November, 2015**, Compare: [1.3.5.1](https://github.com/brendanhay/amazonka/compare/1.3.5.1...1.3.6)

### Fixed

- Upgrading `retry` dependency to `>= 0.7`.
- Fix S3 `BucketLocationConstraint` type de/serialisation. [\#249](https://github.com/brendanhay/amazonka/issues/249)
- Fix S3 `PutBucketACL` header vs request body serialisation. [\#241](https://github.com/brendanhay/amazonka/issues/241)

### Changed

- `await` responses now indicate request fulfillment. [\#245](https://github.com/brendanhay/amazonka/issues/245)

### Updated Services Definitions

- EC2: Documentation updates.
- IAM: Documentation updates.
- ELB: Documentation updates.
- Kinesis: Waiter updates.
- RDS: Documentation and type updates.
- SQS: Documentation updates.
- STS: Documentation updates.
- S3: Minor type and operation updates (`UploadPart` headers).
- DeviceFarm: Documentation updates.
- API Gateway: Multiple type, operation, and documentation updates.


## [1.3.5.1](https://github.com/brendanhay/amazonka/tree/1.3.5.1)
Released: **18 November, 2015**, Compare: [1.3.5](https://github.com/brendanhay/amazonka/compare/1.3.5...1.3.5.1)

### Fixed

- Fixed ambigiuty issue when using `lens >= 4.13`
- Constraining `retry < 0.7` to avoid breaking changes.


## [1.3.5](https://github.com/brendanhay/amazonka/tree/1.3.5)
Released: **27 October, 2015**, Compare: [1.3.4](https://github.com/brendanhay/amazonka/compare/1.3.4...1.3.5)

### Updated Services Definitions

- SSM: Multiple additions and documentation updates.
- DynamoDB: Paginator updates.


### New Libraries

- `amazonka-apigateway`: API Gateway Service.


## [1.3.4](https://github.com/brendanhay/amazonka/tree/1.3.4)
Released: **25 October, 2015**, Compare: [1.3.3](https://github.com/brendanhay/amazonka/compare/1.3.3...1.3.4)

### Updated Services Definitions

- AutoScaling: Documentation updates.
- EC2: Paginator updates.
- Glacier: Paginator additions.
- IAM: Paginator, type, and documentation updates.
- KMS: Multiple type, operation, and documentation updates.
- S3: Minor type updates. (Server side encryption related.)

### New Libraries

- `amazonka-iot-dataplane`: Internet of Things Data Plane.


## [1.3.3](https://github.com/brendanhay/amazonka/tree/1.3.3)
Released: **08 October, 2015**, Compare: [1.3.2.1](https://github.com/brendanhay/amazonka/compare/1.3.2.1...1.3.3)

### Fixed

- Fix S3 `GetBucketLocation` response deserialisation. [\#228](https://github.com/brendanhay/amazonka/issues/228), [\#237](https://github.com/brendanhay/amazonka/issues/237).
- `runResourceT` added to documentation example. [\#232](https://github.com/brendanhay/amazonka/issues/232).
- Add S3 infrequent access storage class. [\#234](https://github.com/brendanhay/amazonka/issues/234), [\#238](https://github.com/brendanhay/amazonka/issues/238).

### Updated Service Definitions

- CloudFormation: Documentation updates, `DescribeAccountLimits` operation added.
- CloudFront: API version `2015-07-27` support.
- CloudTrail: Documentation updates, various tag operations added.
- Config: Miscellaneous type and documentation updates
- EC2: API version `2015-10-01` support, spot blocks support.
- ElasticBeanStalk: Documentation updates.
- RDS: Miscellaneous type and documentation updates.
- SES: Miscellaneous type and documentation updates.
- WorkSpaces: Volume encryption types added.

### New Libraries

- `amazonka-elasticsearch`: ElasticSearch Service.
- `amazonka-inspector`: Inspector Service.
- `amazonka-iot`: Internet of Things Platform.
- `amazonka-kinesis-firehose`: Kinesis Firehose Service.
- `amazonka-marketplace-analytics`: Marketplace Commerce Analytics Service.
- `amazonka-waf`: Web Application Firewall Service.


## [1.3.2](https://github.com/brendanhay/amazonka/tree/1.3.2)
Released: **18 September, 2015**, Compare: [1.3.1](https://github.com/brendanhay/amazonka/compare/1.3.1...1.3.2)

### Fixed

- Ensure opqaue JSON bodies are handled correctly. [\#224](https://github.com/brendanhay/amazonka/issues/224) [\#226](https://github.com/brendanhay/amazonka/issues/226)

### Updated Service Definitions

- EC2: Documentation updates.
- EFS: Documentation updates.
- Route53: Inverted, Measured Latency, and Child Health Check updates.
- CloudWatchLogs
    * `InvalidOperationException` error matcher added.
    * `DescribeExportTasks` operation added
    * `CreateExportTask` operation added
    * `CancelExportTask` operation added
    * `ExportTask`, `ExportTaskExecutionInfo`, `ExportTaskStatus` types added.
- S3
    * Infrequent access storage tier updates.
    * `GetBucketLifecycle` operation removed (deprecated).
    * `PutBucketLifecycle` operation removed (deprecated).
    * `Rule` product removed (deprecated).
    * `GetbucketLifecycleConfiguration` operation added.
    * `PutBucketLifecycleConfiguration` operation added.
    * `BucketLifecycleConfiguration` product added.
    * `LifecycleRule` product added.

## [1.3.1](https://github.com/brendanhay/amazonka/tree/1.3.1)
Released: **09 September, 2015**, Compare: [1.3.0](https://github.com/brendanhay/amazonka/compare/1.3.0...1.3.1)

### Fixed

- Fix S3 `ListObject` pagination regression. [\#218](https://github.com/brendanhay/amazonka/issues/218)
- Corrected IS08601 timezone de/serialisation. [\#217](https://github.com/brendanhay/amazonka/issues/217)
- Remove required EC2 VPC `isDefault` member. [\#214](https://github.com/brendanhay/amazonka/issues/214)
- Correctly named MachineLearning `CreateDataSourceFromS3`. [4805074](https://github.com/brendanhay/amazonka/commit/4805074)

### Updated Service Definitions

- DeviceFarm
- IAM
- RDS
- ImportExport
- Route53
- CloudWatchLogs
- Kinesis
- DataPipeline
- EFS: Namespace rename from ElasticFileSystem -> EFS.


## [1.3.0](https://github.com/brendanhay/amazonka/tree/1.3.0)
Released: **03 September, 2015**, Compare: [1.2.0.2](https://github.com/brendanhay/amazonka/compare/1.2.0.2...1.3.0)

### Changed

- Renamed all HTTP status code response lenses from `*Status` to `*ResponseStatus`.

### Fixed

- Fix malformed SNS Subscribe request. [\#209](https://github.com/brendanhay/amazonka/issues/209)
- Fix `amazonka-core` cabal build error due to lax `vector` constraints. [\#208](https://github.com/brendanhay/amazonka/issues/208)
- Override SQS message attribute type, `QueueAttributeName` -> `MessageAttributeName`. [\#199](https://github.com/brendanhay/amazonka/issues/199)
- Re-enable stackage test builds for all service libraries. [\#170](https://github.com/brendanhay/amazonka/issues/170)


## [1.2.0.2](https://github.com/brendanhay/amazonka/tree/1.2.0.2)
Released: **29 August, 2015**, Compare: [1.2.0.1](https://github.com/brendanhay/amazonka/compare/1.2.0.1...1.2.0.2)


## [1.2.0.1](https://github.com/brendanhay/amazonka/tree/1.2.0.1)
Released: **28 August, 2015**, Compare: [1.2.0](https://github.com/brendanhay/amazonka/compare/1.2.0...1.2.0.1)


## [1.2.0](https://github.com/brendanhay/amazonka/tree/1.2.0)
Released: **27 August, 2015**, Compare: [1.1.0](https://github.com/brendanhay/amazonka/compare/1.1.0...1.2.0)


## [1.1.0](https://github.com/brendanhay/amazonka/tree/1.1.0)
Released: **21 August, 2015**, Compare: [1.0.1](https://github.com/brendanhay/amazonka/compare/1.0.1...1.1.0)


## [1.0.1](https://github.com/brendanhay/amazonka/tree/1.0.1)
Released: **18 August, 2015**, Compare: [1.0.0](https://github.com/brendanhay/amazonka/compare/1.0.0...1.0.1)


## [1.0.0](https://github.com/brendanhay/amazonka/tree/1.0.0)
Released: **16 August, 2015**, Compare: [0.3.6](https://github.com/brendanhay/amazonka/compare/0.3.6...1.0.0)
