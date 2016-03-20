# Change Log

## [1.3.8](https://github.com/brendanhay/amazonka/tree/1.3.8)
Released: **20 March, 2016**, Compare: [1.3.8](https://github.com/brendanhay/amazonka/compare/1.3.7...1.3.8)

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
Released: **18 December, 2015**, Compare: [1.3.7](https://github.com/brendanhay/amazonka/compare/1.3.6...1.3.7)

### Fixed

- Fix SWF `PollFor{Activity,Decision}Task` response deserialisation. [\#257](https://github.com/brendanhay/amazonka/issues/257)

### Changes

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
Released: **18 November, 2015**, Compare: [1.3.6](https://github.com/brendanhay/amazonka/compare/1.3.5.1...1.3.6)

### Fixed

- Upgrading `retry` dependency to `>= 0.7`.
- Fix S3 `BucketLocationConstraint` type de/serialisation. [\#249](https://github.com/brendanhay/amazonka/issues/249)
- Fix S3 `PutBucketACL` header vs request body serialisation. [\#241](https://github.com/brendanhay/amazonka/issues/241)

### Changes

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
Released: **18 November, 2015**, Compare: [1.3.5.1](https://github.com/brendanhay/amazonka/compare/1.3.5...1.3.5.1)

### Fixed

- Fixed ambigiuty issue when using `lens >= 4.13`
- Constraining `retry < 0.7` to avoid breaking changes.


## [1.3.5](https://github.com/brendanhay/amazonka/tree/1.3.5)
Released: **27 October, 2015**, Compare: [1.3.5](https://github.com/brendanhay/amazonka/compare/1.3.4...1.3.5)

### Updated Services Definitions

- SSM: Multiple additions and documentation updates.
- DynamoDB: Paginator updates.


### New Libraries

- `amazonka-apigateway`: API Gateway Service.


## [1.3.4](https://github.com/brendanhay/amazonka/tree/1.3.4)
Released: **25 October, 2015**, Compare: [1.3.4](https://github.com/brendanhay/amazonka/compare/1.3.3...1.3.4)

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
