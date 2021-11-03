# Change Log

## [2.0 RC1](https://github.com/brendanhay/amazonka/tree/2.0-rc1)
Released: **22nd October, 2021**, Compare: [1.6.1](https://github.com/brendanhay/amazonka/compare/1.6.1...2.0-rc1)

### Major Changes

- Every service has been regenerated.

- Enums (sum types) have been replaced with pattern synonyms. (Thanks @rossabaker)
  - This reduces GHC memory usage on some large packages (notably `amazonka-ec2`), as well as makes them more robust against new enum values in regions, responses, etc.

- Naming
  - Record smart constructors (previously `describeInstances`, `getObject`, etc.) are now strictly prefixed with `new`, such as `newDescribeInstances`.
  - Generated lenses are no longer exported from the top-level `Network.AWS.<name>` module - instead a `Network.AWS.<name>.Lens` module is provided.
  - Generated lenses no longer use mnemonic or heuristically assigned prefixes such as `dirsrsInstances` and instead strictly prefix using the type name `describeInstances_instances` - following the form `<type>_<field>`.
  - You may prefer to use a library like [`generic-lens`](https://hackage.haskell.org/package/generic-lens) instead of the long lens names.

- Exports
  - Every `amazonka-*` package re-exports the `Network.AWS.Prelude` module.
  - All type constructors (Such as record constructors) are now exported by default.

- CI
  - Nix, Bazel, and GitHub Actions are used for CI
  - `nix-build-uncached` is used to prevent spurious rebuilds in CI [#627](https://github.com/brendanhay/amazonka/pull/627)
  - CPP supporting GHC < 8.8 has been removed.
  - While GHC 8.6 is not in the CI matrix, it currently builds, so packages depend on `base >= 4.12`.

### Fixed

- Fix trailing slash bug in gen model
[\#528](https://github.com/brendanhay/amazonka/pull/528)
- Close connections immediately (when we know that we can)
[\#493](https://github.com/brendanhay/amazonka/pull/493)
- Remove the Content-Length header when doing V4 streaming signatures
[\#547](https://github.com/brendanhay/amazonka/pull/547)
- Ensure that KerberosAttributes is parsed correctly from an empty response
[\#512](https://github.com/brendanhay/amazonka/pull/512)
- Correct Stockholm in FromText Region
[\#565](https://github.com/brendanhay/amazonka/pull/572)
- Fix MonadUnlifIO compiler errors on ghc-8.10.1
[\#572](https://github.com/brendanhay/amazonka/pull/572)
- Deduplicate CreateCertificateFromCSR fixtures
[\#500](https://github.com/brendanhay/amazonka/pull/500)
- CloudFormation ListChangeSets and DescribeChangeSets are paginated
[\#620](https://github.com/brendanhay/amazonka/pull/620)
- Use `signingName`, not `endpointPrefix`, when signing requests
[\#622](https://github.com/brendanhay/amazonka/pull/622)
- Add `x-amz-glacier-version` to glacier headers (and extend generator to let `operationPlugins` support wildcards)
[\#623](https://github.com/brendanhay/amazonka/pull/623)
- Add required fields for Glacier multipart uploade
[\#624](https://github.com/brendanhay/amazonka/pull/624)
- If nonstandard ports are used, include them in signed host header
[\#625](https://github.com/brendanhay/amazonka/pull/625)
- Duplicate files that differ only in case have been removed
[\#637](https://github.com/brendanhay/amazonka/pull/637)
- S3 object sizes are now `Integer` instead of `Int`
[\#649](https://github.com/brendanhay/amazonka/pull/649)
- Fix getting regions from named profiles
[\#654](https://github.com/brendanhay/amazonka/pull/654)
- amazonka-rds now supports the `DestinationRegion` pseudo-parameter for cross-region requests
[\#661](https://github.com/brendanhay/amazonka/pull/661)
- Fixed S3 envelope encryption
[\#669](https://github.com/brendanhay/amazonka/pull/669)
- Fixed S3 to use vhost endpoints, parse `LocationConstraint`s properly, and ensure `Content-MD5` header correctly set.
[\#673](https://github.com/brendanhay/amazonka/pull/673)
- Fixed S3 to not set empty `Content-Encoding`, and to be able to set `Content-Encoding` without breaking signing
[\#681](https://github.com/brendanhay/amazonka/pull/681)

### Other Changes

- Bump the upper bound in the dependency on http-client
[\#526](https://github.com/brendanhay/amazonka/pull/526)
- Added new regions to ELB, RedShift, Route53.
[\#541](https://github.com/brendanhay/amazonka/pull/541)
- Update service definitions for cloudwatch-events
[\#544](https://github.com/brendanhay/amazonka/pull/544)
- Add MonadFail instance for AWST
[\#551](https://github.com/brendanhay/amazonka/pull/551)
- Add an instance for `PrimMonad m => PrimMonad (AWST' r m)`
[\#510](https://github.com/brendanhay/amazonka/pull/510)
- Set startedAt as optional in AWS batch
[\#561](https://github.com/brendanhay/amazonka/pull/561)
- Add `ignoredPaginators` to service definition.
[\#578](https://github.com/brendanhay/amazonka/pull/578)
- Adds paginators to AWS Secrets Manager API
[\#576](https://github.com/brendanhay/amazonka/pull/576)
- Add intelligent tiering S3 type
[\#570](https://github.com/brendanhay/amazonka/pull/570)
- AWS service descriptions appear in haddocks
[\#619](https://github.com/brendanhay/amazonka/pull/619)
- Drop some `MonadThrow` and `MonadCatch` constraints
[\#626](https://github.com/brendanhay/amazonka/pull/626)
- Use an in-tree script to generate services, instead of pulling the repo into the Nix store
[\#639](https://github.com/brendanhay/amazonka/pull/639)
- Provide script to audit missing service configurations from boto
[\#644](https://github.com/brendanhay/amazonka/pull/644)

### New libraries

- `amazonka-apigatewaymanagementapi`: API management strategies allow you to monitor and manage APIs in a secure and scalable way. [Overview](https://aws.amazon.com/api-gateway/api-management/)
- `amazonka-eks`: Amazon Elastic Kubernetes Service (Amazon EKS) is a managed container service to run and scale Kubernetes applications in the cloud or on-premises. [Overview](https://aws.amazon.com/eks/)
[\#618](https://github.com/brendanhay/amazonka/pull/618)
- `amazonka-qldb`: Fully managed ledger database that provides a transparent, immutable, and cryptographically verifiable transaction log. Owned by a central trusted authority. [Overview](https://aws.amazon.com/qldb/)
[\#621](https://github.com/brendanhay/amazonka/pull/621)
- `amazonka-sesv2`: High-scale inbound and outbound cloud email service --- V2 API. [Overview](https://aws.amazon.com/ses/)
[\#633](https://github.com/brendanhay/amazonka/pull/633)
- `amazonka-textract`: Easily extract printed text, handwriting, and data from any document. [Overview](https://aws.amazon.com/textract/)
- `amazonka-accessanalyzer`: Help identify potential resource-access risks by enabling you to identify any policies that grant access to an external principal. [Overview](https://docs.aws.amazon.com/access-analyzer/latest/APIReference/Welcome.html)
- `amazonka-account`: API operations to modify an AWS account itself. [Overview](https://docs.aws.amazon.com/access-analyzer/latest/APIReference/Welcome.html)
- `amazonka-amp`: A managed Prometheus-compatible monitoring and alerting service that makes it easy to monitor containerized applications and infrastructure at scale. [Overview](https://aws.amazon.com/prometheus/)
- `amazonka-amplify`: Purpose-built tools and services that make it quick and easy for front-end web and mobile developers build full-stack applications on AWS. [Overview](https://aws.amazon.com/amplify/)
- `amazonka-amplifybackend`: Programmatic interface to the AWS Amplify Admin UI. [Overview](https://docs.aws.amazon.com/amplify-admin-ui/latest/APIReference/what-is-admin-ui.html)
- `amazonka-apigatewayv2`: Version 2 of the API Gateway API, for HTTP and WebSocket APIs. [Overview](https://docs.aws.amazon.com/apigatewayv2/latest/api-reference/api-reference.html)
- `amazonka-appconfig`: A feature of AWS Systems Manager, to make it easy to quickly and safely configure, validate, and deploy application configurations. [Overview](https://aws.amazon.com/systems-manager/features/appconfig)
- `amazonka-appflow`: Fully managed integration service that securely transfers data between Software-as-a-Service (SaaS) applications and AWS services. [Overview](https://aws.amazon.com/appflow/)
- `amazonka-appintegrations`: Configure and connect external applications to Amazon Connect. [Overview](https://docs.aws.amazon.com/appintegrations/latest/APIReference/Welcome.html)
- `amazonka-application-insights`: Facilitates observability for applications and their underlying AWS resources. [Overview](https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch-application-insights.html)
- `amazonka-applicationcostprofiler`: Track the consumption of shared AWS resources used by software applications and report granular cost breakdown across tenant base. [Overview](https://aws.amazon.com/aws-cost-management/aws-application-cost-profiler/)
- `amazonka-appmesh`: Service mesh that provides application-level networking for services to communicate with each other across multiple types of compute infrastructure. [Overview](https://aws.amazon.com/app-mesh/)
- `amazonka-apprunner`: Fully managed container application service. [Overview](https://aws.amazon.com/apprunner/)
- `amazonka-auditmanager`: Continuously audit AWS usage to simplify risk assessment and compliance. [Overview](https://aws.amazon.com/audit-manager/)
- `amazonka-backup`: Centrally manage and automate backups across AWS services. [Overview](https://aws.amazon.com/backup/)
- `amazonka-braket`: Managed quantum computing service. [Overview](https://aws.amazon.com/braket/)
- `amazonka-chime-sdk-identity`: Integrate identity providers with Amazon Chime SDK Messaging. [Overview](https://docs.aws.amazon.com/chime/latest/APIReference/API_Operations_Amazon_Chime_SDK_Identity.html)
- `amazonka-chime-sdk-messaging`: Create messaging applications that run on the Amazon Chime service. [Overview](https://docs.aws.amazon.com/chime/latest/dg/using-the-messaging-sdk.html)
- `amazonka-chime`: Communications service that lets you meet, chat, and place business calls inside and outside your organization, all using a single application. [Overview](https://aws.amazon.com/chime/)
- `amazonka-cloudcontrol`: Create, read, update, delete, and list (CRUD-L) your cloud resources that belong to a wide range of services—both AWS and third-party. [Overview](https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/what-is-cloudcontrolapi.html)
- `amazonka-codeartifact`: Managed artifact repository service to securely store, publish, and share software packages. [Overview](https://aws.amazon.com/codeartifact/)
- `amazonka-codeguru-reviewer`: Program analysis and machine learning service to detect potential defects and improvements in Java and Python code. [Overview](https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/welcome.html)
- `amazonka-codeguruprofiler`: Collects runtime performance data from applications, and provides recommendations to help fine-tune application performance. [Overview](https://docs.aws.amazon.com/codeguru/latest/profiler-ug/what-is-codeguru-profiler.html)
- `amazonka-codestar-connections`: Connect services like AWS CodePipeline to third-party source code providers. [Overview](https://docs.aws.amazon.com/codestar-connections/latest/APIReference/Welcome.html)
- `amazonka-codestar-notifications`: Subscribe to events from AWS CodeBuild, AWS CodeCommit, AWS CodeDeploy, and AWS CodePipeline. [Overview](https://docs.aws.amazon.com/codestar-notifications/latest/APIReference/Welcome.html)
- `amazonka-comprehendmedical`: Extract information from unstructured medical text. [Overview](https://aws.amazon.com/comprehend/medical/)
- `amazonka-compute-optimizer`: Recommends optimal AWS resources to reduce costs and improve performance. [Overview](https://aws.amazon.com/compute-optimizer/)
- `amazonka-connect-contact-lens`: Undertand the sentiment and trends of customer conversations to identify crucial company and product feedback. [Overview](https://aws.amazon.com/connect/contact-lens/)
- `amazonka-connectparticipant`: Amazon Connect APIs for chat participants, such as agents and customers. [Overview](https://docs.aws.amazon.com/connect-participant/latest/APIReference/Welcome.html)
- `amazonka-customer-profiles`: Unified view of customer profiles in Amazon Connect. [Overview](https://aws.amazon.com/connect/customer-profiles/)
- `amazonka-databrew`: Visual data preparation tool for data analysts and data scientists to clean and normalize data. [Overview](https://aws.amazon.com/glue/features/databrew/)
- `amazonka-dataexchange`: Easily find and subscribe to third-party data in the cloud. [Overview](https://aws.amazon.com/data-exchange/)
- `amazonka-datasync`: Connect to your storage, connect to our storage, and start copying. [Overview](https://aws.amazon.com/datasync/)
- `amazonka-detective`: Analyze and visualize security data to get to the root cause of potential security issues. [Overview](https://aws.amazon.com/detective/)
- `amazonka-devops-guru`: ML-powered cloud operations service to improve application availability. [Overview](https://aws.amazon.com/devops-guru/)
- `amazonka-dlm`: Data Lifecycle Manager --- manage the lifecycle of your AWS resources. [Overview](https://docs.aws.amazon.com/dlm/latest/APIReference/Welcome.html)
- `amazonka-docdb`: Managed document database service. [Overview](https://aws.amazon.com/documentdb/)
- `amazonka-ebs`: High performance block storage. [Overview](https://aws.amazon.com/ebs/)
- `amazonka-ec2-instance-connect`: Connect to Linux EC2 instances using SSH. [Overview](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Connect-using-EC2-Instance-Connect.html)
- `amazonka-ecr-public`: Public, managed container image registry service. [Overview](https://docs.aws.amazon.com/AmazonECRPublic/latest/APIReference/Welcome.html)
- `amazonka-elastic-inference`: Attach low-cost GPU-powered acceleration to Amazon EC2 and Sagemaker instances or Amazon ECS tasks. [Overview](https://aws.amazon.com/machine-learning/elastic-inference/)
- `amazonka-emr-containers`: Amazon EMR on EKS. [Overview](https://docs.aws.amazon.com/emr-on-eks/latest/APIReference/Welcome.html)
- `amazonka-finspace-data`: Data operations for Amazon FinSpace. [Overview](https://docs.aws.amazon.com/finspace/latest/data-api/fs-api-welcome.html)
- `amazonka-finspace`: Data management and analytics service purpose-built for the financial services industry. [Overview](https://aws.amazon.com/finspace/)
- `amazonka-fis`: `us-east-1` as a service. [Overview](https://aws.amazon.com/fis/)
- `amazonka-forecast`: Time-series forecasting service based on machine learning, and built for business metrics analysis. [Overview](https://aws.amazon.com/forecast/)
- `amazonka-forecastquery`: Amazon Forecast Query Service. [Overview](https://docs.aws.amazon.com/forecast/latest/dg/API_Operations_Amazon_Forecast_Query_Service.html)
- `amazonka-frauddetector`: Managed fraud detection service using machine learning to identify potentially fraudulent activities and catch fraud faster. [Overview](https://aws.amazon.com/fraud-detector/)
- `amazonka-globalaccelerator`: Improve global application availability and performance using the AWS global network. [Overview](https://aws.amazon.com/global-accelerator/)
- `amazonka-grafana`: Managed service for the Grafana analytics platform. [Overview](https://aws.amazon.com/grafana/)
- `amazonka-greengrassv2`: [Overview](https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-v2-whats-new.html)
- `amazonka-groundstation`: Control satellites and ingest data with fully managed Ground Station as a Service. [Overview](https://aws.amazon.com/ground-station/)
- `amazonka-healthlake`: Securely store, transform, query, and analyze health data. [Overview](https://aws.amazon.com/healthlake/)
- `amazonka-honeycode`: Quickly build mobile and web apps for teams—without programming. [Overview](https://honeycode.aws)
- `amazonka-identitystore`: AWS Single Sign-On (SSO) Identity Store. [Overview](https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/welcome.html)
- `amazonka-imagebuilder`: AWS EC2 Image Builder --- Automate the creation, management, and deployment of customized, secure, and up-to-date server images. [Overview](https://docs.aws.amazon.com/imagebuilder/latest/userguide/what-is-image-builder.html)
- `amazonka-iot1click-devices`:  [Overview](https://docs.aws.amazon.com/iot-1-click/1.0/devices-apireference/what-is-iot.html)
- `amazonka-iot1click-projects`: [Overview](https://docs.aws.amazon.com/iot-1-click/latest/projects-apireference/Welcome.html)
- `amazonka-iotdeviceadvisor`: Managed cloud-based test capability to validate IoT devices for reliable and secure connectivity with AWS IoT Core. [Overview](https://aws.amazon.com/iot-core/device-advisor/)
- `amazonka-iotevents-data`: Send inputs to detectors, list detectors, and view or update a detector's status. [Overview](https://docs.aws.amazon.com/iotevents/latest/apireference/Welcome.html)
- `amazonka-iotevents`: Easily detect and respond to events from IoT sensors and applications. [Overview](https://aws.amazon.com/iot-events/)
- `amazonka-iotfleethub`: Build standalone web applications for monitoring the health of your device fleets. [Overview](https://docs.aws.amazon.com/iot/latest/fleethubuserguide/what-is-aws-iot-monitor.html)
- `amazonka-iotsecuretunneling`: Establish bidirectional communication to remote devices over a secure connection that is managed by AWS IoT. [Overview](https://docs.aws.amazon.com/iot/latest/developerguide/secure-tunneling.html)
- `amazonka-iotsitewise`: Collect, organize, and analyze data from industrial equipment at scale. [Overview](https://aws.amazon.com/iot-sitewise/)
- `amazonka-iotthingsgraph`: Visually develop IoT applications. [Overview](https://aws.amazon.com/iot-things-graph/)
- `amazonka-iotwireless`: Connect, manage, and secure LoRaWAN devices at scale. [Overview](https://aws.amazon.com/iot-core/lorawan/)
- `amazonka-ivs`: Live streaming solution, ideal for creating interactive video experiences. [Overview](https://aws.amazon.com/ivs/)
- `amazonka-kafka`: Control plane operations for Amazon Managed Streaming for Apache Kafka. [Overview](https://aws.amazon.com/msk/what-is-kafka/)
- `amazonka-kafkaconnect`: Deploy fully managed connectors built for Kafka Connect. [Overview](https://docs.aws.amazon.com/msk/latest/developerguide/msk-connect.html)
- `amazonka-kinesis-video-signaling`: Kinesis Video Streams with WebRTC. [Overview](https://docs.aws.amazon.com/kinesisvideostreams-webrtc-dg/latest/devguide/what-is-kvswebrtc.html)
- `amazonka-kinesisanalyticsv2`: Managed service that you can use to process and analyze streaming data. [Overview](https://docs.aws.amazon.com/kinesisanalytics/latest/apiv2/Welcome.html)
- `amazonka-lakeformation`: Build a secure data lake in days. [Overview](https://aws.amazon.com/lake-formation/)
- `amazonka-license-manager`: Set rules to manage, discover, and report software license usage. [Overview](https://aws.amazon.com/license-manager/)
- `amazonka-location`: Securely and easily add location data to applications. [Overview](https://aws.amazon.com/location/)
- `amazonka-lookoutequipment`: Detect abnormal equipment behavior by analyzing sensor data. [Overview](https://aws.amazon.com/lookout-for-equipment/)
- `amazonka-lookoutmetrics`: Automatically detect anomalies in metrics and identify their root cause. [Overview](https://aws.amazon.com/lookout-for-metrics/)
- `amazonka-lookoutvision`: Spot product defects using computer vision to automate quality inspection. [Overview](Spot product defects using computer vision to automate quality inspection)
- `amazonka-macie`: Amazon Macie Classic. [Overview](https://docs.aws.amazon.com/macie/latest/userguide/what-is-macie.html)
- `amazonka-macie2`: Discover and protect your sensitive data at scale. [Overview](https://aws.amazon.com/macie/)
- `amazonka-managedblockchain`: Easily create and manage scalable blockchain networks. [Overview](https://aws.amazon.com/managed-blockchain/)
- `amazonka-marketplace-catalog`: [Overview](https://docs.aws.amazon.com/marketplace-catalog/latest/api-reference/welcome.html)
- `amazonka-mediaconnect`: Secure, reliable live video transport. [Overview](https://aws.amazon.com/mediaconnect/)
- `amazonka-mediapackage-vod`: [Overview](https://docs.aws.amazon.com/mediapackage/latest/ug/vod-content.html)
- `amazonka-mediatailor`: Linear channel assembly and personalized ad-insertion. [Overview](https://aws.amazon.com/mediatailor/)
- `amazonka-memorydb`: Redis-compatible, durable, in-memory database service. [Overview](https://aws.amazon.com/memorydb/)
- `amazonka-mgn`: AWS Application Migration Service. [Overview](https://aws.amazon.com/application-migration-service/)
- `amazonka-migrationhub-config`: [Overview](https://docs.aws.amazon.com/migrationhub-home-region/latest/APIReference/Welcome.html)
- `amazonka-mwaa`: Highly available, secure, and managed workflow orchestration for Apache Airflow. [Overview](https://aws.amazon.com/managed-workflows-for-apache-airflow/)
- `amazonka-neptune`: Build and run graph applications with highly connected datasets. [Overview](https://aws.amazon.com/neptune/)
- `amazonka-network-firewall`: Deploy network security across your Amazon VPCs. [Overview](https://aws.amazon.com/network-firewall/)
- `amazonka-networkmanager`: Create a global network in which you can monitor your AWS and on-premises networks that are built around transit gateways. [Overview](https://docs.aws.amazon.com/networkmanager/latest/APIReference/Welcome.html)
- `amazonka-nimble`: Empower creative studios to produce visual effects, animation, and interactive content entirely in the cloud. [Overview](https://aws.amazon.com/nimble-studio/)
- `amazonka-opensearch`: Distributed, open-source search and analytics suite. (Successor to Amazon Elasticsearch Service.) [Overview](https://aws.amazon.com/opensearch-service/the-elk-stack/what-is-opensearch/)
- `amazonka-outposts`: Run AWS infrastructure and services on premises. [Overview](https://aws.amazon.com/outposts/)
- `amazonka-panorama`: Improve your operations with computer vision at the edge. [Overview](https://aws.amazon.com/panorama/)
- `amazonka-personalize-events`: Event ingestion for Amazon Personalize. [Overview](https://docs.aws.amazon.com/personalize/latest/dg/recording-events.html)
- `amazonka-personalize-runtime`: [Overview](https://docs.aws.amazon.com/personalize/latest/dg/API_Operations_Amazon_Personalize_Runtime.html)
- `amazonka-personalize`: Create real-time personalized user experiences faster at scale. [Overview](https://aws.amazon.com/personalize/)
- `amazonka-pi`: Analyze and tune Amazon RDS database performance. [Overview](https://aws.amazon.com/rds/performance-insights/)
- `amazonka-pinpoint-email`: [Overview](https://docs.aws.amazon.com/pinpoint-email/latest/APIReference/Welcome.html)
- `amazonka-pinpoint-sms-voice`: [Overview](https://docs.aws.amazon.com/pinpoint-sms-voice/latest/APIReference/welcome.html)
- `amazonka-proton`: Automate management for container and serverless deployments. [Overview](https://aws.amazon.com/proton/)
- `amazonka-qldb-session`: [Overview](https://docs.aws.amazon.com/qldb/latest/developerguide/API_Operations_Amazon_QLDB_Session.html)
- `amazonka-quicksight`: Scalable, serverless, embeddable, ML-powered BI service. [Overview](https://aws.amazon.com/quicksight/)
- `amazonka-ram`: Securely share your resources across AWS accounts, organizational units (OUs), and with IAM users and roles. [Overview](https://aws.amazon.com/ram/)
- `amazonka-rds-data`: Run SQL statements on Amazon Aurora Serverless DB cluster. [Overview](https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html)
- `amazonka-redshift-data`: Access Amazon Redshift data over HTTPS. [Overview](https://docs.aws.amazon.com/redshift/latest/mgmt/data-api.html)
- `amazonka-robomaker`: Run, scale, and automate robotics simulation. [Overview](https://aws.amazon.com/robomaker/)
- `amazonka-route53-recovery-cluster`: Simplify and automate recovery for highly available applications. [Overview](https://aws.amazon.com/route53/application-recovery-controller/)
- `amazonka-route53-recovery-control-config`: [Overview](https://docs.aws.amazon.com/recovery-cluster/latest/api/what-is-recovery-control.html)
- `amazonka-route53-recovery-readiness`: [Overview](https://awscli.amazonaws.com/v2/documentation/api/latest/reference/route53-recovery-readiness/index.html)
- `amazonka-route53resolver`: [Overview](https://docs.aws.amazon.com/Route53/latest/APIReference/API_Operations_Amazon_Route_53_Resolver.html)
- `amazonka-s3outposts`: Object storage in your on-premises environments. [Overview](https://aws.amazon.com/s3/outposts/)
- `amazonka-sagemaker-a2i-runtime`: Add human judgment to any machine learning application. [Overview](https://docs.aws.amazon.com/augmented-ai/2019-11-07/APIReference/Welcome.html)
- `amazonka-sagemaker-edge`: Manage and monitor ML models across fleets of smart devices. [Overview](https://aws.amazon.com/sagemaker/edge-manager/)
- `amazonka-sagemaker-featurestore-runtime`: A fully managed repository for machine learning features. [Overview](https://aws.amazon.com/sagemaker/feature-store/)
- `amazonka-savingsplans`: Flexible pricing model for AWS Services. [Overview](https://aws.amazon.com/savingsplans/)
- `amazonka-schemas`: Collect and organize schemas so that your schemas are in logical groups. [Overview](https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-schema.html)
- `amazonka-securityhub`: Automate AWS security checks and centralize security alerts. [Overview](https://aws.amazon.com/security-hub/)
- `amazonka-service-quotas`: View and manage your quotas. [Overview](https://docs.aws.amazon.com/servicequotas/2019-06-24/apireference/Welcome.html)
- `amazonka-servicecatalog-appregistry`: Create a repository of your applications and associated resources. [Overview](https://docs.aws.amazon.com/servicecatalog/latest/adminguide/appregistry.html)
- `amazonka-signer`: Managed code-signing service to ensure the trust and integrity of your code. [Overview](https://docs.aws.amazon.com/signer/latest/developerguide/Welcome.html)
- `amazonka-sms-voice`:
- `amazonka-snow-device-management`: [Overview](https://docs.aws.amazon.com/snowball/latest/api-reference/API_Operations_AWS_Snow_Device_Management.html)
- `amazonka-ssm-contacts`: Systems Manager Incident Manager Contacts. [Overview](https://docs.aws.amazon.com/incident-manager/latest/APIReference/API_Operations_AWS_Systems_Manager_Incident_Manager_Contacts.html)
- `amazonka-ssm-incidents`: Incident management console to help users mitigate and recover from incidents. [Overview](https://docs.aws.amazon.com/incident-manager/latest/userguide/what-is-incident-manager.html)
- `amazonka-sso-admin`: [Overview](https://docs.aws.amazon.com/singlesignon/latest/APIReference/welcome.html)
- `amazonka-sso-oidc`: AWS Single Sign-On OpenID Connect. [Overview](https://docs.aws.amazon.com/singlesignon/latest/OIDCAPIReference/Welcome.html)
- `amazonka-sso`: Single Sign-On Portal. Centrally manage access to multiple AWS accounts or applications. [Overview](https://aws.amazon.com/single-sign-on/)
- `amazonka-synthetics`: Amazon CloudWatch Synthetics --- create canaries, configurable scripts that run on a schedule, to monitor your endpoints and APIs. [Overview](https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries.html)
- `amazonka-transfer`: Simple, secure, and scalable file transfers. [Overview](https://aws.amazon.com/aws-transfer-family/)
- `amazonka-voice-id`: Real-time caller authentication and fraud risk detection using ML-powered voice analysis. [Overview](https://aws.amazon.com/connect/voice-id/)
- `amazonka-wellarchitected`: Programmatic access to the AWS Well-Architected Tool. [Overview](https://docs.aws.amazon.com/wellarchitected/latest/APIReference/Welcome.html)
- `amazonka-wisdom`: Deliver agents the information they need to solve issues in real-time. [Overview](https://aws.amazon.com/connect/wisdom/)
- `amazonka-worklink`: Provide secure mobile access to internal websites and web apps. [Overview](https://aws.amazon.com/worklink/)
- `amazonka-workmailmessageflow`: [Overview](https://docs.aws.amazon.com/workmail/latest/APIReference/API_Operations_Amazon_WorkMail_Message_Flow.html)

## [1.6.1](https://github.com/brendanhay/amazonka/tree/1.6.1)
Released: **03 Feb, 2019**, Compare: [1.6.0](https://github.com/brendanhay/amazonka/compare/1.6.0...1.6.1)

### Fixed

- Correctly catch exceptions in perform. [\#464](https://github.com/brendanhay/amazonka/pull/464)

### Changed

- Add a `MonadUnliftIO` instance for `AWST'`. [\#461](https://github.com/brendanhay/amazonka/pull/461)
- Add FromText instances for Int64, String and Seconds. [\#489](https://github.com/brendanhay/amazonka/pull/489)
- Add generalBracket for MonadMask instance on AWST'. [\#492](https://github.com/brendanhay/amazonka/pull/492)
- Remove fractional seconds from serialization of iso8601 timestamps [\#502](https://github.com/brendanhay/amazonka/pull/502)


## [1.6.0](https://github.com/brendanhay/amazonka/tree/1.6.0)
Released: **16 May, 2018**, Compare: [1.5.0](https://github.com/brendanhay/amazonka/compare/1.5.0...1.6.0)

### Fixed

- GHC 8.4 compatibility. [\#456](https://github.com/brendanhay/amazonka/pull/456)
- Conduit `1.3` compatibility. [\#449](https://github.com/brendanhay/amazonka/pull/449)
- S3 `BucketName` now has a `FromJSON` instance. [\#452](https://github.com/brendanhay/amazonka/pull/452)
- S3 `ListObjectsV2` is now named correctly. [\#447](https://github.com/brendanhay/amazonka/pull/447)
- HTTP `Expect` headers are now stripped when presigning URLs. [\#444](https://github.com/brendanhay/amazonka/pull/444)
- Duplicate generated files no longer exist on case-insensitive file systems. [\#429](https://github.com/brendanhay/amazonka/pull/429), [\#655](https://github.com/brendanhay/amazonka/pull/655)
- EC2 metadata's instance identity document now has the correct(er) field types. [\#428](https://github.com/brendanhay/amazonka/pull/428)
- OpsWorks `DescribeApps` and `DescribeStacks` now correctly handle optional attribute values. [\#436](https://github.com/brendanhay/amazonka/pull/436), [\#438](https://github.com/brendanhay/amazonka/pull/438)

### Breaking Changes

- Lambda and other `rest-json` services with binary response payloads now return a raw `ByteString` instead of `HashMap Text Value`,
  fixing an issue where an empty body could not be deserialized. [\#428](https://github.com/brendanhay/amazonka/pull/394), [\#428](https://github.com/brendanhay/amazonka/pull/407)

### New Libraries

- `amazonka-alexa-business`: Alexa for Business SDK. [Overview](https://aws.amazon.com/alexaforbusiness/)
- `amazonka-appsync`: Automatically update data in web and mobile applications in real time, and updates data for offline users as soon as they reconnect. [Overview](https://aws.amazon.com/appsync/)
- `amazonka-autoscaling-plans`: Create instructions to configure dynamic scaling for the scalable resources in your application. [Overview](https://aws.amazon.com/autoscaling/)
- `amazonka-certificatemanager-pca`: Create a secure private certificate authority (CA). [Overview](https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaWelcome.html)
- `amazonka-cloud9`: A cloud IDE for writing, running, and debugging code. [Overview](https://aws.amazon.com/cloud9/)
- `amazonka-comprehend`: Natural language processing (NLP) service that uses machine learning to find insights and relationships in text. [Overview](https://aws.amazon.com/comprehend/)
- `amazonka-connect`: Simple to use, cloud-based contact center. [Overview](https://aws.amazon.com/connect/)
- `amazonka-cost-explorer`: Dive deeper into your cost and usage data to identify trends, pinpoint cost drivers, and detect anomalies. [Overview](https://aws.amazon.com/aws-cost-management/aws-cost-explorer/)
- `amazonka-fms`: Centrally configure and manage firewall rules across accounts and applications. [Overview](https://aws.amazon.com/firewall-manager/)
- `amazonka-guardduty`: Intelligent threat detection and continuous monitoring to protect your AWS accounts and workloads. [Overview](https://aws.amazon.com/guardduty/)
- `amazonka-iot-analytics`: Analytics for IoT devices. [Overview](https://aws.amazon.com/iot-analytics/)
- `amazonka-iot-jobs-dataplane`: Define a set of remote operations that are sent to and executed on one or more devices connected to AWS IoT. [Overview](https://docs.aws.amazon.com/iot/latest/developerguide/iot-jobs.html)
- `amazonka-kinesis-video`: Capture, process, and store video streams for analytics and machine learning. [Overview](https://aws.amazon.com/kinesis/video-streams/)
- `amazonka-kinesis-video-media`: Media support for Kinesis Video. [Overview](https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_Operations_Amazon_Kinesis_Video_Streams_Media.html)
- `amazonka-kinesis-video-archived-media`: Archived media support for Kinesis Video. [Overview](https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_Operations_Amazon_Kinesis_Video_Streams_Archived_Media.html)
- `amazonka-mediaconvert`: Process video files and clips to prepare on-demand content for distribution or archiving. [Overview](https://aws.amazon.com/mediaconvert/)
- `amazonka-medialive`: Encode live video for broadcast and streaming to any device. [Overview](https://aws.amazon.com/medialive/)
- `amazonka-mediapackage`: Easily prepare and protect video for delivery to Internet devices. [Overview](https://aws.amazon.com/mediapackage/)
- `amazonka-mediastore`: Store and deliver video assets for live streaming media workflows. [Overview](https://aws.amazon.com/mediastore/)
- `amazonka-mediastore-dataplane`: MediaStore data plane. [Overview](https://docs.aws.amazon.com/mediastore/latest/apireference/API_Operations_AWS_Elemental_MediaStore_Data_Plane.html)
- `amazonka-mq`: Managed message broker service for Apache ActiveMQ. [Overview](https://aws.amazon.com/amazon-mq/)
- `amazonka-resourcegroups`: Resource groups make it easier to manage and automate tasks on large numbers of resources at one time. [Overview](https://docs.aws.amazon.com/ARG/latest/userguide/welcome.html)
- `amazonka-route53-autonaming`: Auto naming makes it easier to provision instances for microservices by automating DNS configuration. [Overview](https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/autonaming.html)
- `amazonka-sagemaker`: Build, train, and deploy machine learning models at scale. [Overview](https://aws.amazon.com/sagemaker/)
- `amazonka-sagemaker-runtime`: Get inferences from SageMaker models. [Overview](https://docs.aws.amazon.com/sagemaker/latest/dg/API_Operations_Amazon_SageMaker_Runtime.html)
- `amazonka-secretsmanager`: Easily rotate, manage, and retrieve database credentials, API keys, and other secrets through their lifecycle. [Overview](https://aws.amazon.com/secrets-manager/)
- `amazonka-serverlessrepo`: Discover, deploy, and publish serverless applications. [Overview](https://aws.amazon.com/serverless/serverlessrepo/)
- `amazonka-transcribe`: Automatic speech recognition. [Overview](https://aws.amazon.com/transcribe/)
- `amazonka-translate`: A neural machine translation service that delivers fast, high-quality, and affordable language translation. [Overview](https://aws.amazon.com/translate/)
- `amazonka-workmail`: Secure, managed business email and calendar service with support for existing desktop and mobile email client applications. [Overview](https://aws.amazon.com/workfmail/)

### Updated Service Definitions

> All service definitions and services have been updated and regenerated.
Please see each individual library's commits for a list of changes.


## [1.5.0](https://github.com/brendanhay/amazonka/tree/1.5.0)
Released: **15 November, 2017**, Compare: [1.4.5](https://github.com/brendanhay/amazonka/compare/1.4.5...1.5.0)

### Fixed

- V4 Signing Metadata is now correctly calculated for chunked request bodies. [\#403](https://github.com/brendanhay/amazonka/pull/403)
- DynamoDB Query/Scan pagination will correctly return all available items. [\#392](https://github.com/brendanhay/amazonka/pull/392)
- S3 `ReplicationStatus` is now parsed correctly. [\#372](https://github.com/brendanhay/amazonka/pull/372)
- OpsWorks `LayerAttributes` now correctly returns `Maybe` for `Map` values. [\#398](https://github.com/brendanhay/amazonka/pull/398)
- `newLogger` now (correctly) does not set binary mode for any passed handle. [\#381](https://github.com/brendanhay/amazonka/pull/381)
- Improved support for handling S3's `list-type=2` query strings. [\#391](https://github.com/brendanhay/amazonka/pull/391)
- Cabal files now have their `license-field` changed from `OtherLicense` to the correct `MPL-2.0`.

### Added

- Add AWS Signer for V2 Header Authentication. [\#383](https://github.com/brendanhay/amazonka/pull/383)
- Add support for ECS credentials discovery via the ECS container agent. [\#388](https://github.com/brendanhay/amazonka/pull/388)
- Add new regions `Montreal` (ca-central-1) and `London` (eu-west-2). [\#367](https://github.com/brendanhay/amazonka/pull/367)
- Add `hashedFileRange` and `chunkedFileRange` for preparing request bodies from file ranges. [\#359](https://github.com/brendanhay/amazonka/pull/359)

### New Libraries

- `amazonka-mobile`: Add and configure features for mobile apps, including authentication, data storage, backend logic, push notifications, content delivery, and analytics. [Overview](https://aws.amazon.com/mobile)
- `amazonka-pricing`: Price lists, pricing details, and pricing overview. [Overview](https://aws.amazon.com/pricing)
- `amazonka-athena`: An interactive query service that makes it easy to analyze data in Amazon S3 using standard SQL. [Overview](https://aws.amazon.com/athena)
- `amazonka-cloudhsmv2`: The newest (incompatible) API of AWS CloudHSM. [Overview](https://aws.amazon.com/cloudhsmv2)
- `amazonka-codestar`: Use a variety of project templates to start developing applications on Amazon EC2, AWS Lambda, and AWS Elastic Beanstalk. [Overview](https://aws.amazon.com/codestar)
- `amazonka-dynamodb-dax`: DynamoDB Accelerator (DAX) is a fully managed, highly available, in-memory cache for DynamoDB that delivers up to a 10x performance improvement. [Overview](https://aws.amazon.com/dynamodb/dax)
- `amazonka-glue`: A fully managed extract, transform, and load (ETL) service that makes it easy for customers to prepare and load their data for analytics. [Overview](https://aws.amazon.com/glue)
- `amazonka-greengrass`: Run local compute, messaging, data caching, and sync capabilities for connected devices in a secure way. [Overview](https://aws.amazon.com/greengrass)
- `amazonka-lex-runtime`: Build applications using a speech or text interface powered by the same technology that powers Amazon Alexa. [Overview](https://aws.amazon.com/lex)
- `amazonka-lex-models`: Build applications using a speech or text interface powered by the same technology that powers Amazon Alexa. [Overview](https://aws.amazon.com/lex)
- `amazonka-marketplace-entitlement`: Markplace entitlements service. [Overview](https://aws.amazon.com/marketplace)
- `amazonka-resourcegroupstagging`: Group and tag AWS resources. [Overview](https://docs.aws.amazon.com/resourcegroupstagging)

### Updated Service Definitions

> All service definitions and services have been updated and regenerated.
Please see each individual library's commits for a list of changes.


## [1.4.5](https://github.com/brendanhay/amazonka/tree/1.4.5)
Released: **04 December, 2016**, Compare: [1.4.4](https://github.com/brendanhay/amazonka/compare/1.4.4...1.4.5)

### Fixed

- Generated Haddock documentation is now more readable/consistent. [\#331](https://github.com/brendanhay/amazonka/pull/331)
- `Expect: 100-continue` HTTP headers are now only added to S3 `PutObject` requests. [\#338](https://github.com/brendanhay/amazonka/pull/338)

### Changed

- Add new regions `Ohio` (us-east-2) and `Seoul` (ap-northeast-2). [\#334](https://github.com/brendanhay/amazonka/pull/334)
- The `Bombay` region has been renamed to `Mumbai`. [\#334](https://github.com/brendanhay/amazonka/pull/334)
- Route53 HostedZone and DelegateSet identifiers are now stripped, similarly to other SDKs. [\#336](https://github.com/brendanhay/amazonka/pull/336)

### New Libraries

- `amazonka-xray`: Analyze and debug production, distributed applications, such as those built using a microservices architecture. [Overview](https://aws.amazon.com/xray/)
- `amazonka-stepfunctions`: Coordinate the components of distributed applications and microservices using visual workflows. [Overview](https://aws.amazon.com/step-functions/)
- `amazonka-ssm`: Automate collecting system inventory, applying OS patches, creation of AMIs, and configuring OSes and applications at scale. [API Reference](http://docs.aws.amazon.com/ssm/latest/APIReference/Welcome.html)
- `amazonka-snowball` (+ `snowball-edge`): Data transport solution using secure appliances to transfer large data into and out of AWS. [Overview](https://aws.amazon.com/snowball/)
- `amazonka-shield`: DDoS protection service for web applications using ELB, CloudFront, and Route 53. [Overview](https://aws.amazon.com/shield/)
- `amazonka-rekognition`: Image analysis service for detecting objects, scenes, and faces in images. [Overview](https://aws.amazon.com/rekognition/)
- `amazonka-polly`: Turn text into lifelike speech. Supports 24 languages and 47 lifelike voices. [Overview](https://aws.amazon.com/polly/)
- `amazonka-pinpoint`: Targeted push notification campaigns to improve engagement in mobile apps. [Overview](https://aws.amazon.com/pinpoint/)
- `amazonka-opsworks-cm`: Managed Chef Automated for OpsWorks. [Overview](https://aws.amazon.com/opsworks/)
- `amazonka-lightsail`: Launch and manage a virtual private servers. [Overview](https://aws.amazon.com/lightsail/)
- `amazonka-health`: Personalized service dashboard of your AWS service health. [Overview](https://aws.amazon.com/premiumsupport/phd/)
- `amazonka-codebuild`: Continuously build and test your code, paying for what you use. [Overview](https://aws.amazon.com/codebuild/)
- `amazonka-appstream` (Version 2): Stream desktop applications to any device running a browser. [Overview](https://aws.amazon.com/appstream2/)
- `amazonka-budgets`: Plan your usage and costs. [User Guide](http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/budgets-managing-costs.html)
- `amazonka-sms`: Automate, schedule, and track incremental replications of live server volumes from on premise to AWS. [Overview](https://aws.amazon.com/server-migration-service/)

### Updated Service Definitions

> The following services contain a large number of definition updates.
Please review the linked commit for each library for specific changes:

- [APIGateway](https://github.com/brendanhay/amazonka/commit/1b724ac) - `amazonka-apigateway`
- [ApplicationAutoScaling](https://github.com/brendanhay/amazonka/commit/pe279d35) - `amazonka-application-autoscaling`
- [AutoScaling](https://github.com/brendanhay/amazonka/commit/ed347ed) - `amazonka-autoscaling`
- [CertificateManager](https://github.com/brendanhay/amazonka/commit/d930be6) - `amazonka-certificatemanager`
- [CloudFormation](https://github.com/brendanhay/amazonka/commit/ce82485) - `amazonka-cloudformation`
- [CloudFront](https://github.com/brendanhay/amazonka/commit/4e292a8) - `amazonka-cloudfront`
- [CloudTrail](https://github.com/brendanhay/amazonka/commit/fc26b72) - `amazonka-cloudtrail`
- [CloudWatchLogs](https://github.com/brendanhay/amazonka/commit/2eec1a3) - `amazonka-cloudwatch-logs`
- [CloudWatch](https://github.com/brendanhay/amazonka/commit/b023497) - `amazonka-cloudwatch`
- [CodeDeploy](https://github.com/brendanhay/amazonka/commit/p5f4eee6) - `amazonka-codedeploy`
- [CodePipeline](https://github.com/brendanhay/amazonka/commit/3a3ce67) - `amazonka-codepipeline`
- [CognitoIdentityProvider](https://github.com/brendanhay/amazonka/commit/388099f) - `amazonka-cognito-idp`
- [Config](https://github.com/brendanhay/amazonka/commit/84dda25) - `amazonka-config`
- [DeviceFarm](https://github.com/brendanhay/amazonka/commit/b13f2fa) - `amazonka-devicefarm`
- [DirectConnect](https://github.com/brendanhay/amazonka/commit/bd0fd3d) - `amazonka-directconnect`
- [DirectoryService](https://github.com/brendanhay/amazonka/commit/582c047) - `amazonka-ds`
- [EC2](https://github.com/brendanhay/amazonka/commit/4117a08) - `amazonka-ec2`
- [ECR](https://github.com/brendanhay/amazonka/commit/3362a22) - `amazonka-ecr`
- [ECS](https://github.com/brendanhay/amazonka/commit/c731732) - `amazonka-ecs`
- [EFS](https://github.com/brendanhay/amazonka/commit/62e9351) - `amazonka-efs`
- [ELBv2](https://github.com/brendanhay/amazonka/commit/8c5ae35) - `amazonka-elbv2`
- [EMR](https://github.com/brendanhay/amazonka/commit/0538f37) - `amazonka-emr`
- [ElastiCache](https://github.com/brendanhay/amazonka/commit/9c2e52d) - `amazonka-elasticache`
- [ElasticBeanstalk](https://github.com/brendanhay/amazonka/commit/16320d2) - `amazonka-elasticbeanstalk`
- [ElasticTranscoder](https://github.com/brendanhay/amazonka/commit/c216e5c) - `amazonka-elastictranscoder`
- [GameLift](https://github.com/brendanhay/amazonka/commit/bfd74cf) - `amazonka-gamelift`
- [Glacier](https://github.com/brendanhay/amazonka/commit/70c7268) - `amazonka-glacier`
- [IoT](https://github.com/brendanhay/amazonka/commit/25176bd) - `amazonka-iot`
- [KMS](https://github.com/brendanhay/amazonka/commit/2e28104) - `amazonka-kms`
- [KinesisAnalytics](https://github.com/brendanhay/amazonka/commit/8df7d3d) - `amazonka-kinesis-analytics`
- [Kinesis](https://github.com/brendanhay/amazonka/commit/ab93e87) - `amazonka-kinesis`
- [Lambda](https://github.com/brendanhay/amazonka/commit/570d674) - `amazonka-lambda`
- [MarketplaceMetering](https://github.com/brendanhay/amazonka/commit/d93c185) - `amazonka-marketplace-metering`
- [OpsWorks](https://github.com/brendanhay/amazonka/commit/e49f255) - `amazonka-opsworks`
- [RDS](https://github.com/brendanhay/amazonka/commit/0df4ee5) - `amazonka-rds`
- [Redshift](https://github.com/brendanhay/amazonka/commit/e0c9f54) - `amazonka-redshift`
- [Route53](https://github.com/brendanhay/amazonka/commit/1a48a46) - `amazonka-route53`
- [S3](https://github.com/brendanhay/amazonka/commit/9852585) - `amazonka-s3`
- [SES](https://github.com/brendanhay/amazonka/commit/38150dc) - `amazonka-ses`
- [SQS](https://github.com/brendanhay/amazonka/commit/ac22d92) - `amazonka-sqs`
- [ServiceCatalog](https://github.com/brendanhay/amazonka/commit/e91184d) - `amazonka-servicecatalog`
- [WAF](https://github.com/brendanhay/amazonka/commit/86bcd26) - `amazonka-waf`


## [1.4.4](https://github.com/brendanhay/amazonka/tree/1.4.4)
Released: **23 October, 2016**, Compare: [1.4.3](https://github.com/brendanhay/amazonka/compare/1.4.3...1.4.4)

### Fixed [\#306](https://github.com/brendanhay/amazonka/pull/306)

- Kinesis `SharedLevelMetrics` now correctly deserializes empty responses. [\#306](https://github.com/brendanhay/amazonka/pull/306)

### Changed

- `newEnv` no longer takes `Region` as a parameter and instead defaults to `us-east-1`
  per other SDK behaviour. The new `Env` can be configured using the `envRegion` lens
  or the `within` combinator.
- Region is now discovered via the `InstanceIdentity` metadata document. [\#308](https://github.com/brendanhay/amazonka/pull/308)
- Region can now be overridden via the `AWS_REGION` environment variable. [\#308](https://github.com/brendanhay/amazonka/pull/308)


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

### New Libraries

- `amazonka-discovery`: Discover on-premises application inventory and dependencies. [Overview](https://aws.amazon.com/application-discovery/)
- `amazonka-application-autoscaling`: General purpose scaling of AWS resources. [API Reference](http://docs.aws.amazon.com/ApplicationAutoScaling/latest/APIReference/Welcome.html)

### Updated Service Definitions

- [WorkSpaces](https://github.com/brendanhay/amazonka/commit/b869bf0)
- [StorageGateway](https://github.com/brendanhay/amazonka/commit/9af316)
- [SSM](https://github.com/brendanhay/amazonka/commit/542f50c)
- [S3](https://github.com/brendanhay/amazonka/commit/84d66a6)
- [RDS](https://github.com/brendanhay/amazonka/commit/98edec9)
- [EC2](https://github.com/brendanhay/amazonka/commit/93b6b72)


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

### New Libraries

- `amazonka-apigateway`: API Gateway Service.

### Updated Services Definitions

- SSM: Multiple additions and documentation updates.
- DynamoDB: Paginator updates.


## [1.3.4](https://github.com/brendanhay/amazonka/tree/1.3.4)
Released: **25 October, 2015**, Compare: [1.3.3](https://github.com/brendanhay/amazonka/compare/1.3.3...1.3.4)

### New Libraries

- `amazonka-iot-dataplane`: Internet of Things Data Plane.

### Updated Services Definitions

- AutoScaling: Documentation updates.
- EC2: Paginator updates.
- Glacier: Paginator additions.
- IAM: Paginator, type, and documentation updates.
- KMS: Multiple type, operation, and documentation updates.
- S3: Minor type updates. (Server side encryption related.)


## [1.3.3](https://github.com/brendanhay/amazonka/tree/1.3.3)
Released: **08 October, 2015**, Compare: [1.3.2.1](https://github.com/brendanhay/amazonka/compare/1.3.2.1...1.3.3)

### Fixed

- Fix S3 `GetBucketLocation` response deserialisation. [\#228](https://github.com/brendanhay/amazonka/issues/228), [\#237](https://github.com/brendanhay/amazonka/issues/237).
- `runResourceT` added to documentation example. [\#232](https://github.com/brendanhay/amazonka/issues/232).
- Add S3 infrequent access storage class. [\#234](https://github.com/brendanhay/amazonka/issues/234), [\#238](https://github.com/brendanhay/amazonka/issues/238)

### New Libraries

- `amazonka-elasticsearch`: ElasticSearch Service.
- `amazonka-inspector`: Inspector Service.
- `amazonka-iot`: Internet of Things Platform.
- `amazonka-kinesis-firehose`: Kinesis Firehose Service.
- `amazonka-marketplace-analytics`: Marketplace Commerce Analytics Service.
- `amazonka-waf`: Web Application Firewall Service.

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
