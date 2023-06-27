{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Config.Types.ResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AWS__ACM__Certificate,
        ResourceType_AWS__AccessAnalyzer__Analyzer,
        ResourceType_AWS__AmazonMQ__Broker,
        ResourceType_AWS__Amplify__App,
        ResourceType_AWS__ApiGatewayV2__Api,
        ResourceType_AWS__ApiGatewayV2__Stage,
        ResourceType_AWS__ApiGateway__RestApi,
        ResourceType_AWS__ApiGateway__Stage,
        ResourceType_AWS__AppConfig__Application,
        ResourceType_AWS__AppConfig__ConfigurationProfile,
        ResourceType_AWS__AppConfig__DeploymentStrategy,
        ResourceType_AWS__AppConfig__Environment,
        ResourceType_AWS__AppFlow__Flow,
        ResourceType_AWS__AppMesh__VirtualNode,
        ResourceType_AWS__AppMesh__VirtualService,
        ResourceType_AWS__AppRunner__VpcConnector,
        ResourceType_AWS__AppStream__Application,
        ResourceType_AWS__AppStream__DirectoryConfig,
        ResourceType_AWS__AppSync__GraphQLApi,
        ResourceType_AWS__Athena__DataCatalog,
        ResourceType_AWS__Athena__WorkGroup,
        ResourceType_AWS__AuditManager__Assessment,
        ResourceType_AWS__AutoScaling__AutoScalingGroup,
        ResourceType_AWS__AutoScaling__LaunchConfiguration,
        ResourceType_AWS__AutoScaling__ScalingPolicy,
        ResourceType_AWS__AutoScaling__ScheduledAction,
        ResourceType_AWS__AutoScaling__WarmPool,
        ResourceType_AWS__Backup__BackupPlan,
        ResourceType_AWS__Backup__BackupSelection,
        ResourceType_AWS__Backup__BackupVault,
        ResourceType_AWS__Backup__RecoveryPoint,
        ResourceType_AWS__Backup__ReportPlan,
        ResourceType_AWS__Batch__ComputeEnvironment,
        ResourceType_AWS__Batch__JobQueue,
        ResourceType_AWS__Budgets__BudgetsAction,
        ResourceType_AWS__Cassandra__Keyspace,
        ResourceType_AWS__Cloud9__EnvironmentEC2,
        ResourceType_AWS__CloudFormation__Stack,
        ResourceType_AWS__CloudFront__Distribution,
        ResourceType_AWS__CloudFront__StreamingDistribution,
        ResourceType_AWS__CloudTrail__Trail,
        ResourceType_AWS__CloudWatch__Alarm,
        ResourceType_AWS__CloudWatch__MetricStream,
        ResourceType_AWS__CodeArtifact__Repository,
        ResourceType_AWS__CodeBuild__Project,
        ResourceType_AWS__CodeDeploy__Application,
        ResourceType_AWS__CodeDeploy__DeploymentConfig,
        ResourceType_AWS__CodeDeploy__DeploymentGroup,
        ResourceType_AWS__CodeGuruReviewer__RepositoryAssociation,
        ResourceType_AWS__CodePipeline__Pipeline,
        ResourceType_AWS__Config__ConformancePackCompliance,
        ResourceType_AWS__Config__ResourceCompliance,
        ResourceType_AWS__Connect__PhoneNumber,
        ResourceType_AWS__CustomerProfiles__Domain,
        ResourceType_AWS__DMS__Certificate,
        ResourceType_AWS__DMS__EventSubscription,
        ResourceType_AWS__DMS__ReplicationSubnetGroup,
        ResourceType_AWS__DataSync__LocationEFS,
        ResourceType_AWS__DataSync__LocationFSxLustre,
        ResourceType_AWS__DataSync__LocationFSxWindows,
        ResourceType_AWS__DataSync__LocationHDFS,
        ResourceType_AWS__DataSync__LocationNFS,
        ResourceType_AWS__DataSync__LocationObjectStorage,
        ResourceType_AWS__DataSync__LocationS3,
        ResourceType_AWS__DataSync__LocationSMB,
        ResourceType_AWS__DataSync__Task,
        ResourceType_AWS__Detective__Graph,
        ResourceType_AWS__DeviceFarm__InstanceProfile,
        ResourceType_AWS__DeviceFarm__Project,
        ResourceType_AWS__DeviceFarm__TestGridProject,
        ResourceType_AWS__DynamoDB__Table,
        ResourceType_AWS__EC2__CustomerGateway,
        ResourceType_AWS__EC2__DHCPOptions,
        ResourceType_AWS__EC2__EC2Fleet,
        ResourceType_AWS__EC2__EIP,
        ResourceType_AWS__EC2__EgressOnlyInternetGateway,
        ResourceType_AWS__EC2__FlowLog,
        ResourceType_AWS__EC2__Host,
        ResourceType_AWS__EC2__IPAM,
        ResourceType_AWS__EC2__Instance,
        ResourceType_AWS__EC2__InternetGateway,
        ResourceType_AWS__EC2__LaunchTemplate,
        ResourceType_AWS__EC2__NatGateway,
        ResourceType_AWS__EC2__NetworkAcl,
        ResourceType_AWS__EC2__NetworkInsightsAccessScopeAnalysis,
        ResourceType_AWS__EC2__NetworkInsightsPath,
        ResourceType_AWS__EC2__NetworkInterface,
        ResourceType_AWS__EC2__PrefixList,
        ResourceType_AWS__EC2__RegisteredHAInstance,
        ResourceType_AWS__EC2__RouteTable,
        ResourceType_AWS__EC2__SecurityGroup,
        ResourceType_AWS__EC2__SpotFleet,
        ResourceType_AWS__EC2__Subnet,
        ResourceType_AWS__EC2__SubnetRouteTableAssociation,
        ResourceType_AWS__EC2__TrafficMirrorFilter,
        ResourceType_AWS__EC2__TrafficMirrorSession,
        ResourceType_AWS__EC2__TrafficMirrorTarget,
        ResourceType_AWS__EC2__TransitGateway,
        ResourceType_AWS__EC2__TransitGatewayAttachment,
        ResourceType_AWS__EC2__TransitGatewayRouteTable,
        ResourceType_AWS__EC2__VPC,
        ResourceType_AWS__EC2__VPCEndpoint,
        ResourceType_AWS__EC2__VPCEndpointService,
        ResourceType_AWS__EC2__VPCPeeringConnection,
        ResourceType_AWS__EC2__VPNConnection,
        ResourceType_AWS__EC2__VPNGateway,
        ResourceType_AWS__EC2__Volume,
        ResourceType_AWS__ECR__PublicRepository,
        ResourceType_AWS__ECR__PullThroughCacheRule,
        ResourceType_AWS__ECR__RegistryPolicy,
        ResourceType_AWS__ECR__Repository,
        ResourceType_AWS__ECS__Cluster,
        ResourceType_AWS__ECS__Service,
        ResourceType_AWS__ECS__TaskDefinition,
        ResourceType_AWS__ECS__TaskSet,
        ResourceType_AWS__EFS__AccessPoint,
        ResourceType_AWS__EFS__FileSystem,
        ResourceType_AWS__EKS__Addon,
        ResourceType_AWS__EKS__Cluster,
        ResourceType_AWS__EKS__FargateProfile,
        ResourceType_AWS__EKS__IdentityProviderConfig,
        ResourceType_AWS__EMR__SecurityConfiguration,
        ResourceType_AWS__ElasticBeanstalk__Application,
        ResourceType_AWS__ElasticBeanstalk__ApplicationVersion,
        ResourceType_AWS__ElasticBeanstalk__Environment,
        ResourceType_AWS__ElasticLoadBalancingV2__Listener,
        ResourceType_AWS__ElasticLoadBalancingV2__LoadBalancer,
        ResourceType_AWS__ElasticLoadBalancing__LoadBalancer,
        ResourceType_AWS__Elasticsearch__Domain,
        ResourceType_AWS__EventSchemas__Discoverer,
        ResourceType_AWS__EventSchemas__Registry,
        ResourceType_AWS__EventSchemas__RegistryPolicy,
        ResourceType_AWS__EventSchemas__Schema,
        ResourceType_AWS__Events__ApiDestination,
        ResourceType_AWS__Events__Archive,
        ResourceType_AWS__Events__Connection,
        ResourceType_AWS__Events__Endpoint,
        ResourceType_AWS__Events__EventBus,
        ResourceType_AWS__Events__Rule,
        ResourceType_AWS__Evidently__Project,
        ResourceType_AWS__FIS__ExperimentTemplate,
        ResourceType_AWS__Forecast__Dataset,
        ResourceType_AWS__FraudDetector__EntityType,
        ResourceType_AWS__FraudDetector__Label,
        ResourceType_AWS__FraudDetector__Outcome,
        ResourceType_AWS__FraudDetector__Variable,
        ResourceType_AWS__GlobalAccelerator__Accelerator,
        ResourceType_AWS__GlobalAccelerator__EndpointGroup,
        ResourceType_AWS__GlobalAccelerator__Listener,
        ResourceType_AWS__Glue__Classifier,
        ResourceType_AWS__Glue__Job,
        ResourceType_AWS__Glue__MLTransform,
        ResourceType_AWS__GroundStation__Config,
        ResourceType_AWS__GuardDuty__Detector,
        ResourceType_AWS__GuardDuty__Filter,
        ResourceType_AWS__GuardDuty__IPSet,
        ResourceType_AWS__GuardDuty__ThreatIntelSet,
        ResourceType_AWS__HealthLake__FHIRDatastore,
        ResourceType_AWS__IAM__Group,
        ResourceType_AWS__IAM__Policy,
        ResourceType_AWS__IAM__Role,
        ResourceType_AWS__IAM__SAMLProvider,
        ResourceType_AWS__IAM__ServerCertificate,
        ResourceType_AWS__IAM__User,
        ResourceType_AWS__IVS__Channel,
        ResourceType_AWS__IVS__PlaybackKeyPair,
        ResourceType_AWS__IVS__RecordingConfiguration,
        ResourceType_AWS__ImageBuilder__ContainerRecipe,
        ResourceType_AWS__ImageBuilder__DistributionConfiguration,
        ResourceType_AWS__ImageBuilder__ImagePipeline,
        ResourceType_AWS__ImageBuilder__InfrastructureConfiguration,
        ResourceType_AWS__IoTAnalytics__Channel,
        ResourceType_AWS__IoTAnalytics__Dataset,
        ResourceType_AWS__IoTAnalytics__Datastore,
        ResourceType_AWS__IoTAnalytics__Pipeline,
        ResourceType_AWS__IoTEvents__AlarmModel,
        ResourceType_AWS__IoTEvents__DetectorModel,
        ResourceType_AWS__IoTEvents__Input,
        ResourceType_AWS__IoTSiteWise__AssetModel,
        ResourceType_AWS__IoTSiteWise__Dashboard,
        ResourceType_AWS__IoTSiteWise__Gateway,
        ResourceType_AWS__IoTSiteWise__Portal,
        ResourceType_AWS__IoTSiteWise__Project,
        ResourceType_AWS__IoTTwinMaker__Entity,
        ResourceType_AWS__IoTTwinMaker__Scene,
        ResourceType_AWS__IoTTwinMaker__Workspace,
        ResourceType_AWS__IoTWireless__ServiceProfile,
        ResourceType_AWS__IoT__AccountAuditConfiguration,
        ResourceType_AWS__IoT__Authorizer,
        ResourceType_AWS__IoT__CustomMetric,
        ResourceType_AWS__IoT__Dimension,
        ResourceType_AWS__IoT__FleetMetric,
        ResourceType_AWS__IoT__MitigationAction,
        ResourceType_AWS__IoT__Policy,
        ResourceType_AWS__IoT__RoleAlias,
        ResourceType_AWS__IoT__ScheduledAudit,
        ResourceType_AWS__IoT__SecurityProfile,
        ResourceType_AWS__KMS__Key,
        ResourceType_AWS__KinesisAnalyticsV2__Application,
        ResourceType_AWS__KinesisFirehose__DeliveryStream,
        ResourceType_AWS__KinesisVideo__SignalingChannel,
        ResourceType_AWS__Kinesis__Stream,
        ResourceType_AWS__Kinesis__StreamConsumer,
        ResourceType_AWS__Lambda__Function,
        ResourceType_AWS__Lex__Bot,
        ResourceType_AWS__Lex__BotAlias,
        ResourceType_AWS__Lightsail__Bucket,
        ResourceType_AWS__Lightsail__Certificate,
        ResourceType_AWS__Lightsail__Disk,
        ResourceType_AWS__Lightsail__StaticIp,
        ResourceType_AWS__LookoutMetrics__Alert,
        ResourceType_AWS__LookoutVision__Project,
        ResourceType_AWS__MSK__Cluster,
        ResourceType_AWS__MediaPackage__PackagingConfiguration,
        ResourceType_AWS__MediaPackage__PackagingGroup,
        ResourceType_AWS__NetworkFirewall__Firewall,
        ResourceType_AWS__NetworkFirewall__FirewallPolicy,
        ResourceType_AWS__NetworkFirewall__RuleGroup,
        ResourceType_AWS__NetworkManager__Device,
        ResourceType_AWS__NetworkManager__GlobalNetwork,
        ResourceType_AWS__NetworkManager__Link,
        ResourceType_AWS__NetworkManager__Site,
        ResourceType_AWS__NetworkManager__TransitGatewayRegistration,
        ResourceType_AWS__OpenSearch__Domain,
        ResourceType_AWS__Panorama__Package,
        ResourceType_AWS__Pinpoint__App,
        ResourceType_AWS__Pinpoint__ApplicationSettings,
        ResourceType_AWS__Pinpoint__Campaign,
        ResourceType_AWS__Pinpoint__InAppTemplate,
        ResourceType_AWS__Pinpoint__Segment,
        ResourceType_AWS__QLDB__Ledger,
        ResourceType_AWS__RDS__DBCluster,
        ResourceType_AWS__RDS__DBClusterSnapshot,
        ResourceType_AWS__RDS__DBInstance,
        ResourceType_AWS__RDS__DBSecurityGroup,
        ResourceType_AWS__RDS__DBSnapshot,
        ResourceType_AWS__RDS__DBSubnetGroup,
        ResourceType_AWS__RDS__EventSubscription,
        ResourceType_AWS__RDS__GlobalCluster,
        ResourceType_AWS__RUM__AppMonitor,
        ResourceType_AWS__Redshift__Cluster,
        ResourceType_AWS__Redshift__ClusterParameterGroup,
        ResourceType_AWS__Redshift__ClusterSecurityGroup,
        ResourceType_AWS__Redshift__ClusterSnapshot,
        ResourceType_AWS__Redshift__ClusterSubnetGroup,
        ResourceType_AWS__Redshift__EventSubscription,
        ResourceType_AWS__Redshift__ScheduledAction,
        ResourceType_AWS__ResilienceHub__ResiliencyPolicy,
        ResourceType_AWS__RoboMaker__RobotApplication,
        ResourceType_AWS__RoboMaker__RobotApplicationVersion,
        ResourceType_AWS__RoboMaker__SimulationApplication,
        ResourceType_AWS__Route53RecoveryControl__Cluster,
        ResourceType_AWS__Route53RecoveryControl__ControlPanel,
        ResourceType_AWS__Route53RecoveryControl__RoutingControl,
        ResourceType_AWS__Route53RecoveryControl__SafetyRule,
        ResourceType_AWS__Route53RecoveryReadiness__Cell,
        ResourceType_AWS__Route53RecoveryReadiness__ReadinessCheck,
        ResourceType_AWS__Route53RecoveryReadiness__RecoveryGroup,
        ResourceType_AWS__Route53RecoveryReadiness__ResourceSet,
        ResourceType_AWS__Route53Resolver__FirewallDomainList,
        ResourceType_AWS__Route53Resolver__FirewallRuleGroupAssociation,
        ResourceType_AWS__Route53Resolver__ResolverEndpoint,
        ResourceType_AWS__Route53Resolver__ResolverRule,
        ResourceType_AWS__Route53Resolver__ResolverRuleAssociation,
        ResourceType_AWS__Route53__HostedZone,
        ResourceType_AWS__S3__AccountPublicAccessBlock,
        ResourceType_AWS__S3__Bucket,
        ResourceType_AWS__S3__MultiRegionAccessPoint,
        ResourceType_AWS__S3__StorageLens,
        ResourceType_AWS__SES__ConfigurationSet,
        ResourceType_AWS__SES__ContactList,
        ResourceType_AWS__SES__ReceiptFilter,
        ResourceType_AWS__SES__ReceiptRuleSet,
        ResourceType_AWS__SES__Template,
        ResourceType_AWS__SNS__Topic,
        ResourceType_AWS__SQS__Queue,
        ResourceType_AWS__SSM__AssociationCompliance,
        ResourceType_AWS__SSM__FileData,
        ResourceType_AWS__SSM__ManagedInstanceInventory,
        ResourceType_AWS__SSM__PatchCompliance,
        ResourceType_AWS__SageMaker__AppImageConfig,
        ResourceType_AWS__SageMaker__CodeRepository,
        ResourceType_AWS__SageMaker__Domain,
        ResourceType_AWS__SageMaker__Image,
        ResourceType_AWS__SageMaker__Model,
        ResourceType_AWS__SageMaker__NotebookInstanceLifecycleConfig,
        ResourceType_AWS__SageMaker__Workteam,
        ResourceType_AWS__SecretsManager__Secret,
        ResourceType_AWS__ServiceCatalog__CloudFormationProduct,
        ResourceType_AWS__ServiceCatalog__CloudFormationProvisionedProduct,
        ResourceType_AWS__ServiceCatalog__Portfolio,
        ResourceType_AWS__ServiceDiscovery__HttpNamespace,
        ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace,
        ResourceType_AWS__ServiceDiscovery__Service,
        ResourceType_AWS__ShieldRegional__Protection,
        ResourceType_AWS__Shield__Protection,
        ResourceType_AWS__Signer__SigningProfile,
        ResourceType_AWS__StepFunctions__Activity,
        ResourceType_AWS__StepFunctions__StateMachine,
        ResourceType_AWS__Transfer__Agreement,
        ResourceType_AWS__Transfer__Connector,
        ResourceType_AWS__Transfer__Workflow,
        ResourceType_AWS__WAFRegional__RateBasedRule,
        ResourceType_AWS__WAFRegional__Rule,
        ResourceType_AWS__WAFRegional__RuleGroup,
        ResourceType_AWS__WAFRegional__WebACL,
        ResourceType_AWS__WAF__RateBasedRule,
        ResourceType_AWS__WAF__Rule,
        ResourceType_AWS__WAF__RuleGroup,
        ResourceType_AWS__WAF__WebACL,
        ResourceType_AWS__WAFv2__IPSet,
        ResourceType_AWS__WAFv2__ManagedRuleSet,
        ResourceType_AWS__WAFv2__RegexPatternSet,
        ResourceType_AWS__WAFv2__RuleGroup,
        ResourceType_AWS__WAFv2__WebACL,
        ResourceType_AWS__WorkSpaces__ConnectionAlias,
        ResourceType_AWS__WorkSpaces__Workspace,
        ResourceType_AWS__XRay__EncryptionConfig
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ResourceType_AWS__ACM__Certificate :: ResourceType
pattern ResourceType_AWS__ACM__Certificate = ResourceType' "AWS::ACM::Certificate"

pattern ResourceType_AWS__AccessAnalyzer__Analyzer :: ResourceType
pattern ResourceType_AWS__AccessAnalyzer__Analyzer = ResourceType' "AWS::AccessAnalyzer::Analyzer"

pattern ResourceType_AWS__AmazonMQ__Broker :: ResourceType
pattern ResourceType_AWS__AmazonMQ__Broker = ResourceType' "AWS::AmazonMQ::Broker"

pattern ResourceType_AWS__Amplify__App :: ResourceType
pattern ResourceType_AWS__Amplify__App = ResourceType' "AWS::Amplify::App"

pattern ResourceType_AWS__ApiGatewayV2__Api :: ResourceType
pattern ResourceType_AWS__ApiGatewayV2__Api = ResourceType' "AWS::ApiGatewayV2::Api"

pattern ResourceType_AWS__ApiGatewayV2__Stage :: ResourceType
pattern ResourceType_AWS__ApiGatewayV2__Stage = ResourceType' "AWS::ApiGatewayV2::Stage"

pattern ResourceType_AWS__ApiGateway__RestApi :: ResourceType
pattern ResourceType_AWS__ApiGateway__RestApi = ResourceType' "AWS::ApiGateway::RestApi"

pattern ResourceType_AWS__ApiGateway__Stage :: ResourceType
pattern ResourceType_AWS__ApiGateway__Stage = ResourceType' "AWS::ApiGateway::Stage"

pattern ResourceType_AWS__AppConfig__Application :: ResourceType
pattern ResourceType_AWS__AppConfig__Application = ResourceType' "AWS::AppConfig::Application"

pattern ResourceType_AWS__AppConfig__ConfigurationProfile :: ResourceType
pattern ResourceType_AWS__AppConfig__ConfigurationProfile = ResourceType' "AWS::AppConfig::ConfigurationProfile"

pattern ResourceType_AWS__AppConfig__DeploymentStrategy :: ResourceType
pattern ResourceType_AWS__AppConfig__DeploymentStrategy = ResourceType' "AWS::AppConfig::DeploymentStrategy"

pattern ResourceType_AWS__AppConfig__Environment :: ResourceType
pattern ResourceType_AWS__AppConfig__Environment = ResourceType' "AWS::AppConfig::Environment"

pattern ResourceType_AWS__AppFlow__Flow :: ResourceType
pattern ResourceType_AWS__AppFlow__Flow = ResourceType' "AWS::AppFlow::Flow"

pattern ResourceType_AWS__AppMesh__VirtualNode :: ResourceType
pattern ResourceType_AWS__AppMesh__VirtualNode = ResourceType' "AWS::AppMesh::VirtualNode"

pattern ResourceType_AWS__AppMesh__VirtualService :: ResourceType
pattern ResourceType_AWS__AppMesh__VirtualService = ResourceType' "AWS::AppMesh::VirtualService"

pattern ResourceType_AWS__AppRunner__VpcConnector :: ResourceType
pattern ResourceType_AWS__AppRunner__VpcConnector = ResourceType' "AWS::AppRunner::VpcConnector"

pattern ResourceType_AWS__AppStream__Application :: ResourceType
pattern ResourceType_AWS__AppStream__Application = ResourceType' "AWS::AppStream::Application"

pattern ResourceType_AWS__AppStream__DirectoryConfig :: ResourceType
pattern ResourceType_AWS__AppStream__DirectoryConfig = ResourceType' "AWS::AppStream::DirectoryConfig"

pattern ResourceType_AWS__AppSync__GraphQLApi :: ResourceType
pattern ResourceType_AWS__AppSync__GraphQLApi = ResourceType' "AWS::AppSync::GraphQLApi"

pattern ResourceType_AWS__Athena__DataCatalog :: ResourceType
pattern ResourceType_AWS__Athena__DataCatalog = ResourceType' "AWS::Athena::DataCatalog"

pattern ResourceType_AWS__Athena__WorkGroup :: ResourceType
pattern ResourceType_AWS__Athena__WorkGroup = ResourceType' "AWS::Athena::WorkGroup"

pattern ResourceType_AWS__AuditManager__Assessment :: ResourceType
pattern ResourceType_AWS__AuditManager__Assessment = ResourceType' "AWS::AuditManager::Assessment"

pattern ResourceType_AWS__AutoScaling__AutoScalingGroup :: ResourceType
pattern ResourceType_AWS__AutoScaling__AutoScalingGroup = ResourceType' "AWS::AutoScaling::AutoScalingGroup"

pattern ResourceType_AWS__AutoScaling__LaunchConfiguration :: ResourceType
pattern ResourceType_AWS__AutoScaling__LaunchConfiguration = ResourceType' "AWS::AutoScaling::LaunchConfiguration"

pattern ResourceType_AWS__AutoScaling__ScalingPolicy :: ResourceType
pattern ResourceType_AWS__AutoScaling__ScalingPolicy = ResourceType' "AWS::AutoScaling::ScalingPolicy"

pattern ResourceType_AWS__AutoScaling__ScheduledAction :: ResourceType
pattern ResourceType_AWS__AutoScaling__ScheduledAction = ResourceType' "AWS::AutoScaling::ScheduledAction"

pattern ResourceType_AWS__AutoScaling__WarmPool :: ResourceType
pattern ResourceType_AWS__AutoScaling__WarmPool = ResourceType' "AWS::AutoScaling::WarmPool"

pattern ResourceType_AWS__Backup__BackupPlan :: ResourceType
pattern ResourceType_AWS__Backup__BackupPlan = ResourceType' "AWS::Backup::BackupPlan"

pattern ResourceType_AWS__Backup__BackupSelection :: ResourceType
pattern ResourceType_AWS__Backup__BackupSelection = ResourceType' "AWS::Backup::BackupSelection"

pattern ResourceType_AWS__Backup__BackupVault :: ResourceType
pattern ResourceType_AWS__Backup__BackupVault = ResourceType' "AWS::Backup::BackupVault"

pattern ResourceType_AWS__Backup__RecoveryPoint :: ResourceType
pattern ResourceType_AWS__Backup__RecoveryPoint = ResourceType' "AWS::Backup::RecoveryPoint"

pattern ResourceType_AWS__Backup__ReportPlan :: ResourceType
pattern ResourceType_AWS__Backup__ReportPlan = ResourceType' "AWS::Backup::ReportPlan"

pattern ResourceType_AWS__Batch__ComputeEnvironment :: ResourceType
pattern ResourceType_AWS__Batch__ComputeEnvironment = ResourceType' "AWS::Batch::ComputeEnvironment"

pattern ResourceType_AWS__Batch__JobQueue :: ResourceType
pattern ResourceType_AWS__Batch__JobQueue = ResourceType' "AWS::Batch::JobQueue"

pattern ResourceType_AWS__Budgets__BudgetsAction :: ResourceType
pattern ResourceType_AWS__Budgets__BudgetsAction = ResourceType' "AWS::Budgets::BudgetsAction"

pattern ResourceType_AWS__Cassandra__Keyspace :: ResourceType
pattern ResourceType_AWS__Cassandra__Keyspace = ResourceType' "AWS::Cassandra::Keyspace"

pattern ResourceType_AWS__Cloud9__EnvironmentEC2 :: ResourceType
pattern ResourceType_AWS__Cloud9__EnvironmentEC2 = ResourceType' "AWS::Cloud9::EnvironmentEC2"

pattern ResourceType_AWS__CloudFormation__Stack :: ResourceType
pattern ResourceType_AWS__CloudFormation__Stack = ResourceType' "AWS::CloudFormation::Stack"

pattern ResourceType_AWS__CloudFront__Distribution :: ResourceType
pattern ResourceType_AWS__CloudFront__Distribution = ResourceType' "AWS::CloudFront::Distribution"

pattern ResourceType_AWS__CloudFront__StreamingDistribution :: ResourceType
pattern ResourceType_AWS__CloudFront__StreamingDistribution = ResourceType' "AWS::CloudFront::StreamingDistribution"

pattern ResourceType_AWS__CloudTrail__Trail :: ResourceType
pattern ResourceType_AWS__CloudTrail__Trail = ResourceType' "AWS::CloudTrail::Trail"

pattern ResourceType_AWS__CloudWatch__Alarm :: ResourceType
pattern ResourceType_AWS__CloudWatch__Alarm = ResourceType' "AWS::CloudWatch::Alarm"

pattern ResourceType_AWS__CloudWatch__MetricStream :: ResourceType
pattern ResourceType_AWS__CloudWatch__MetricStream = ResourceType' "AWS::CloudWatch::MetricStream"

pattern ResourceType_AWS__CodeArtifact__Repository :: ResourceType
pattern ResourceType_AWS__CodeArtifact__Repository = ResourceType' "AWS::CodeArtifact::Repository"

pattern ResourceType_AWS__CodeBuild__Project :: ResourceType
pattern ResourceType_AWS__CodeBuild__Project = ResourceType' "AWS::CodeBuild::Project"

pattern ResourceType_AWS__CodeDeploy__Application :: ResourceType
pattern ResourceType_AWS__CodeDeploy__Application = ResourceType' "AWS::CodeDeploy::Application"

pattern ResourceType_AWS__CodeDeploy__DeploymentConfig :: ResourceType
pattern ResourceType_AWS__CodeDeploy__DeploymentConfig = ResourceType' "AWS::CodeDeploy::DeploymentConfig"

pattern ResourceType_AWS__CodeDeploy__DeploymentGroup :: ResourceType
pattern ResourceType_AWS__CodeDeploy__DeploymentGroup = ResourceType' "AWS::CodeDeploy::DeploymentGroup"

pattern ResourceType_AWS__CodeGuruReviewer__RepositoryAssociation :: ResourceType
pattern ResourceType_AWS__CodeGuruReviewer__RepositoryAssociation = ResourceType' "AWS::CodeGuruReviewer::RepositoryAssociation"

pattern ResourceType_AWS__CodePipeline__Pipeline :: ResourceType
pattern ResourceType_AWS__CodePipeline__Pipeline = ResourceType' "AWS::CodePipeline::Pipeline"

pattern ResourceType_AWS__Config__ConformancePackCompliance :: ResourceType
pattern ResourceType_AWS__Config__ConformancePackCompliance = ResourceType' "AWS::Config::ConformancePackCompliance"

pattern ResourceType_AWS__Config__ResourceCompliance :: ResourceType
pattern ResourceType_AWS__Config__ResourceCompliance = ResourceType' "AWS::Config::ResourceCompliance"

pattern ResourceType_AWS__Connect__PhoneNumber :: ResourceType
pattern ResourceType_AWS__Connect__PhoneNumber = ResourceType' "AWS::Connect::PhoneNumber"

pattern ResourceType_AWS__CustomerProfiles__Domain :: ResourceType
pattern ResourceType_AWS__CustomerProfiles__Domain = ResourceType' "AWS::CustomerProfiles::Domain"

pattern ResourceType_AWS__DMS__Certificate :: ResourceType
pattern ResourceType_AWS__DMS__Certificate = ResourceType' "AWS::DMS::Certificate"

pattern ResourceType_AWS__DMS__EventSubscription :: ResourceType
pattern ResourceType_AWS__DMS__EventSubscription = ResourceType' "AWS::DMS::EventSubscription"

pattern ResourceType_AWS__DMS__ReplicationSubnetGroup :: ResourceType
pattern ResourceType_AWS__DMS__ReplicationSubnetGroup = ResourceType' "AWS::DMS::ReplicationSubnetGroup"

pattern ResourceType_AWS__DataSync__LocationEFS :: ResourceType
pattern ResourceType_AWS__DataSync__LocationEFS = ResourceType' "AWS::DataSync::LocationEFS"

pattern ResourceType_AWS__DataSync__LocationFSxLustre :: ResourceType
pattern ResourceType_AWS__DataSync__LocationFSxLustre = ResourceType' "AWS::DataSync::LocationFSxLustre"

pattern ResourceType_AWS__DataSync__LocationFSxWindows :: ResourceType
pattern ResourceType_AWS__DataSync__LocationFSxWindows = ResourceType' "AWS::DataSync::LocationFSxWindows"

pattern ResourceType_AWS__DataSync__LocationHDFS :: ResourceType
pattern ResourceType_AWS__DataSync__LocationHDFS = ResourceType' "AWS::DataSync::LocationHDFS"

pattern ResourceType_AWS__DataSync__LocationNFS :: ResourceType
pattern ResourceType_AWS__DataSync__LocationNFS = ResourceType' "AWS::DataSync::LocationNFS"

pattern ResourceType_AWS__DataSync__LocationObjectStorage :: ResourceType
pattern ResourceType_AWS__DataSync__LocationObjectStorage = ResourceType' "AWS::DataSync::LocationObjectStorage"

pattern ResourceType_AWS__DataSync__LocationS3 :: ResourceType
pattern ResourceType_AWS__DataSync__LocationS3 = ResourceType' "AWS::DataSync::LocationS3"

pattern ResourceType_AWS__DataSync__LocationSMB :: ResourceType
pattern ResourceType_AWS__DataSync__LocationSMB = ResourceType' "AWS::DataSync::LocationSMB"

pattern ResourceType_AWS__DataSync__Task :: ResourceType
pattern ResourceType_AWS__DataSync__Task = ResourceType' "AWS::DataSync::Task"

pattern ResourceType_AWS__Detective__Graph :: ResourceType
pattern ResourceType_AWS__Detective__Graph = ResourceType' "AWS::Detective::Graph"

pattern ResourceType_AWS__DeviceFarm__InstanceProfile :: ResourceType
pattern ResourceType_AWS__DeviceFarm__InstanceProfile = ResourceType' "AWS::DeviceFarm::InstanceProfile"

pattern ResourceType_AWS__DeviceFarm__Project :: ResourceType
pattern ResourceType_AWS__DeviceFarm__Project = ResourceType' "AWS::DeviceFarm::Project"

pattern ResourceType_AWS__DeviceFarm__TestGridProject :: ResourceType
pattern ResourceType_AWS__DeviceFarm__TestGridProject = ResourceType' "AWS::DeviceFarm::TestGridProject"

pattern ResourceType_AWS__DynamoDB__Table :: ResourceType
pattern ResourceType_AWS__DynamoDB__Table = ResourceType' "AWS::DynamoDB::Table"

pattern ResourceType_AWS__EC2__CustomerGateway :: ResourceType
pattern ResourceType_AWS__EC2__CustomerGateway = ResourceType' "AWS::EC2::CustomerGateway"

pattern ResourceType_AWS__EC2__DHCPOptions :: ResourceType
pattern ResourceType_AWS__EC2__DHCPOptions = ResourceType' "AWS::EC2::DHCPOptions"

pattern ResourceType_AWS__EC2__EC2Fleet :: ResourceType
pattern ResourceType_AWS__EC2__EC2Fleet = ResourceType' "AWS::EC2::EC2Fleet"

pattern ResourceType_AWS__EC2__EIP :: ResourceType
pattern ResourceType_AWS__EC2__EIP = ResourceType' "AWS::EC2::EIP"

pattern ResourceType_AWS__EC2__EgressOnlyInternetGateway :: ResourceType
pattern ResourceType_AWS__EC2__EgressOnlyInternetGateway = ResourceType' "AWS::EC2::EgressOnlyInternetGateway"

pattern ResourceType_AWS__EC2__FlowLog :: ResourceType
pattern ResourceType_AWS__EC2__FlowLog = ResourceType' "AWS::EC2::FlowLog"

pattern ResourceType_AWS__EC2__Host :: ResourceType
pattern ResourceType_AWS__EC2__Host = ResourceType' "AWS::EC2::Host"

pattern ResourceType_AWS__EC2__IPAM :: ResourceType
pattern ResourceType_AWS__EC2__IPAM = ResourceType' "AWS::EC2::IPAM"

pattern ResourceType_AWS__EC2__Instance :: ResourceType
pattern ResourceType_AWS__EC2__Instance = ResourceType' "AWS::EC2::Instance"

pattern ResourceType_AWS__EC2__InternetGateway :: ResourceType
pattern ResourceType_AWS__EC2__InternetGateway = ResourceType' "AWS::EC2::InternetGateway"

pattern ResourceType_AWS__EC2__LaunchTemplate :: ResourceType
pattern ResourceType_AWS__EC2__LaunchTemplate = ResourceType' "AWS::EC2::LaunchTemplate"

pattern ResourceType_AWS__EC2__NatGateway :: ResourceType
pattern ResourceType_AWS__EC2__NatGateway = ResourceType' "AWS::EC2::NatGateway"

pattern ResourceType_AWS__EC2__NetworkAcl :: ResourceType
pattern ResourceType_AWS__EC2__NetworkAcl = ResourceType' "AWS::EC2::NetworkAcl"

pattern ResourceType_AWS__EC2__NetworkInsightsAccessScopeAnalysis :: ResourceType
pattern ResourceType_AWS__EC2__NetworkInsightsAccessScopeAnalysis = ResourceType' "AWS::EC2::NetworkInsightsAccessScopeAnalysis"

pattern ResourceType_AWS__EC2__NetworkInsightsPath :: ResourceType
pattern ResourceType_AWS__EC2__NetworkInsightsPath = ResourceType' "AWS::EC2::NetworkInsightsPath"

pattern ResourceType_AWS__EC2__NetworkInterface :: ResourceType
pattern ResourceType_AWS__EC2__NetworkInterface = ResourceType' "AWS::EC2::NetworkInterface"

pattern ResourceType_AWS__EC2__PrefixList :: ResourceType
pattern ResourceType_AWS__EC2__PrefixList = ResourceType' "AWS::EC2::PrefixList"

pattern ResourceType_AWS__EC2__RegisteredHAInstance :: ResourceType
pattern ResourceType_AWS__EC2__RegisteredHAInstance = ResourceType' "AWS::EC2::RegisteredHAInstance"

pattern ResourceType_AWS__EC2__RouteTable :: ResourceType
pattern ResourceType_AWS__EC2__RouteTable = ResourceType' "AWS::EC2::RouteTable"

pattern ResourceType_AWS__EC2__SecurityGroup :: ResourceType
pattern ResourceType_AWS__EC2__SecurityGroup = ResourceType' "AWS::EC2::SecurityGroup"

pattern ResourceType_AWS__EC2__SpotFleet :: ResourceType
pattern ResourceType_AWS__EC2__SpotFleet = ResourceType' "AWS::EC2::SpotFleet"

pattern ResourceType_AWS__EC2__Subnet :: ResourceType
pattern ResourceType_AWS__EC2__Subnet = ResourceType' "AWS::EC2::Subnet"

pattern ResourceType_AWS__EC2__SubnetRouteTableAssociation :: ResourceType
pattern ResourceType_AWS__EC2__SubnetRouteTableAssociation = ResourceType' "AWS::EC2::SubnetRouteTableAssociation"

pattern ResourceType_AWS__EC2__TrafficMirrorFilter :: ResourceType
pattern ResourceType_AWS__EC2__TrafficMirrorFilter = ResourceType' "AWS::EC2::TrafficMirrorFilter"

pattern ResourceType_AWS__EC2__TrafficMirrorSession :: ResourceType
pattern ResourceType_AWS__EC2__TrafficMirrorSession = ResourceType' "AWS::EC2::TrafficMirrorSession"

pattern ResourceType_AWS__EC2__TrafficMirrorTarget :: ResourceType
pattern ResourceType_AWS__EC2__TrafficMirrorTarget = ResourceType' "AWS::EC2::TrafficMirrorTarget"

pattern ResourceType_AWS__EC2__TransitGateway :: ResourceType
pattern ResourceType_AWS__EC2__TransitGateway = ResourceType' "AWS::EC2::TransitGateway"

pattern ResourceType_AWS__EC2__TransitGatewayAttachment :: ResourceType
pattern ResourceType_AWS__EC2__TransitGatewayAttachment = ResourceType' "AWS::EC2::TransitGatewayAttachment"

pattern ResourceType_AWS__EC2__TransitGatewayRouteTable :: ResourceType
pattern ResourceType_AWS__EC2__TransitGatewayRouteTable = ResourceType' "AWS::EC2::TransitGatewayRouteTable"

pattern ResourceType_AWS__EC2__VPC :: ResourceType
pattern ResourceType_AWS__EC2__VPC = ResourceType' "AWS::EC2::VPC"

pattern ResourceType_AWS__EC2__VPCEndpoint :: ResourceType
pattern ResourceType_AWS__EC2__VPCEndpoint = ResourceType' "AWS::EC2::VPCEndpoint"

pattern ResourceType_AWS__EC2__VPCEndpointService :: ResourceType
pattern ResourceType_AWS__EC2__VPCEndpointService = ResourceType' "AWS::EC2::VPCEndpointService"

pattern ResourceType_AWS__EC2__VPCPeeringConnection :: ResourceType
pattern ResourceType_AWS__EC2__VPCPeeringConnection = ResourceType' "AWS::EC2::VPCPeeringConnection"

pattern ResourceType_AWS__EC2__VPNConnection :: ResourceType
pattern ResourceType_AWS__EC2__VPNConnection = ResourceType' "AWS::EC2::VPNConnection"

pattern ResourceType_AWS__EC2__VPNGateway :: ResourceType
pattern ResourceType_AWS__EC2__VPNGateway = ResourceType' "AWS::EC2::VPNGateway"

pattern ResourceType_AWS__EC2__Volume :: ResourceType
pattern ResourceType_AWS__EC2__Volume = ResourceType' "AWS::EC2::Volume"

pattern ResourceType_AWS__ECR__PublicRepository :: ResourceType
pattern ResourceType_AWS__ECR__PublicRepository = ResourceType' "AWS::ECR::PublicRepository"

pattern ResourceType_AWS__ECR__PullThroughCacheRule :: ResourceType
pattern ResourceType_AWS__ECR__PullThroughCacheRule = ResourceType' "AWS::ECR::PullThroughCacheRule"

pattern ResourceType_AWS__ECR__RegistryPolicy :: ResourceType
pattern ResourceType_AWS__ECR__RegistryPolicy = ResourceType' "AWS::ECR::RegistryPolicy"

pattern ResourceType_AWS__ECR__Repository :: ResourceType
pattern ResourceType_AWS__ECR__Repository = ResourceType' "AWS::ECR::Repository"

pattern ResourceType_AWS__ECS__Cluster :: ResourceType
pattern ResourceType_AWS__ECS__Cluster = ResourceType' "AWS::ECS::Cluster"

pattern ResourceType_AWS__ECS__Service :: ResourceType
pattern ResourceType_AWS__ECS__Service = ResourceType' "AWS::ECS::Service"

pattern ResourceType_AWS__ECS__TaskDefinition :: ResourceType
pattern ResourceType_AWS__ECS__TaskDefinition = ResourceType' "AWS::ECS::TaskDefinition"

pattern ResourceType_AWS__ECS__TaskSet :: ResourceType
pattern ResourceType_AWS__ECS__TaskSet = ResourceType' "AWS::ECS::TaskSet"

pattern ResourceType_AWS__EFS__AccessPoint :: ResourceType
pattern ResourceType_AWS__EFS__AccessPoint = ResourceType' "AWS::EFS::AccessPoint"

pattern ResourceType_AWS__EFS__FileSystem :: ResourceType
pattern ResourceType_AWS__EFS__FileSystem = ResourceType' "AWS::EFS::FileSystem"

pattern ResourceType_AWS__EKS__Addon :: ResourceType
pattern ResourceType_AWS__EKS__Addon = ResourceType' "AWS::EKS::Addon"

pattern ResourceType_AWS__EKS__Cluster :: ResourceType
pattern ResourceType_AWS__EKS__Cluster = ResourceType' "AWS::EKS::Cluster"

pattern ResourceType_AWS__EKS__FargateProfile :: ResourceType
pattern ResourceType_AWS__EKS__FargateProfile = ResourceType' "AWS::EKS::FargateProfile"

pattern ResourceType_AWS__EKS__IdentityProviderConfig :: ResourceType
pattern ResourceType_AWS__EKS__IdentityProviderConfig = ResourceType' "AWS::EKS::IdentityProviderConfig"

pattern ResourceType_AWS__EMR__SecurityConfiguration :: ResourceType
pattern ResourceType_AWS__EMR__SecurityConfiguration = ResourceType' "AWS::EMR::SecurityConfiguration"

pattern ResourceType_AWS__ElasticBeanstalk__Application :: ResourceType
pattern ResourceType_AWS__ElasticBeanstalk__Application = ResourceType' "AWS::ElasticBeanstalk::Application"

pattern ResourceType_AWS__ElasticBeanstalk__ApplicationVersion :: ResourceType
pattern ResourceType_AWS__ElasticBeanstalk__ApplicationVersion = ResourceType' "AWS::ElasticBeanstalk::ApplicationVersion"

pattern ResourceType_AWS__ElasticBeanstalk__Environment :: ResourceType
pattern ResourceType_AWS__ElasticBeanstalk__Environment = ResourceType' "AWS::ElasticBeanstalk::Environment"

pattern ResourceType_AWS__ElasticLoadBalancingV2__Listener :: ResourceType
pattern ResourceType_AWS__ElasticLoadBalancingV2__Listener = ResourceType' "AWS::ElasticLoadBalancingV2::Listener"

pattern ResourceType_AWS__ElasticLoadBalancingV2__LoadBalancer :: ResourceType
pattern ResourceType_AWS__ElasticLoadBalancingV2__LoadBalancer = ResourceType' "AWS::ElasticLoadBalancingV2::LoadBalancer"

pattern ResourceType_AWS__ElasticLoadBalancing__LoadBalancer :: ResourceType
pattern ResourceType_AWS__ElasticLoadBalancing__LoadBalancer = ResourceType' "AWS::ElasticLoadBalancing::LoadBalancer"

pattern ResourceType_AWS__Elasticsearch__Domain :: ResourceType
pattern ResourceType_AWS__Elasticsearch__Domain = ResourceType' "AWS::Elasticsearch::Domain"

pattern ResourceType_AWS__EventSchemas__Discoverer :: ResourceType
pattern ResourceType_AWS__EventSchemas__Discoverer = ResourceType' "AWS::EventSchemas::Discoverer"

pattern ResourceType_AWS__EventSchemas__Registry :: ResourceType
pattern ResourceType_AWS__EventSchemas__Registry = ResourceType' "AWS::EventSchemas::Registry"

pattern ResourceType_AWS__EventSchemas__RegistryPolicy :: ResourceType
pattern ResourceType_AWS__EventSchemas__RegistryPolicy = ResourceType' "AWS::EventSchemas::RegistryPolicy"

pattern ResourceType_AWS__EventSchemas__Schema :: ResourceType
pattern ResourceType_AWS__EventSchemas__Schema = ResourceType' "AWS::EventSchemas::Schema"

pattern ResourceType_AWS__Events__ApiDestination :: ResourceType
pattern ResourceType_AWS__Events__ApiDestination = ResourceType' "AWS::Events::ApiDestination"

pattern ResourceType_AWS__Events__Archive :: ResourceType
pattern ResourceType_AWS__Events__Archive = ResourceType' "AWS::Events::Archive"

pattern ResourceType_AWS__Events__Connection :: ResourceType
pattern ResourceType_AWS__Events__Connection = ResourceType' "AWS::Events::Connection"

pattern ResourceType_AWS__Events__Endpoint :: ResourceType
pattern ResourceType_AWS__Events__Endpoint = ResourceType' "AWS::Events::Endpoint"

pattern ResourceType_AWS__Events__EventBus :: ResourceType
pattern ResourceType_AWS__Events__EventBus = ResourceType' "AWS::Events::EventBus"

pattern ResourceType_AWS__Events__Rule :: ResourceType
pattern ResourceType_AWS__Events__Rule = ResourceType' "AWS::Events::Rule"

pattern ResourceType_AWS__Evidently__Project :: ResourceType
pattern ResourceType_AWS__Evidently__Project = ResourceType' "AWS::Evidently::Project"

pattern ResourceType_AWS__FIS__ExperimentTemplate :: ResourceType
pattern ResourceType_AWS__FIS__ExperimentTemplate = ResourceType' "AWS::FIS::ExperimentTemplate"

pattern ResourceType_AWS__Forecast__Dataset :: ResourceType
pattern ResourceType_AWS__Forecast__Dataset = ResourceType' "AWS::Forecast::Dataset"

pattern ResourceType_AWS__FraudDetector__EntityType :: ResourceType
pattern ResourceType_AWS__FraudDetector__EntityType = ResourceType' "AWS::FraudDetector::EntityType"

pattern ResourceType_AWS__FraudDetector__Label :: ResourceType
pattern ResourceType_AWS__FraudDetector__Label = ResourceType' "AWS::FraudDetector::Label"

pattern ResourceType_AWS__FraudDetector__Outcome :: ResourceType
pattern ResourceType_AWS__FraudDetector__Outcome = ResourceType' "AWS::FraudDetector::Outcome"

pattern ResourceType_AWS__FraudDetector__Variable :: ResourceType
pattern ResourceType_AWS__FraudDetector__Variable = ResourceType' "AWS::FraudDetector::Variable"

pattern ResourceType_AWS__GlobalAccelerator__Accelerator :: ResourceType
pattern ResourceType_AWS__GlobalAccelerator__Accelerator = ResourceType' "AWS::GlobalAccelerator::Accelerator"

pattern ResourceType_AWS__GlobalAccelerator__EndpointGroup :: ResourceType
pattern ResourceType_AWS__GlobalAccelerator__EndpointGroup = ResourceType' "AWS::GlobalAccelerator::EndpointGroup"

pattern ResourceType_AWS__GlobalAccelerator__Listener :: ResourceType
pattern ResourceType_AWS__GlobalAccelerator__Listener = ResourceType' "AWS::GlobalAccelerator::Listener"

pattern ResourceType_AWS__Glue__Classifier :: ResourceType
pattern ResourceType_AWS__Glue__Classifier = ResourceType' "AWS::Glue::Classifier"

pattern ResourceType_AWS__Glue__Job :: ResourceType
pattern ResourceType_AWS__Glue__Job = ResourceType' "AWS::Glue::Job"

pattern ResourceType_AWS__Glue__MLTransform :: ResourceType
pattern ResourceType_AWS__Glue__MLTransform = ResourceType' "AWS::Glue::MLTransform"

pattern ResourceType_AWS__GroundStation__Config :: ResourceType
pattern ResourceType_AWS__GroundStation__Config = ResourceType' "AWS::GroundStation::Config"

pattern ResourceType_AWS__GuardDuty__Detector :: ResourceType
pattern ResourceType_AWS__GuardDuty__Detector = ResourceType' "AWS::GuardDuty::Detector"

pattern ResourceType_AWS__GuardDuty__Filter :: ResourceType
pattern ResourceType_AWS__GuardDuty__Filter = ResourceType' "AWS::GuardDuty::Filter"

pattern ResourceType_AWS__GuardDuty__IPSet :: ResourceType
pattern ResourceType_AWS__GuardDuty__IPSet = ResourceType' "AWS::GuardDuty::IPSet"

pattern ResourceType_AWS__GuardDuty__ThreatIntelSet :: ResourceType
pattern ResourceType_AWS__GuardDuty__ThreatIntelSet = ResourceType' "AWS::GuardDuty::ThreatIntelSet"

pattern ResourceType_AWS__HealthLake__FHIRDatastore :: ResourceType
pattern ResourceType_AWS__HealthLake__FHIRDatastore = ResourceType' "AWS::HealthLake::FHIRDatastore"

pattern ResourceType_AWS__IAM__Group :: ResourceType
pattern ResourceType_AWS__IAM__Group = ResourceType' "AWS::IAM::Group"

pattern ResourceType_AWS__IAM__Policy :: ResourceType
pattern ResourceType_AWS__IAM__Policy = ResourceType' "AWS::IAM::Policy"

pattern ResourceType_AWS__IAM__Role :: ResourceType
pattern ResourceType_AWS__IAM__Role = ResourceType' "AWS::IAM::Role"

pattern ResourceType_AWS__IAM__SAMLProvider :: ResourceType
pattern ResourceType_AWS__IAM__SAMLProvider = ResourceType' "AWS::IAM::SAMLProvider"

pattern ResourceType_AWS__IAM__ServerCertificate :: ResourceType
pattern ResourceType_AWS__IAM__ServerCertificate = ResourceType' "AWS::IAM::ServerCertificate"

pattern ResourceType_AWS__IAM__User :: ResourceType
pattern ResourceType_AWS__IAM__User = ResourceType' "AWS::IAM::User"

pattern ResourceType_AWS__IVS__Channel :: ResourceType
pattern ResourceType_AWS__IVS__Channel = ResourceType' "AWS::IVS::Channel"

pattern ResourceType_AWS__IVS__PlaybackKeyPair :: ResourceType
pattern ResourceType_AWS__IVS__PlaybackKeyPair = ResourceType' "AWS::IVS::PlaybackKeyPair"

pattern ResourceType_AWS__IVS__RecordingConfiguration :: ResourceType
pattern ResourceType_AWS__IVS__RecordingConfiguration = ResourceType' "AWS::IVS::RecordingConfiguration"

pattern ResourceType_AWS__ImageBuilder__ContainerRecipe :: ResourceType
pattern ResourceType_AWS__ImageBuilder__ContainerRecipe = ResourceType' "AWS::ImageBuilder::ContainerRecipe"

pattern ResourceType_AWS__ImageBuilder__DistributionConfiguration :: ResourceType
pattern ResourceType_AWS__ImageBuilder__DistributionConfiguration = ResourceType' "AWS::ImageBuilder::DistributionConfiguration"

pattern ResourceType_AWS__ImageBuilder__ImagePipeline :: ResourceType
pattern ResourceType_AWS__ImageBuilder__ImagePipeline = ResourceType' "AWS::ImageBuilder::ImagePipeline"

pattern ResourceType_AWS__ImageBuilder__InfrastructureConfiguration :: ResourceType
pattern ResourceType_AWS__ImageBuilder__InfrastructureConfiguration = ResourceType' "AWS::ImageBuilder::InfrastructureConfiguration"

pattern ResourceType_AWS__IoTAnalytics__Channel :: ResourceType
pattern ResourceType_AWS__IoTAnalytics__Channel = ResourceType' "AWS::IoTAnalytics::Channel"

pattern ResourceType_AWS__IoTAnalytics__Dataset :: ResourceType
pattern ResourceType_AWS__IoTAnalytics__Dataset = ResourceType' "AWS::IoTAnalytics::Dataset"

pattern ResourceType_AWS__IoTAnalytics__Datastore :: ResourceType
pattern ResourceType_AWS__IoTAnalytics__Datastore = ResourceType' "AWS::IoTAnalytics::Datastore"

pattern ResourceType_AWS__IoTAnalytics__Pipeline :: ResourceType
pattern ResourceType_AWS__IoTAnalytics__Pipeline = ResourceType' "AWS::IoTAnalytics::Pipeline"

pattern ResourceType_AWS__IoTEvents__AlarmModel :: ResourceType
pattern ResourceType_AWS__IoTEvents__AlarmModel = ResourceType' "AWS::IoTEvents::AlarmModel"

pattern ResourceType_AWS__IoTEvents__DetectorModel :: ResourceType
pattern ResourceType_AWS__IoTEvents__DetectorModel = ResourceType' "AWS::IoTEvents::DetectorModel"

pattern ResourceType_AWS__IoTEvents__Input :: ResourceType
pattern ResourceType_AWS__IoTEvents__Input = ResourceType' "AWS::IoTEvents::Input"

pattern ResourceType_AWS__IoTSiteWise__AssetModel :: ResourceType
pattern ResourceType_AWS__IoTSiteWise__AssetModel = ResourceType' "AWS::IoTSiteWise::AssetModel"

pattern ResourceType_AWS__IoTSiteWise__Dashboard :: ResourceType
pattern ResourceType_AWS__IoTSiteWise__Dashboard = ResourceType' "AWS::IoTSiteWise::Dashboard"

pattern ResourceType_AWS__IoTSiteWise__Gateway :: ResourceType
pattern ResourceType_AWS__IoTSiteWise__Gateway = ResourceType' "AWS::IoTSiteWise::Gateway"

pattern ResourceType_AWS__IoTSiteWise__Portal :: ResourceType
pattern ResourceType_AWS__IoTSiteWise__Portal = ResourceType' "AWS::IoTSiteWise::Portal"

pattern ResourceType_AWS__IoTSiteWise__Project :: ResourceType
pattern ResourceType_AWS__IoTSiteWise__Project = ResourceType' "AWS::IoTSiteWise::Project"

pattern ResourceType_AWS__IoTTwinMaker__Entity :: ResourceType
pattern ResourceType_AWS__IoTTwinMaker__Entity = ResourceType' "AWS::IoTTwinMaker::Entity"

pattern ResourceType_AWS__IoTTwinMaker__Scene :: ResourceType
pattern ResourceType_AWS__IoTTwinMaker__Scene = ResourceType' "AWS::IoTTwinMaker::Scene"

pattern ResourceType_AWS__IoTTwinMaker__Workspace :: ResourceType
pattern ResourceType_AWS__IoTTwinMaker__Workspace = ResourceType' "AWS::IoTTwinMaker::Workspace"

pattern ResourceType_AWS__IoTWireless__ServiceProfile :: ResourceType
pattern ResourceType_AWS__IoTWireless__ServiceProfile = ResourceType' "AWS::IoTWireless::ServiceProfile"

pattern ResourceType_AWS__IoT__AccountAuditConfiguration :: ResourceType
pattern ResourceType_AWS__IoT__AccountAuditConfiguration = ResourceType' "AWS::IoT::AccountAuditConfiguration"

pattern ResourceType_AWS__IoT__Authorizer :: ResourceType
pattern ResourceType_AWS__IoT__Authorizer = ResourceType' "AWS::IoT::Authorizer"

pattern ResourceType_AWS__IoT__CustomMetric :: ResourceType
pattern ResourceType_AWS__IoT__CustomMetric = ResourceType' "AWS::IoT::CustomMetric"

pattern ResourceType_AWS__IoT__Dimension :: ResourceType
pattern ResourceType_AWS__IoT__Dimension = ResourceType' "AWS::IoT::Dimension"

pattern ResourceType_AWS__IoT__FleetMetric :: ResourceType
pattern ResourceType_AWS__IoT__FleetMetric = ResourceType' "AWS::IoT::FleetMetric"

pattern ResourceType_AWS__IoT__MitigationAction :: ResourceType
pattern ResourceType_AWS__IoT__MitigationAction = ResourceType' "AWS::IoT::MitigationAction"

pattern ResourceType_AWS__IoT__Policy :: ResourceType
pattern ResourceType_AWS__IoT__Policy = ResourceType' "AWS::IoT::Policy"

pattern ResourceType_AWS__IoT__RoleAlias :: ResourceType
pattern ResourceType_AWS__IoT__RoleAlias = ResourceType' "AWS::IoT::RoleAlias"

pattern ResourceType_AWS__IoT__ScheduledAudit :: ResourceType
pattern ResourceType_AWS__IoT__ScheduledAudit = ResourceType' "AWS::IoT::ScheduledAudit"

pattern ResourceType_AWS__IoT__SecurityProfile :: ResourceType
pattern ResourceType_AWS__IoT__SecurityProfile = ResourceType' "AWS::IoT::SecurityProfile"

pattern ResourceType_AWS__KMS__Key :: ResourceType
pattern ResourceType_AWS__KMS__Key = ResourceType' "AWS::KMS::Key"

pattern ResourceType_AWS__KinesisAnalyticsV2__Application :: ResourceType
pattern ResourceType_AWS__KinesisAnalyticsV2__Application = ResourceType' "AWS::KinesisAnalyticsV2::Application"

pattern ResourceType_AWS__KinesisFirehose__DeliveryStream :: ResourceType
pattern ResourceType_AWS__KinesisFirehose__DeliveryStream = ResourceType' "AWS::KinesisFirehose::DeliveryStream"

pattern ResourceType_AWS__KinesisVideo__SignalingChannel :: ResourceType
pattern ResourceType_AWS__KinesisVideo__SignalingChannel = ResourceType' "AWS::KinesisVideo::SignalingChannel"

pattern ResourceType_AWS__Kinesis__Stream :: ResourceType
pattern ResourceType_AWS__Kinesis__Stream = ResourceType' "AWS::Kinesis::Stream"

pattern ResourceType_AWS__Kinesis__StreamConsumer :: ResourceType
pattern ResourceType_AWS__Kinesis__StreamConsumer = ResourceType' "AWS::Kinesis::StreamConsumer"

pattern ResourceType_AWS__Lambda__Function :: ResourceType
pattern ResourceType_AWS__Lambda__Function = ResourceType' "AWS::Lambda::Function"

pattern ResourceType_AWS__Lex__Bot :: ResourceType
pattern ResourceType_AWS__Lex__Bot = ResourceType' "AWS::Lex::Bot"

pattern ResourceType_AWS__Lex__BotAlias :: ResourceType
pattern ResourceType_AWS__Lex__BotAlias = ResourceType' "AWS::Lex::BotAlias"

pattern ResourceType_AWS__Lightsail__Bucket :: ResourceType
pattern ResourceType_AWS__Lightsail__Bucket = ResourceType' "AWS::Lightsail::Bucket"

pattern ResourceType_AWS__Lightsail__Certificate :: ResourceType
pattern ResourceType_AWS__Lightsail__Certificate = ResourceType' "AWS::Lightsail::Certificate"

pattern ResourceType_AWS__Lightsail__Disk :: ResourceType
pattern ResourceType_AWS__Lightsail__Disk = ResourceType' "AWS::Lightsail::Disk"

pattern ResourceType_AWS__Lightsail__StaticIp :: ResourceType
pattern ResourceType_AWS__Lightsail__StaticIp = ResourceType' "AWS::Lightsail::StaticIp"

pattern ResourceType_AWS__LookoutMetrics__Alert :: ResourceType
pattern ResourceType_AWS__LookoutMetrics__Alert = ResourceType' "AWS::LookoutMetrics::Alert"

pattern ResourceType_AWS__LookoutVision__Project :: ResourceType
pattern ResourceType_AWS__LookoutVision__Project = ResourceType' "AWS::LookoutVision::Project"

pattern ResourceType_AWS__MSK__Cluster :: ResourceType
pattern ResourceType_AWS__MSK__Cluster = ResourceType' "AWS::MSK::Cluster"

pattern ResourceType_AWS__MediaPackage__PackagingConfiguration :: ResourceType
pattern ResourceType_AWS__MediaPackage__PackagingConfiguration = ResourceType' "AWS::MediaPackage::PackagingConfiguration"

pattern ResourceType_AWS__MediaPackage__PackagingGroup :: ResourceType
pattern ResourceType_AWS__MediaPackage__PackagingGroup = ResourceType' "AWS::MediaPackage::PackagingGroup"

pattern ResourceType_AWS__NetworkFirewall__Firewall :: ResourceType
pattern ResourceType_AWS__NetworkFirewall__Firewall = ResourceType' "AWS::NetworkFirewall::Firewall"

pattern ResourceType_AWS__NetworkFirewall__FirewallPolicy :: ResourceType
pattern ResourceType_AWS__NetworkFirewall__FirewallPolicy = ResourceType' "AWS::NetworkFirewall::FirewallPolicy"

pattern ResourceType_AWS__NetworkFirewall__RuleGroup :: ResourceType
pattern ResourceType_AWS__NetworkFirewall__RuleGroup = ResourceType' "AWS::NetworkFirewall::RuleGroup"

pattern ResourceType_AWS__NetworkManager__Device :: ResourceType
pattern ResourceType_AWS__NetworkManager__Device = ResourceType' "AWS::NetworkManager::Device"

pattern ResourceType_AWS__NetworkManager__GlobalNetwork :: ResourceType
pattern ResourceType_AWS__NetworkManager__GlobalNetwork = ResourceType' "AWS::NetworkManager::GlobalNetwork"

pattern ResourceType_AWS__NetworkManager__Link :: ResourceType
pattern ResourceType_AWS__NetworkManager__Link = ResourceType' "AWS::NetworkManager::Link"

pattern ResourceType_AWS__NetworkManager__Site :: ResourceType
pattern ResourceType_AWS__NetworkManager__Site = ResourceType' "AWS::NetworkManager::Site"

pattern ResourceType_AWS__NetworkManager__TransitGatewayRegistration :: ResourceType
pattern ResourceType_AWS__NetworkManager__TransitGatewayRegistration = ResourceType' "AWS::NetworkManager::TransitGatewayRegistration"

pattern ResourceType_AWS__OpenSearch__Domain :: ResourceType
pattern ResourceType_AWS__OpenSearch__Domain = ResourceType' "AWS::OpenSearch::Domain"

pattern ResourceType_AWS__Panorama__Package :: ResourceType
pattern ResourceType_AWS__Panorama__Package = ResourceType' "AWS::Panorama::Package"

pattern ResourceType_AWS__Pinpoint__App :: ResourceType
pattern ResourceType_AWS__Pinpoint__App = ResourceType' "AWS::Pinpoint::App"

pattern ResourceType_AWS__Pinpoint__ApplicationSettings :: ResourceType
pattern ResourceType_AWS__Pinpoint__ApplicationSettings = ResourceType' "AWS::Pinpoint::ApplicationSettings"

pattern ResourceType_AWS__Pinpoint__Campaign :: ResourceType
pattern ResourceType_AWS__Pinpoint__Campaign = ResourceType' "AWS::Pinpoint::Campaign"

pattern ResourceType_AWS__Pinpoint__InAppTemplate :: ResourceType
pattern ResourceType_AWS__Pinpoint__InAppTemplate = ResourceType' "AWS::Pinpoint::InAppTemplate"

pattern ResourceType_AWS__Pinpoint__Segment :: ResourceType
pattern ResourceType_AWS__Pinpoint__Segment = ResourceType' "AWS::Pinpoint::Segment"

pattern ResourceType_AWS__QLDB__Ledger :: ResourceType
pattern ResourceType_AWS__QLDB__Ledger = ResourceType' "AWS::QLDB::Ledger"

pattern ResourceType_AWS__RDS__DBCluster :: ResourceType
pattern ResourceType_AWS__RDS__DBCluster = ResourceType' "AWS::RDS::DBCluster"

pattern ResourceType_AWS__RDS__DBClusterSnapshot :: ResourceType
pattern ResourceType_AWS__RDS__DBClusterSnapshot = ResourceType' "AWS::RDS::DBClusterSnapshot"

pattern ResourceType_AWS__RDS__DBInstance :: ResourceType
pattern ResourceType_AWS__RDS__DBInstance = ResourceType' "AWS::RDS::DBInstance"

pattern ResourceType_AWS__RDS__DBSecurityGroup :: ResourceType
pattern ResourceType_AWS__RDS__DBSecurityGroup = ResourceType' "AWS::RDS::DBSecurityGroup"

pattern ResourceType_AWS__RDS__DBSnapshot :: ResourceType
pattern ResourceType_AWS__RDS__DBSnapshot = ResourceType' "AWS::RDS::DBSnapshot"

pattern ResourceType_AWS__RDS__DBSubnetGroup :: ResourceType
pattern ResourceType_AWS__RDS__DBSubnetGroup = ResourceType' "AWS::RDS::DBSubnetGroup"

pattern ResourceType_AWS__RDS__EventSubscription :: ResourceType
pattern ResourceType_AWS__RDS__EventSubscription = ResourceType' "AWS::RDS::EventSubscription"

pattern ResourceType_AWS__RDS__GlobalCluster :: ResourceType
pattern ResourceType_AWS__RDS__GlobalCluster = ResourceType' "AWS::RDS::GlobalCluster"

pattern ResourceType_AWS__RUM__AppMonitor :: ResourceType
pattern ResourceType_AWS__RUM__AppMonitor = ResourceType' "AWS::RUM::AppMonitor"

pattern ResourceType_AWS__Redshift__Cluster :: ResourceType
pattern ResourceType_AWS__Redshift__Cluster = ResourceType' "AWS::Redshift::Cluster"

pattern ResourceType_AWS__Redshift__ClusterParameterGroup :: ResourceType
pattern ResourceType_AWS__Redshift__ClusterParameterGroup = ResourceType' "AWS::Redshift::ClusterParameterGroup"

pattern ResourceType_AWS__Redshift__ClusterSecurityGroup :: ResourceType
pattern ResourceType_AWS__Redshift__ClusterSecurityGroup = ResourceType' "AWS::Redshift::ClusterSecurityGroup"

pattern ResourceType_AWS__Redshift__ClusterSnapshot :: ResourceType
pattern ResourceType_AWS__Redshift__ClusterSnapshot = ResourceType' "AWS::Redshift::ClusterSnapshot"

pattern ResourceType_AWS__Redshift__ClusterSubnetGroup :: ResourceType
pattern ResourceType_AWS__Redshift__ClusterSubnetGroup = ResourceType' "AWS::Redshift::ClusterSubnetGroup"

pattern ResourceType_AWS__Redshift__EventSubscription :: ResourceType
pattern ResourceType_AWS__Redshift__EventSubscription = ResourceType' "AWS::Redshift::EventSubscription"

pattern ResourceType_AWS__Redshift__ScheduledAction :: ResourceType
pattern ResourceType_AWS__Redshift__ScheduledAction = ResourceType' "AWS::Redshift::ScheduledAction"

pattern ResourceType_AWS__ResilienceHub__ResiliencyPolicy :: ResourceType
pattern ResourceType_AWS__ResilienceHub__ResiliencyPolicy = ResourceType' "AWS::ResilienceHub::ResiliencyPolicy"

pattern ResourceType_AWS__RoboMaker__RobotApplication :: ResourceType
pattern ResourceType_AWS__RoboMaker__RobotApplication = ResourceType' "AWS::RoboMaker::RobotApplication"

pattern ResourceType_AWS__RoboMaker__RobotApplicationVersion :: ResourceType
pattern ResourceType_AWS__RoboMaker__RobotApplicationVersion = ResourceType' "AWS::RoboMaker::RobotApplicationVersion"

pattern ResourceType_AWS__RoboMaker__SimulationApplication :: ResourceType
pattern ResourceType_AWS__RoboMaker__SimulationApplication = ResourceType' "AWS::RoboMaker::SimulationApplication"

pattern ResourceType_AWS__Route53RecoveryControl__Cluster :: ResourceType
pattern ResourceType_AWS__Route53RecoveryControl__Cluster = ResourceType' "AWS::Route53RecoveryControl::Cluster"

pattern ResourceType_AWS__Route53RecoveryControl__ControlPanel :: ResourceType
pattern ResourceType_AWS__Route53RecoveryControl__ControlPanel = ResourceType' "AWS::Route53RecoveryControl::ControlPanel"

pattern ResourceType_AWS__Route53RecoveryControl__RoutingControl :: ResourceType
pattern ResourceType_AWS__Route53RecoveryControl__RoutingControl = ResourceType' "AWS::Route53RecoveryControl::RoutingControl"

pattern ResourceType_AWS__Route53RecoveryControl__SafetyRule :: ResourceType
pattern ResourceType_AWS__Route53RecoveryControl__SafetyRule = ResourceType' "AWS::Route53RecoveryControl::SafetyRule"

pattern ResourceType_AWS__Route53RecoveryReadiness__Cell :: ResourceType
pattern ResourceType_AWS__Route53RecoveryReadiness__Cell = ResourceType' "AWS::Route53RecoveryReadiness::Cell"

pattern ResourceType_AWS__Route53RecoveryReadiness__ReadinessCheck :: ResourceType
pattern ResourceType_AWS__Route53RecoveryReadiness__ReadinessCheck = ResourceType' "AWS::Route53RecoveryReadiness::ReadinessCheck"

pattern ResourceType_AWS__Route53RecoveryReadiness__RecoveryGroup :: ResourceType
pattern ResourceType_AWS__Route53RecoveryReadiness__RecoveryGroup = ResourceType' "AWS::Route53RecoveryReadiness::RecoveryGroup"

pattern ResourceType_AWS__Route53RecoveryReadiness__ResourceSet :: ResourceType
pattern ResourceType_AWS__Route53RecoveryReadiness__ResourceSet = ResourceType' "AWS::Route53RecoveryReadiness::ResourceSet"

pattern ResourceType_AWS__Route53Resolver__FirewallDomainList :: ResourceType
pattern ResourceType_AWS__Route53Resolver__FirewallDomainList = ResourceType' "AWS::Route53Resolver::FirewallDomainList"

pattern ResourceType_AWS__Route53Resolver__FirewallRuleGroupAssociation :: ResourceType
pattern ResourceType_AWS__Route53Resolver__FirewallRuleGroupAssociation = ResourceType' "AWS::Route53Resolver::FirewallRuleGroupAssociation"

pattern ResourceType_AWS__Route53Resolver__ResolverEndpoint :: ResourceType
pattern ResourceType_AWS__Route53Resolver__ResolverEndpoint = ResourceType' "AWS::Route53Resolver::ResolverEndpoint"

pattern ResourceType_AWS__Route53Resolver__ResolverRule :: ResourceType
pattern ResourceType_AWS__Route53Resolver__ResolverRule = ResourceType' "AWS::Route53Resolver::ResolverRule"

pattern ResourceType_AWS__Route53Resolver__ResolverRuleAssociation :: ResourceType
pattern ResourceType_AWS__Route53Resolver__ResolverRuleAssociation = ResourceType' "AWS::Route53Resolver::ResolverRuleAssociation"

pattern ResourceType_AWS__Route53__HostedZone :: ResourceType
pattern ResourceType_AWS__Route53__HostedZone = ResourceType' "AWS::Route53::HostedZone"

pattern ResourceType_AWS__S3__AccountPublicAccessBlock :: ResourceType
pattern ResourceType_AWS__S3__AccountPublicAccessBlock = ResourceType' "AWS::S3::AccountPublicAccessBlock"

pattern ResourceType_AWS__S3__Bucket :: ResourceType
pattern ResourceType_AWS__S3__Bucket = ResourceType' "AWS::S3::Bucket"

pattern ResourceType_AWS__S3__MultiRegionAccessPoint :: ResourceType
pattern ResourceType_AWS__S3__MultiRegionAccessPoint = ResourceType' "AWS::S3::MultiRegionAccessPoint"

pattern ResourceType_AWS__S3__StorageLens :: ResourceType
pattern ResourceType_AWS__S3__StorageLens = ResourceType' "AWS::S3::StorageLens"

pattern ResourceType_AWS__SES__ConfigurationSet :: ResourceType
pattern ResourceType_AWS__SES__ConfigurationSet = ResourceType' "AWS::SES::ConfigurationSet"

pattern ResourceType_AWS__SES__ContactList :: ResourceType
pattern ResourceType_AWS__SES__ContactList = ResourceType' "AWS::SES::ContactList"

pattern ResourceType_AWS__SES__ReceiptFilter :: ResourceType
pattern ResourceType_AWS__SES__ReceiptFilter = ResourceType' "AWS::SES::ReceiptFilter"

pattern ResourceType_AWS__SES__ReceiptRuleSet :: ResourceType
pattern ResourceType_AWS__SES__ReceiptRuleSet = ResourceType' "AWS::SES::ReceiptRuleSet"

pattern ResourceType_AWS__SES__Template :: ResourceType
pattern ResourceType_AWS__SES__Template = ResourceType' "AWS::SES::Template"

pattern ResourceType_AWS__SNS__Topic :: ResourceType
pattern ResourceType_AWS__SNS__Topic = ResourceType' "AWS::SNS::Topic"

pattern ResourceType_AWS__SQS__Queue :: ResourceType
pattern ResourceType_AWS__SQS__Queue = ResourceType' "AWS::SQS::Queue"

pattern ResourceType_AWS__SSM__AssociationCompliance :: ResourceType
pattern ResourceType_AWS__SSM__AssociationCompliance = ResourceType' "AWS::SSM::AssociationCompliance"

pattern ResourceType_AWS__SSM__FileData :: ResourceType
pattern ResourceType_AWS__SSM__FileData = ResourceType' "AWS::SSM::FileData"

pattern ResourceType_AWS__SSM__ManagedInstanceInventory :: ResourceType
pattern ResourceType_AWS__SSM__ManagedInstanceInventory = ResourceType' "AWS::SSM::ManagedInstanceInventory"

pattern ResourceType_AWS__SSM__PatchCompliance :: ResourceType
pattern ResourceType_AWS__SSM__PatchCompliance = ResourceType' "AWS::SSM::PatchCompliance"

pattern ResourceType_AWS__SageMaker__AppImageConfig :: ResourceType
pattern ResourceType_AWS__SageMaker__AppImageConfig = ResourceType' "AWS::SageMaker::AppImageConfig"

pattern ResourceType_AWS__SageMaker__CodeRepository :: ResourceType
pattern ResourceType_AWS__SageMaker__CodeRepository = ResourceType' "AWS::SageMaker::CodeRepository"

pattern ResourceType_AWS__SageMaker__Domain :: ResourceType
pattern ResourceType_AWS__SageMaker__Domain = ResourceType' "AWS::SageMaker::Domain"

pattern ResourceType_AWS__SageMaker__Image :: ResourceType
pattern ResourceType_AWS__SageMaker__Image = ResourceType' "AWS::SageMaker::Image"

pattern ResourceType_AWS__SageMaker__Model :: ResourceType
pattern ResourceType_AWS__SageMaker__Model = ResourceType' "AWS::SageMaker::Model"

pattern ResourceType_AWS__SageMaker__NotebookInstanceLifecycleConfig :: ResourceType
pattern ResourceType_AWS__SageMaker__NotebookInstanceLifecycleConfig = ResourceType' "AWS::SageMaker::NotebookInstanceLifecycleConfig"

pattern ResourceType_AWS__SageMaker__Workteam :: ResourceType
pattern ResourceType_AWS__SageMaker__Workteam = ResourceType' "AWS::SageMaker::Workteam"

pattern ResourceType_AWS__SecretsManager__Secret :: ResourceType
pattern ResourceType_AWS__SecretsManager__Secret = ResourceType' "AWS::SecretsManager::Secret"

pattern ResourceType_AWS__ServiceCatalog__CloudFormationProduct :: ResourceType
pattern ResourceType_AWS__ServiceCatalog__CloudFormationProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProduct"

pattern ResourceType_AWS__ServiceCatalog__CloudFormationProvisionedProduct :: ResourceType
pattern ResourceType_AWS__ServiceCatalog__CloudFormationProvisionedProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProvisionedProduct"

pattern ResourceType_AWS__ServiceCatalog__Portfolio :: ResourceType
pattern ResourceType_AWS__ServiceCatalog__Portfolio = ResourceType' "AWS::ServiceCatalog::Portfolio"

pattern ResourceType_AWS__ServiceDiscovery__HttpNamespace :: ResourceType
pattern ResourceType_AWS__ServiceDiscovery__HttpNamespace = ResourceType' "AWS::ServiceDiscovery::HttpNamespace"

pattern ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace :: ResourceType
pattern ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace = ResourceType' "AWS::ServiceDiscovery::PublicDnsNamespace"

pattern ResourceType_AWS__ServiceDiscovery__Service :: ResourceType
pattern ResourceType_AWS__ServiceDiscovery__Service = ResourceType' "AWS::ServiceDiscovery::Service"

pattern ResourceType_AWS__ShieldRegional__Protection :: ResourceType
pattern ResourceType_AWS__ShieldRegional__Protection = ResourceType' "AWS::ShieldRegional::Protection"

pattern ResourceType_AWS__Shield__Protection :: ResourceType
pattern ResourceType_AWS__Shield__Protection = ResourceType' "AWS::Shield::Protection"

pattern ResourceType_AWS__Signer__SigningProfile :: ResourceType
pattern ResourceType_AWS__Signer__SigningProfile = ResourceType' "AWS::Signer::SigningProfile"

pattern ResourceType_AWS__StepFunctions__Activity :: ResourceType
pattern ResourceType_AWS__StepFunctions__Activity = ResourceType' "AWS::StepFunctions::Activity"

pattern ResourceType_AWS__StepFunctions__StateMachine :: ResourceType
pattern ResourceType_AWS__StepFunctions__StateMachine = ResourceType' "AWS::StepFunctions::StateMachine"

pattern ResourceType_AWS__Transfer__Agreement :: ResourceType
pattern ResourceType_AWS__Transfer__Agreement = ResourceType' "AWS::Transfer::Agreement"

pattern ResourceType_AWS__Transfer__Connector :: ResourceType
pattern ResourceType_AWS__Transfer__Connector = ResourceType' "AWS::Transfer::Connector"

pattern ResourceType_AWS__Transfer__Workflow :: ResourceType
pattern ResourceType_AWS__Transfer__Workflow = ResourceType' "AWS::Transfer::Workflow"

pattern ResourceType_AWS__WAFRegional__RateBasedRule :: ResourceType
pattern ResourceType_AWS__WAFRegional__RateBasedRule = ResourceType' "AWS::WAFRegional::RateBasedRule"

pattern ResourceType_AWS__WAFRegional__Rule :: ResourceType
pattern ResourceType_AWS__WAFRegional__Rule = ResourceType' "AWS::WAFRegional::Rule"

pattern ResourceType_AWS__WAFRegional__RuleGroup :: ResourceType
pattern ResourceType_AWS__WAFRegional__RuleGroup = ResourceType' "AWS::WAFRegional::RuleGroup"

pattern ResourceType_AWS__WAFRegional__WebACL :: ResourceType
pattern ResourceType_AWS__WAFRegional__WebACL = ResourceType' "AWS::WAFRegional::WebACL"

pattern ResourceType_AWS__WAF__RateBasedRule :: ResourceType
pattern ResourceType_AWS__WAF__RateBasedRule = ResourceType' "AWS::WAF::RateBasedRule"

pattern ResourceType_AWS__WAF__Rule :: ResourceType
pattern ResourceType_AWS__WAF__Rule = ResourceType' "AWS::WAF::Rule"

pattern ResourceType_AWS__WAF__RuleGroup :: ResourceType
pattern ResourceType_AWS__WAF__RuleGroup = ResourceType' "AWS::WAF::RuleGroup"

pattern ResourceType_AWS__WAF__WebACL :: ResourceType
pattern ResourceType_AWS__WAF__WebACL = ResourceType' "AWS::WAF::WebACL"

pattern ResourceType_AWS__WAFv2__IPSet :: ResourceType
pattern ResourceType_AWS__WAFv2__IPSet = ResourceType' "AWS::WAFv2::IPSet"

pattern ResourceType_AWS__WAFv2__ManagedRuleSet :: ResourceType
pattern ResourceType_AWS__WAFv2__ManagedRuleSet = ResourceType' "AWS::WAFv2::ManagedRuleSet"

pattern ResourceType_AWS__WAFv2__RegexPatternSet :: ResourceType
pattern ResourceType_AWS__WAFv2__RegexPatternSet = ResourceType' "AWS::WAFv2::RegexPatternSet"

pattern ResourceType_AWS__WAFv2__RuleGroup :: ResourceType
pattern ResourceType_AWS__WAFv2__RuleGroup = ResourceType' "AWS::WAFv2::RuleGroup"

pattern ResourceType_AWS__WAFv2__WebACL :: ResourceType
pattern ResourceType_AWS__WAFv2__WebACL = ResourceType' "AWS::WAFv2::WebACL"

pattern ResourceType_AWS__WorkSpaces__ConnectionAlias :: ResourceType
pattern ResourceType_AWS__WorkSpaces__ConnectionAlias = ResourceType' "AWS::WorkSpaces::ConnectionAlias"

pattern ResourceType_AWS__WorkSpaces__Workspace :: ResourceType
pattern ResourceType_AWS__WorkSpaces__Workspace = ResourceType' "AWS::WorkSpaces::Workspace"

pattern ResourceType_AWS__XRay__EncryptionConfig :: ResourceType
pattern ResourceType_AWS__XRay__EncryptionConfig = ResourceType' "AWS::XRay::EncryptionConfig"

{-# COMPLETE
  ResourceType_AWS__ACM__Certificate,
  ResourceType_AWS__AccessAnalyzer__Analyzer,
  ResourceType_AWS__AmazonMQ__Broker,
  ResourceType_AWS__Amplify__App,
  ResourceType_AWS__ApiGatewayV2__Api,
  ResourceType_AWS__ApiGatewayV2__Stage,
  ResourceType_AWS__ApiGateway__RestApi,
  ResourceType_AWS__ApiGateway__Stage,
  ResourceType_AWS__AppConfig__Application,
  ResourceType_AWS__AppConfig__ConfigurationProfile,
  ResourceType_AWS__AppConfig__DeploymentStrategy,
  ResourceType_AWS__AppConfig__Environment,
  ResourceType_AWS__AppFlow__Flow,
  ResourceType_AWS__AppMesh__VirtualNode,
  ResourceType_AWS__AppMesh__VirtualService,
  ResourceType_AWS__AppRunner__VpcConnector,
  ResourceType_AWS__AppStream__Application,
  ResourceType_AWS__AppStream__DirectoryConfig,
  ResourceType_AWS__AppSync__GraphQLApi,
  ResourceType_AWS__Athena__DataCatalog,
  ResourceType_AWS__Athena__WorkGroup,
  ResourceType_AWS__AuditManager__Assessment,
  ResourceType_AWS__AutoScaling__AutoScalingGroup,
  ResourceType_AWS__AutoScaling__LaunchConfiguration,
  ResourceType_AWS__AutoScaling__ScalingPolicy,
  ResourceType_AWS__AutoScaling__ScheduledAction,
  ResourceType_AWS__AutoScaling__WarmPool,
  ResourceType_AWS__Backup__BackupPlan,
  ResourceType_AWS__Backup__BackupSelection,
  ResourceType_AWS__Backup__BackupVault,
  ResourceType_AWS__Backup__RecoveryPoint,
  ResourceType_AWS__Backup__ReportPlan,
  ResourceType_AWS__Batch__ComputeEnvironment,
  ResourceType_AWS__Batch__JobQueue,
  ResourceType_AWS__Budgets__BudgetsAction,
  ResourceType_AWS__Cassandra__Keyspace,
  ResourceType_AWS__Cloud9__EnvironmentEC2,
  ResourceType_AWS__CloudFormation__Stack,
  ResourceType_AWS__CloudFront__Distribution,
  ResourceType_AWS__CloudFront__StreamingDistribution,
  ResourceType_AWS__CloudTrail__Trail,
  ResourceType_AWS__CloudWatch__Alarm,
  ResourceType_AWS__CloudWatch__MetricStream,
  ResourceType_AWS__CodeArtifact__Repository,
  ResourceType_AWS__CodeBuild__Project,
  ResourceType_AWS__CodeDeploy__Application,
  ResourceType_AWS__CodeDeploy__DeploymentConfig,
  ResourceType_AWS__CodeDeploy__DeploymentGroup,
  ResourceType_AWS__CodeGuruReviewer__RepositoryAssociation,
  ResourceType_AWS__CodePipeline__Pipeline,
  ResourceType_AWS__Config__ConformancePackCompliance,
  ResourceType_AWS__Config__ResourceCompliance,
  ResourceType_AWS__Connect__PhoneNumber,
  ResourceType_AWS__CustomerProfiles__Domain,
  ResourceType_AWS__DMS__Certificate,
  ResourceType_AWS__DMS__EventSubscription,
  ResourceType_AWS__DMS__ReplicationSubnetGroup,
  ResourceType_AWS__DataSync__LocationEFS,
  ResourceType_AWS__DataSync__LocationFSxLustre,
  ResourceType_AWS__DataSync__LocationFSxWindows,
  ResourceType_AWS__DataSync__LocationHDFS,
  ResourceType_AWS__DataSync__LocationNFS,
  ResourceType_AWS__DataSync__LocationObjectStorage,
  ResourceType_AWS__DataSync__LocationS3,
  ResourceType_AWS__DataSync__LocationSMB,
  ResourceType_AWS__DataSync__Task,
  ResourceType_AWS__Detective__Graph,
  ResourceType_AWS__DeviceFarm__InstanceProfile,
  ResourceType_AWS__DeviceFarm__Project,
  ResourceType_AWS__DeviceFarm__TestGridProject,
  ResourceType_AWS__DynamoDB__Table,
  ResourceType_AWS__EC2__CustomerGateway,
  ResourceType_AWS__EC2__DHCPOptions,
  ResourceType_AWS__EC2__EC2Fleet,
  ResourceType_AWS__EC2__EIP,
  ResourceType_AWS__EC2__EgressOnlyInternetGateway,
  ResourceType_AWS__EC2__FlowLog,
  ResourceType_AWS__EC2__Host,
  ResourceType_AWS__EC2__IPAM,
  ResourceType_AWS__EC2__Instance,
  ResourceType_AWS__EC2__InternetGateway,
  ResourceType_AWS__EC2__LaunchTemplate,
  ResourceType_AWS__EC2__NatGateway,
  ResourceType_AWS__EC2__NetworkAcl,
  ResourceType_AWS__EC2__NetworkInsightsAccessScopeAnalysis,
  ResourceType_AWS__EC2__NetworkInsightsPath,
  ResourceType_AWS__EC2__NetworkInterface,
  ResourceType_AWS__EC2__PrefixList,
  ResourceType_AWS__EC2__RegisteredHAInstance,
  ResourceType_AWS__EC2__RouteTable,
  ResourceType_AWS__EC2__SecurityGroup,
  ResourceType_AWS__EC2__SpotFleet,
  ResourceType_AWS__EC2__Subnet,
  ResourceType_AWS__EC2__SubnetRouteTableAssociation,
  ResourceType_AWS__EC2__TrafficMirrorFilter,
  ResourceType_AWS__EC2__TrafficMirrorSession,
  ResourceType_AWS__EC2__TrafficMirrorTarget,
  ResourceType_AWS__EC2__TransitGateway,
  ResourceType_AWS__EC2__TransitGatewayAttachment,
  ResourceType_AWS__EC2__TransitGatewayRouteTable,
  ResourceType_AWS__EC2__VPC,
  ResourceType_AWS__EC2__VPCEndpoint,
  ResourceType_AWS__EC2__VPCEndpointService,
  ResourceType_AWS__EC2__VPCPeeringConnection,
  ResourceType_AWS__EC2__VPNConnection,
  ResourceType_AWS__EC2__VPNGateway,
  ResourceType_AWS__EC2__Volume,
  ResourceType_AWS__ECR__PublicRepository,
  ResourceType_AWS__ECR__PullThroughCacheRule,
  ResourceType_AWS__ECR__RegistryPolicy,
  ResourceType_AWS__ECR__Repository,
  ResourceType_AWS__ECS__Cluster,
  ResourceType_AWS__ECS__Service,
  ResourceType_AWS__ECS__TaskDefinition,
  ResourceType_AWS__ECS__TaskSet,
  ResourceType_AWS__EFS__AccessPoint,
  ResourceType_AWS__EFS__FileSystem,
  ResourceType_AWS__EKS__Addon,
  ResourceType_AWS__EKS__Cluster,
  ResourceType_AWS__EKS__FargateProfile,
  ResourceType_AWS__EKS__IdentityProviderConfig,
  ResourceType_AWS__EMR__SecurityConfiguration,
  ResourceType_AWS__ElasticBeanstalk__Application,
  ResourceType_AWS__ElasticBeanstalk__ApplicationVersion,
  ResourceType_AWS__ElasticBeanstalk__Environment,
  ResourceType_AWS__ElasticLoadBalancingV2__Listener,
  ResourceType_AWS__ElasticLoadBalancingV2__LoadBalancer,
  ResourceType_AWS__ElasticLoadBalancing__LoadBalancer,
  ResourceType_AWS__Elasticsearch__Domain,
  ResourceType_AWS__EventSchemas__Discoverer,
  ResourceType_AWS__EventSchemas__Registry,
  ResourceType_AWS__EventSchemas__RegistryPolicy,
  ResourceType_AWS__EventSchemas__Schema,
  ResourceType_AWS__Events__ApiDestination,
  ResourceType_AWS__Events__Archive,
  ResourceType_AWS__Events__Connection,
  ResourceType_AWS__Events__Endpoint,
  ResourceType_AWS__Events__EventBus,
  ResourceType_AWS__Events__Rule,
  ResourceType_AWS__Evidently__Project,
  ResourceType_AWS__FIS__ExperimentTemplate,
  ResourceType_AWS__Forecast__Dataset,
  ResourceType_AWS__FraudDetector__EntityType,
  ResourceType_AWS__FraudDetector__Label,
  ResourceType_AWS__FraudDetector__Outcome,
  ResourceType_AWS__FraudDetector__Variable,
  ResourceType_AWS__GlobalAccelerator__Accelerator,
  ResourceType_AWS__GlobalAccelerator__EndpointGroup,
  ResourceType_AWS__GlobalAccelerator__Listener,
  ResourceType_AWS__Glue__Classifier,
  ResourceType_AWS__Glue__Job,
  ResourceType_AWS__Glue__MLTransform,
  ResourceType_AWS__GroundStation__Config,
  ResourceType_AWS__GuardDuty__Detector,
  ResourceType_AWS__GuardDuty__Filter,
  ResourceType_AWS__GuardDuty__IPSet,
  ResourceType_AWS__GuardDuty__ThreatIntelSet,
  ResourceType_AWS__HealthLake__FHIRDatastore,
  ResourceType_AWS__IAM__Group,
  ResourceType_AWS__IAM__Policy,
  ResourceType_AWS__IAM__Role,
  ResourceType_AWS__IAM__SAMLProvider,
  ResourceType_AWS__IAM__ServerCertificate,
  ResourceType_AWS__IAM__User,
  ResourceType_AWS__IVS__Channel,
  ResourceType_AWS__IVS__PlaybackKeyPair,
  ResourceType_AWS__IVS__RecordingConfiguration,
  ResourceType_AWS__ImageBuilder__ContainerRecipe,
  ResourceType_AWS__ImageBuilder__DistributionConfiguration,
  ResourceType_AWS__ImageBuilder__ImagePipeline,
  ResourceType_AWS__ImageBuilder__InfrastructureConfiguration,
  ResourceType_AWS__IoTAnalytics__Channel,
  ResourceType_AWS__IoTAnalytics__Dataset,
  ResourceType_AWS__IoTAnalytics__Datastore,
  ResourceType_AWS__IoTAnalytics__Pipeline,
  ResourceType_AWS__IoTEvents__AlarmModel,
  ResourceType_AWS__IoTEvents__DetectorModel,
  ResourceType_AWS__IoTEvents__Input,
  ResourceType_AWS__IoTSiteWise__AssetModel,
  ResourceType_AWS__IoTSiteWise__Dashboard,
  ResourceType_AWS__IoTSiteWise__Gateway,
  ResourceType_AWS__IoTSiteWise__Portal,
  ResourceType_AWS__IoTSiteWise__Project,
  ResourceType_AWS__IoTTwinMaker__Entity,
  ResourceType_AWS__IoTTwinMaker__Scene,
  ResourceType_AWS__IoTTwinMaker__Workspace,
  ResourceType_AWS__IoTWireless__ServiceProfile,
  ResourceType_AWS__IoT__AccountAuditConfiguration,
  ResourceType_AWS__IoT__Authorizer,
  ResourceType_AWS__IoT__CustomMetric,
  ResourceType_AWS__IoT__Dimension,
  ResourceType_AWS__IoT__FleetMetric,
  ResourceType_AWS__IoT__MitigationAction,
  ResourceType_AWS__IoT__Policy,
  ResourceType_AWS__IoT__RoleAlias,
  ResourceType_AWS__IoT__ScheduledAudit,
  ResourceType_AWS__IoT__SecurityProfile,
  ResourceType_AWS__KMS__Key,
  ResourceType_AWS__KinesisAnalyticsV2__Application,
  ResourceType_AWS__KinesisFirehose__DeliveryStream,
  ResourceType_AWS__KinesisVideo__SignalingChannel,
  ResourceType_AWS__Kinesis__Stream,
  ResourceType_AWS__Kinesis__StreamConsumer,
  ResourceType_AWS__Lambda__Function,
  ResourceType_AWS__Lex__Bot,
  ResourceType_AWS__Lex__BotAlias,
  ResourceType_AWS__Lightsail__Bucket,
  ResourceType_AWS__Lightsail__Certificate,
  ResourceType_AWS__Lightsail__Disk,
  ResourceType_AWS__Lightsail__StaticIp,
  ResourceType_AWS__LookoutMetrics__Alert,
  ResourceType_AWS__LookoutVision__Project,
  ResourceType_AWS__MSK__Cluster,
  ResourceType_AWS__MediaPackage__PackagingConfiguration,
  ResourceType_AWS__MediaPackage__PackagingGroup,
  ResourceType_AWS__NetworkFirewall__Firewall,
  ResourceType_AWS__NetworkFirewall__FirewallPolicy,
  ResourceType_AWS__NetworkFirewall__RuleGroup,
  ResourceType_AWS__NetworkManager__Device,
  ResourceType_AWS__NetworkManager__GlobalNetwork,
  ResourceType_AWS__NetworkManager__Link,
  ResourceType_AWS__NetworkManager__Site,
  ResourceType_AWS__NetworkManager__TransitGatewayRegistration,
  ResourceType_AWS__OpenSearch__Domain,
  ResourceType_AWS__Panorama__Package,
  ResourceType_AWS__Pinpoint__App,
  ResourceType_AWS__Pinpoint__ApplicationSettings,
  ResourceType_AWS__Pinpoint__Campaign,
  ResourceType_AWS__Pinpoint__InAppTemplate,
  ResourceType_AWS__Pinpoint__Segment,
  ResourceType_AWS__QLDB__Ledger,
  ResourceType_AWS__RDS__DBCluster,
  ResourceType_AWS__RDS__DBClusterSnapshot,
  ResourceType_AWS__RDS__DBInstance,
  ResourceType_AWS__RDS__DBSecurityGroup,
  ResourceType_AWS__RDS__DBSnapshot,
  ResourceType_AWS__RDS__DBSubnetGroup,
  ResourceType_AWS__RDS__EventSubscription,
  ResourceType_AWS__RDS__GlobalCluster,
  ResourceType_AWS__RUM__AppMonitor,
  ResourceType_AWS__Redshift__Cluster,
  ResourceType_AWS__Redshift__ClusterParameterGroup,
  ResourceType_AWS__Redshift__ClusterSecurityGroup,
  ResourceType_AWS__Redshift__ClusterSnapshot,
  ResourceType_AWS__Redshift__ClusterSubnetGroup,
  ResourceType_AWS__Redshift__EventSubscription,
  ResourceType_AWS__Redshift__ScheduledAction,
  ResourceType_AWS__ResilienceHub__ResiliencyPolicy,
  ResourceType_AWS__RoboMaker__RobotApplication,
  ResourceType_AWS__RoboMaker__RobotApplicationVersion,
  ResourceType_AWS__RoboMaker__SimulationApplication,
  ResourceType_AWS__Route53RecoveryControl__Cluster,
  ResourceType_AWS__Route53RecoveryControl__ControlPanel,
  ResourceType_AWS__Route53RecoveryControl__RoutingControl,
  ResourceType_AWS__Route53RecoveryControl__SafetyRule,
  ResourceType_AWS__Route53RecoveryReadiness__Cell,
  ResourceType_AWS__Route53RecoveryReadiness__ReadinessCheck,
  ResourceType_AWS__Route53RecoveryReadiness__RecoveryGroup,
  ResourceType_AWS__Route53RecoveryReadiness__ResourceSet,
  ResourceType_AWS__Route53Resolver__FirewallDomainList,
  ResourceType_AWS__Route53Resolver__FirewallRuleGroupAssociation,
  ResourceType_AWS__Route53Resolver__ResolverEndpoint,
  ResourceType_AWS__Route53Resolver__ResolverRule,
  ResourceType_AWS__Route53Resolver__ResolverRuleAssociation,
  ResourceType_AWS__Route53__HostedZone,
  ResourceType_AWS__S3__AccountPublicAccessBlock,
  ResourceType_AWS__S3__Bucket,
  ResourceType_AWS__S3__MultiRegionAccessPoint,
  ResourceType_AWS__S3__StorageLens,
  ResourceType_AWS__SES__ConfigurationSet,
  ResourceType_AWS__SES__ContactList,
  ResourceType_AWS__SES__ReceiptFilter,
  ResourceType_AWS__SES__ReceiptRuleSet,
  ResourceType_AWS__SES__Template,
  ResourceType_AWS__SNS__Topic,
  ResourceType_AWS__SQS__Queue,
  ResourceType_AWS__SSM__AssociationCompliance,
  ResourceType_AWS__SSM__FileData,
  ResourceType_AWS__SSM__ManagedInstanceInventory,
  ResourceType_AWS__SSM__PatchCompliance,
  ResourceType_AWS__SageMaker__AppImageConfig,
  ResourceType_AWS__SageMaker__CodeRepository,
  ResourceType_AWS__SageMaker__Domain,
  ResourceType_AWS__SageMaker__Image,
  ResourceType_AWS__SageMaker__Model,
  ResourceType_AWS__SageMaker__NotebookInstanceLifecycleConfig,
  ResourceType_AWS__SageMaker__Workteam,
  ResourceType_AWS__SecretsManager__Secret,
  ResourceType_AWS__ServiceCatalog__CloudFormationProduct,
  ResourceType_AWS__ServiceCatalog__CloudFormationProvisionedProduct,
  ResourceType_AWS__ServiceCatalog__Portfolio,
  ResourceType_AWS__ServiceDiscovery__HttpNamespace,
  ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace,
  ResourceType_AWS__ServiceDiscovery__Service,
  ResourceType_AWS__ShieldRegional__Protection,
  ResourceType_AWS__Shield__Protection,
  ResourceType_AWS__Signer__SigningProfile,
  ResourceType_AWS__StepFunctions__Activity,
  ResourceType_AWS__StepFunctions__StateMachine,
  ResourceType_AWS__Transfer__Agreement,
  ResourceType_AWS__Transfer__Connector,
  ResourceType_AWS__Transfer__Workflow,
  ResourceType_AWS__WAFRegional__RateBasedRule,
  ResourceType_AWS__WAFRegional__Rule,
  ResourceType_AWS__WAFRegional__RuleGroup,
  ResourceType_AWS__WAFRegional__WebACL,
  ResourceType_AWS__WAF__RateBasedRule,
  ResourceType_AWS__WAF__Rule,
  ResourceType_AWS__WAF__RuleGroup,
  ResourceType_AWS__WAF__WebACL,
  ResourceType_AWS__WAFv2__IPSet,
  ResourceType_AWS__WAFv2__ManagedRuleSet,
  ResourceType_AWS__WAFv2__RegexPatternSet,
  ResourceType_AWS__WAFv2__RuleGroup,
  ResourceType_AWS__WAFv2__WebACL,
  ResourceType_AWS__WorkSpaces__ConnectionAlias,
  ResourceType_AWS__WorkSpaces__Workspace,
  ResourceType_AWS__XRay__EncryptionConfig,
  ResourceType'
  #-}
