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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AWS__ACM__Certificate,
        ResourceType_AWS__AccessAnalyzer__Analyzer,
        ResourceType_AWS__ApiGatewayV2__Api,
        ResourceType_AWS__ApiGatewayV2__Stage,
        ResourceType_AWS__ApiGateway__RestApi,
        ResourceType_AWS__ApiGateway__Stage,
        ResourceType_AWS__AppConfig__Application,
        ResourceType_AWS__AppSync__GraphQLApi,
        ResourceType_AWS__Athena__DataCatalog,
        ResourceType_AWS__Athena__WorkGroup,
        ResourceType_AWS__AutoScaling__AutoScalingGroup,
        ResourceType_AWS__AutoScaling__LaunchConfiguration,
        ResourceType_AWS__AutoScaling__ScalingPolicy,
        ResourceType_AWS__AutoScaling__ScheduledAction,
        ResourceType_AWS__Backup__BackupPlan,
        ResourceType_AWS__Backup__BackupSelection,
        ResourceType_AWS__Backup__BackupVault,
        ResourceType_AWS__Backup__RecoveryPoint,
        ResourceType_AWS__Batch__ComputeEnvironment,
        ResourceType_AWS__Batch__JobQueue,
        ResourceType_AWS__CloudFormation__Stack,
        ResourceType_AWS__CloudFront__Distribution,
        ResourceType_AWS__CloudFront__StreamingDistribution,
        ResourceType_AWS__CloudTrail__Trail,
        ResourceType_AWS__CloudWatch__Alarm,
        ResourceType_AWS__CodeBuild__Project,
        ResourceType_AWS__CodeDeploy__Application,
        ResourceType_AWS__CodeDeploy__DeploymentConfig,
        ResourceType_AWS__CodeDeploy__DeploymentGroup,
        ResourceType_AWS__CodePipeline__Pipeline,
        ResourceType_AWS__Config__ConformancePackCompliance,
        ResourceType_AWS__Config__ResourceCompliance,
        ResourceType_AWS__DMS__Certificate,
        ResourceType_AWS__DMS__EventSubscription,
        ResourceType_AWS__DMS__ReplicationSubnetGroup,
        ResourceType_AWS__DataSync__LocationEFS,
        ResourceType_AWS__DataSync__LocationFSxLustre,
        ResourceType_AWS__DataSync__LocationNFS,
        ResourceType_AWS__DataSync__LocationS3,
        ResourceType_AWS__DataSync__LocationSMB,
        ResourceType_AWS__DataSync__Task,
        ResourceType_AWS__Detective__Graph,
        ResourceType_AWS__DynamoDB__Table,
        ResourceType_AWS__EC2__CustomerGateway,
        ResourceType_AWS__EC2__EIP,
        ResourceType_AWS__EC2__EgressOnlyInternetGateway,
        ResourceType_AWS__EC2__FlowLog,
        ResourceType_AWS__EC2__Host,
        ResourceType_AWS__EC2__Instance,
        ResourceType_AWS__EC2__InternetGateway,
        ResourceType_AWS__EC2__LaunchTemplate,
        ResourceType_AWS__EC2__NatGateway,
        ResourceType_AWS__EC2__NetworkAcl,
        ResourceType_AWS__EC2__NetworkInsightsAccessScopeAnalysis,
        ResourceType_AWS__EC2__NetworkInterface,
        ResourceType_AWS__EC2__RegisteredHAInstance,
        ResourceType_AWS__EC2__RouteTable,
        ResourceType_AWS__EC2__SecurityGroup,
        ResourceType_AWS__EC2__Subnet,
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
        ResourceType_AWS__ECR__Repository,
        ResourceType_AWS__ECS__Cluster,
        ResourceType_AWS__ECS__Service,
        ResourceType_AWS__ECS__TaskDefinition,
        ResourceType_AWS__EFS__AccessPoint,
        ResourceType_AWS__EFS__FileSystem,
        ResourceType_AWS__EKS__Cluster,
        ResourceType_AWS__EKS__FargateProfile,
        ResourceType_AWS__EMR__SecurityConfiguration,
        ResourceType_AWS__ElasticBeanstalk__Application,
        ResourceType_AWS__ElasticBeanstalk__ApplicationVersion,
        ResourceType_AWS__ElasticBeanstalk__Environment,
        ResourceType_AWS__ElasticLoadBalancingV2__Listener,
        ResourceType_AWS__ElasticLoadBalancingV2__LoadBalancer,
        ResourceType_AWS__ElasticLoadBalancing__LoadBalancer,
        ResourceType_AWS__Elasticsearch__Domain,
        ResourceType_AWS__GlobalAccelerator__Accelerator,
        ResourceType_AWS__GlobalAccelerator__EndpointGroup,
        ResourceType_AWS__GlobalAccelerator__Listener,
        ResourceType_AWS__Glue__Job,
        ResourceType_AWS__GuardDuty__Detector,
        ResourceType_AWS__GuardDuty__IPSet,
        ResourceType_AWS__GuardDuty__ThreatIntelSet,
        ResourceType_AWS__IAM__Group,
        ResourceType_AWS__IAM__Policy,
        ResourceType_AWS__IAM__Role,
        ResourceType_AWS__IAM__User,
        ResourceType_AWS__KMS__Key,
        ResourceType_AWS__Kinesis__Stream,
        ResourceType_AWS__Kinesis__StreamConsumer,
        ResourceType_AWS__Lambda__Function,
        ResourceType_AWS__MSK__Cluster,
        ResourceType_AWS__NetworkFirewall__Firewall,
        ResourceType_AWS__NetworkFirewall__FirewallPolicy,
        ResourceType_AWS__NetworkFirewall__RuleGroup,
        ResourceType_AWS__OpenSearch__Domain,
        ResourceType_AWS__QLDB__Ledger,
        ResourceType_AWS__RDS__DBCluster,
        ResourceType_AWS__RDS__DBClusterSnapshot,
        ResourceType_AWS__RDS__DBInstance,
        ResourceType_AWS__RDS__DBSecurityGroup,
        ResourceType_AWS__RDS__DBSnapshot,
        ResourceType_AWS__RDS__DBSubnetGroup,
        ResourceType_AWS__RDS__EventSubscription,
        ResourceType_AWS__Redshift__Cluster,
        ResourceType_AWS__Redshift__ClusterParameterGroup,
        ResourceType_AWS__Redshift__ClusterSecurityGroup,
        ResourceType_AWS__Redshift__ClusterSnapshot,
        ResourceType_AWS__Redshift__ClusterSubnetGroup,
        ResourceType_AWS__Redshift__EventSubscription,
        ResourceType_AWS__Route53Resolver__ResolverEndpoint,
        ResourceType_AWS__Route53Resolver__ResolverRule,
        ResourceType_AWS__Route53Resolver__ResolverRuleAssociation,
        ResourceType_AWS__Route53__HostedZone,
        ResourceType_AWS__S3__AccountPublicAccessBlock,
        ResourceType_AWS__S3__Bucket,
        ResourceType_AWS__SES__ConfigurationSet,
        ResourceType_AWS__SES__ContactList,
        ResourceType_AWS__SNS__Topic,
        ResourceType_AWS__SQS__Queue,
        ResourceType_AWS__SSM__AssociationCompliance,
        ResourceType_AWS__SSM__FileData,
        ResourceType_AWS__SSM__ManagedInstanceInventory,
        ResourceType_AWS__SSM__PatchCompliance,
        ResourceType_AWS__SageMaker__CodeRepository,
        ResourceType_AWS__SageMaker__Model,
        ResourceType_AWS__SageMaker__NotebookInstanceLifecycleConfig,
        ResourceType_AWS__SageMaker__Workteam,
        ResourceType_AWS__SecretsManager__Secret,
        ResourceType_AWS__ServiceCatalog__CloudFormationProduct,
        ResourceType_AWS__ServiceCatalog__CloudFormationProvisionedProduct,
        ResourceType_AWS__ServiceCatalog__Portfolio,
        ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace,
        ResourceType_AWS__ServiceDiscovery__Service,
        ResourceType_AWS__ShieldRegional__Protection,
        ResourceType_AWS__Shield__Protection,
        ResourceType_AWS__StepFunctions__Activity,
        ResourceType_AWS__StepFunctions__StateMachine,
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

pattern ResourceType_AWS__AppSync__GraphQLApi :: ResourceType
pattern ResourceType_AWS__AppSync__GraphQLApi = ResourceType' "AWS::AppSync::GraphQLApi"

pattern ResourceType_AWS__Athena__DataCatalog :: ResourceType
pattern ResourceType_AWS__Athena__DataCatalog = ResourceType' "AWS::Athena::DataCatalog"

pattern ResourceType_AWS__Athena__WorkGroup :: ResourceType
pattern ResourceType_AWS__Athena__WorkGroup = ResourceType' "AWS::Athena::WorkGroup"

pattern ResourceType_AWS__AutoScaling__AutoScalingGroup :: ResourceType
pattern ResourceType_AWS__AutoScaling__AutoScalingGroup = ResourceType' "AWS::AutoScaling::AutoScalingGroup"

pattern ResourceType_AWS__AutoScaling__LaunchConfiguration :: ResourceType
pattern ResourceType_AWS__AutoScaling__LaunchConfiguration = ResourceType' "AWS::AutoScaling::LaunchConfiguration"

pattern ResourceType_AWS__AutoScaling__ScalingPolicy :: ResourceType
pattern ResourceType_AWS__AutoScaling__ScalingPolicy = ResourceType' "AWS::AutoScaling::ScalingPolicy"

pattern ResourceType_AWS__AutoScaling__ScheduledAction :: ResourceType
pattern ResourceType_AWS__AutoScaling__ScheduledAction = ResourceType' "AWS::AutoScaling::ScheduledAction"

pattern ResourceType_AWS__Backup__BackupPlan :: ResourceType
pattern ResourceType_AWS__Backup__BackupPlan = ResourceType' "AWS::Backup::BackupPlan"

pattern ResourceType_AWS__Backup__BackupSelection :: ResourceType
pattern ResourceType_AWS__Backup__BackupSelection = ResourceType' "AWS::Backup::BackupSelection"

pattern ResourceType_AWS__Backup__BackupVault :: ResourceType
pattern ResourceType_AWS__Backup__BackupVault = ResourceType' "AWS::Backup::BackupVault"

pattern ResourceType_AWS__Backup__RecoveryPoint :: ResourceType
pattern ResourceType_AWS__Backup__RecoveryPoint = ResourceType' "AWS::Backup::RecoveryPoint"

pattern ResourceType_AWS__Batch__ComputeEnvironment :: ResourceType
pattern ResourceType_AWS__Batch__ComputeEnvironment = ResourceType' "AWS::Batch::ComputeEnvironment"

pattern ResourceType_AWS__Batch__JobQueue :: ResourceType
pattern ResourceType_AWS__Batch__JobQueue = ResourceType' "AWS::Batch::JobQueue"

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

pattern ResourceType_AWS__CodeBuild__Project :: ResourceType
pattern ResourceType_AWS__CodeBuild__Project = ResourceType' "AWS::CodeBuild::Project"

pattern ResourceType_AWS__CodeDeploy__Application :: ResourceType
pattern ResourceType_AWS__CodeDeploy__Application = ResourceType' "AWS::CodeDeploy::Application"

pattern ResourceType_AWS__CodeDeploy__DeploymentConfig :: ResourceType
pattern ResourceType_AWS__CodeDeploy__DeploymentConfig = ResourceType' "AWS::CodeDeploy::DeploymentConfig"

pattern ResourceType_AWS__CodeDeploy__DeploymentGroup :: ResourceType
pattern ResourceType_AWS__CodeDeploy__DeploymentGroup = ResourceType' "AWS::CodeDeploy::DeploymentGroup"

pattern ResourceType_AWS__CodePipeline__Pipeline :: ResourceType
pattern ResourceType_AWS__CodePipeline__Pipeline = ResourceType' "AWS::CodePipeline::Pipeline"

pattern ResourceType_AWS__Config__ConformancePackCompliance :: ResourceType
pattern ResourceType_AWS__Config__ConformancePackCompliance = ResourceType' "AWS::Config::ConformancePackCompliance"

pattern ResourceType_AWS__Config__ResourceCompliance :: ResourceType
pattern ResourceType_AWS__Config__ResourceCompliance = ResourceType' "AWS::Config::ResourceCompliance"

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

pattern ResourceType_AWS__DataSync__LocationNFS :: ResourceType
pattern ResourceType_AWS__DataSync__LocationNFS = ResourceType' "AWS::DataSync::LocationNFS"

pattern ResourceType_AWS__DataSync__LocationS3 :: ResourceType
pattern ResourceType_AWS__DataSync__LocationS3 = ResourceType' "AWS::DataSync::LocationS3"

pattern ResourceType_AWS__DataSync__LocationSMB :: ResourceType
pattern ResourceType_AWS__DataSync__LocationSMB = ResourceType' "AWS::DataSync::LocationSMB"

pattern ResourceType_AWS__DataSync__Task :: ResourceType
pattern ResourceType_AWS__DataSync__Task = ResourceType' "AWS::DataSync::Task"

pattern ResourceType_AWS__Detective__Graph :: ResourceType
pattern ResourceType_AWS__Detective__Graph = ResourceType' "AWS::Detective::Graph"

pattern ResourceType_AWS__DynamoDB__Table :: ResourceType
pattern ResourceType_AWS__DynamoDB__Table = ResourceType' "AWS::DynamoDB::Table"

pattern ResourceType_AWS__EC2__CustomerGateway :: ResourceType
pattern ResourceType_AWS__EC2__CustomerGateway = ResourceType' "AWS::EC2::CustomerGateway"

pattern ResourceType_AWS__EC2__EIP :: ResourceType
pattern ResourceType_AWS__EC2__EIP = ResourceType' "AWS::EC2::EIP"

pattern ResourceType_AWS__EC2__EgressOnlyInternetGateway :: ResourceType
pattern ResourceType_AWS__EC2__EgressOnlyInternetGateway = ResourceType' "AWS::EC2::EgressOnlyInternetGateway"

pattern ResourceType_AWS__EC2__FlowLog :: ResourceType
pattern ResourceType_AWS__EC2__FlowLog = ResourceType' "AWS::EC2::FlowLog"

pattern ResourceType_AWS__EC2__Host :: ResourceType
pattern ResourceType_AWS__EC2__Host = ResourceType' "AWS::EC2::Host"

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

pattern ResourceType_AWS__EC2__NetworkInterface :: ResourceType
pattern ResourceType_AWS__EC2__NetworkInterface = ResourceType' "AWS::EC2::NetworkInterface"

pattern ResourceType_AWS__EC2__RegisteredHAInstance :: ResourceType
pattern ResourceType_AWS__EC2__RegisteredHAInstance = ResourceType' "AWS::EC2::RegisteredHAInstance"

pattern ResourceType_AWS__EC2__RouteTable :: ResourceType
pattern ResourceType_AWS__EC2__RouteTable = ResourceType' "AWS::EC2::RouteTable"

pattern ResourceType_AWS__EC2__SecurityGroup :: ResourceType
pattern ResourceType_AWS__EC2__SecurityGroup = ResourceType' "AWS::EC2::SecurityGroup"

pattern ResourceType_AWS__EC2__Subnet :: ResourceType
pattern ResourceType_AWS__EC2__Subnet = ResourceType' "AWS::EC2::Subnet"

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

pattern ResourceType_AWS__ECR__Repository :: ResourceType
pattern ResourceType_AWS__ECR__Repository = ResourceType' "AWS::ECR::Repository"

pattern ResourceType_AWS__ECS__Cluster :: ResourceType
pattern ResourceType_AWS__ECS__Cluster = ResourceType' "AWS::ECS::Cluster"

pattern ResourceType_AWS__ECS__Service :: ResourceType
pattern ResourceType_AWS__ECS__Service = ResourceType' "AWS::ECS::Service"

pattern ResourceType_AWS__ECS__TaskDefinition :: ResourceType
pattern ResourceType_AWS__ECS__TaskDefinition = ResourceType' "AWS::ECS::TaskDefinition"

pattern ResourceType_AWS__EFS__AccessPoint :: ResourceType
pattern ResourceType_AWS__EFS__AccessPoint = ResourceType' "AWS::EFS::AccessPoint"

pattern ResourceType_AWS__EFS__FileSystem :: ResourceType
pattern ResourceType_AWS__EFS__FileSystem = ResourceType' "AWS::EFS::FileSystem"

pattern ResourceType_AWS__EKS__Cluster :: ResourceType
pattern ResourceType_AWS__EKS__Cluster = ResourceType' "AWS::EKS::Cluster"

pattern ResourceType_AWS__EKS__FargateProfile :: ResourceType
pattern ResourceType_AWS__EKS__FargateProfile = ResourceType' "AWS::EKS::FargateProfile"

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

pattern ResourceType_AWS__GlobalAccelerator__Accelerator :: ResourceType
pattern ResourceType_AWS__GlobalAccelerator__Accelerator = ResourceType' "AWS::GlobalAccelerator::Accelerator"

pattern ResourceType_AWS__GlobalAccelerator__EndpointGroup :: ResourceType
pattern ResourceType_AWS__GlobalAccelerator__EndpointGroup = ResourceType' "AWS::GlobalAccelerator::EndpointGroup"

pattern ResourceType_AWS__GlobalAccelerator__Listener :: ResourceType
pattern ResourceType_AWS__GlobalAccelerator__Listener = ResourceType' "AWS::GlobalAccelerator::Listener"

pattern ResourceType_AWS__Glue__Job :: ResourceType
pattern ResourceType_AWS__Glue__Job = ResourceType' "AWS::Glue::Job"

pattern ResourceType_AWS__GuardDuty__Detector :: ResourceType
pattern ResourceType_AWS__GuardDuty__Detector = ResourceType' "AWS::GuardDuty::Detector"

pattern ResourceType_AWS__GuardDuty__IPSet :: ResourceType
pattern ResourceType_AWS__GuardDuty__IPSet = ResourceType' "AWS::GuardDuty::IPSet"

pattern ResourceType_AWS__GuardDuty__ThreatIntelSet :: ResourceType
pattern ResourceType_AWS__GuardDuty__ThreatIntelSet = ResourceType' "AWS::GuardDuty::ThreatIntelSet"

pattern ResourceType_AWS__IAM__Group :: ResourceType
pattern ResourceType_AWS__IAM__Group = ResourceType' "AWS::IAM::Group"

pattern ResourceType_AWS__IAM__Policy :: ResourceType
pattern ResourceType_AWS__IAM__Policy = ResourceType' "AWS::IAM::Policy"

pattern ResourceType_AWS__IAM__Role :: ResourceType
pattern ResourceType_AWS__IAM__Role = ResourceType' "AWS::IAM::Role"

pattern ResourceType_AWS__IAM__User :: ResourceType
pattern ResourceType_AWS__IAM__User = ResourceType' "AWS::IAM::User"

pattern ResourceType_AWS__KMS__Key :: ResourceType
pattern ResourceType_AWS__KMS__Key = ResourceType' "AWS::KMS::Key"

pattern ResourceType_AWS__Kinesis__Stream :: ResourceType
pattern ResourceType_AWS__Kinesis__Stream = ResourceType' "AWS::Kinesis::Stream"

pattern ResourceType_AWS__Kinesis__StreamConsumer :: ResourceType
pattern ResourceType_AWS__Kinesis__StreamConsumer = ResourceType' "AWS::Kinesis::StreamConsumer"

pattern ResourceType_AWS__Lambda__Function :: ResourceType
pattern ResourceType_AWS__Lambda__Function = ResourceType' "AWS::Lambda::Function"

pattern ResourceType_AWS__MSK__Cluster :: ResourceType
pattern ResourceType_AWS__MSK__Cluster = ResourceType' "AWS::MSK::Cluster"

pattern ResourceType_AWS__NetworkFirewall__Firewall :: ResourceType
pattern ResourceType_AWS__NetworkFirewall__Firewall = ResourceType' "AWS::NetworkFirewall::Firewall"

pattern ResourceType_AWS__NetworkFirewall__FirewallPolicy :: ResourceType
pattern ResourceType_AWS__NetworkFirewall__FirewallPolicy = ResourceType' "AWS::NetworkFirewall::FirewallPolicy"

pattern ResourceType_AWS__NetworkFirewall__RuleGroup :: ResourceType
pattern ResourceType_AWS__NetworkFirewall__RuleGroup = ResourceType' "AWS::NetworkFirewall::RuleGroup"

pattern ResourceType_AWS__OpenSearch__Domain :: ResourceType
pattern ResourceType_AWS__OpenSearch__Domain = ResourceType' "AWS::OpenSearch::Domain"

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

pattern ResourceType_AWS__SES__ConfigurationSet :: ResourceType
pattern ResourceType_AWS__SES__ConfigurationSet = ResourceType' "AWS::SES::ConfigurationSet"

pattern ResourceType_AWS__SES__ContactList :: ResourceType
pattern ResourceType_AWS__SES__ContactList = ResourceType' "AWS::SES::ContactList"

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

pattern ResourceType_AWS__SageMaker__CodeRepository :: ResourceType
pattern ResourceType_AWS__SageMaker__CodeRepository = ResourceType' "AWS::SageMaker::CodeRepository"

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

pattern ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace :: ResourceType
pattern ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace = ResourceType' "AWS::ServiceDiscovery::PublicDnsNamespace"

pattern ResourceType_AWS__ServiceDiscovery__Service :: ResourceType
pattern ResourceType_AWS__ServiceDiscovery__Service = ResourceType' "AWS::ServiceDiscovery::Service"

pattern ResourceType_AWS__ShieldRegional__Protection :: ResourceType
pattern ResourceType_AWS__ShieldRegional__Protection = ResourceType' "AWS::ShieldRegional::Protection"

pattern ResourceType_AWS__Shield__Protection :: ResourceType
pattern ResourceType_AWS__Shield__Protection = ResourceType' "AWS::Shield::Protection"

pattern ResourceType_AWS__StepFunctions__Activity :: ResourceType
pattern ResourceType_AWS__StepFunctions__Activity = ResourceType' "AWS::StepFunctions::Activity"

pattern ResourceType_AWS__StepFunctions__StateMachine :: ResourceType
pattern ResourceType_AWS__StepFunctions__StateMachine = ResourceType' "AWS::StepFunctions::StateMachine"

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
  ResourceType_AWS__ApiGatewayV2__Api,
  ResourceType_AWS__ApiGatewayV2__Stage,
  ResourceType_AWS__ApiGateway__RestApi,
  ResourceType_AWS__ApiGateway__Stage,
  ResourceType_AWS__AppConfig__Application,
  ResourceType_AWS__AppSync__GraphQLApi,
  ResourceType_AWS__Athena__DataCatalog,
  ResourceType_AWS__Athena__WorkGroup,
  ResourceType_AWS__AutoScaling__AutoScalingGroup,
  ResourceType_AWS__AutoScaling__LaunchConfiguration,
  ResourceType_AWS__AutoScaling__ScalingPolicy,
  ResourceType_AWS__AutoScaling__ScheduledAction,
  ResourceType_AWS__Backup__BackupPlan,
  ResourceType_AWS__Backup__BackupSelection,
  ResourceType_AWS__Backup__BackupVault,
  ResourceType_AWS__Backup__RecoveryPoint,
  ResourceType_AWS__Batch__ComputeEnvironment,
  ResourceType_AWS__Batch__JobQueue,
  ResourceType_AWS__CloudFormation__Stack,
  ResourceType_AWS__CloudFront__Distribution,
  ResourceType_AWS__CloudFront__StreamingDistribution,
  ResourceType_AWS__CloudTrail__Trail,
  ResourceType_AWS__CloudWatch__Alarm,
  ResourceType_AWS__CodeBuild__Project,
  ResourceType_AWS__CodeDeploy__Application,
  ResourceType_AWS__CodeDeploy__DeploymentConfig,
  ResourceType_AWS__CodeDeploy__DeploymentGroup,
  ResourceType_AWS__CodePipeline__Pipeline,
  ResourceType_AWS__Config__ConformancePackCompliance,
  ResourceType_AWS__Config__ResourceCompliance,
  ResourceType_AWS__DMS__Certificate,
  ResourceType_AWS__DMS__EventSubscription,
  ResourceType_AWS__DMS__ReplicationSubnetGroup,
  ResourceType_AWS__DataSync__LocationEFS,
  ResourceType_AWS__DataSync__LocationFSxLustre,
  ResourceType_AWS__DataSync__LocationNFS,
  ResourceType_AWS__DataSync__LocationS3,
  ResourceType_AWS__DataSync__LocationSMB,
  ResourceType_AWS__DataSync__Task,
  ResourceType_AWS__Detective__Graph,
  ResourceType_AWS__DynamoDB__Table,
  ResourceType_AWS__EC2__CustomerGateway,
  ResourceType_AWS__EC2__EIP,
  ResourceType_AWS__EC2__EgressOnlyInternetGateway,
  ResourceType_AWS__EC2__FlowLog,
  ResourceType_AWS__EC2__Host,
  ResourceType_AWS__EC2__Instance,
  ResourceType_AWS__EC2__InternetGateway,
  ResourceType_AWS__EC2__LaunchTemplate,
  ResourceType_AWS__EC2__NatGateway,
  ResourceType_AWS__EC2__NetworkAcl,
  ResourceType_AWS__EC2__NetworkInsightsAccessScopeAnalysis,
  ResourceType_AWS__EC2__NetworkInterface,
  ResourceType_AWS__EC2__RegisteredHAInstance,
  ResourceType_AWS__EC2__RouteTable,
  ResourceType_AWS__EC2__SecurityGroup,
  ResourceType_AWS__EC2__Subnet,
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
  ResourceType_AWS__ECR__Repository,
  ResourceType_AWS__ECS__Cluster,
  ResourceType_AWS__ECS__Service,
  ResourceType_AWS__ECS__TaskDefinition,
  ResourceType_AWS__EFS__AccessPoint,
  ResourceType_AWS__EFS__FileSystem,
  ResourceType_AWS__EKS__Cluster,
  ResourceType_AWS__EKS__FargateProfile,
  ResourceType_AWS__EMR__SecurityConfiguration,
  ResourceType_AWS__ElasticBeanstalk__Application,
  ResourceType_AWS__ElasticBeanstalk__ApplicationVersion,
  ResourceType_AWS__ElasticBeanstalk__Environment,
  ResourceType_AWS__ElasticLoadBalancingV2__Listener,
  ResourceType_AWS__ElasticLoadBalancingV2__LoadBalancer,
  ResourceType_AWS__ElasticLoadBalancing__LoadBalancer,
  ResourceType_AWS__Elasticsearch__Domain,
  ResourceType_AWS__GlobalAccelerator__Accelerator,
  ResourceType_AWS__GlobalAccelerator__EndpointGroup,
  ResourceType_AWS__GlobalAccelerator__Listener,
  ResourceType_AWS__Glue__Job,
  ResourceType_AWS__GuardDuty__Detector,
  ResourceType_AWS__GuardDuty__IPSet,
  ResourceType_AWS__GuardDuty__ThreatIntelSet,
  ResourceType_AWS__IAM__Group,
  ResourceType_AWS__IAM__Policy,
  ResourceType_AWS__IAM__Role,
  ResourceType_AWS__IAM__User,
  ResourceType_AWS__KMS__Key,
  ResourceType_AWS__Kinesis__Stream,
  ResourceType_AWS__Kinesis__StreamConsumer,
  ResourceType_AWS__Lambda__Function,
  ResourceType_AWS__MSK__Cluster,
  ResourceType_AWS__NetworkFirewall__Firewall,
  ResourceType_AWS__NetworkFirewall__FirewallPolicy,
  ResourceType_AWS__NetworkFirewall__RuleGroup,
  ResourceType_AWS__OpenSearch__Domain,
  ResourceType_AWS__QLDB__Ledger,
  ResourceType_AWS__RDS__DBCluster,
  ResourceType_AWS__RDS__DBClusterSnapshot,
  ResourceType_AWS__RDS__DBInstance,
  ResourceType_AWS__RDS__DBSecurityGroup,
  ResourceType_AWS__RDS__DBSnapshot,
  ResourceType_AWS__RDS__DBSubnetGroup,
  ResourceType_AWS__RDS__EventSubscription,
  ResourceType_AWS__Redshift__Cluster,
  ResourceType_AWS__Redshift__ClusterParameterGroup,
  ResourceType_AWS__Redshift__ClusterSecurityGroup,
  ResourceType_AWS__Redshift__ClusterSnapshot,
  ResourceType_AWS__Redshift__ClusterSubnetGroup,
  ResourceType_AWS__Redshift__EventSubscription,
  ResourceType_AWS__Route53Resolver__ResolverEndpoint,
  ResourceType_AWS__Route53Resolver__ResolverRule,
  ResourceType_AWS__Route53Resolver__ResolverRuleAssociation,
  ResourceType_AWS__Route53__HostedZone,
  ResourceType_AWS__S3__AccountPublicAccessBlock,
  ResourceType_AWS__S3__Bucket,
  ResourceType_AWS__SES__ConfigurationSet,
  ResourceType_AWS__SES__ContactList,
  ResourceType_AWS__SNS__Topic,
  ResourceType_AWS__SQS__Queue,
  ResourceType_AWS__SSM__AssociationCompliance,
  ResourceType_AWS__SSM__FileData,
  ResourceType_AWS__SSM__ManagedInstanceInventory,
  ResourceType_AWS__SSM__PatchCompliance,
  ResourceType_AWS__SageMaker__CodeRepository,
  ResourceType_AWS__SageMaker__Model,
  ResourceType_AWS__SageMaker__NotebookInstanceLifecycleConfig,
  ResourceType_AWS__SageMaker__Workteam,
  ResourceType_AWS__SecretsManager__Secret,
  ResourceType_AWS__ServiceCatalog__CloudFormationProduct,
  ResourceType_AWS__ServiceCatalog__CloudFormationProvisionedProduct,
  ResourceType_AWS__ServiceCatalog__Portfolio,
  ResourceType_AWS__ServiceDiscovery__PublicDnsNamespace,
  ResourceType_AWS__ServiceDiscovery__Service,
  ResourceType_AWS__ShieldRegional__Protection,
  ResourceType_AWS__Shield__Protection,
  ResourceType_AWS__StepFunctions__Activity,
  ResourceType_AWS__StepFunctions__StateMachine,
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
