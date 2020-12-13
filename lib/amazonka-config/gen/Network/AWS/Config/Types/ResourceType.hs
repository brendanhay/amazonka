{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceType
  ( ResourceType
      ( ResourceType',
        AWSEC2CustomerGateway,
        AWSEC2EIP,
        AWSEC2Host,
        AWSEC2Instance,
        AWSEC2InternetGateway,
        AWSEC2NetworkACL,
        AWSEC2NetworkInterface,
        AWSEC2RouteTable,
        AWSEC2SecurityGroup,
        AWSEC2Subnet,
        AWSCloudTrailTrail,
        AWSEC2Volume,
        AWSEC2VPC,
        AWSEC2VPNConnection,
        AWSEC2VPNGateway,
        AWSEC2RegisteredHAInstance,
        AWSEC2NatGateway,
        AWSEC2EgressOnlyInternetGateway,
        AWSEC2VPCEndpoint,
        AWSEC2VPCEndpointService,
        AWSEC2FlowLog,
        AWSEC2VPCPeeringConnection,
        AWSElasticsearchDomain,
        AWSIAMGroup,
        AWSIAMPolicy,
        AWSIAMRole,
        AWSIAMUser,
        AWSELASTICLOADBALANCINGV2LoadBalancer,
        AWSAcmCertificate,
        AWSRDSDBInstance,
        AWSRDSDBSubnetGroup,
        AWSRDSDBSecurityGroup,
        AWSRDSDBSnapshot,
        AWSRDSDBCluster,
        AWSRDSDBClusterSnapshot,
        AWSRDSEventSubscription,
        AWSS3Bucket,
        AWSS3AccountPublicAccessBlock,
        AWSRedshiftCluster,
        AWSRedshiftClusterSnapshot,
        AWSRedshiftClusterParameterGroup,
        AWSRedshiftClusterSecurityGroup,
        AWSRedshiftClusterSubnetGroup,
        AWSRedshiftEventSubscription,
        AWSSsmManagedInstanceInventory,
        AWSCloudWatchAlarm,
        AWSCloudFormationStack,
        AWSElasticLoadBalancingLoadBalancer,
        AWSAutoScalingAutoScalingGroup,
        AWSAutoScalingLaunchConfiguration,
        AWSAutoScalingScalingPolicy,
        AWSAutoScalingScheduledAction,
        AWSDynamoDBTable,
        AWSCodeBuildProject,
        AWSWafRateBasedRule,
        AWSWafRule,
        AWSWafRuleGroup,
        AWSWafWebACL,
        AWSWAFRegionalRateBasedRule,
        AWSWAFRegionalRule,
        AWSWAFRegionalRuleGroup,
        AWSWAFRegionalWebACL,
        AWSCloudFrontDistribution,
        AWSCloudFrontStreamingDistribution,
        AWSLambdaFunction,
        AWSElasticBeanstalkApplication,
        AWSElasticBeanstalkApplicationVersion,
        AWSElasticBeanstalkEnvironment,
        AWSWAFV2WebACL,
        AWSWAFV2RuleGroup,
        AWSWAFV2IPSet,
        AWSWAFV2RegexPatternSet,
        AWSWAFV2ManagedRuleSet,
        AWSXRayEncryptionConfig,
        AWSSsmAssociationCompliance,
        AWSSsmPatchCompliance,
        AWSShieldProtection,
        AWSShieldRegionalProtection,
        AWSConfigResourceCompliance,
        AWSAPIGatewayStage,
        AWSAPIGatewayRestAPI,
        AWSAPIGATEWAYV2Stage,
        AWSAPIGATEWAYV2API,
        AWSCodePipelinePipeline,
        AWSServiceCatalogCloudFormationProvisionedProduct,
        AWSServiceCatalogCloudFormationProduct,
        AWSServiceCatalogPortfolio,
        AWSSqsQueue,
        AWSKMSKey,
        AWSQldbLedger,
        AWSSecretsManagerSecret,
        AWSSNSTopic,
        AWSSsmFileData
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceType = ResourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AWSEC2CustomerGateway :: ResourceType
pattern AWSEC2CustomerGateway = ResourceType' "AWS::EC2::CustomerGateway"

pattern AWSEC2EIP :: ResourceType
pattern AWSEC2EIP = ResourceType' "AWS::EC2::EIP"

pattern AWSEC2Host :: ResourceType
pattern AWSEC2Host = ResourceType' "AWS::EC2::Host"

pattern AWSEC2Instance :: ResourceType
pattern AWSEC2Instance = ResourceType' "AWS::EC2::Instance"

pattern AWSEC2InternetGateway :: ResourceType
pattern AWSEC2InternetGateway = ResourceType' "AWS::EC2::InternetGateway"

pattern AWSEC2NetworkACL :: ResourceType
pattern AWSEC2NetworkACL = ResourceType' "AWS::EC2::NetworkAcl"

pattern AWSEC2NetworkInterface :: ResourceType
pattern AWSEC2NetworkInterface = ResourceType' "AWS::EC2::NetworkInterface"

pattern AWSEC2RouteTable :: ResourceType
pattern AWSEC2RouteTable = ResourceType' "AWS::EC2::RouteTable"

pattern AWSEC2SecurityGroup :: ResourceType
pattern AWSEC2SecurityGroup = ResourceType' "AWS::EC2::SecurityGroup"

pattern AWSEC2Subnet :: ResourceType
pattern AWSEC2Subnet = ResourceType' "AWS::EC2::Subnet"

pattern AWSCloudTrailTrail :: ResourceType
pattern AWSCloudTrailTrail = ResourceType' "AWS::CloudTrail::Trail"

pattern AWSEC2Volume :: ResourceType
pattern AWSEC2Volume = ResourceType' "AWS::EC2::Volume"

pattern AWSEC2VPC :: ResourceType
pattern AWSEC2VPC = ResourceType' "AWS::EC2::VPC"

pattern AWSEC2VPNConnection :: ResourceType
pattern AWSEC2VPNConnection = ResourceType' "AWS::EC2::VPNConnection"

pattern AWSEC2VPNGateway :: ResourceType
pattern AWSEC2VPNGateway = ResourceType' "AWS::EC2::VPNGateway"

pattern AWSEC2RegisteredHAInstance :: ResourceType
pattern AWSEC2RegisteredHAInstance = ResourceType' "AWS::EC2::RegisteredHAInstance"

pattern AWSEC2NatGateway :: ResourceType
pattern AWSEC2NatGateway = ResourceType' "AWS::EC2::NatGateway"

pattern AWSEC2EgressOnlyInternetGateway :: ResourceType
pattern AWSEC2EgressOnlyInternetGateway = ResourceType' "AWS::EC2::EgressOnlyInternetGateway"

pattern AWSEC2VPCEndpoint :: ResourceType
pattern AWSEC2VPCEndpoint = ResourceType' "AWS::EC2::VPCEndpoint"

pattern AWSEC2VPCEndpointService :: ResourceType
pattern AWSEC2VPCEndpointService = ResourceType' "AWS::EC2::VPCEndpointService"

pattern AWSEC2FlowLog :: ResourceType
pattern AWSEC2FlowLog = ResourceType' "AWS::EC2::FlowLog"

pattern AWSEC2VPCPeeringConnection :: ResourceType
pattern AWSEC2VPCPeeringConnection = ResourceType' "AWS::EC2::VPCPeeringConnection"

pattern AWSElasticsearchDomain :: ResourceType
pattern AWSElasticsearchDomain = ResourceType' "AWS::Elasticsearch::Domain"

pattern AWSIAMGroup :: ResourceType
pattern AWSIAMGroup = ResourceType' "AWS::IAM::Group"

pattern AWSIAMPolicy :: ResourceType
pattern AWSIAMPolicy = ResourceType' "AWS::IAM::Policy"

pattern AWSIAMRole :: ResourceType
pattern AWSIAMRole = ResourceType' "AWS::IAM::Role"

pattern AWSIAMUser :: ResourceType
pattern AWSIAMUser = ResourceType' "AWS::IAM::User"

pattern AWSELASTICLOADBALANCINGV2LoadBalancer :: ResourceType
pattern AWSELASTICLOADBALANCINGV2LoadBalancer = ResourceType' "AWS::ElasticLoadBalancingV2::LoadBalancer"

pattern AWSAcmCertificate :: ResourceType
pattern AWSAcmCertificate = ResourceType' "AWS::ACM::Certificate"

pattern AWSRDSDBInstance :: ResourceType
pattern AWSRDSDBInstance = ResourceType' "AWS::RDS::DBInstance"

pattern AWSRDSDBSubnetGroup :: ResourceType
pattern AWSRDSDBSubnetGroup = ResourceType' "AWS::RDS::DBSubnetGroup"

pattern AWSRDSDBSecurityGroup :: ResourceType
pattern AWSRDSDBSecurityGroup = ResourceType' "AWS::RDS::DBSecurityGroup"

pattern AWSRDSDBSnapshot :: ResourceType
pattern AWSRDSDBSnapshot = ResourceType' "AWS::RDS::DBSnapshot"

pattern AWSRDSDBCluster :: ResourceType
pattern AWSRDSDBCluster = ResourceType' "AWS::RDS::DBCluster"

pattern AWSRDSDBClusterSnapshot :: ResourceType
pattern AWSRDSDBClusterSnapshot = ResourceType' "AWS::RDS::DBClusterSnapshot"

pattern AWSRDSEventSubscription :: ResourceType
pattern AWSRDSEventSubscription = ResourceType' "AWS::RDS::EventSubscription"

pattern AWSS3Bucket :: ResourceType
pattern AWSS3Bucket = ResourceType' "AWS::S3::Bucket"

pattern AWSS3AccountPublicAccessBlock :: ResourceType
pattern AWSS3AccountPublicAccessBlock = ResourceType' "AWS::S3::AccountPublicAccessBlock"

pattern AWSRedshiftCluster :: ResourceType
pattern AWSRedshiftCluster = ResourceType' "AWS::Redshift::Cluster"

pattern AWSRedshiftClusterSnapshot :: ResourceType
pattern AWSRedshiftClusterSnapshot = ResourceType' "AWS::Redshift::ClusterSnapshot"

pattern AWSRedshiftClusterParameterGroup :: ResourceType
pattern AWSRedshiftClusterParameterGroup = ResourceType' "AWS::Redshift::ClusterParameterGroup"

pattern AWSRedshiftClusterSecurityGroup :: ResourceType
pattern AWSRedshiftClusterSecurityGroup = ResourceType' "AWS::Redshift::ClusterSecurityGroup"

pattern AWSRedshiftClusterSubnetGroup :: ResourceType
pattern AWSRedshiftClusterSubnetGroup = ResourceType' "AWS::Redshift::ClusterSubnetGroup"

pattern AWSRedshiftEventSubscription :: ResourceType
pattern AWSRedshiftEventSubscription = ResourceType' "AWS::Redshift::EventSubscription"

pattern AWSSsmManagedInstanceInventory :: ResourceType
pattern AWSSsmManagedInstanceInventory = ResourceType' "AWS::SSM::ManagedInstanceInventory"

pattern AWSCloudWatchAlarm :: ResourceType
pattern AWSCloudWatchAlarm = ResourceType' "AWS::CloudWatch::Alarm"

pattern AWSCloudFormationStack :: ResourceType
pattern AWSCloudFormationStack = ResourceType' "AWS::CloudFormation::Stack"

pattern AWSElasticLoadBalancingLoadBalancer :: ResourceType
pattern AWSElasticLoadBalancingLoadBalancer = ResourceType' "AWS::ElasticLoadBalancing::LoadBalancer"

pattern AWSAutoScalingAutoScalingGroup :: ResourceType
pattern AWSAutoScalingAutoScalingGroup = ResourceType' "AWS::AutoScaling::AutoScalingGroup"

pattern AWSAutoScalingLaunchConfiguration :: ResourceType
pattern AWSAutoScalingLaunchConfiguration = ResourceType' "AWS::AutoScaling::LaunchConfiguration"

pattern AWSAutoScalingScalingPolicy :: ResourceType
pattern AWSAutoScalingScalingPolicy = ResourceType' "AWS::AutoScaling::ScalingPolicy"

pattern AWSAutoScalingScheduledAction :: ResourceType
pattern AWSAutoScalingScheduledAction = ResourceType' "AWS::AutoScaling::ScheduledAction"

pattern AWSDynamoDBTable :: ResourceType
pattern AWSDynamoDBTable = ResourceType' "AWS::DynamoDB::Table"

pattern AWSCodeBuildProject :: ResourceType
pattern AWSCodeBuildProject = ResourceType' "AWS::CodeBuild::Project"

pattern AWSWafRateBasedRule :: ResourceType
pattern AWSWafRateBasedRule = ResourceType' "AWS::WAF::RateBasedRule"

pattern AWSWafRule :: ResourceType
pattern AWSWafRule = ResourceType' "AWS::WAF::Rule"

pattern AWSWafRuleGroup :: ResourceType
pattern AWSWafRuleGroup = ResourceType' "AWS::WAF::RuleGroup"

pattern AWSWafWebACL :: ResourceType
pattern AWSWafWebACL = ResourceType' "AWS::WAF::WebACL"

pattern AWSWAFRegionalRateBasedRule :: ResourceType
pattern AWSWAFRegionalRateBasedRule = ResourceType' "AWS::WAFRegional::RateBasedRule"

pattern AWSWAFRegionalRule :: ResourceType
pattern AWSWAFRegionalRule = ResourceType' "AWS::WAFRegional::Rule"

pattern AWSWAFRegionalRuleGroup :: ResourceType
pattern AWSWAFRegionalRuleGroup = ResourceType' "AWS::WAFRegional::RuleGroup"

pattern AWSWAFRegionalWebACL :: ResourceType
pattern AWSWAFRegionalWebACL = ResourceType' "AWS::WAFRegional::WebACL"

pattern AWSCloudFrontDistribution :: ResourceType
pattern AWSCloudFrontDistribution = ResourceType' "AWS::CloudFront::Distribution"

pattern AWSCloudFrontStreamingDistribution :: ResourceType
pattern AWSCloudFrontStreamingDistribution = ResourceType' "AWS::CloudFront::StreamingDistribution"

pattern AWSLambdaFunction :: ResourceType
pattern AWSLambdaFunction = ResourceType' "AWS::Lambda::Function"

pattern AWSElasticBeanstalkApplication :: ResourceType
pattern AWSElasticBeanstalkApplication = ResourceType' "AWS::ElasticBeanstalk::Application"

pattern AWSElasticBeanstalkApplicationVersion :: ResourceType
pattern AWSElasticBeanstalkApplicationVersion = ResourceType' "AWS::ElasticBeanstalk::ApplicationVersion"

pattern AWSElasticBeanstalkEnvironment :: ResourceType
pattern AWSElasticBeanstalkEnvironment = ResourceType' "AWS::ElasticBeanstalk::Environment"

pattern AWSWAFV2WebACL :: ResourceType
pattern AWSWAFV2WebACL = ResourceType' "AWS::WAFv2::WebACL"

pattern AWSWAFV2RuleGroup :: ResourceType
pattern AWSWAFV2RuleGroup = ResourceType' "AWS::WAFv2::RuleGroup"

pattern AWSWAFV2IPSet :: ResourceType
pattern AWSWAFV2IPSet = ResourceType' "AWS::WAFv2::IPSet"

pattern AWSWAFV2RegexPatternSet :: ResourceType
pattern AWSWAFV2RegexPatternSet = ResourceType' "AWS::WAFv2::RegexPatternSet"

pattern AWSWAFV2ManagedRuleSet :: ResourceType
pattern AWSWAFV2ManagedRuleSet = ResourceType' "AWS::WAFv2::ManagedRuleSet"

pattern AWSXRayEncryptionConfig :: ResourceType
pattern AWSXRayEncryptionConfig = ResourceType' "AWS::XRay::EncryptionConfig"

pattern AWSSsmAssociationCompliance :: ResourceType
pattern AWSSsmAssociationCompliance = ResourceType' "AWS::SSM::AssociationCompliance"

pattern AWSSsmPatchCompliance :: ResourceType
pattern AWSSsmPatchCompliance = ResourceType' "AWS::SSM::PatchCompliance"

pattern AWSShieldProtection :: ResourceType
pattern AWSShieldProtection = ResourceType' "AWS::Shield::Protection"

pattern AWSShieldRegionalProtection :: ResourceType
pattern AWSShieldRegionalProtection = ResourceType' "AWS::ShieldRegional::Protection"

pattern AWSConfigResourceCompliance :: ResourceType
pattern AWSConfigResourceCompliance = ResourceType' "AWS::Config::ResourceCompliance"

pattern AWSAPIGatewayStage :: ResourceType
pattern AWSAPIGatewayStage = ResourceType' "AWS::ApiGateway::Stage"

pattern AWSAPIGatewayRestAPI :: ResourceType
pattern AWSAPIGatewayRestAPI = ResourceType' "AWS::ApiGateway::RestApi"

pattern AWSAPIGATEWAYV2Stage :: ResourceType
pattern AWSAPIGATEWAYV2Stage = ResourceType' "AWS::ApiGatewayV2::Stage"

pattern AWSAPIGATEWAYV2API :: ResourceType
pattern AWSAPIGATEWAYV2API = ResourceType' "AWS::ApiGatewayV2::Api"

pattern AWSCodePipelinePipeline :: ResourceType
pattern AWSCodePipelinePipeline = ResourceType' "AWS::CodePipeline::Pipeline"

pattern AWSServiceCatalogCloudFormationProvisionedProduct :: ResourceType
pattern AWSServiceCatalogCloudFormationProvisionedProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProvisionedProduct"

pattern AWSServiceCatalogCloudFormationProduct :: ResourceType
pattern AWSServiceCatalogCloudFormationProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProduct"

pattern AWSServiceCatalogPortfolio :: ResourceType
pattern AWSServiceCatalogPortfolio = ResourceType' "AWS::ServiceCatalog::Portfolio"

pattern AWSSqsQueue :: ResourceType
pattern AWSSqsQueue = ResourceType' "AWS::SQS::Queue"

pattern AWSKMSKey :: ResourceType
pattern AWSKMSKey = ResourceType' "AWS::KMS::Key"

pattern AWSQldbLedger :: ResourceType
pattern AWSQldbLedger = ResourceType' "AWS::QLDB::Ledger"

pattern AWSSecretsManagerSecret :: ResourceType
pattern AWSSecretsManagerSecret = ResourceType' "AWS::SecretsManager::Secret"

pattern AWSSNSTopic :: ResourceType
pattern AWSSNSTopic = ResourceType' "AWS::SNS::Topic"

pattern AWSSsmFileData :: ResourceType
pattern AWSSsmFileData = ResourceType' "AWS::SSM::FileData"

{-# COMPLETE
  AWSEC2CustomerGateway,
  AWSEC2EIP,
  AWSEC2Host,
  AWSEC2Instance,
  AWSEC2InternetGateway,
  AWSEC2NetworkACL,
  AWSEC2NetworkInterface,
  AWSEC2RouteTable,
  AWSEC2SecurityGroup,
  AWSEC2Subnet,
  AWSCloudTrailTrail,
  AWSEC2Volume,
  AWSEC2VPC,
  AWSEC2VPNConnection,
  AWSEC2VPNGateway,
  AWSEC2RegisteredHAInstance,
  AWSEC2NatGateway,
  AWSEC2EgressOnlyInternetGateway,
  AWSEC2VPCEndpoint,
  AWSEC2VPCEndpointService,
  AWSEC2FlowLog,
  AWSEC2VPCPeeringConnection,
  AWSElasticsearchDomain,
  AWSIAMGroup,
  AWSIAMPolicy,
  AWSIAMRole,
  AWSIAMUser,
  AWSELASTICLOADBALANCINGV2LoadBalancer,
  AWSAcmCertificate,
  AWSRDSDBInstance,
  AWSRDSDBSubnetGroup,
  AWSRDSDBSecurityGroup,
  AWSRDSDBSnapshot,
  AWSRDSDBCluster,
  AWSRDSDBClusterSnapshot,
  AWSRDSEventSubscription,
  AWSS3Bucket,
  AWSS3AccountPublicAccessBlock,
  AWSRedshiftCluster,
  AWSRedshiftClusterSnapshot,
  AWSRedshiftClusterParameterGroup,
  AWSRedshiftClusterSecurityGroup,
  AWSRedshiftClusterSubnetGroup,
  AWSRedshiftEventSubscription,
  AWSSsmManagedInstanceInventory,
  AWSCloudWatchAlarm,
  AWSCloudFormationStack,
  AWSElasticLoadBalancingLoadBalancer,
  AWSAutoScalingAutoScalingGroup,
  AWSAutoScalingLaunchConfiguration,
  AWSAutoScalingScalingPolicy,
  AWSAutoScalingScheduledAction,
  AWSDynamoDBTable,
  AWSCodeBuildProject,
  AWSWafRateBasedRule,
  AWSWafRule,
  AWSWafRuleGroup,
  AWSWafWebACL,
  AWSWAFRegionalRateBasedRule,
  AWSWAFRegionalRule,
  AWSWAFRegionalRuleGroup,
  AWSWAFRegionalWebACL,
  AWSCloudFrontDistribution,
  AWSCloudFrontStreamingDistribution,
  AWSLambdaFunction,
  AWSElasticBeanstalkApplication,
  AWSElasticBeanstalkApplicationVersion,
  AWSElasticBeanstalkEnvironment,
  AWSWAFV2WebACL,
  AWSWAFV2RuleGroup,
  AWSWAFV2IPSet,
  AWSWAFV2RegexPatternSet,
  AWSWAFV2ManagedRuleSet,
  AWSXRayEncryptionConfig,
  AWSSsmAssociationCompliance,
  AWSSsmPatchCompliance,
  AWSShieldProtection,
  AWSShieldRegionalProtection,
  AWSConfigResourceCompliance,
  AWSAPIGatewayStage,
  AWSAPIGatewayRestAPI,
  AWSAPIGATEWAYV2Stage,
  AWSAPIGATEWAYV2API,
  AWSCodePipelinePipeline,
  AWSServiceCatalogCloudFormationProvisionedProduct,
  AWSServiceCatalogCloudFormationProduct,
  AWSServiceCatalogPortfolio,
  AWSSqsQueue,
  AWSKMSKey,
  AWSQldbLedger,
  AWSSecretsManagerSecret,
  AWSSNSTopic,
  AWSSsmFileData,
  ResourceType'
  #-}
