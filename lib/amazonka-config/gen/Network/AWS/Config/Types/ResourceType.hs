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
        AWSAPIGATEWAYV2API,
        AWSAPIGATEWAYV2Stage,
        AWSAPIGatewayRestAPI,
        AWSAPIGatewayStage,
        AWSAcmCertificate,
        AWSAutoScalingAutoScalingGroup,
        AWSAutoScalingLaunchConfiguration,
        AWSAutoScalingScalingPolicy,
        AWSAutoScalingScheduledAction,
        AWSCloudFormationStack,
        AWSCloudFrontDistribution,
        AWSCloudFrontStreamingDistribution,
        AWSCloudTrailTrail,
        AWSCloudWatchAlarm,
        AWSCodeBuildProject,
        AWSCodePipelinePipeline,
        AWSConfigResourceCompliance,
        AWSDynamoDBTable,
        AWSEC2CustomerGateway,
        AWSEC2EIP,
        AWSEC2EgressOnlyInternetGateway,
        AWSEC2FlowLog,
        AWSEC2Host,
        AWSEC2Instance,
        AWSEC2InternetGateway,
        AWSEC2NatGateway,
        AWSEC2NetworkACL,
        AWSEC2NetworkInterface,
        AWSEC2RegisteredHAInstance,
        AWSEC2RouteTable,
        AWSEC2SecurityGroup,
        AWSEC2Subnet,
        AWSEC2VPC,
        AWSEC2VPCEndpoint,
        AWSEC2VPCEndpointService,
        AWSEC2VPCPeeringConnection,
        AWSEC2VPNConnection,
        AWSEC2VPNGateway,
        AWSEC2Volume,
        AWSELASTICLOADBALANCINGV2LoadBalancer,
        AWSElasticBeanstalkApplication,
        AWSElasticBeanstalkApplicationVersion,
        AWSElasticBeanstalkEnvironment,
        AWSElasticLoadBalancingLoadBalancer,
        AWSElasticsearchDomain,
        AWSIAMGroup,
        AWSIAMPolicy,
        AWSIAMRole,
        AWSIAMUser,
        AWSKMSKey,
        AWSLambdaFunction,
        AWSQldbLedger,
        AWSRDSDBCluster,
        AWSRDSDBClusterSnapshot,
        AWSRDSDBInstance,
        AWSRDSDBSecurityGroup,
        AWSRDSDBSnapshot,
        AWSRDSDBSubnetGroup,
        AWSRDSEventSubscription,
        AWSRedshiftCluster,
        AWSRedshiftClusterParameterGroup,
        AWSRedshiftClusterSecurityGroup,
        AWSRedshiftClusterSnapshot,
        AWSRedshiftClusterSubnetGroup,
        AWSRedshiftEventSubscription,
        AWSS3AccountPublicAccessBlock,
        AWSS3Bucket,
        AWSSNSTopic,
        AWSSecretsManagerSecret,
        AWSServiceCatalogCloudFormationProduct,
        AWSServiceCatalogCloudFormationProvisionedProduct,
        AWSServiceCatalogPortfolio,
        AWSShieldProtection,
        AWSShieldRegionalProtection,
        AWSSqsQueue,
        AWSSsmAssociationCompliance,
        AWSSsmFileData,
        AWSSsmManagedInstanceInventory,
        AWSSsmPatchCompliance,
        AWSWAFRegionalRateBasedRule,
        AWSWAFRegionalRule,
        AWSWAFRegionalRuleGroup,
        AWSWAFRegionalWebACL,
        AWSWAFV2IPSet,
        AWSWAFV2ManagedRuleSet,
        AWSWAFV2RegexPatternSet,
        AWSWAFV2RuleGroup,
        AWSWAFV2WebACL,
        AWSWafRateBasedRule,
        AWSWafRule,
        AWSWafRuleGroup,
        AWSWafWebACL,
        AWSXRayEncryptionConfig
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

pattern AWSAPIGATEWAYV2API :: ResourceType
pattern AWSAPIGATEWAYV2API = ResourceType' "AWS::ApiGatewayV2::Api"

pattern AWSAPIGATEWAYV2Stage :: ResourceType
pattern AWSAPIGATEWAYV2Stage = ResourceType' "AWS::ApiGatewayV2::Stage"

pattern AWSAPIGatewayRestAPI :: ResourceType
pattern AWSAPIGatewayRestAPI = ResourceType' "AWS::ApiGateway::RestApi"

pattern AWSAPIGatewayStage :: ResourceType
pattern AWSAPIGatewayStage = ResourceType' "AWS::ApiGateway::Stage"

pattern AWSAcmCertificate :: ResourceType
pattern AWSAcmCertificate = ResourceType' "AWS::ACM::Certificate"

pattern AWSAutoScalingAutoScalingGroup :: ResourceType
pattern AWSAutoScalingAutoScalingGroup = ResourceType' "AWS::AutoScaling::AutoScalingGroup"

pattern AWSAutoScalingLaunchConfiguration :: ResourceType
pattern AWSAutoScalingLaunchConfiguration = ResourceType' "AWS::AutoScaling::LaunchConfiguration"

pattern AWSAutoScalingScalingPolicy :: ResourceType
pattern AWSAutoScalingScalingPolicy = ResourceType' "AWS::AutoScaling::ScalingPolicy"

pattern AWSAutoScalingScheduledAction :: ResourceType
pattern AWSAutoScalingScheduledAction = ResourceType' "AWS::AutoScaling::ScheduledAction"

pattern AWSCloudFormationStack :: ResourceType
pattern AWSCloudFormationStack = ResourceType' "AWS::CloudFormation::Stack"

pattern AWSCloudFrontDistribution :: ResourceType
pattern AWSCloudFrontDistribution = ResourceType' "AWS::CloudFront::Distribution"

pattern AWSCloudFrontStreamingDistribution :: ResourceType
pattern AWSCloudFrontStreamingDistribution = ResourceType' "AWS::CloudFront::StreamingDistribution"

pattern AWSCloudTrailTrail :: ResourceType
pattern AWSCloudTrailTrail = ResourceType' "AWS::CloudTrail::Trail"

pattern AWSCloudWatchAlarm :: ResourceType
pattern AWSCloudWatchAlarm = ResourceType' "AWS::CloudWatch::Alarm"

pattern AWSCodeBuildProject :: ResourceType
pattern AWSCodeBuildProject = ResourceType' "AWS::CodeBuild::Project"

pattern AWSCodePipelinePipeline :: ResourceType
pattern AWSCodePipelinePipeline = ResourceType' "AWS::CodePipeline::Pipeline"

pattern AWSConfigResourceCompliance :: ResourceType
pattern AWSConfigResourceCompliance = ResourceType' "AWS::Config::ResourceCompliance"

pattern AWSDynamoDBTable :: ResourceType
pattern AWSDynamoDBTable = ResourceType' "AWS::DynamoDB::Table"

pattern AWSEC2CustomerGateway :: ResourceType
pattern AWSEC2CustomerGateway = ResourceType' "AWS::EC2::CustomerGateway"

pattern AWSEC2EIP :: ResourceType
pattern AWSEC2EIP = ResourceType' "AWS::EC2::EIP"

pattern AWSEC2EgressOnlyInternetGateway :: ResourceType
pattern AWSEC2EgressOnlyInternetGateway = ResourceType' "AWS::EC2::EgressOnlyInternetGateway"

pattern AWSEC2FlowLog :: ResourceType
pattern AWSEC2FlowLog = ResourceType' "AWS::EC2::FlowLog"

pattern AWSEC2Host :: ResourceType
pattern AWSEC2Host = ResourceType' "AWS::EC2::Host"

pattern AWSEC2Instance :: ResourceType
pattern AWSEC2Instance = ResourceType' "AWS::EC2::Instance"

pattern AWSEC2InternetGateway :: ResourceType
pattern AWSEC2InternetGateway = ResourceType' "AWS::EC2::InternetGateway"

pattern AWSEC2NatGateway :: ResourceType
pattern AWSEC2NatGateway = ResourceType' "AWS::EC2::NatGateway"

pattern AWSEC2NetworkACL :: ResourceType
pattern AWSEC2NetworkACL = ResourceType' "AWS::EC2::NetworkAcl"

pattern AWSEC2NetworkInterface :: ResourceType
pattern AWSEC2NetworkInterface = ResourceType' "AWS::EC2::NetworkInterface"

pattern AWSEC2RegisteredHAInstance :: ResourceType
pattern AWSEC2RegisteredHAInstance = ResourceType' "AWS::EC2::RegisteredHAInstance"

pattern AWSEC2RouteTable :: ResourceType
pattern AWSEC2RouteTable = ResourceType' "AWS::EC2::RouteTable"

pattern AWSEC2SecurityGroup :: ResourceType
pattern AWSEC2SecurityGroup = ResourceType' "AWS::EC2::SecurityGroup"

pattern AWSEC2Subnet :: ResourceType
pattern AWSEC2Subnet = ResourceType' "AWS::EC2::Subnet"

pattern AWSEC2VPC :: ResourceType
pattern AWSEC2VPC = ResourceType' "AWS::EC2::VPC"

pattern AWSEC2VPCEndpoint :: ResourceType
pattern AWSEC2VPCEndpoint = ResourceType' "AWS::EC2::VPCEndpoint"

pattern AWSEC2VPCEndpointService :: ResourceType
pattern AWSEC2VPCEndpointService = ResourceType' "AWS::EC2::VPCEndpointService"

pattern AWSEC2VPCPeeringConnection :: ResourceType
pattern AWSEC2VPCPeeringConnection = ResourceType' "AWS::EC2::VPCPeeringConnection"

pattern AWSEC2VPNConnection :: ResourceType
pattern AWSEC2VPNConnection = ResourceType' "AWS::EC2::VPNConnection"

pattern AWSEC2VPNGateway :: ResourceType
pattern AWSEC2VPNGateway = ResourceType' "AWS::EC2::VPNGateway"

pattern AWSEC2Volume :: ResourceType
pattern AWSEC2Volume = ResourceType' "AWS::EC2::Volume"

pattern AWSELASTICLOADBALANCINGV2LoadBalancer :: ResourceType
pattern AWSELASTICLOADBALANCINGV2LoadBalancer = ResourceType' "AWS::ElasticLoadBalancingV2::LoadBalancer"

pattern AWSElasticBeanstalkApplication :: ResourceType
pattern AWSElasticBeanstalkApplication = ResourceType' "AWS::ElasticBeanstalk::Application"

pattern AWSElasticBeanstalkApplicationVersion :: ResourceType
pattern AWSElasticBeanstalkApplicationVersion = ResourceType' "AWS::ElasticBeanstalk::ApplicationVersion"

pattern AWSElasticBeanstalkEnvironment :: ResourceType
pattern AWSElasticBeanstalkEnvironment = ResourceType' "AWS::ElasticBeanstalk::Environment"

pattern AWSElasticLoadBalancingLoadBalancer :: ResourceType
pattern AWSElasticLoadBalancingLoadBalancer = ResourceType' "AWS::ElasticLoadBalancing::LoadBalancer"

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

pattern AWSKMSKey :: ResourceType
pattern AWSKMSKey = ResourceType' "AWS::KMS::Key"

pattern AWSLambdaFunction :: ResourceType
pattern AWSLambdaFunction = ResourceType' "AWS::Lambda::Function"

pattern AWSQldbLedger :: ResourceType
pattern AWSQldbLedger = ResourceType' "AWS::QLDB::Ledger"

pattern AWSRDSDBCluster :: ResourceType
pattern AWSRDSDBCluster = ResourceType' "AWS::RDS::DBCluster"

pattern AWSRDSDBClusterSnapshot :: ResourceType
pattern AWSRDSDBClusterSnapshot = ResourceType' "AWS::RDS::DBClusterSnapshot"

pattern AWSRDSDBInstance :: ResourceType
pattern AWSRDSDBInstance = ResourceType' "AWS::RDS::DBInstance"

pattern AWSRDSDBSecurityGroup :: ResourceType
pattern AWSRDSDBSecurityGroup = ResourceType' "AWS::RDS::DBSecurityGroup"

pattern AWSRDSDBSnapshot :: ResourceType
pattern AWSRDSDBSnapshot = ResourceType' "AWS::RDS::DBSnapshot"

pattern AWSRDSDBSubnetGroup :: ResourceType
pattern AWSRDSDBSubnetGroup = ResourceType' "AWS::RDS::DBSubnetGroup"

pattern AWSRDSEventSubscription :: ResourceType
pattern AWSRDSEventSubscription = ResourceType' "AWS::RDS::EventSubscription"

pattern AWSRedshiftCluster :: ResourceType
pattern AWSRedshiftCluster = ResourceType' "AWS::Redshift::Cluster"

pattern AWSRedshiftClusterParameterGroup :: ResourceType
pattern AWSRedshiftClusterParameterGroup = ResourceType' "AWS::Redshift::ClusterParameterGroup"

pattern AWSRedshiftClusterSecurityGroup :: ResourceType
pattern AWSRedshiftClusterSecurityGroup = ResourceType' "AWS::Redshift::ClusterSecurityGroup"

pattern AWSRedshiftClusterSnapshot :: ResourceType
pattern AWSRedshiftClusterSnapshot = ResourceType' "AWS::Redshift::ClusterSnapshot"

pattern AWSRedshiftClusterSubnetGroup :: ResourceType
pattern AWSRedshiftClusterSubnetGroup = ResourceType' "AWS::Redshift::ClusterSubnetGroup"

pattern AWSRedshiftEventSubscription :: ResourceType
pattern AWSRedshiftEventSubscription = ResourceType' "AWS::Redshift::EventSubscription"

pattern AWSS3AccountPublicAccessBlock :: ResourceType
pattern AWSS3AccountPublicAccessBlock = ResourceType' "AWS::S3::AccountPublicAccessBlock"

pattern AWSS3Bucket :: ResourceType
pattern AWSS3Bucket = ResourceType' "AWS::S3::Bucket"

pattern AWSSNSTopic :: ResourceType
pattern AWSSNSTopic = ResourceType' "AWS::SNS::Topic"

pattern AWSSecretsManagerSecret :: ResourceType
pattern AWSSecretsManagerSecret = ResourceType' "AWS::SecretsManager::Secret"

pattern AWSServiceCatalogCloudFormationProduct :: ResourceType
pattern AWSServiceCatalogCloudFormationProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProduct"

pattern AWSServiceCatalogCloudFormationProvisionedProduct :: ResourceType
pattern AWSServiceCatalogCloudFormationProvisionedProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProvisionedProduct"

pattern AWSServiceCatalogPortfolio :: ResourceType
pattern AWSServiceCatalogPortfolio = ResourceType' "AWS::ServiceCatalog::Portfolio"

pattern AWSShieldProtection :: ResourceType
pattern AWSShieldProtection = ResourceType' "AWS::Shield::Protection"

pattern AWSShieldRegionalProtection :: ResourceType
pattern AWSShieldRegionalProtection = ResourceType' "AWS::ShieldRegional::Protection"

pattern AWSSqsQueue :: ResourceType
pattern AWSSqsQueue = ResourceType' "AWS::SQS::Queue"

pattern AWSSsmAssociationCompliance :: ResourceType
pattern AWSSsmAssociationCompliance = ResourceType' "AWS::SSM::AssociationCompliance"

pattern AWSSsmFileData :: ResourceType
pattern AWSSsmFileData = ResourceType' "AWS::SSM::FileData"

pattern AWSSsmManagedInstanceInventory :: ResourceType
pattern AWSSsmManagedInstanceInventory = ResourceType' "AWS::SSM::ManagedInstanceInventory"

pattern AWSSsmPatchCompliance :: ResourceType
pattern AWSSsmPatchCompliance = ResourceType' "AWS::SSM::PatchCompliance"

pattern AWSWAFRegionalRateBasedRule :: ResourceType
pattern AWSWAFRegionalRateBasedRule = ResourceType' "AWS::WAFRegional::RateBasedRule"

pattern AWSWAFRegionalRule :: ResourceType
pattern AWSWAFRegionalRule = ResourceType' "AWS::WAFRegional::Rule"

pattern AWSWAFRegionalRuleGroup :: ResourceType
pattern AWSWAFRegionalRuleGroup = ResourceType' "AWS::WAFRegional::RuleGroup"

pattern AWSWAFRegionalWebACL :: ResourceType
pattern AWSWAFRegionalWebACL = ResourceType' "AWS::WAFRegional::WebACL"

pattern AWSWAFV2IPSet :: ResourceType
pattern AWSWAFV2IPSet = ResourceType' "AWS::WAFv2::IPSet"

pattern AWSWAFV2ManagedRuleSet :: ResourceType
pattern AWSWAFV2ManagedRuleSet = ResourceType' "AWS::WAFv2::ManagedRuleSet"

pattern AWSWAFV2RegexPatternSet :: ResourceType
pattern AWSWAFV2RegexPatternSet = ResourceType' "AWS::WAFv2::RegexPatternSet"

pattern AWSWAFV2RuleGroup :: ResourceType
pattern AWSWAFV2RuleGroup = ResourceType' "AWS::WAFv2::RuleGroup"

pattern AWSWAFV2WebACL :: ResourceType
pattern AWSWAFV2WebACL = ResourceType' "AWS::WAFv2::WebACL"

pattern AWSWafRateBasedRule :: ResourceType
pattern AWSWafRateBasedRule = ResourceType' "AWS::WAF::RateBasedRule"

pattern AWSWafRule :: ResourceType
pattern AWSWafRule = ResourceType' "AWS::WAF::Rule"

pattern AWSWafRuleGroup :: ResourceType
pattern AWSWafRuleGroup = ResourceType' "AWS::WAF::RuleGroup"

pattern AWSWafWebACL :: ResourceType
pattern AWSWafWebACL = ResourceType' "AWS::WAF::WebACL"

pattern AWSXRayEncryptionConfig :: ResourceType
pattern AWSXRayEncryptionConfig = ResourceType' "AWS::XRay::EncryptionConfig"

{-# COMPLETE
  AWSAPIGATEWAYV2API,
  AWSAPIGATEWAYV2Stage,
  AWSAPIGatewayRestAPI,
  AWSAPIGatewayStage,
  AWSAcmCertificate,
  AWSAutoScalingAutoScalingGroup,
  AWSAutoScalingLaunchConfiguration,
  AWSAutoScalingScalingPolicy,
  AWSAutoScalingScheduledAction,
  AWSCloudFormationStack,
  AWSCloudFrontDistribution,
  AWSCloudFrontStreamingDistribution,
  AWSCloudTrailTrail,
  AWSCloudWatchAlarm,
  AWSCodeBuildProject,
  AWSCodePipelinePipeline,
  AWSConfigResourceCompliance,
  AWSDynamoDBTable,
  AWSEC2CustomerGateway,
  AWSEC2EIP,
  AWSEC2EgressOnlyInternetGateway,
  AWSEC2FlowLog,
  AWSEC2Host,
  AWSEC2Instance,
  AWSEC2InternetGateway,
  AWSEC2NatGateway,
  AWSEC2NetworkACL,
  AWSEC2NetworkInterface,
  AWSEC2RegisteredHAInstance,
  AWSEC2RouteTable,
  AWSEC2SecurityGroup,
  AWSEC2Subnet,
  AWSEC2VPC,
  AWSEC2VPCEndpoint,
  AWSEC2VPCEndpointService,
  AWSEC2VPCPeeringConnection,
  AWSEC2VPNConnection,
  AWSEC2VPNGateway,
  AWSEC2Volume,
  AWSELASTICLOADBALANCINGV2LoadBalancer,
  AWSElasticBeanstalkApplication,
  AWSElasticBeanstalkApplicationVersion,
  AWSElasticBeanstalkEnvironment,
  AWSElasticLoadBalancingLoadBalancer,
  AWSElasticsearchDomain,
  AWSIAMGroup,
  AWSIAMPolicy,
  AWSIAMRole,
  AWSIAMUser,
  AWSKMSKey,
  AWSLambdaFunction,
  AWSQldbLedger,
  AWSRDSDBCluster,
  AWSRDSDBClusterSnapshot,
  AWSRDSDBInstance,
  AWSRDSDBSecurityGroup,
  AWSRDSDBSnapshot,
  AWSRDSDBSubnetGroup,
  AWSRDSEventSubscription,
  AWSRedshiftCluster,
  AWSRedshiftClusterParameterGroup,
  AWSRedshiftClusterSecurityGroup,
  AWSRedshiftClusterSnapshot,
  AWSRedshiftClusterSubnetGroup,
  AWSRedshiftEventSubscription,
  AWSS3AccountPublicAccessBlock,
  AWSS3Bucket,
  AWSSNSTopic,
  AWSSecretsManagerSecret,
  AWSServiceCatalogCloudFormationProduct,
  AWSServiceCatalogCloudFormationProvisionedProduct,
  AWSServiceCatalogPortfolio,
  AWSShieldProtection,
  AWSShieldRegionalProtection,
  AWSSqsQueue,
  AWSSsmAssociationCompliance,
  AWSSsmFileData,
  AWSSsmManagedInstanceInventory,
  AWSSsmPatchCompliance,
  AWSWAFRegionalRateBasedRule,
  AWSWAFRegionalRule,
  AWSWAFRegionalRuleGroup,
  AWSWAFRegionalWebACL,
  AWSWAFV2IPSet,
  AWSWAFV2ManagedRuleSet,
  AWSWAFV2RegexPatternSet,
  AWSWAFV2RuleGroup,
  AWSWAFV2WebACL,
  AWSWafRateBasedRule,
  AWSWafRule,
  AWSWafRuleGroup,
  AWSWafWebACL,
  AWSXRayEncryptionConfig,
  ResourceType'
  #-}
