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
        ResourceTypeAwsEC2CustomerGateway,
        ResourceTypeAwsEC2Eip,
        ResourceTypeAwsEC2Host,
        ResourceTypeAwsEC2Instance,
        ResourceTypeAwsEC2InternetGateway,
        ResourceTypeAwsEC2NetworkAcl,
        ResourceTypeAwsEC2NetworkInterface,
        ResourceTypeAwsEC2RouteTable,
        ResourceTypeAwsEC2SecurityGroup,
        ResourceTypeAwsEC2Subnet,
        ResourceTypeAwsCloudTrailTrail,
        ResourceTypeAwsEC2Volume,
        ResourceTypeAwsEC2Vpc,
        ResourceTypeAwsEC2VPNConnection,
        ResourceTypeAwsEC2VPNGateway,
        ResourceTypeAwsEC2RegisteredHAInstance,
        ResourceTypeAwsEC2NatGateway,
        ResourceTypeAwsEC2EgressOnlyInternetGateway,
        ResourceTypeAwsEC2VPCEndpoint,
        ResourceTypeAwsEC2VPCEndpointService,
        ResourceTypeAwsEC2FlowLog,
        ResourceTypeAwsEC2VPCPeeringConnection,
        ResourceTypeAwsElasticsearchDomain,
        ResourceTypeAwsIamGroup,
        ResourceTypeAwsIamPolicy,
        ResourceTypeAwsIamRole,
        ResourceTypeAwsIamUser,
        ResourceTypeAwsELASTICLOADBALANCINGV2LoadBalancer,
        ResourceTypeAwsAcmCertificate,
        ResourceTypeAwsRdsDBInstance,
        ResourceTypeAwsRdsDBSubnetGroup,
        ResourceTypeAwsRdsDBSecurityGroup,
        ResourceTypeAwsRdsDBSnapshot,
        ResourceTypeAwsRdsDBCluster,
        ResourceTypeAwsRdsDBClusterSnapshot,
        ResourceTypeAwsRdsEventSubscription,
        ResourceTypeAwsS3Bucket,
        ResourceTypeAwsS3AccountPublicAccessBlock,
        ResourceTypeAwsRedshiftCluster,
        ResourceTypeAwsRedshiftClusterSnapshot,
        ResourceTypeAwsRedshiftClusterParameterGroup,
        ResourceTypeAwsRedshiftClusterSecurityGroup,
        ResourceTypeAwsRedshiftClusterSubnetGroup,
        ResourceTypeAwsRedshiftEventSubscription,
        ResourceTypeAwsSsmManagedInstanceInventory,
        ResourceTypeAwsCloudWatchAlarm,
        ResourceTypeAwsCloudFormationStack,
        ResourceTypeAwsElasticLoadBalancingLoadBalancer,
        ResourceTypeAwsAutoScalingAutoScalingGroup,
        ResourceTypeAwsAutoScalingLaunchConfiguration,
        ResourceTypeAwsAutoScalingScalingPolicy,
        ResourceTypeAwsAutoScalingScheduledAction,
        ResourceTypeAwsDynamoDBTable,
        ResourceTypeAwsCodeBuildProject,
        ResourceTypeAwsWafRateBasedRule,
        ResourceTypeAwsWafRule,
        ResourceTypeAwsWafRuleGroup,
        ResourceTypeAwsWafWebACL,
        ResourceTypeAwsWAFRegionalRateBasedRule,
        ResourceTypeAwsWAFRegionalRule,
        ResourceTypeAwsWAFRegionalRuleGroup,
        ResourceTypeAwsWAFRegionalWebACL,
        ResourceTypeAwsCloudFrontDistribution,
        ResourceTypeAwsCloudFrontStreamingDistribution,
        ResourceTypeAwsLambdaFunction,
        ResourceTypeAwsElasticBeanstalkApplication,
        ResourceTypeAwsElasticBeanstalkApplicationVersion,
        ResourceTypeAwsElasticBeanstalkEnvironment,
        ResourceTypeAwsWAFV2WebACL,
        ResourceTypeAwsWAFV2RuleGroup,
        ResourceTypeAwsWAFV2IPSet,
        ResourceTypeAwsWAFV2RegexPatternSet,
        ResourceTypeAwsWAFV2ManagedRuleSet,
        ResourceTypeAwsXRayEncryptionConfig,
        ResourceTypeAwsSsmAssociationCompliance,
        ResourceTypeAwsSsmPatchCompliance,
        ResourceTypeAwsShieldProtection,
        ResourceTypeAwsShieldRegionalProtection,
        ResourceTypeAwsConfigResourceCompliance,
        ResourceTypeAwsApiGatewayStage,
        ResourceTypeAwsApiGatewayRestApi,
        ResourceTypeAwsAPIGATEWAYV2Stage,
        ResourceTypeAwsAPIGATEWAYV2Api,
        ResourceTypeAwsCodePipelinePipeline,
        ResourceTypeAwsServiceCatalogCloudFormationProvisionedProduct,
        ResourceTypeAwsServiceCatalogCloudFormationProduct,
        ResourceTypeAwsServiceCatalogPortfolio,
        ResourceTypeAwsSqsQueue,
        ResourceTypeAwsKmsKey,
        ResourceTypeAwsQldbLedger,
        ResourceTypeAwsSecretsManagerSecret,
        ResourceTypeAwsSnsTopic,
        ResourceTypeAwsSsmFileData,
        fromResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ResourceType = ResourceType' {fromResourceType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ResourceTypeAwsEC2CustomerGateway :: ResourceType
pattern ResourceTypeAwsEC2CustomerGateway = ResourceType' "AWS::EC2::CustomerGateway"

pattern ResourceTypeAwsEC2Eip :: ResourceType
pattern ResourceTypeAwsEC2Eip = ResourceType' "AWS::EC2::EIP"

pattern ResourceTypeAwsEC2Host :: ResourceType
pattern ResourceTypeAwsEC2Host = ResourceType' "AWS::EC2::Host"

pattern ResourceTypeAwsEC2Instance :: ResourceType
pattern ResourceTypeAwsEC2Instance = ResourceType' "AWS::EC2::Instance"

pattern ResourceTypeAwsEC2InternetGateway :: ResourceType
pattern ResourceTypeAwsEC2InternetGateway = ResourceType' "AWS::EC2::InternetGateway"

pattern ResourceTypeAwsEC2NetworkAcl :: ResourceType
pattern ResourceTypeAwsEC2NetworkAcl = ResourceType' "AWS::EC2::NetworkAcl"

pattern ResourceTypeAwsEC2NetworkInterface :: ResourceType
pattern ResourceTypeAwsEC2NetworkInterface = ResourceType' "AWS::EC2::NetworkInterface"

pattern ResourceTypeAwsEC2RouteTable :: ResourceType
pattern ResourceTypeAwsEC2RouteTable = ResourceType' "AWS::EC2::RouteTable"

pattern ResourceTypeAwsEC2SecurityGroup :: ResourceType
pattern ResourceTypeAwsEC2SecurityGroup = ResourceType' "AWS::EC2::SecurityGroup"

pattern ResourceTypeAwsEC2Subnet :: ResourceType
pattern ResourceTypeAwsEC2Subnet = ResourceType' "AWS::EC2::Subnet"

pattern ResourceTypeAwsCloudTrailTrail :: ResourceType
pattern ResourceTypeAwsCloudTrailTrail = ResourceType' "AWS::CloudTrail::Trail"

pattern ResourceTypeAwsEC2Volume :: ResourceType
pattern ResourceTypeAwsEC2Volume = ResourceType' "AWS::EC2::Volume"

pattern ResourceTypeAwsEC2Vpc :: ResourceType
pattern ResourceTypeAwsEC2Vpc = ResourceType' "AWS::EC2::VPC"

pattern ResourceTypeAwsEC2VPNConnection :: ResourceType
pattern ResourceTypeAwsEC2VPNConnection = ResourceType' "AWS::EC2::VPNConnection"

pattern ResourceTypeAwsEC2VPNGateway :: ResourceType
pattern ResourceTypeAwsEC2VPNGateway = ResourceType' "AWS::EC2::VPNGateway"

pattern ResourceTypeAwsEC2RegisteredHAInstance :: ResourceType
pattern ResourceTypeAwsEC2RegisteredHAInstance = ResourceType' "AWS::EC2::RegisteredHAInstance"

pattern ResourceTypeAwsEC2NatGateway :: ResourceType
pattern ResourceTypeAwsEC2NatGateway = ResourceType' "AWS::EC2::NatGateway"

pattern ResourceTypeAwsEC2EgressOnlyInternetGateway :: ResourceType
pattern ResourceTypeAwsEC2EgressOnlyInternetGateway = ResourceType' "AWS::EC2::EgressOnlyInternetGateway"

pattern ResourceTypeAwsEC2VPCEndpoint :: ResourceType
pattern ResourceTypeAwsEC2VPCEndpoint = ResourceType' "AWS::EC2::VPCEndpoint"

pattern ResourceTypeAwsEC2VPCEndpointService :: ResourceType
pattern ResourceTypeAwsEC2VPCEndpointService = ResourceType' "AWS::EC2::VPCEndpointService"

pattern ResourceTypeAwsEC2FlowLog :: ResourceType
pattern ResourceTypeAwsEC2FlowLog = ResourceType' "AWS::EC2::FlowLog"

pattern ResourceTypeAwsEC2VPCPeeringConnection :: ResourceType
pattern ResourceTypeAwsEC2VPCPeeringConnection = ResourceType' "AWS::EC2::VPCPeeringConnection"

pattern ResourceTypeAwsElasticsearchDomain :: ResourceType
pattern ResourceTypeAwsElasticsearchDomain = ResourceType' "AWS::Elasticsearch::Domain"

pattern ResourceTypeAwsIamGroup :: ResourceType
pattern ResourceTypeAwsIamGroup = ResourceType' "AWS::IAM::Group"

pattern ResourceTypeAwsIamPolicy :: ResourceType
pattern ResourceTypeAwsIamPolicy = ResourceType' "AWS::IAM::Policy"

pattern ResourceTypeAwsIamRole :: ResourceType
pattern ResourceTypeAwsIamRole = ResourceType' "AWS::IAM::Role"

pattern ResourceTypeAwsIamUser :: ResourceType
pattern ResourceTypeAwsIamUser = ResourceType' "AWS::IAM::User"

pattern ResourceTypeAwsELASTICLOADBALANCINGV2LoadBalancer :: ResourceType
pattern ResourceTypeAwsELASTICLOADBALANCINGV2LoadBalancer = ResourceType' "AWS::ElasticLoadBalancingV2::LoadBalancer"

pattern ResourceTypeAwsAcmCertificate :: ResourceType
pattern ResourceTypeAwsAcmCertificate = ResourceType' "AWS::ACM::Certificate"

pattern ResourceTypeAwsRdsDBInstance :: ResourceType
pattern ResourceTypeAwsRdsDBInstance = ResourceType' "AWS::RDS::DBInstance"

pattern ResourceTypeAwsRdsDBSubnetGroup :: ResourceType
pattern ResourceTypeAwsRdsDBSubnetGroup = ResourceType' "AWS::RDS::DBSubnetGroup"

pattern ResourceTypeAwsRdsDBSecurityGroup :: ResourceType
pattern ResourceTypeAwsRdsDBSecurityGroup = ResourceType' "AWS::RDS::DBSecurityGroup"

pattern ResourceTypeAwsRdsDBSnapshot :: ResourceType
pattern ResourceTypeAwsRdsDBSnapshot = ResourceType' "AWS::RDS::DBSnapshot"

pattern ResourceTypeAwsRdsDBCluster :: ResourceType
pattern ResourceTypeAwsRdsDBCluster = ResourceType' "AWS::RDS::DBCluster"

pattern ResourceTypeAwsRdsDBClusterSnapshot :: ResourceType
pattern ResourceTypeAwsRdsDBClusterSnapshot = ResourceType' "AWS::RDS::DBClusterSnapshot"

pattern ResourceTypeAwsRdsEventSubscription :: ResourceType
pattern ResourceTypeAwsRdsEventSubscription = ResourceType' "AWS::RDS::EventSubscription"

pattern ResourceTypeAwsS3Bucket :: ResourceType
pattern ResourceTypeAwsS3Bucket = ResourceType' "AWS::S3::Bucket"

pattern ResourceTypeAwsS3AccountPublicAccessBlock :: ResourceType
pattern ResourceTypeAwsS3AccountPublicAccessBlock = ResourceType' "AWS::S3::AccountPublicAccessBlock"

pattern ResourceTypeAwsRedshiftCluster :: ResourceType
pattern ResourceTypeAwsRedshiftCluster = ResourceType' "AWS::Redshift::Cluster"

pattern ResourceTypeAwsRedshiftClusterSnapshot :: ResourceType
pattern ResourceTypeAwsRedshiftClusterSnapshot = ResourceType' "AWS::Redshift::ClusterSnapshot"

pattern ResourceTypeAwsRedshiftClusterParameterGroup :: ResourceType
pattern ResourceTypeAwsRedshiftClusterParameterGroup = ResourceType' "AWS::Redshift::ClusterParameterGroup"

pattern ResourceTypeAwsRedshiftClusterSecurityGroup :: ResourceType
pattern ResourceTypeAwsRedshiftClusterSecurityGroup = ResourceType' "AWS::Redshift::ClusterSecurityGroup"

pattern ResourceTypeAwsRedshiftClusterSubnetGroup :: ResourceType
pattern ResourceTypeAwsRedshiftClusterSubnetGroup = ResourceType' "AWS::Redshift::ClusterSubnetGroup"

pattern ResourceTypeAwsRedshiftEventSubscription :: ResourceType
pattern ResourceTypeAwsRedshiftEventSubscription = ResourceType' "AWS::Redshift::EventSubscription"

pattern ResourceTypeAwsSsmManagedInstanceInventory :: ResourceType
pattern ResourceTypeAwsSsmManagedInstanceInventory = ResourceType' "AWS::SSM::ManagedInstanceInventory"

pattern ResourceTypeAwsCloudWatchAlarm :: ResourceType
pattern ResourceTypeAwsCloudWatchAlarm = ResourceType' "AWS::CloudWatch::Alarm"

pattern ResourceTypeAwsCloudFormationStack :: ResourceType
pattern ResourceTypeAwsCloudFormationStack = ResourceType' "AWS::CloudFormation::Stack"

pattern ResourceTypeAwsElasticLoadBalancingLoadBalancer :: ResourceType
pattern ResourceTypeAwsElasticLoadBalancingLoadBalancer = ResourceType' "AWS::ElasticLoadBalancing::LoadBalancer"

pattern ResourceTypeAwsAutoScalingAutoScalingGroup :: ResourceType
pattern ResourceTypeAwsAutoScalingAutoScalingGroup = ResourceType' "AWS::AutoScaling::AutoScalingGroup"

pattern ResourceTypeAwsAutoScalingLaunchConfiguration :: ResourceType
pattern ResourceTypeAwsAutoScalingLaunchConfiguration = ResourceType' "AWS::AutoScaling::LaunchConfiguration"

pattern ResourceTypeAwsAutoScalingScalingPolicy :: ResourceType
pattern ResourceTypeAwsAutoScalingScalingPolicy = ResourceType' "AWS::AutoScaling::ScalingPolicy"

pattern ResourceTypeAwsAutoScalingScheduledAction :: ResourceType
pattern ResourceTypeAwsAutoScalingScheduledAction = ResourceType' "AWS::AutoScaling::ScheduledAction"

pattern ResourceTypeAwsDynamoDBTable :: ResourceType
pattern ResourceTypeAwsDynamoDBTable = ResourceType' "AWS::DynamoDB::Table"

pattern ResourceTypeAwsCodeBuildProject :: ResourceType
pattern ResourceTypeAwsCodeBuildProject = ResourceType' "AWS::CodeBuild::Project"

pattern ResourceTypeAwsWafRateBasedRule :: ResourceType
pattern ResourceTypeAwsWafRateBasedRule = ResourceType' "AWS::WAF::RateBasedRule"

pattern ResourceTypeAwsWafRule :: ResourceType
pattern ResourceTypeAwsWafRule = ResourceType' "AWS::WAF::Rule"

pattern ResourceTypeAwsWafRuleGroup :: ResourceType
pattern ResourceTypeAwsWafRuleGroup = ResourceType' "AWS::WAF::RuleGroup"

pattern ResourceTypeAwsWafWebACL :: ResourceType
pattern ResourceTypeAwsWafWebACL = ResourceType' "AWS::WAF::WebACL"

pattern ResourceTypeAwsWAFRegionalRateBasedRule :: ResourceType
pattern ResourceTypeAwsWAFRegionalRateBasedRule = ResourceType' "AWS::WAFRegional::RateBasedRule"

pattern ResourceTypeAwsWAFRegionalRule :: ResourceType
pattern ResourceTypeAwsWAFRegionalRule = ResourceType' "AWS::WAFRegional::Rule"

pattern ResourceTypeAwsWAFRegionalRuleGroup :: ResourceType
pattern ResourceTypeAwsWAFRegionalRuleGroup = ResourceType' "AWS::WAFRegional::RuleGroup"

pattern ResourceTypeAwsWAFRegionalWebACL :: ResourceType
pattern ResourceTypeAwsWAFRegionalWebACL = ResourceType' "AWS::WAFRegional::WebACL"

pattern ResourceTypeAwsCloudFrontDistribution :: ResourceType
pattern ResourceTypeAwsCloudFrontDistribution = ResourceType' "AWS::CloudFront::Distribution"

pattern ResourceTypeAwsCloudFrontStreamingDistribution :: ResourceType
pattern ResourceTypeAwsCloudFrontStreamingDistribution = ResourceType' "AWS::CloudFront::StreamingDistribution"

pattern ResourceTypeAwsLambdaFunction :: ResourceType
pattern ResourceTypeAwsLambdaFunction = ResourceType' "AWS::Lambda::Function"

pattern ResourceTypeAwsElasticBeanstalkApplication :: ResourceType
pattern ResourceTypeAwsElasticBeanstalkApplication = ResourceType' "AWS::ElasticBeanstalk::Application"

pattern ResourceTypeAwsElasticBeanstalkApplicationVersion :: ResourceType
pattern ResourceTypeAwsElasticBeanstalkApplicationVersion = ResourceType' "AWS::ElasticBeanstalk::ApplicationVersion"

pattern ResourceTypeAwsElasticBeanstalkEnvironment :: ResourceType
pattern ResourceTypeAwsElasticBeanstalkEnvironment = ResourceType' "AWS::ElasticBeanstalk::Environment"

pattern ResourceTypeAwsWAFV2WebACL :: ResourceType
pattern ResourceTypeAwsWAFV2WebACL = ResourceType' "AWS::WAFv2::WebACL"

pattern ResourceTypeAwsWAFV2RuleGroup :: ResourceType
pattern ResourceTypeAwsWAFV2RuleGroup = ResourceType' "AWS::WAFv2::RuleGroup"

pattern ResourceTypeAwsWAFV2IPSet :: ResourceType
pattern ResourceTypeAwsWAFV2IPSet = ResourceType' "AWS::WAFv2::IPSet"

pattern ResourceTypeAwsWAFV2RegexPatternSet :: ResourceType
pattern ResourceTypeAwsWAFV2RegexPatternSet = ResourceType' "AWS::WAFv2::RegexPatternSet"

pattern ResourceTypeAwsWAFV2ManagedRuleSet :: ResourceType
pattern ResourceTypeAwsWAFV2ManagedRuleSet = ResourceType' "AWS::WAFv2::ManagedRuleSet"

pattern ResourceTypeAwsXRayEncryptionConfig :: ResourceType
pattern ResourceTypeAwsXRayEncryptionConfig = ResourceType' "AWS::XRay::EncryptionConfig"

pattern ResourceTypeAwsSsmAssociationCompliance :: ResourceType
pattern ResourceTypeAwsSsmAssociationCompliance = ResourceType' "AWS::SSM::AssociationCompliance"

pattern ResourceTypeAwsSsmPatchCompliance :: ResourceType
pattern ResourceTypeAwsSsmPatchCompliance = ResourceType' "AWS::SSM::PatchCompliance"

pattern ResourceTypeAwsShieldProtection :: ResourceType
pattern ResourceTypeAwsShieldProtection = ResourceType' "AWS::Shield::Protection"

pattern ResourceTypeAwsShieldRegionalProtection :: ResourceType
pattern ResourceTypeAwsShieldRegionalProtection = ResourceType' "AWS::ShieldRegional::Protection"

pattern ResourceTypeAwsConfigResourceCompliance :: ResourceType
pattern ResourceTypeAwsConfigResourceCompliance = ResourceType' "AWS::Config::ResourceCompliance"

pattern ResourceTypeAwsApiGatewayStage :: ResourceType
pattern ResourceTypeAwsApiGatewayStage = ResourceType' "AWS::ApiGateway::Stage"

pattern ResourceTypeAwsApiGatewayRestApi :: ResourceType
pattern ResourceTypeAwsApiGatewayRestApi = ResourceType' "AWS::ApiGateway::RestApi"

pattern ResourceTypeAwsAPIGATEWAYV2Stage :: ResourceType
pattern ResourceTypeAwsAPIGATEWAYV2Stage = ResourceType' "AWS::ApiGatewayV2::Stage"

pattern ResourceTypeAwsAPIGATEWAYV2Api :: ResourceType
pattern ResourceTypeAwsAPIGATEWAYV2Api = ResourceType' "AWS::ApiGatewayV2::Api"

pattern ResourceTypeAwsCodePipelinePipeline :: ResourceType
pattern ResourceTypeAwsCodePipelinePipeline = ResourceType' "AWS::CodePipeline::Pipeline"

pattern ResourceTypeAwsServiceCatalogCloudFormationProvisionedProduct :: ResourceType
pattern ResourceTypeAwsServiceCatalogCloudFormationProvisionedProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProvisionedProduct"

pattern ResourceTypeAwsServiceCatalogCloudFormationProduct :: ResourceType
pattern ResourceTypeAwsServiceCatalogCloudFormationProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProduct"

pattern ResourceTypeAwsServiceCatalogPortfolio :: ResourceType
pattern ResourceTypeAwsServiceCatalogPortfolio = ResourceType' "AWS::ServiceCatalog::Portfolio"

pattern ResourceTypeAwsSqsQueue :: ResourceType
pattern ResourceTypeAwsSqsQueue = ResourceType' "AWS::SQS::Queue"

pattern ResourceTypeAwsKmsKey :: ResourceType
pattern ResourceTypeAwsKmsKey = ResourceType' "AWS::KMS::Key"

pattern ResourceTypeAwsQldbLedger :: ResourceType
pattern ResourceTypeAwsQldbLedger = ResourceType' "AWS::QLDB::Ledger"

pattern ResourceTypeAwsSecretsManagerSecret :: ResourceType
pattern ResourceTypeAwsSecretsManagerSecret = ResourceType' "AWS::SecretsManager::Secret"

pattern ResourceTypeAwsSnsTopic :: ResourceType
pattern ResourceTypeAwsSnsTopic = ResourceType' "AWS::SNS::Topic"

pattern ResourceTypeAwsSsmFileData :: ResourceType
pattern ResourceTypeAwsSsmFileData = ResourceType' "AWS::SSM::FileData"

{-# COMPLETE
  ResourceTypeAwsEC2CustomerGateway,
  ResourceTypeAwsEC2Eip,
  ResourceTypeAwsEC2Host,
  ResourceTypeAwsEC2Instance,
  ResourceTypeAwsEC2InternetGateway,
  ResourceTypeAwsEC2NetworkAcl,
  ResourceTypeAwsEC2NetworkInterface,
  ResourceTypeAwsEC2RouteTable,
  ResourceTypeAwsEC2SecurityGroup,
  ResourceTypeAwsEC2Subnet,
  ResourceTypeAwsCloudTrailTrail,
  ResourceTypeAwsEC2Volume,
  ResourceTypeAwsEC2Vpc,
  ResourceTypeAwsEC2VPNConnection,
  ResourceTypeAwsEC2VPNGateway,
  ResourceTypeAwsEC2RegisteredHAInstance,
  ResourceTypeAwsEC2NatGateway,
  ResourceTypeAwsEC2EgressOnlyInternetGateway,
  ResourceTypeAwsEC2VPCEndpoint,
  ResourceTypeAwsEC2VPCEndpointService,
  ResourceTypeAwsEC2FlowLog,
  ResourceTypeAwsEC2VPCPeeringConnection,
  ResourceTypeAwsElasticsearchDomain,
  ResourceTypeAwsIamGroup,
  ResourceTypeAwsIamPolicy,
  ResourceTypeAwsIamRole,
  ResourceTypeAwsIamUser,
  ResourceTypeAwsELASTICLOADBALANCINGV2LoadBalancer,
  ResourceTypeAwsAcmCertificate,
  ResourceTypeAwsRdsDBInstance,
  ResourceTypeAwsRdsDBSubnetGroup,
  ResourceTypeAwsRdsDBSecurityGroup,
  ResourceTypeAwsRdsDBSnapshot,
  ResourceTypeAwsRdsDBCluster,
  ResourceTypeAwsRdsDBClusterSnapshot,
  ResourceTypeAwsRdsEventSubscription,
  ResourceTypeAwsS3Bucket,
  ResourceTypeAwsS3AccountPublicAccessBlock,
  ResourceTypeAwsRedshiftCluster,
  ResourceTypeAwsRedshiftClusterSnapshot,
  ResourceTypeAwsRedshiftClusterParameterGroup,
  ResourceTypeAwsRedshiftClusterSecurityGroup,
  ResourceTypeAwsRedshiftClusterSubnetGroup,
  ResourceTypeAwsRedshiftEventSubscription,
  ResourceTypeAwsSsmManagedInstanceInventory,
  ResourceTypeAwsCloudWatchAlarm,
  ResourceTypeAwsCloudFormationStack,
  ResourceTypeAwsElasticLoadBalancingLoadBalancer,
  ResourceTypeAwsAutoScalingAutoScalingGroup,
  ResourceTypeAwsAutoScalingLaunchConfiguration,
  ResourceTypeAwsAutoScalingScalingPolicy,
  ResourceTypeAwsAutoScalingScheduledAction,
  ResourceTypeAwsDynamoDBTable,
  ResourceTypeAwsCodeBuildProject,
  ResourceTypeAwsWafRateBasedRule,
  ResourceTypeAwsWafRule,
  ResourceTypeAwsWafRuleGroup,
  ResourceTypeAwsWafWebACL,
  ResourceTypeAwsWAFRegionalRateBasedRule,
  ResourceTypeAwsWAFRegionalRule,
  ResourceTypeAwsWAFRegionalRuleGroup,
  ResourceTypeAwsWAFRegionalWebACL,
  ResourceTypeAwsCloudFrontDistribution,
  ResourceTypeAwsCloudFrontStreamingDistribution,
  ResourceTypeAwsLambdaFunction,
  ResourceTypeAwsElasticBeanstalkApplication,
  ResourceTypeAwsElasticBeanstalkApplicationVersion,
  ResourceTypeAwsElasticBeanstalkEnvironment,
  ResourceTypeAwsWAFV2WebACL,
  ResourceTypeAwsWAFV2RuleGroup,
  ResourceTypeAwsWAFV2IPSet,
  ResourceTypeAwsWAFV2RegexPatternSet,
  ResourceTypeAwsWAFV2ManagedRuleSet,
  ResourceTypeAwsXRayEncryptionConfig,
  ResourceTypeAwsSsmAssociationCompliance,
  ResourceTypeAwsSsmPatchCompliance,
  ResourceTypeAwsShieldProtection,
  ResourceTypeAwsShieldRegionalProtection,
  ResourceTypeAwsConfigResourceCompliance,
  ResourceTypeAwsApiGatewayStage,
  ResourceTypeAwsApiGatewayRestApi,
  ResourceTypeAwsAPIGATEWAYV2Stage,
  ResourceTypeAwsAPIGATEWAYV2Api,
  ResourceTypeAwsCodePipelinePipeline,
  ResourceTypeAwsServiceCatalogCloudFormationProvisionedProduct,
  ResourceTypeAwsServiceCatalogCloudFormationProduct,
  ResourceTypeAwsServiceCatalogPortfolio,
  ResourceTypeAwsSqsQueue,
  ResourceTypeAwsKmsKey,
  ResourceTypeAwsQldbLedger,
  ResourceTypeAwsSecretsManagerSecret,
  ResourceTypeAwsSnsTopic,
  ResourceTypeAwsSsmFileData,
  ResourceType'
  #-}
