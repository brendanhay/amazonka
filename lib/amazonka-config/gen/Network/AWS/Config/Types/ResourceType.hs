{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceType where

import Network.AWS.Prelude

data ResourceType
  = AWSAPIGATEWAYV2API
  | AWSAPIGATEWAYV2Stage
  | AWSAPIGatewayRestAPI
  | AWSAPIGatewayStage
  | AWSAcmCertificate
  | AWSAutoScalingAutoScalingGroup
  | AWSAutoScalingLaunchConfiguration
  | AWSAutoScalingScalingPolicy
  | AWSAutoScalingScheduledAction
  | AWSCloudFormationStack
  | AWSCloudFrontDistribution
  | AWSCloudFrontStreamingDistribution
  | AWSCloudTrailTrail
  | AWSCloudWatchAlarm
  | AWSCodeBuildProject
  | AWSCodePipelinePipeline
  | AWSConfigResourceCompliance
  | AWSDynamoDBTable
  | AWSEC2CustomerGateway
  | AWSEC2EIP
  | AWSEC2EgressOnlyInternetGateway
  | AWSEC2FlowLog
  | AWSEC2Host
  | AWSEC2Instance
  | AWSEC2InternetGateway
  | AWSEC2NatGateway
  | AWSEC2NetworkACL
  | AWSEC2NetworkInterface
  | AWSEC2RegisteredHAInstance
  | AWSEC2RouteTable
  | AWSEC2SecurityGroup
  | AWSEC2Subnet
  | AWSEC2VPC
  | AWSEC2VPCEndpoint
  | AWSEC2VPCEndpointService
  | AWSEC2VPCPeeringConnection
  | AWSEC2VPNConnection
  | AWSEC2VPNGateway
  | AWSEC2Volume
  | AWSELASTICLOADBALANCINGV2LoadBalancer
  | AWSElasticBeanstalkApplication
  | AWSElasticBeanstalkApplicationVersion
  | AWSElasticBeanstalkEnvironment
  | AWSElasticLoadBalancingLoadBalancer
  | AWSElasticsearchDomain
  | AWSIAMGroup
  | AWSIAMPolicy
  | AWSIAMRole
  | AWSIAMUser
  | AWSKMSKey
  | AWSLambdaFunction
  | AWSQldbLedger
  | AWSRDSDBCluster
  | AWSRDSDBClusterSnapshot
  | AWSRDSDBInstance
  | AWSRDSDBSecurityGroup
  | AWSRDSDBSnapshot
  | AWSRDSDBSubnetGroup
  | AWSRDSEventSubscription
  | AWSRedshiftCluster
  | AWSRedshiftClusterParameterGroup
  | AWSRedshiftClusterSecurityGroup
  | AWSRedshiftClusterSnapshot
  | AWSRedshiftClusterSubnetGroup
  | AWSRedshiftEventSubscription
  | AWSS3AccountPublicAccessBlock
  | AWSS3Bucket
  | AWSSNSTopic
  | AWSSecretsManagerSecret
  | AWSServiceCatalogCloudFormationProduct
  | AWSServiceCatalogCloudFormationProvisionedProduct
  | AWSServiceCatalogPortfolio
  | AWSShieldProtection
  | AWSShieldRegionalProtection
  | AWSSqsQueue
  | AWSSsmAssociationCompliance
  | AWSSsmFileData
  | AWSSsmManagedInstanceInventory
  | AWSSsmPatchCompliance
  | AWSWAFRegionalRateBasedRule
  | AWSWAFRegionalRule
  | AWSWAFRegionalRuleGroup
  | AWSWAFRegionalWebACL
  | AWSWAFV2IPSet
  | AWSWAFV2ManagedRuleSet
  | AWSWAFV2RegexPatternSet
  | AWSWAFV2RuleGroup
  | AWSWAFV2WebACL
  | AWSWafRateBasedRule
  | AWSWafRule
  | AWSWafRuleGroup
  | AWSWafWebACL
  | AWSXRayEncryptionConfig
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "aws::apigatewayv2::api" -> pure AWSAPIGATEWAYV2API
      "aws::apigatewayv2::stage" -> pure AWSAPIGATEWAYV2Stage
      "aws::apigateway::restapi" -> pure AWSAPIGatewayRestAPI
      "aws::apigateway::stage" -> pure AWSAPIGatewayStage
      "aws::acm::certificate" -> pure AWSAcmCertificate
      "aws::autoscaling::autoscalinggroup" -> pure AWSAutoScalingAutoScalingGroup
      "aws::autoscaling::launchconfiguration" -> pure AWSAutoScalingLaunchConfiguration
      "aws::autoscaling::scalingpolicy" -> pure AWSAutoScalingScalingPolicy
      "aws::autoscaling::scheduledaction" -> pure AWSAutoScalingScheduledAction
      "aws::cloudformation::stack" -> pure AWSCloudFormationStack
      "aws::cloudfront::distribution" -> pure AWSCloudFrontDistribution
      "aws::cloudfront::streamingdistribution" -> pure AWSCloudFrontStreamingDistribution
      "aws::cloudtrail::trail" -> pure AWSCloudTrailTrail
      "aws::cloudwatch::alarm" -> pure AWSCloudWatchAlarm
      "aws::codebuild::project" -> pure AWSCodeBuildProject
      "aws::codepipeline::pipeline" -> pure AWSCodePipelinePipeline
      "aws::config::resourcecompliance" -> pure AWSConfigResourceCompliance
      "aws::dynamodb::table" -> pure AWSDynamoDBTable
      "aws::ec2::customergateway" -> pure AWSEC2CustomerGateway
      "aws::ec2::eip" -> pure AWSEC2EIP
      "aws::ec2::egressonlyinternetgateway" -> pure AWSEC2EgressOnlyInternetGateway
      "aws::ec2::flowlog" -> pure AWSEC2FlowLog
      "aws::ec2::host" -> pure AWSEC2Host
      "aws::ec2::instance" -> pure AWSEC2Instance
      "aws::ec2::internetgateway" -> pure AWSEC2InternetGateway
      "aws::ec2::natgateway" -> pure AWSEC2NatGateway
      "aws::ec2::networkacl" -> pure AWSEC2NetworkACL
      "aws::ec2::networkinterface" -> pure AWSEC2NetworkInterface
      "aws::ec2::registeredhainstance" -> pure AWSEC2RegisteredHAInstance
      "aws::ec2::routetable" -> pure AWSEC2RouteTable
      "aws::ec2::securitygroup" -> pure AWSEC2SecurityGroup
      "aws::ec2::subnet" -> pure AWSEC2Subnet
      "aws::ec2::vpc" -> pure AWSEC2VPC
      "aws::ec2::vpcendpoint" -> pure AWSEC2VPCEndpoint
      "aws::ec2::vpcendpointservice" -> pure AWSEC2VPCEndpointService
      "aws::ec2::vpcpeeringconnection" -> pure AWSEC2VPCPeeringConnection
      "aws::ec2::vpnconnection" -> pure AWSEC2VPNConnection
      "aws::ec2::vpngateway" -> pure AWSEC2VPNGateway
      "aws::ec2::volume" -> pure AWSEC2Volume
      "aws::elasticloadbalancingv2::loadbalancer" -> pure AWSELASTICLOADBALANCINGV2LoadBalancer
      "aws::elasticbeanstalk::application" -> pure AWSElasticBeanstalkApplication
      "aws::elasticbeanstalk::applicationversion" -> pure AWSElasticBeanstalkApplicationVersion
      "aws::elasticbeanstalk::environment" -> pure AWSElasticBeanstalkEnvironment
      "aws::elasticloadbalancing::loadbalancer" -> pure AWSElasticLoadBalancingLoadBalancer
      "aws::elasticsearch::domain" -> pure AWSElasticsearchDomain
      "aws::iam::group" -> pure AWSIAMGroup
      "aws::iam::policy" -> pure AWSIAMPolicy
      "aws::iam::role" -> pure AWSIAMRole
      "aws::iam::user" -> pure AWSIAMUser
      "aws::kms::key" -> pure AWSKMSKey
      "aws::lambda::function" -> pure AWSLambdaFunction
      "aws::qldb::ledger" -> pure AWSQldbLedger
      "aws::rds::dbcluster" -> pure AWSRDSDBCluster
      "aws::rds::dbclustersnapshot" -> pure AWSRDSDBClusterSnapshot
      "aws::rds::dbinstance" -> pure AWSRDSDBInstance
      "aws::rds::dbsecuritygroup" -> pure AWSRDSDBSecurityGroup
      "aws::rds::dbsnapshot" -> pure AWSRDSDBSnapshot
      "aws::rds::dbsubnetgroup" -> pure AWSRDSDBSubnetGroup
      "aws::rds::eventsubscription" -> pure AWSRDSEventSubscription
      "aws::redshift::cluster" -> pure AWSRedshiftCluster
      "aws::redshift::clusterparametergroup" -> pure AWSRedshiftClusterParameterGroup
      "aws::redshift::clustersecuritygroup" -> pure AWSRedshiftClusterSecurityGroup
      "aws::redshift::clustersnapshot" -> pure AWSRedshiftClusterSnapshot
      "aws::redshift::clustersubnetgroup" -> pure AWSRedshiftClusterSubnetGroup
      "aws::redshift::eventsubscription" -> pure AWSRedshiftEventSubscription
      "aws::s3::accountpublicaccessblock" -> pure AWSS3AccountPublicAccessBlock
      "aws::s3::bucket" -> pure AWSS3Bucket
      "aws::sns::topic" -> pure AWSSNSTopic
      "aws::secretsmanager::secret" -> pure AWSSecretsManagerSecret
      "aws::servicecatalog::cloudformationproduct" -> pure AWSServiceCatalogCloudFormationProduct
      "aws::servicecatalog::cloudformationprovisionedproduct" -> pure AWSServiceCatalogCloudFormationProvisionedProduct
      "aws::servicecatalog::portfolio" -> pure AWSServiceCatalogPortfolio
      "aws::shield::protection" -> pure AWSShieldProtection
      "aws::shieldregional::protection" -> pure AWSShieldRegionalProtection
      "aws::sqs::queue" -> pure AWSSqsQueue
      "aws::ssm::associationcompliance" -> pure AWSSsmAssociationCompliance
      "aws::ssm::filedata" -> pure AWSSsmFileData
      "aws::ssm::managedinstanceinventory" -> pure AWSSsmManagedInstanceInventory
      "aws::ssm::patchcompliance" -> pure AWSSsmPatchCompliance
      "aws::wafregional::ratebasedrule" -> pure AWSWAFRegionalRateBasedRule
      "aws::wafregional::rule" -> pure AWSWAFRegionalRule
      "aws::wafregional::rulegroup" -> pure AWSWAFRegionalRuleGroup
      "aws::wafregional::webacl" -> pure AWSWAFRegionalWebACL
      "aws::wafv2::ipset" -> pure AWSWAFV2IPSet
      "aws::wafv2::managedruleset" -> pure AWSWAFV2ManagedRuleSet
      "aws::wafv2::regexpatternset" -> pure AWSWAFV2RegexPatternSet
      "aws::wafv2::rulegroup" -> pure AWSWAFV2RuleGroup
      "aws::wafv2::webacl" -> pure AWSWAFV2WebACL
      "aws::waf::ratebasedrule" -> pure AWSWafRateBasedRule
      "aws::waf::rule" -> pure AWSWafRule
      "aws::waf::rulegroup" -> pure AWSWafRuleGroup
      "aws::waf::webacl" -> pure AWSWafWebACL
      "aws::xray::encryptionconfig" -> pure AWSXRayEncryptionConfig
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: aws::apigatewayv2::api, aws::apigatewayv2::stage, aws::apigateway::restapi, aws::apigateway::stage, aws::acm::certificate, aws::autoscaling::autoscalinggroup, aws::autoscaling::launchconfiguration, aws::autoscaling::scalingpolicy, aws::autoscaling::scheduledaction, aws::cloudformation::stack, aws::cloudfront::distribution, aws::cloudfront::streamingdistribution, aws::cloudtrail::trail, aws::cloudwatch::alarm, aws::codebuild::project, aws::codepipeline::pipeline, aws::config::resourcecompliance, aws::dynamodb::table, aws::ec2::customergateway, aws::ec2::eip, aws::ec2::egressonlyinternetgateway, aws::ec2::flowlog, aws::ec2::host, aws::ec2::instance, aws::ec2::internetgateway, aws::ec2::natgateway, aws::ec2::networkacl, aws::ec2::networkinterface, aws::ec2::registeredhainstance, aws::ec2::routetable, aws::ec2::securitygroup, aws::ec2::subnet, aws::ec2::vpc, aws::ec2::vpcendpoint, aws::ec2::vpcendpointservice, aws::ec2::vpcpeeringconnection, aws::ec2::vpnconnection, aws::ec2::vpngateway, aws::ec2::volume, aws::elasticloadbalancingv2::loadbalancer, aws::elasticbeanstalk::application, aws::elasticbeanstalk::applicationversion, aws::elasticbeanstalk::environment, aws::elasticloadbalancing::loadbalancer, aws::elasticsearch::domain, aws::iam::group, aws::iam::policy, aws::iam::role, aws::iam::user, aws::kms::key, aws::lambda::function, aws::qldb::ledger, aws::rds::dbcluster, aws::rds::dbclustersnapshot, aws::rds::dbinstance, aws::rds::dbsecuritygroup, aws::rds::dbsnapshot, aws::rds::dbsubnetgroup, aws::rds::eventsubscription, aws::redshift::cluster, aws::redshift::clusterparametergroup, aws::redshift::clustersecuritygroup, aws::redshift::clustersnapshot, aws::redshift::clustersubnetgroup, aws::redshift::eventsubscription, aws::s3::accountpublicaccessblock, aws::s3::bucket, aws::sns::topic, aws::secretsmanager::secret, aws::servicecatalog::cloudformationproduct, aws::servicecatalog::cloudformationprovisionedproduct, aws::servicecatalog::portfolio, aws::shield::protection, aws::shieldregional::protection, aws::sqs::queue, aws::ssm::associationcompliance, aws::ssm::filedata, aws::ssm::managedinstanceinventory, aws::ssm::patchcompliance, aws::wafregional::ratebasedrule, aws::wafregional::rule, aws::wafregional::rulegroup, aws::wafregional::webacl, aws::wafv2::ipset, aws::wafv2::managedruleset, aws::wafv2::regexpatternset, aws::wafv2::rulegroup, aws::wafv2::webacl, aws::waf::ratebasedrule, aws::waf::rule, aws::waf::rulegroup, aws::waf::webacl, aws::xray::encryptionconfig"

instance ToText ResourceType where
  toText = \case
    AWSAPIGATEWAYV2API -> "AWS::ApiGatewayV2::Api"
    AWSAPIGATEWAYV2Stage -> "AWS::ApiGatewayV2::Stage"
    AWSAPIGatewayRestAPI -> "AWS::ApiGateway::RestApi"
    AWSAPIGatewayStage -> "AWS::ApiGateway::Stage"
    AWSAcmCertificate -> "AWS::ACM::Certificate"
    AWSAutoScalingAutoScalingGroup -> "AWS::AutoScaling::AutoScalingGroup"
    AWSAutoScalingLaunchConfiguration -> "AWS::AutoScaling::LaunchConfiguration"
    AWSAutoScalingScalingPolicy -> "AWS::AutoScaling::ScalingPolicy"
    AWSAutoScalingScheduledAction -> "AWS::AutoScaling::ScheduledAction"
    AWSCloudFormationStack -> "AWS::CloudFormation::Stack"
    AWSCloudFrontDistribution -> "AWS::CloudFront::Distribution"
    AWSCloudFrontStreamingDistribution -> "AWS::CloudFront::StreamingDistribution"
    AWSCloudTrailTrail -> "AWS::CloudTrail::Trail"
    AWSCloudWatchAlarm -> "AWS::CloudWatch::Alarm"
    AWSCodeBuildProject -> "AWS::CodeBuild::Project"
    AWSCodePipelinePipeline -> "AWS::CodePipeline::Pipeline"
    AWSConfigResourceCompliance -> "AWS::Config::ResourceCompliance"
    AWSDynamoDBTable -> "AWS::DynamoDB::Table"
    AWSEC2CustomerGateway -> "AWS::EC2::CustomerGateway"
    AWSEC2EIP -> "AWS::EC2::EIP"
    AWSEC2EgressOnlyInternetGateway -> "AWS::EC2::EgressOnlyInternetGateway"
    AWSEC2FlowLog -> "AWS::EC2::FlowLog"
    AWSEC2Host -> "AWS::EC2::Host"
    AWSEC2Instance -> "AWS::EC2::Instance"
    AWSEC2InternetGateway -> "AWS::EC2::InternetGateway"
    AWSEC2NatGateway -> "AWS::EC2::NatGateway"
    AWSEC2NetworkACL -> "AWS::EC2::NetworkAcl"
    AWSEC2NetworkInterface -> "AWS::EC2::NetworkInterface"
    AWSEC2RegisteredHAInstance -> "AWS::EC2::RegisteredHAInstance"
    AWSEC2RouteTable -> "AWS::EC2::RouteTable"
    AWSEC2SecurityGroup -> "AWS::EC2::SecurityGroup"
    AWSEC2Subnet -> "AWS::EC2::Subnet"
    AWSEC2VPC -> "AWS::EC2::VPC"
    AWSEC2VPCEndpoint -> "AWS::EC2::VPCEndpoint"
    AWSEC2VPCEndpointService -> "AWS::EC2::VPCEndpointService"
    AWSEC2VPCPeeringConnection -> "AWS::EC2::VPCPeeringConnection"
    AWSEC2VPNConnection -> "AWS::EC2::VPNConnection"
    AWSEC2VPNGateway -> "AWS::EC2::VPNGateway"
    AWSEC2Volume -> "AWS::EC2::Volume"
    AWSELASTICLOADBALANCINGV2LoadBalancer -> "AWS::ElasticLoadBalancingV2::LoadBalancer"
    AWSElasticBeanstalkApplication -> "AWS::ElasticBeanstalk::Application"
    AWSElasticBeanstalkApplicationVersion -> "AWS::ElasticBeanstalk::ApplicationVersion"
    AWSElasticBeanstalkEnvironment -> "AWS::ElasticBeanstalk::Environment"
    AWSElasticLoadBalancingLoadBalancer -> "AWS::ElasticLoadBalancing::LoadBalancer"
    AWSElasticsearchDomain -> "AWS::Elasticsearch::Domain"
    AWSIAMGroup -> "AWS::IAM::Group"
    AWSIAMPolicy -> "AWS::IAM::Policy"
    AWSIAMRole -> "AWS::IAM::Role"
    AWSIAMUser -> "AWS::IAM::User"
    AWSKMSKey -> "AWS::KMS::Key"
    AWSLambdaFunction -> "AWS::Lambda::Function"
    AWSQldbLedger -> "AWS::QLDB::Ledger"
    AWSRDSDBCluster -> "AWS::RDS::DBCluster"
    AWSRDSDBClusterSnapshot -> "AWS::RDS::DBClusterSnapshot"
    AWSRDSDBInstance -> "AWS::RDS::DBInstance"
    AWSRDSDBSecurityGroup -> "AWS::RDS::DBSecurityGroup"
    AWSRDSDBSnapshot -> "AWS::RDS::DBSnapshot"
    AWSRDSDBSubnetGroup -> "AWS::RDS::DBSubnetGroup"
    AWSRDSEventSubscription -> "AWS::RDS::EventSubscription"
    AWSRedshiftCluster -> "AWS::Redshift::Cluster"
    AWSRedshiftClusterParameterGroup -> "AWS::Redshift::ClusterParameterGroup"
    AWSRedshiftClusterSecurityGroup -> "AWS::Redshift::ClusterSecurityGroup"
    AWSRedshiftClusterSnapshot -> "AWS::Redshift::ClusterSnapshot"
    AWSRedshiftClusterSubnetGroup -> "AWS::Redshift::ClusterSubnetGroup"
    AWSRedshiftEventSubscription -> "AWS::Redshift::EventSubscription"
    AWSS3AccountPublicAccessBlock -> "AWS::S3::AccountPublicAccessBlock"
    AWSS3Bucket -> "AWS::S3::Bucket"
    AWSSNSTopic -> "AWS::SNS::Topic"
    AWSSecretsManagerSecret -> "AWS::SecretsManager::Secret"
    AWSServiceCatalogCloudFormationProduct -> "AWS::ServiceCatalog::CloudFormationProduct"
    AWSServiceCatalogCloudFormationProvisionedProduct -> "AWS::ServiceCatalog::CloudFormationProvisionedProduct"
    AWSServiceCatalogPortfolio -> "AWS::ServiceCatalog::Portfolio"
    AWSShieldProtection -> "AWS::Shield::Protection"
    AWSShieldRegionalProtection -> "AWS::ShieldRegional::Protection"
    AWSSqsQueue -> "AWS::SQS::Queue"
    AWSSsmAssociationCompliance -> "AWS::SSM::AssociationCompliance"
    AWSSsmFileData -> "AWS::SSM::FileData"
    AWSSsmManagedInstanceInventory -> "AWS::SSM::ManagedInstanceInventory"
    AWSSsmPatchCompliance -> "AWS::SSM::PatchCompliance"
    AWSWAFRegionalRateBasedRule -> "AWS::WAFRegional::RateBasedRule"
    AWSWAFRegionalRule -> "AWS::WAFRegional::Rule"
    AWSWAFRegionalRuleGroup -> "AWS::WAFRegional::RuleGroup"
    AWSWAFRegionalWebACL -> "AWS::WAFRegional::WebACL"
    AWSWAFV2IPSet -> "AWS::WAFv2::IPSet"
    AWSWAFV2ManagedRuleSet -> "AWS::WAFv2::ManagedRuleSet"
    AWSWAFV2RegexPatternSet -> "AWS::WAFv2::RegexPatternSet"
    AWSWAFV2RuleGroup -> "AWS::WAFv2::RuleGroup"
    AWSWAFV2WebACL -> "AWS::WAFv2::WebACL"
    AWSWafRateBasedRule -> "AWS::WAF::RateBasedRule"
    AWSWafRule -> "AWS::WAF::Rule"
    AWSWafRuleGroup -> "AWS::WAF::RuleGroup"
    AWSWafWebACL -> "AWS::WAF::WebACL"
    AWSXRayEncryptionConfig -> "AWS::XRay::EncryptionConfig"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance ToJSON ResourceType where
  toJSON = toJSONText

instance FromJSON ResourceType where
  parseJSON = parseJSONText "ResourceType"
