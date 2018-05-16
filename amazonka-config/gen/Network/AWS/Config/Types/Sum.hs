{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Sum where

import Network.AWS.Prelude

data AggregatedSourceStatusType
  = Failed
  | Outdated
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AggregatedSourceStatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "outdated" -> pure Outdated
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing AggregatedSourceStatusType from value: '" <> e
           <> "'. Accepted values: failed, outdated, succeeded"

instance ToText AggregatedSourceStatusType where
    toText = \case
        Failed -> "FAILED"
        Outdated -> "OUTDATED"
        Succeeded -> "SUCCEEDED"

instance Hashable     AggregatedSourceStatusType
instance NFData       AggregatedSourceStatusType
instance ToByteString AggregatedSourceStatusType
instance ToQuery      AggregatedSourceStatusType
instance ToHeader     AggregatedSourceStatusType

instance ToJSON AggregatedSourceStatusType where
    toJSON = toJSONText

instance FromJSON AggregatedSourceStatusType where
    parseJSON = parseJSONText "AggregatedSourceStatusType"

data AggregatedSourceType
  = Account
  | Organization
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AggregatedSourceType where
    parser = takeLowerText >>= \case
        "account" -> pure Account
        "organization" -> pure Organization
        e -> fromTextError $ "Failure parsing AggregatedSourceType from value: '" <> e
           <> "'. Accepted values: account, organization"

instance ToText AggregatedSourceType where
    toText = \case
        Account -> "ACCOUNT"
        Organization -> "ORGANIZATION"

instance Hashable     AggregatedSourceType
instance NFData       AggregatedSourceType
instance ToByteString AggregatedSourceType
instance ToQuery      AggregatedSourceType
instance ToHeader     AggregatedSourceType

instance FromJSON AggregatedSourceType where
    parseJSON = parseJSONText "AggregatedSourceType"

data ChronologicalOrder
  = Forward
  | Reverse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChronologicalOrder where
    parser = takeLowerText >>= \case
        "forward" -> pure Forward
        "reverse" -> pure Reverse
        e -> fromTextError $ "Failure parsing ChronologicalOrder from value: '" <> e
           <> "'. Accepted values: forward, reverse"

instance ToText ChronologicalOrder where
    toText = \case
        Forward -> "Forward"
        Reverse -> "Reverse"

instance Hashable     ChronologicalOrder
instance NFData       ChronologicalOrder
instance ToByteString ChronologicalOrder
instance ToQuery      ChronologicalOrder
instance ToHeader     ChronologicalOrder

instance ToJSON ChronologicalOrder where
    toJSON = toJSONText

data ComplianceType
  = Compliant
  | InsufficientData
  | NonCompliant
  | NotApplicable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComplianceType where
    parser = takeLowerText >>= \case
        "compliant" -> pure Compliant
        "insufficient_data" -> pure InsufficientData
        "non_compliant" -> pure NonCompliant
        "not_applicable" -> pure NotApplicable
        e -> fromTextError $ "Failure parsing ComplianceType from value: '" <> e
           <> "'. Accepted values: compliant, insufficient_data, non_compliant, not_applicable"

instance ToText ComplianceType where
    toText = \case
        Compliant -> "COMPLIANT"
        InsufficientData -> "INSUFFICIENT_DATA"
        NonCompliant -> "NON_COMPLIANT"
        NotApplicable -> "NOT_APPLICABLE"

instance Hashable     ComplianceType
instance NFData       ComplianceType
instance ToByteString ComplianceType
instance ToQuery      ComplianceType
instance ToHeader     ComplianceType

instance ToJSON ComplianceType where
    toJSON = toJSONText

instance FromJSON ComplianceType where
    parseJSON = parseJSONText "ComplianceType"

data ConfigRuleComplianceSummaryGroupKey
  = AWSRegion
  | AccountId
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigRuleComplianceSummaryGroupKey where
    parser = takeLowerText >>= \case
        "aws_region" -> pure AWSRegion
        "account_id" -> pure AccountId
        e -> fromTextError $ "Failure parsing ConfigRuleComplianceSummaryGroupKey from value: '" <> e
           <> "'. Accepted values: aws_region, account_id"

instance ToText ConfigRuleComplianceSummaryGroupKey where
    toText = \case
        AWSRegion -> "AWS_REGION"
        AccountId -> "ACCOUNT_ID"

instance Hashable     ConfigRuleComplianceSummaryGroupKey
instance NFData       ConfigRuleComplianceSummaryGroupKey
instance ToByteString ConfigRuleComplianceSummaryGroupKey
instance ToQuery      ConfigRuleComplianceSummaryGroupKey
instance ToHeader     ConfigRuleComplianceSummaryGroupKey

instance ToJSON ConfigRuleComplianceSummaryGroupKey where
    toJSON = toJSONText

data ConfigRuleState
  = Active
  | Deleting
  | DeletingResults
  | Evaluating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigRuleState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "deleting" -> pure Deleting
        "deleting_results" -> pure DeletingResults
        "evaluating" -> pure Evaluating
        e -> fromTextError $ "Failure parsing ConfigRuleState from value: '" <> e
           <> "'. Accepted values: active, deleting, deleting_results, evaluating"

instance ToText ConfigRuleState where
    toText = \case
        Active -> "ACTIVE"
        Deleting -> "DELETING"
        DeletingResults -> "DELETING_RESULTS"
        Evaluating -> "EVALUATING"

instance Hashable     ConfigRuleState
instance NFData       ConfigRuleState
instance ToByteString ConfigRuleState
instance ToQuery      ConfigRuleState
instance ToHeader     ConfigRuleState

instance ToJSON ConfigRuleState where
    toJSON = toJSONText

instance FromJSON ConfigRuleState where
    parseJSON = parseJSONText "ConfigRuleState"

data ConfigurationItemStatus
  = OK
  | ResourceDeleted
  | ResourceDeletedNotRecorded
  | ResourceDiscovered
  | ResourceNotRecorded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigurationItemStatus where
    parser = takeLowerText >>= \case
        "ok" -> pure OK
        "resourcedeleted" -> pure ResourceDeleted
        "resourcedeletednotrecorded" -> pure ResourceDeletedNotRecorded
        "resourcediscovered" -> pure ResourceDiscovered
        "resourcenotrecorded" -> pure ResourceNotRecorded
        e -> fromTextError $ "Failure parsing ConfigurationItemStatus from value: '" <> e
           <> "'. Accepted values: ok, resourcedeleted, resourcedeletednotrecorded, resourcediscovered, resourcenotrecorded"

instance ToText ConfigurationItemStatus where
    toText = \case
        OK -> "OK"
        ResourceDeleted -> "ResourceDeleted"
        ResourceDeletedNotRecorded -> "ResourceDeletedNotRecorded"
        ResourceDiscovered -> "ResourceDiscovered"
        ResourceNotRecorded -> "ResourceNotRecorded"

instance Hashable     ConfigurationItemStatus
instance NFData       ConfigurationItemStatus
instance ToByteString ConfigurationItemStatus
instance ToQuery      ConfigurationItemStatus
instance ToHeader     ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
    parseJSON = parseJSONText "ConfigurationItemStatus"

data DeliveryStatus
  = DSFailure
  | DSNotApplicable
  | DSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeliveryStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure DSFailure
        "not_applicable" -> pure DSNotApplicable
        "success" -> pure DSSuccess
        e -> fromTextError $ "Failure parsing DeliveryStatus from value: '" <> e
           <> "'. Accepted values: failure, not_applicable, success"

instance ToText DeliveryStatus where
    toText = \case
        DSFailure -> "Failure"
        DSNotApplicable -> "Not_Applicable"
        DSSuccess -> "Success"

instance Hashable     DeliveryStatus
instance NFData       DeliveryStatus
instance ToByteString DeliveryStatus
instance ToQuery      DeliveryStatus
instance ToHeader     DeliveryStatus

instance FromJSON DeliveryStatus where
    parseJSON = parseJSONText "DeliveryStatus"

data EventSource =
  AWS_Config
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventSource where
    parser = takeLowerText >>= \case
        "aws.config" -> pure AWS_Config
        e -> fromTextError $ "Failure parsing EventSource from value: '" <> e
           <> "'. Accepted values: aws.config"

instance ToText EventSource where
    toText = \case
        AWS_Config -> "aws.config"

instance Hashable     EventSource
instance NFData       EventSource
instance ToByteString EventSource
instance ToQuery      EventSource
instance ToHeader     EventSource

instance ToJSON EventSource where
    toJSON = toJSONText

instance FromJSON EventSource where
    parseJSON = parseJSONText "EventSource"

data MaximumExecutionFrequency
  = OneHour
  | SixHours
  | ThreeHours
  | TwelveHours
  | TwentyFourHours
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MaximumExecutionFrequency where
    parser = takeLowerText >>= \case
        "one_hour" -> pure OneHour
        "six_hours" -> pure SixHours
        "three_hours" -> pure ThreeHours
        "twelve_hours" -> pure TwelveHours
        "twentyfour_hours" -> pure TwentyFourHours
        e -> fromTextError $ "Failure parsing MaximumExecutionFrequency from value: '" <> e
           <> "'. Accepted values: one_hour, six_hours, three_hours, twelve_hours, twentyfour_hours"

instance ToText MaximumExecutionFrequency where
    toText = \case
        OneHour -> "One_Hour"
        SixHours -> "Six_Hours"
        ThreeHours -> "Three_Hours"
        TwelveHours -> "Twelve_Hours"
        TwentyFourHours -> "TwentyFour_Hours"

instance Hashable     MaximumExecutionFrequency
instance NFData       MaximumExecutionFrequency
instance ToByteString MaximumExecutionFrequency
instance ToQuery      MaximumExecutionFrequency
instance ToHeader     MaximumExecutionFrequency

instance ToJSON MaximumExecutionFrequency where
    toJSON = toJSONText

instance FromJSON MaximumExecutionFrequency where
    parseJSON = parseJSONText "MaximumExecutionFrequency"

data MessageType
  = ConfigurationItemChangeNotification
  | ConfigurationSnapshotDeliveryCompleted
  | OversizedConfigurationItemChangeNotification
  | ScheduledNotification
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MessageType where
    parser = takeLowerText >>= \case
        "configurationitemchangenotification" -> pure ConfigurationItemChangeNotification
        "configurationsnapshotdeliverycompleted" -> pure ConfigurationSnapshotDeliveryCompleted
        "oversizedconfigurationitemchangenotification" -> pure OversizedConfigurationItemChangeNotification
        "schedulednotification" -> pure ScheduledNotification
        e -> fromTextError $ "Failure parsing MessageType from value: '" <> e
           <> "'. Accepted values: configurationitemchangenotification, configurationsnapshotdeliverycompleted, oversizedconfigurationitemchangenotification, schedulednotification"

instance ToText MessageType where
    toText = \case
        ConfigurationItemChangeNotification -> "ConfigurationItemChangeNotification"
        ConfigurationSnapshotDeliveryCompleted -> "ConfigurationSnapshotDeliveryCompleted"
        OversizedConfigurationItemChangeNotification -> "OversizedConfigurationItemChangeNotification"
        ScheduledNotification -> "ScheduledNotification"

instance Hashable     MessageType
instance NFData       MessageType
instance ToByteString MessageType
instance ToQuery      MessageType
instance ToHeader     MessageType

instance ToJSON MessageType where
    toJSON = toJSONText

instance FromJSON MessageType where
    parseJSON = parseJSONText "MessageType"

data Owner
  = AWS
  | CustomLambda
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Owner where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "custom_lambda" -> pure CustomLambda
        e -> fromTextError $ "Failure parsing Owner from value: '" <> e
           <> "'. Accepted values: aws, custom_lambda"

instance ToText Owner where
    toText = \case
        AWS -> "AWS"
        CustomLambda -> "CUSTOM_LAMBDA"

instance Hashable     Owner
instance NFData       Owner
instance ToByteString Owner
instance ToQuery      Owner
instance ToHeader     Owner

instance ToJSON Owner where
    toJSON = toJSONText

instance FromJSON Owner where
    parseJSON = parseJSONText "Owner"

data RecorderStatus
  = Failure
  | Pending
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecorderStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "pending" -> pure Pending
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing RecorderStatus from value: '" <> e
           <> "'. Accepted values: failure, pending, success"

instance ToText RecorderStatus where
    toText = \case
        Failure -> "Failure"
        Pending -> "Pending"
        Success -> "Success"

instance Hashable     RecorderStatus
instance NFData       RecorderStatus
instance ToByteString RecorderStatus
instance ToQuery      RecorderStatus
instance ToHeader     RecorderStatus

instance FromJSON RecorderStatus where
    parseJSON = parseJSONText "RecorderStatus"

data ResourceType
  = AWSAcmCertificate
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
  | AWSDynamoDBTable
  | AWSEC2CustomerGateway
  | AWSEC2EIP
  | AWSEC2Host
  | AWSEC2Instance
  | AWSEC2InternetGateway
  | AWSEC2NetworkACL
  | AWSEC2NetworkInterface
  | AWSEC2RouteTable
  | AWSEC2SecurityGroup
  | AWSEC2Subnet
  | AWSEC2VPC
  | AWSEC2VPNConnection
  | AWSEC2VPNGateway
  | AWSEC2Volume
  | AWSELASTICLOADBALANCINGV2LoadBalancer
  | AWSElasticBeanstalkApplication
  | AWSElasticBeanstalkApplicationVersion
  | AWSElasticBeanstalkEnvironment
  | AWSElasticLoadBalancingLoadBalancer
  | AWSIAMGroup
  | AWSIAMPolicy
  | AWSIAMRole
  | AWSIAMUser
  | AWSLambdaFunction
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
  | AWSS3Bucket
  | AWSSsmManagedInstanceInventory
  | AWSWAFRegionalRateBasedRule
  | AWSWAFRegionalRule
  | AWSWAFRegionalRuleGroup
  | AWSWAFRegionalWebACL
  | AWSWafRateBasedRule
  | AWSWafRule
  | AWSWafRuleGroup
  | AWSWafWebACL
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
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
        "aws::dynamodb::table" -> pure AWSDynamoDBTable
        "aws::ec2::customergateway" -> pure AWSEC2CustomerGateway
        "aws::ec2::eip" -> pure AWSEC2EIP
        "aws::ec2::host" -> pure AWSEC2Host
        "aws::ec2::instance" -> pure AWSEC2Instance
        "aws::ec2::internetgateway" -> pure AWSEC2InternetGateway
        "aws::ec2::networkacl" -> pure AWSEC2NetworkACL
        "aws::ec2::networkinterface" -> pure AWSEC2NetworkInterface
        "aws::ec2::routetable" -> pure AWSEC2RouteTable
        "aws::ec2::securitygroup" -> pure AWSEC2SecurityGroup
        "aws::ec2::subnet" -> pure AWSEC2Subnet
        "aws::ec2::vpc" -> pure AWSEC2VPC
        "aws::ec2::vpnconnection" -> pure AWSEC2VPNConnection
        "aws::ec2::vpngateway" -> pure AWSEC2VPNGateway
        "aws::ec2::volume" -> pure AWSEC2Volume
        "aws::elasticloadbalancingv2::loadbalancer" -> pure AWSELASTICLOADBALANCINGV2LoadBalancer
        "aws::elasticbeanstalk::application" -> pure AWSElasticBeanstalkApplication
        "aws::elasticbeanstalk::applicationversion" -> pure AWSElasticBeanstalkApplicationVersion
        "aws::elasticbeanstalk::environment" -> pure AWSElasticBeanstalkEnvironment
        "aws::elasticloadbalancing::loadbalancer" -> pure AWSElasticLoadBalancingLoadBalancer
        "aws::iam::group" -> pure AWSIAMGroup
        "aws::iam::policy" -> pure AWSIAMPolicy
        "aws::iam::role" -> pure AWSIAMRole
        "aws::iam::user" -> pure AWSIAMUser
        "aws::lambda::function" -> pure AWSLambdaFunction
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
        "aws::s3::bucket" -> pure AWSS3Bucket
        "aws::ssm::managedinstanceinventory" -> pure AWSSsmManagedInstanceInventory
        "aws::wafregional::ratebasedrule" -> pure AWSWAFRegionalRateBasedRule
        "aws::wafregional::rule" -> pure AWSWAFRegionalRule
        "aws::wafregional::rulegroup" -> pure AWSWAFRegionalRuleGroup
        "aws::wafregional::webacl" -> pure AWSWAFRegionalWebACL
        "aws::waf::ratebasedrule" -> pure AWSWafRateBasedRule
        "aws::waf::rule" -> pure AWSWafRule
        "aws::waf::rulegroup" -> pure AWSWafRuleGroup
        "aws::waf::webacl" -> pure AWSWafWebACL
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: aws::acm::certificate, aws::autoscaling::autoscalinggroup, aws::autoscaling::launchconfiguration, aws::autoscaling::scalingpolicy, aws::autoscaling::scheduledaction, aws::cloudformation::stack, aws::cloudfront::distribution, aws::cloudfront::streamingdistribution, aws::cloudtrail::trail, aws::cloudwatch::alarm, aws::codebuild::project, aws::dynamodb::table, aws::ec2::customergateway, aws::ec2::eip, aws::ec2::host, aws::ec2::instance, aws::ec2::internetgateway, aws::ec2::networkacl, aws::ec2::networkinterface, aws::ec2::routetable, aws::ec2::securitygroup, aws::ec2::subnet, aws::ec2::vpc, aws::ec2::vpnconnection, aws::ec2::vpngateway, aws::ec2::volume, aws::elasticloadbalancingv2::loadbalancer, aws::elasticbeanstalk::application, aws::elasticbeanstalk::applicationversion, aws::elasticbeanstalk::environment, aws::elasticloadbalancing::loadbalancer, aws::iam::group, aws::iam::policy, aws::iam::role, aws::iam::user, aws::lambda::function, aws::rds::dbinstance, aws::rds::dbsecuritygroup, aws::rds::dbsnapshot, aws::rds::dbsubnetgroup, aws::rds::eventsubscription, aws::redshift::cluster, aws::redshift::clusterparametergroup, aws::redshift::clustersecuritygroup, aws::redshift::clustersnapshot, aws::redshift::clustersubnetgroup, aws::redshift::eventsubscription, aws::s3::bucket, aws::ssm::managedinstanceinventory, aws::wafregional::ratebasedrule, aws::wafregional::rule, aws::wafregional::rulegroup, aws::wafregional::webacl, aws::waf::ratebasedrule, aws::waf::rule, aws::waf::rulegroup, aws::waf::webacl"

instance ToText ResourceType where
    toText = \case
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
        AWSDynamoDBTable -> "AWS::DynamoDB::Table"
        AWSEC2CustomerGateway -> "AWS::EC2::CustomerGateway"
        AWSEC2EIP -> "AWS::EC2::EIP"
        AWSEC2Host -> "AWS::EC2::Host"
        AWSEC2Instance -> "AWS::EC2::Instance"
        AWSEC2InternetGateway -> "AWS::EC2::InternetGateway"
        AWSEC2NetworkACL -> "AWS::EC2::NetworkAcl"
        AWSEC2NetworkInterface -> "AWS::EC2::NetworkInterface"
        AWSEC2RouteTable -> "AWS::EC2::RouteTable"
        AWSEC2SecurityGroup -> "AWS::EC2::SecurityGroup"
        AWSEC2Subnet -> "AWS::EC2::Subnet"
        AWSEC2VPC -> "AWS::EC2::VPC"
        AWSEC2VPNConnection -> "AWS::EC2::VPNConnection"
        AWSEC2VPNGateway -> "AWS::EC2::VPNGateway"
        AWSEC2Volume -> "AWS::EC2::Volume"
        AWSELASTICLOADBALANCINGV2LoadBalancer -> "AWS::ElasticLoadBalancingV2::LoadBalancer"
        AWSElasticBeanstalkApplication -> "AWS::ElasticBeanstalk::Application"
        AWSElasticBeanstalkApplicationVersion -> "AWS::ElasticBeanstalk::ApplicationVersion"
        AWSElasticBeanstalkEnvironment -> "AWS::ElasticBeanstalk::Environment"
        AWSElasticLoadBalancingLoadBalancer -> "AWS::ElasticLoadBalancing::LoadBalancer"
        AWSIAMGroup -> "AWS::IAM::Group"
        AWSIAMPolicy -> "AWS::IAM::Policy"
        AWSIAMRole -> "AWS::IAM::Role"
        AWSIAMUser -> "AWS::IAM::User"
        AWSLambdaFunction -> "AWS::Lambda::Function"
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
        AWSS3Bucket -> "AWS::S3::Bucket"
        AWSSsmManagedInstanceInventory -> "AWS::SSM::ManagedInstanceInventory"
        AWSWAFRegionalRateBasedRule -> "AWS::WAFRegional::RateBasedRule"
        AWSWAFRegionalRule -> "AWS::WAFRegional::Rule"
        AWSWAFRegionalRuleGroup -> "AWS::WAFRegional::RuleGroup"
        AWSWAFRegionalWebACL -> "AWS::WAFRegional::WebACL"
        AWSWafRateBasedRule -> "AWS::WAF::RateBasedRule"
        AWSWafRule -> "AWS::WAF::Rule"
        AWSWafRuleGroup -> "AWS::WAF::RuleGroup"
        AWSWafWebACL -> "AWS::WAF::WebACL"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
