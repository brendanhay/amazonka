{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.ResourceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayStageDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2ApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2StageDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupVaultDetails
import Amazonka.SecurityHub.Types.AwsBackupRecoveryPointDetails
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDetails
import Amazonka.SecurityHub.Types.AwsCloudFormationStackDetails
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails
import Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails
import Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails
import Amazonka.SecurityHub.Types.AwsDynamoDbTableDetails
import Amazonka.SecurityHub.Types.AwsEc2EipDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsEc2SubnetDetails
import Amazonka.SecurityHub.Types.AwsEc2TransitGatewayDetails
import Amazonka.SecurityHub.Types.AwsEc2VolumeDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionDetails
import Amazonka.SecurityHub.Types.AwsEcrContainerImageDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDetails
import Amazonka.SecurityHub.Types.AwsEcsContainerDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDetails
import Amazonka.SecurityHub.Types.AwsEfsAccessPointDetails
import Amazonka.SecurityHub.Types.AwsEksClusterDetails
import Amazonka.SecurityHub.Types.AwsElasticBeanstalkEnvironmentDetails
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainDetails
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerDetails
import Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerDetails
import Amazonka.SecurityHub.Types.AwsIamAccessKeyDetails
import Amazonka.SecurityHub.Types.AwsIamGroupDetails
import Amazonka.SecurityHub.Types.AwsIamPolicyDetails
import Amazonka.SecurityHub.Types.AwsIamRoleDetails
import Amazonka.SecurityHub.Types.AwsIamUserDetails
import Amazonka.SecurityHub.Types.AwsKinesisStreamDetails
import Amazonka.SecurityHub.Types.AwsKmsKeyDetails
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDetails
import Amazonka.SecurityHub.Types.AwsLambdaLayerVersionDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallPolicyDetails
import Amazonka.SecurityHub.Types.AwsNetworkFirewallRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceDetails
import Amazonka.SecurityHub.Types.AwsRdsDbSecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsRdsDbSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsEventSubscriptionDetails
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails
import Amazonka.SecurityHub.Types.AwsS3AccountPublicAccessBlockDetails
import Amazonka.SecurityHub.Types.AwsS3BucketDetails
import Amazonka.SecurityHub.Types.AwsS3ObjectDetails
import Amazonka.SecurityHub.Types.AwsSecretsManagerSecretDetails
import Amazonka.SecurityHub.Types.AwsSnsTopicDetails
import Amazonka.SecurityHub.Types.AwsSqsQueueDetails
import Amazonka.SecurityHub.Types.AwsSsmPatchComplianceDetails
import Amazonka.SecurityHub.Types.AwsWafRateBasedRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRateBasedRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafRegionalWebAclDetails
import Amazonka.SecurityHub.Types.AwsWafRuleDetails
import Amazonka.SecurityHub.Types.AwsWafRuleGroupDetails
import Amazonka.SecurityHub.Types.AwsWafWebAclDetails
import Amazonka.SecurityHub.Types.AwsXrayEncryptionConfigDetails
import Amazonka.SecurityHub.Types.ContainerDetails

-- | Additional details about a resource related to a finding.
--
-- To provide the details, use the object that corresponds to the resource
-- type. For example, if the resource type is @AwsEc2Instance@, then you
-- use the @AwsEc2Instance@ object to provide the details.
--
-- If the type-specific object does not contain all of the fields you want
-- to populate, then you use the @Other@ object to populate those
-- additional fields.
--
-- You also use the @Other@ object to populate the details when the
-- selected type does not have a corresponding object.
--
-- /See:/ 'newResourceDetails' smart constructor.
data ResourceDetails = ResourceDetails'
  { -- | Details about an Network Firewall rule group.
    awsNetworkFirewallRuleGroup :: Prelude.Maybe AwsNetworkFirewallRuleGroupDetails,
    -- | Details about an EC2 instance related to a finding.
    awsEc2Instance :: Prelude.Maybe AwsEc2InstanceDetails,
    -- | Details about an IAM role.
    awsIamRole :: Prelude.Maybe AwsIamRoleDetails,
    -- | Details about the Amazon S3 Public Access Block configuration for an
    -- account.
    awsS3AccountPublicAccessBlock :: Prelude.Maybe AwsS3AccountPublicAccessBlockDetails,
    -- | Details about an SQS queue.
    awsSqsQueue :: Prelude.Maybe AwsSqsQueueDetails,
    -- | Details about a load balancer.
    awsElbv2LoadBalancer :: Prelude.Maybe AwsElbv2LoadBalancerDetails,
    -- | Details about an Amazon CloudWatch alarm. An alarm allows you to monitor
    -- and receive alerts about your Amazon Web Services resources and
    -- applications across multiple Regions.
    awsCloudWatchAlarm :: Prelude.Maybe AwsCloudWatchAlarmDetails,
    -- | Details about an IAM permissions policy.
    awsIamPolicy :: Prelude.Maybe AwsIamPolicyDetails,
    -- | Details for an EC2 network interface.
    awsEc2NetworkInterface :: Prelude.Maybe AwsEc2NetworkInterfaceDetails,
    -- | Details about an Amazon EKS cluster.
    awsEksCluster :: Prelude.Maybe AwsEksClusterDetails,
    -- | Details about a rate-based rule for global resources.
    awsWafRateBasedRule :: Prelude.Maybe AwsWafRateBasedRuleDetails,
    -- | Provides information about a Docker container that\'s part of a task.
    awsEcsContainer :: Prelude.Maybe AwsEcsContainerDetails,
    -- | Details about an RDS event notification subscription.
    awsRdsEventSubscription :: Prelude.Maybe AwsRdsEventSubscriptionDetails,
    -- | Details about an CloudFormation stack. A stack is a collection of Amazon
    -- Web Services resources that you can manage as a single unit.
    awsCloudFormationStack :: Prelude.Maybe AwsCloudFormationStackDetails,
    -- | Details for an WAF web ACL.
    awsWafWebAcl :: Prelude.Maybe AwsWafWebAclDetails,
    -- | Details about a resource that are not available in a type-specific
    -- details object. Use the @Other@ object in the following cases.
    --
    -- -   The type-specific object does not contain all of the fields that you
    --     want to populate. In this case, first use the type-specific object
    --     to populate those fields. Use the @Other@ object to populate the
    --     fields that are missing from the type-specific object.
    --
    -- -   The resource type does not have a corresponding object. This
    --     includes resources for which the type is @Other@.
    other :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Provides information about the state of a patch on an instance based on
    -- the patch baseline that was used to patch the instance.
    awsSsmPatchCompliance :: Prelude.Maybe AwsSsmPatchComplianceDetails,
    -- | Details about an Amazon OpenSearch Service domain.
    awsOpenSearchServiceDomain :: Prelude.Maybe AwsOpenSearchServiceDomainDetails,
    -- | Details about a DynamoDB table.
    awsDynamoDbTable :: Prelude.Maybe AwsDynamoDbTableDetails,
    -- | Details for an Amazon EC2 volume.
    awsEc2Volume :: Prelude.Maybe AwsEc2VolumeDetails,
    -- | Provides details about an Certificate Manager certificate.
    awsCertificateManagerCertificate :: Prelude.Maybe AwsCertificateManagerCertificateDetails,
    -- | Details about an Amazon EC2 VPN connection.
    awsEc2VpnConnection :: Prelude.Maybe AwsEc2VpnConnectionDetails,
    -- | Details about a subnet in Amazon EC2.
    awsEc2Subnet :: Prelude.Maybe AwsEc2SubnetDetails,
    -- | Contains details about an IAM group.
    awsIamGroup :: Prelude.Maybe AwsIamGroupDetails,
    -- | Details about an IAM user.
    awsIamUser :: Prelude.Maybe AwsIamUserDetails,
    -- | Provides details about an Backup backup vault.
    awsBackupBackupVault :: Prelude.Maybe AwsBackupBackupVaultDetails,
    -- | Provides information about a version 2 API in Amazon API Gateway.
    awsApiGatewayV2Api :: Prelude.Maybe AwsApiGatewayV2ApiDetails,
    -- | Details about a task definition. A task definition describes the
    -- container and volume definitions of an Amazon Elastic Container Service
    -- task.
    awsEcsTaskDefinition :: Prelude.Maybe AwsEcsTaskDefinitionDetails,
    -- | Details for an autoscaling group.
    awsAutoScalingAutoScalingGroup :: Prelude.Maybe AwsAutoScalingAutoScalingGroupDetails,
    -- | Details about an EC2 network access control list (ACL).
    awsEc2NetworkAcl :: Prelude.Maybe AwsEc2NetworkAclDetails,
    -- | Details about an Amazon EFS access point. An access point is an
    -- application-specific view into an EFS file system that applies an
    -- operating system user and group, and a file system path, to any file
    -- system request made through the access point.
    awsEfsAccessPoint :: Prelude.Maybe AwsEfsAccessPointDetails,
    -- | Details about the service configuration for a VPC endpoint service.
    awsEc2VpcEndpointService :: Prelude.Maybe AwsEc2VpcEndpointServiceDetails,
    -- | Details for an CodeBuild project.
    awsCodeBuildProject :: Prelude.Maybe AwsCodeBuildProjectDetails,
    -- | Details about an Amazon RDS database instance.
    awsRdsDbInstance :: Prelude.Maybe AwsRdsDbInstanceDetails,
    -- | Details for an Elasticsearch domain.
    awsElasticsearchDomain :: Prelude.Maybe AwsElasticsearchDomainDetails,
    -- | Information about an Amazon ECR image.
    awsEcrContainerImage :: Prelude.Maybe AwsEcrContainerImageDetails,
    -- | Provides details about a launch configuration.
    awsAutoScalingLaunchConfiguration :: Prelude.Maybe AwsAutoScalingLaunchConfigurationDetails,
    -- | Details about an IAM access key related to a finding.
    awsIamAccessKey :: Prelude.Maybe AwsIamAccessKeyDetails,
    -- | Contains details about an Amazon Redshift cluster.
    awsRedshiftCluster :: Prelude.Maybe AwsRedshiftClusterDetails,
    -- | Details about an Network Firewall firewall policy.
    awsNetworkFirewallFirewallPolicy :: Prelude.Maybe AwsNetworkFirewallFirewallPolicyDetails,
    -- | Details for an Amazon EC2 VPC.
    awsEc2Vpc :: Prelude.Maybe AwsEc2VpcDetails,
    -- | Information about an Amazon Elastic Container Registry repository.
    awsEcrRepository :: Prelude.Maybe AwsEcrRepositoryDetails,
    -- | Details about an WAF rule group for Regional resources.
    awsWafRegionalRuleGroup :: Prelude.Maybe AwsWafRegionalRuleGroupDetails,
    -- | Details about a rate-based rule for Regional resources.
    awsWafRegionalRateBasedRule :: Prelude.Maybe AwsWafRegionalRateBasedRuleDetails,
    -- | Details about an WAF rule for Regional resources.
    awsWafRegionalRule :: Prelude.Maybe AwsWafRegionalRuleDetails,
    -- | Provides information about a version 2 stage for Amazon API Gateway.
    awsApiGatewayV2Stage :: Prelude.Maybe AwsApiGatewayV2StageDetails,
    -- | Details about an Amazon RDS database cluster.
    awsRdsDbCluster :: Prelude.Maybe AwsRdsDbClusterDetails,
    -- | Details about an S3 object related to a finding.
    awsS3Object :: Prelude.Maybe AwsS3ObjectDetails,
    -- | Details about an Network Firewall firewall.
    awsNetworkFirewallFirewall :: Prelude.Maybe AwsNetworkFirewallFirewallDetails,
    -- | Details about an WAF rule group for global resources.
    awsWafRuleGroup :: Prelude.Maybe AwsWafRuleGroupDetails,
    -- | Details about an Amazon EC2 VPC peering connection. A VPC peering
    -- connection is a networking connection between two VPCs that enables you
    -- to route traffic between them privately.
    awsEc2VpcPeeringConnection :: Prelude.Maybe AwsEc2VpcPeeringConnectionDetails,
    -- | Details about an Amazon EC2 transit gateway that interconnects your
    -- virtual private clouds (VPC) and on-premises networks.
    awsEc2TransitGateway :: Prelude.Maybe AwsEc2TransitGatewayDetails,
    -- | Details about an S3 bucket related to a finding.
    awsS3Bucket :: Prelude.Maybe AwsS3BucketDetails,
    -- | Details about an WAF web access control list (web ACL) for Regional
    -- resources.
    awsWafRegionalWebAcl :: Prelude.Maybe AwsWafRegionalWebAclDetails,
    -- | Details about an Elastic IP address.
    awsEc2Eip :: Prelude.Maybe AwsEc2EipDetails,
    -- | Provides details about an Backup backup plan.
    awsBackupBackupPlan :: Prelude.Maybe AwsBackupBackupPlanDetails,
    -- | Details about a Lambda function.
    awsLambdaFunction :: Prelude.Maybe AwsLambdaFunctionDetails,
    -- | Details about a container resource related to a finding.
    container :: Prelude.Maybe ContainerDetails,
    -- | Details about an Amazon RDS database cluster snapshot.
    awsRdsDbClusterSnapshot :: Prelude.Maybe AwsRdsDbClusterSnapshotDetails,
    -- | Contains details about a Classic Load Balancer.
    awsElbLoadBalancer :: Prelude.Maybe AwsElbLoadBalancerDetails,
    -- | Provides details about a CloudTrail trail.
    awsCloudTrailTrail :: Prelude.Maybe AwsCloudTrailTrailDetails,
    -- | Details about a service within an ECS cluster.
    awsEcsService :: Prelude.Maybe AwsEcsServiceDetails,
    -- | Provides information about a version 1 Amazon API Gateway stage.
    awsApiGatewayStage :: Prelude.Maybe AwsApiGatewayStageDetails,
    -- | Details about an SNS topic.
    awsSnsTopic :: Prelude.Maybe AwsSnsTopicDetails,
    -- | Details about an Amazon RDS DB security group.
    awsRdsDbSecurityGroup :: Prelude.Maybe AwsRdsDbSecurityGroupDetails,
    -- | Details about an Amazon Kinesis data stream.
    awsKinesisStream :: Prelude.Maybe AwsKinesisStreamDetails,
    -- | Details for an EC2 security group.
    awsEc2SecurityGroup :: Prelude.Maybe AwsEc2SecurityGroupDetails,
    -- | Details about an Amazon ECS cluster.
    awsEcsCluster :: Prelude.Maybe AwsEcsClusterDetails,
    -- | Provides information about a REST API in version 1 of Amazon API
    -- Gateway.
    awsApiGatewayRestApi :: Prelude.Maybe AwsApiGatewayRestApiDetails,
    -- | Details about an WAF rule for global resources.
    awsWafRule :: Prelude.Maybe AwsWafRuleDetails,
    -- | Provides details about an Backup backup, or recovery point.
    awsBackupRecoveryPoint :: Prelude.Maybe AwsBackupRecoveryPointDetails,
    -- | Details about an Amazon RDS database snapshot.
    awsRdsDbSnapshot :: Prelude.Maybe AwsRdsDbSnapshotDetails,
    -- | Details about an Elastic Beanstalk environment.
    awsElasticBeanstalkEnvironment :: Prelude.Maybe AwsElasticBeanstalkEnvironmentDetails,
    -- | Details about a task in a cluster.
    awsEcsTask :: Prelude.Maybe AwsEcsTaskDetails,
    -- | Information about the encryption configuration for X-Ray.
    awsXrayEncryptionConfig :: Prelude.Maybe AwsXrayEncryptionConfigDetails,
    -- | Details for a Lambda layer version.
    awsLambdaLayerVersion :: Prelude.Maybe AwsLambdaLayerVersionDetails,
    -- | Details about an KMS key.
    awsKmsKey :: Prelude.Maybe AwsKmsKeyDetails,
    -- | Details about a Secrets Manager secret.
    awsSecretsManagerSecret :: Prelude.Maybe AwsSecretsManagerSecretDetails,
    -- | Details about a CloudFront distribution.
    awsCloudFrontDistribution :: Prelude.Maybe AwsCloudFrontDistributionDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsNetworkFirewallRuleGroup', 'resourceDetails_awsNetworkFirewallRuleGroup' - Details about an Network Firewall rule group.
--
-- 'awsEc2Instance', 'resourceDetails_awsEc2Instance' - Details about an EC2 instance related to a finding.
--
-- 'awsIamRole', 'resourceDetails_awsIamRole' - Details about an IAM role.
--
-- 'awsS3AccountPublicAccessBlock', 'resourceDetails_awsS3AccountPublicAccessBlock' - Details about the Amazon S3 Public Access Block configuration for an
-- account.
--
-- 'awsSqsQueue', 'resourceDetails_awsSqsQueue' - Details about an SQS queue.
--
-- 'awsElbv2LoadBalancer', 'resourceDetails_awsElbv2LoadBalancer' - Details about a load balancer.
--
-- 'awsCloudWatchAlarm', 'resourceDetails_awsCloudWatchAlarm' - Details about an Amazon CloudWatch alarm. An alarm allows you to monitor
-- and receive alerts about your Amazon Web Services resources and
-- applications across multiple Regions.
--
-- 'awsIamPolicy', 'resourceDetails_awsIamPolicy' - Details about an IAM permissions policy.
--
-- 'awsEc2NetworkInterface', 'resourceDetails_awsEc2NetworkInterface' - Details for an EC2 network interface.
--
-- 'awsEksCluster', 'resourceDetails_awsEksCluster' - Details about an Amazon EKS cluster.
--
-- 'awsWafRateBasedRule', 'resourceDetails_awsWafRateBasedRule' - Details about a rate-based rule for global resources.
--
-- 'awsEcsContainer', 'resourceDetails_awsEcsContainer' - Provides information about a Docker container that\'s part of a task.
--
-- 'awsRdsEventSubscription', 'resourceDetails_awsRdsEventSubscription' - Details about an RDS event notification subscription.
--
-- 'awsCloudFormationStack', 'resourceDetails_awsCloudFormationStack' - Details about an CloudFormation stack. A stack is a collection of Amazon
-- Web Services resources that you can manage as a single unit.
--
-- 'awsWafWebAcl', 'resourceDetails_awsWafWebAcl' - Details for an WAF web ACL.
--
-- 'other', 'resourceDetails_other' - Details about a resource that are not available in a type-specific
-- details object. Use the @Other@ object in the following cases.
--
-- -   The type-specific object does not contain all of the fields that you
--     want to populate. In this case, first use the type-specific object
--     to populate those fields. Use the @Other@ object to populate the
--     fields that are missing from the type-specific object.
--
-- -   The resource type does not have a corresponding object. This
--     includes resources for which the type is @Other@.
--
-- 'awsSsmPatchCompliance', 'resourceDetails_awsSsmPatchCompliance' - Provides information about the state of a patch on an instance based on
-- the patch baseline that was used to patch the instance.
--
-- 'awsOpenSearchServiceDomain', 'resourceDetails_awsOpenSearchServiceDomain' - Details about an Amazon OpenSearch Service domain.
--
-- 'awsDynamoDbTable', 'resourceDetails_awsDynamoDbTable' - Details about a DynamoDB table.
--
-- 'awsEc2Volume', 'resourceDetails_awsEc2Volume' - Details for an Amazon EC2 volume.
--
-- 'awsCertificateManagerCertificate', 'resourceDetails_awsCertificateManagerCertificate' - Provides details about an Certificate Manager certificate.
--
-- 'awsEc2VpnConnection', 'resourceDetails_awsEc2VpnConnection' - Details about an Amazon EC2 VPN connection.
--
-- 'awsEc2Subnet', 'resourceDetails_awsEc2Subnet' - Details about a subnet in Amazon EC2.
--
-- 'awsIamGroup', 'resourceDetails_awsIamGroup' - Contains details about an IAM group.
--
-- 'awsIamUser', 'resourceDetails_awsIamUser' - Details about an IAM user.
--
-- 'awsBackupBackupVault', 'resourceDetails_awsBackupBackupVault' - Provides details about an Backup backup vault.
--
-- 'awsApiGatewayV2Api', 'resourceDetails_awsApiGatewayV2Api' - Provides information about a version 2 API in Amazon API Gateway.
--
-- 'awsEcsTaskDefinition', 'resourceDetails_awsEcsTaskDefinition' - Details about a task definition. A task definition describes the
-- container and volume definitions of an Amazon Elastic Container Service
-- task.
--
-- 'awsAutoScalingAutoScalingGroup', 'resourceDetails_awsAutoScalingAutoScalingGroup' - Details for an autoscaling group.
--
-- 'awsEc2NetworkAcl', 'resourceDetails_awsEc2NetworkAcl' - Details about an EC2 network access control list (ACL).
--
-- 'awsEfsAccessPoint', 'resourceDetails_awsEfsAccessPoint' - Details about an Amazon EFS access point. An access point is an
-- application-specific view into an EFS file system that applies an
-- operating system user and group, and a file system path, to any file
-- system request made through the access point.
--
-- 'awsEc2VpcEndpointService', 'resourceDetails_awsEc2VpcEndpointService' - Details about the service configuration for a VPC endpoint service.
--
-- 'awsCodeBuildProject', 'resourceDetails_awsCodeBuildProject' - Details for an CodeBuild project.
--
-- 'awsRdsDbInstance', 'resourceDetails_awsRdsDbInstance' - Details about an Amazon RDS database instance.
--
-- 'awsElasticsearchDomain', 'resourceDetails_awsElasticsearchDomain' - Details for an Elasticsearch domain.
--
-- 'awsEcrContainerImage', 'resourceDetails_awsEcrContainerImage' - Information about an Amazon ECR image.
--
-- 'awsAutoScalingLaunchConfiguration', 'resourceDetails_awsAutoScalingLaunchConfiguration' - Provides details about a launch configuration.
--
-- 'awsIamAccessKey', 'resourceDetails_awsIamAccessKey' - Details about an IAM access key related to a finding.
--
-- 'awsRedshiftCluster', 'resourceDetails_awsRedshiftCluster' - Contains details about an Amazon Redshift cluster.
--
-- 'awsNetworkFirewallFirewallPolicy', 'resourceDetails_awsNetworkFirewallFirewallPolicy' - Details about an Network Firewall firewall policy.
--
-- 'awsEc2Vpc', 'resourceDetails_awsEc2Vpc' - Details for an Amazon EC2 VPC.
--
-- 'awsEcrRepository', 'resourceDetails_awsEcrRepository' - Information about an Amazon Elastic Container Registry repository.
--
-- 'awsWafRegionalRuleGroup', 'resourceDetails_awsWafRegionalRuleGroup' - Details about an WAF rule group for Regional resources.
--
-- 'awsWafRegionalRateBasedRule', 'resourceDetails_awsWafRegionalRateBasedRule' - Details about a rate-based rule for Regional resources.
--
-- 'awsWafRegionalRule', 'resourceDetails_awsWafRegionalRule' - Details about an WAF rule for Regional resources.
--
-- 'awsApiGatewayV2Stage', 'resourceDetails_awsApiGatewayV2Stage' - Provides information about a version 2 stage for Amazon API Gateway.
--
-- 'awsRdsDbCluster', 'resourceDetails_awsRdsDbCluster' - Details about an Amazon RDS database cluster.
--
-- 'awsS3Object', 'resourceDetails_awsS3Object' - Details about an S3 object related to a finding.
--
-- 'awsNetworkFirewallFirewall', 'resourceDetails_awsNetworkFirewallFirewall' - Details about an Network Firewall firewall.
--
-- 'awsWafRuleGroup', 'resourceDetails_awsWafRuleGroup' - Details about an WAF rule group for global resources.
--
-- 'awsEc2VpcPeeringConnection', 'resourceDetails_awsEc2VpcPeeringConnection' - Details about an Amazon EC2 VPC peering connection. A VPC peering
-- connection is a networking connection between two VPCs that enables you
-- to route traffic between them privately.
--
-- 'awsEc2TransitGateway', 'resourceDetails_awsEc2TransitGateway' - Details about an Amazon EC2 transit gateway that interconnects your
-- virtual private clouds (VPC) and on-premises networks.
--
-- 'awsS3Bucket', 'resourceDetails_awsS3Bucket' - Details about an S3 bucket related to a finding.
--
-- 'awsWafRegionalWebAcl', 'resourceDetails_awsWafRegionalWebAcl' - Details about an WAF web access control list (web ACL) for Regional
-- resources.
--
-- 'awsEc2Eip', 'resourceDetails_awsEc2Eip' - Details about an Elastic IP address.
--
-- 'awsBackupBackupPlan', 'resourceDetails_awsBackupBackupPlan' - Provides details about an Backup backup plan.
--
-- 'awsLambdaFunction', 'resourceDetails_awsLambdaFunction' - Details about a Lambda function.
--
-- 'container', 'resourceDetails_container' - Details about a container resource related to a finding.
--
-- 'awsRdsDbClusterSnapshot', 'resourceDetails_awsRdsDbClusterSnapshot' - Details about an Amazon RDS database cluster snapshot.
--
-- 'awsElbLoadBalancer', 'resourceDetails_awsElbLoadBalancer' - Contains details about a Classic Load Balancer.
--
-- 'awsCloudTrailTrail', 'resourceDetails_awsCloudTrailTrail' - Provides details about a CloudTrail trail.
--
-- 'awsEcsService', 'resourceDetails_awsEcsService' - Details about a service within an ECS cluster.
--
-- 'awsApiGatewayStage', 'resourceDetails_awsApiGatewayStage' - Provides information about a version 1 Amazon API Gateway stage.
--
-- 'awsSnsTopic', 'resourceDetails_awsSnsTopic' - Details about an SNS topic.
--
-- 'awsRdsDbSecurityGroup', 'resourceDetails_awsRdsDbSecurityGroup' - Details about an Amazon RDS DB security group.
--
-- 'awsKinesisStream', 'resourceDetails_awsKinesisStream' - Details about an Amazon Kinesis data stream.
--
-- 'awsEc2SecurityGroup', 'resourceDetails_awsEc2SecurityGroup' - Details for an EC2 security group.
--
-- 'awsEcsCluster', 'resourceDetails_awsEcsCluster' - Details about an Amazon ECS cluster.
--
-- 'awsApiGatewayRestApi', 'resourceDetails_awsApiGatewayRestApi' - Provides information about a REST API in version 1 of Amazon API
-- Gateway.
--
-- 'awsWafRule', 'resourceDetails_awsWafRule' - Details about an WAF rule for global resources.
--
-- 'awsBackupRecoveryPoint', 'resourceDetails_awsBackupRecoveryPoint' - Provides details about an Backup backup, or recovery point.
--
-- 'awsRdsDbSnapshot', 'resourceDetails_awsRdsDbSnapshot' - Details about an Amazon RDS database snapshot.
--
-- 'awsElasticBeanstalkEnvironment', 'resourceDetails_awsElasticBeanstalkEnvironment' - Details about an Elastic Beanstalk environment.
--
-- 'awsEcsTask', 'resourceDetails_awsEcsTask' - Details about a task in a cluster.
--
-- 'awsXrayEncryptionConfig', 'resourceDetails_awsXrayEncryptionConfig' - Information about the encryption configuration for X-Ray.
--
-- 'awsLambdaLayerVersion', 'resourceDetails_awsLambdaLayerVersion' - Details for a Lambda layer version.
--
-- 'awsKmsKey', 'resourceDetails_awsKmsKey' - Details about an KMS key.
--
-- 'awsSecretsManagerSecret', 'resourceDetails_awsSecretsManagerSecret' - Details about a Secrets Manager secret.
--
-- 'awsCloudFrontDistribution', 'resourceDetails_awsCloudFrontDistribution' - Details about a CloudFront distribution.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { awsNetworkFirewallRuleGroup =
        Prelude.Nothing,
      awsEc2Instance = Prelude.Nothing,
      awsIamRole = Prelude.Nothing,
      awsS3AccountPublicAccessBlock = Prelude.Nothing,
      awsSqsQueue = Prelude.Nothing,
      awsElbv2LoadBalancer = Prelude.Nothing,
      awsCloudWatchAlarm = Prelude.Nothing,
      awsIamPolicy = Prelude.Nothing,
      awsEc2NetworkInterface = Prelude.Nothing,
      awsEksCluster = Prelude.Nothing,
      awsWafRateBasedRule = Prelude.Nothing,
      awsEcsContainer = Prelude.Nothing,
      awsRdsEventSubscription = Prelude.Nothing,
      awsCloudFormationStack = Prelude.Nothing,
      awsWafWebAcl = Prelude.Nothing,
      other = Prelude.Nothing,
      awsSsmPatchCompliance = Prelude.Nothing,
      awsOpenSearchServiceDomain = Prelude.Nothing,
      awsDynamoDbTable = Prelude.Nothing,
      awsEc2Volume = Prelude.Nothing,
      awsCertificateManagerCertificate = Prelude.Nothing,
      awsEc2VpnConnection = Prelude.Nothing,
      awsEc2Subnet = Prelude.Nothing,
      awsIamGroup = Prelude.Nothing,
      awsIamUser = Prelude.Nothing,
      awsBackupBackupVault = Prelude.Nothing,
      awsApiGatewayV2Api = Prelude.Nothing,
      awsEcsTaskDefinition = Prelude.Nothing,
      awsAutoScalingAutoScalingGroup = Prelude.Nothing,
      awsEc2NetworkAcl = Prelude.Nothing,
      awsEfsAccessPoint = Prelude.Nothing,
      awsEc2VpcEndpointService = Prelude.Nothing,
      awsCodeBuildProject = Prelude.Nothing,
      awsRdsDbInstance = Prelude.Nothing,
      awsElasticsearchDomain = Prelude.Nothing,
      awsEcrContainerImage = Prelude.Nothing,
      awsAutoScalingLaunchConfiguration = Prelude.Nothing,
      awsIamAccessKey = Prelude.Nothing,
      awsRedshiftCluster = Prelude.Nothing,
      awsNetworkFirewallFirewallPolicy = Prelude.Nothing,
      awsEc2Vpc = Prelude.Nothing,
      awsEcrRepository = Prelude.Nothing,
      awsWafRegionalRuleGroup = Prelude.Nothing,
      awsWafRegionalRateBasedRule = Prelude.Nothing,
      awsWafRegionalRule = Prelude.Nothing,
      awsApiGatewayV2Stage = Prelude.Nothing,
      awsRdsDbCluster = Prelude.Nothing,
      awsS3Object = Prelude.Nothing,
      awsNetworkFirewallFirewall = Prelude.Nothing,
      awsWafRuleGroup = Prelude.Nothing,
      awsEc2VpcPeeringConnection = Prelude.Nothing,
      awsEc2TransitGateway = Prelude.Nothing,
      awsS3Bucket = Prelude.Nothing,
      awsWafRegionalWebAcl = Prelude.Nothing,
      awsEc2Eip = Prelude.Nothing,
      awsBackupBackupPlan = Prelude.Nothing,
      awsLambdaFunction = Prelude.Nothing,
      container = Prelude.Nothing,
      awsRdsDbClusterSnapshot = Prelude.Nothing,
      awsElbLoadBalancer = Prelude.Nothing,
      awsCloudTrailTrail = Prelude.Nothing,
      awsEcsService = Prelude.Nothing,
      awsApiGatewayStage = Prelude.Nothing,
      awsSnsTopic = Prelude.Nothing,
      awsRdsDbSecurityGroup = Prelude.Nothing,
      awsKinesisStream = Prelude.Nothing,
      awsEc2SecurityGroup = Prelude.Nothing,
      awsEcsCluster = Prelude.Nothing,
      awsApiGatewayRestApi = Prelude.Nothing,
      awsWafRule = Prelude.Nothing,
      awsBackupRecoveryPoint = Prelude.Nothing,
      awsRdsDbSnapshot = Prelude.Nothing,
      awsElasticBeanstalkEnvironment = Prelude.Nothing,
      awsEcsTask = Prelude.Nothing,
      awsXrayEncryptionConfig = Prelude.Nothing,
      awsLambdaLayerVersion = Prelude.Nothing,
      awsKmsKey = Prelude.Nothing,
      awsSecretsManagerSecret = Prelude.Nothing,
      awsCloudFrontDistribution = Prelude.Nothing
    }

-- | Details about an Network Firewall rule group.
resourceDetails_awsNetworkFirewallRuleGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsNetworkFirewallRuleGroupDetails)
resourceDetails_awsNetworkFirewallRuleGroup = Lens.lens (\ResourceDetails' {awsNetworkFirewallRuleGroup} -> awsNetworkFirewallRuleGroup) (\s@ResourceDetails' {} a -> s {awsNetworkFirewallRuleGroup = a} :: ResourceDetails)

-- | Details about an EC2 instance related to a finding.
resourceDetails_awsEc2Instance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2InstanceDetails)
resourceDetails_awsEc2Instance = Lens.lens (\ResourceDetails' {awsEc2Instance} -> awsEc2Instance) (\s@ResourceDetails' {} a -> s {awsEc2Instance = a} :: ResourceDetails)

-- | Details about an IAM role.
resourceDetails_awsIamRole :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamRoleDetails)
resourceDetails_awsIamRole = Lens.lens (\ResourceDetails' {awsIamRole} -> awsIamRole) (\s@ResourceDetails' {} a -> s {awsIamRole = a} :: ResourceDetails)

-- | Details about the Amazon S3 Public Access Block configuration for an
-- account.
resourceDetails_awsS3AccountPublicAccessBlock :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsS3AccountPublicAccessBlockDetails)
resourceDetails_awsS3AccountPublicAccessBlock = Lens.lens (\ResourceDetails' {awsS3AccountPublicAccessBlock} -> awsS3AccountPublicAccessBlock) (\s@ResourceDetails' {} a -> s {awsS3AccountPublicAccessBlock = a} :: ResourceDetails)

-- | Details about an SQS queue.
resourceDetails_awsSqsQueue :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSqsQueueDetails)
resourceDetails_awsSqsQueue = Lens.lens (\ResourceDetails' {awsSqsQueue} -> awsSqsQueue) (\s@ResourceDetails' {} a -> s {awsSqsQueue = a} :: ResourceDetails)

-- | Details about a load balancer.
resourceDetails_awsElbv2LoadBalancer :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElbv2LoadBalancerDetails)
resourceDetails_awsElbv2LoadBalancer = Lens.lens (\ResourceDetails' {awsElbv2LoadBalancer} -> awsElbv2LoadBalancer) (\s@ResourceDetails' {} a -> s {awsElbv2LoadBalancer = a} :: ResourceDetails)

-- | Details about an Amazon CloudWatch alarm. An alarm allows you to monitor
-- and receive alerts about your Amazon Web Services resources and
-- applications across multiple Regions.
resourceDetails_awsCloudWatchAlarm :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCloudWatchAlarmDetails)
resourceDetails_awsCloudWatchAlarm = Lens.lens (\ResourceDetails' {awsCloudWatchAlarm} -> awsCloudWatchAlarm) (\s@ResourceDetails' {} a -> s {awsCloudWatchAlarm = a} :: ResourceDetails)

-- | Details about an IAM permissions policy.
resourceDetails_awsIamPolicy :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamPolicyDetails)
resourceDetails_awsIamPolicy = Lens.lens (\ResourceDetails' {awsIamPolicy} -> awsIamPolicy) (\s@ResourceDetails' {} a -> s {awsIamPolicy = a} :: ResourceDetails)

-- | Details for an EC2 network interface.
resourceDetails_awsEc2NetworkInterface :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2NetworkInterfaceDetails)
resourceDetails_awsEc2NetworkInterface = Lens.lens (\ResourceDetails' {awsEc2NetworkInterface} -> awsEc2NetworkInterface) (\s@ResourceDetails' {} a -> s {awsEc2NetworkInterface = a} :: ResourceDetails)

-- | Details about an Amazon EKS cluster.
resourceDetails_awsEksCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEksClusterDetails)
resourceDetails_awsEksCluster = Lens.lens (\ResourceDetails' {awsEksCluster} -> awsEksCluster) (\s@ResourceDetails' {} a -> s {awsEksCluster = a} :: ResourceDetails)

-- | Details about a rate-based rule for global resources.
resourceDetails_awsWafRateBasedRule :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRateBasedRuleDetails)
resourceDetails_awsWafRateBasedRule = Lens.lens (\ResourceDetails' {awsWafRateBasedRule} -> awsWafRateBasedRule) (\s@ResourceDetails' {} a -> s {awsWafRateBasedRule = a} :: ResourceDetails)

-- | Provides information about a Docker container that\'s part of a task.
resourceDetails_awsEcsContainer :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsContainerDetails)
resourceDetails_awsEcsContainer = Lens.lens (\ResourceDetails' {awsEcsContainer} -> awsEcsContainer) (\s@ResourceDetails' {} a -> s {awsEcsContainer = a} :: ResourceDetails)

-- | Details about an RDS event notification subscription.
resourceDetails_awsRdsEventSubscription :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsEventSubscriptionDetails)
resourceDetails_awsRdsEventSubscription = Lens.lens (\ResourceDetails' {awsRdsEventSubscription} -> awsRdsEventSubscription) (\s@ResourceDetails' {} a -> s {awsRdsEventSubscription = a} :: ResourceDetails)

-- | Details about an CloudFormation stack. A stack is a collection of Amazon
-- Web Services resources that you can manage as a single unit.
resourceDetails_awsCloudFormationStack :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCloudFormationStackDetails)
resourceDetails_awsCloudFormationStack = Lens.lens (\ResourceDetails' {awsCloudFormationStack} -> awsCloudFormationStack) (\s@ResourceDetails' {} a -> s {awsCloudFormationStack = a} :: ResourceDetails)

-- | Details for an WAF web ACL.
resourceDetails_awsWafWebAcl :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafWebAclDetails)
resourceDetails_awsWafWebAcl = Lens.lens (\ResourceDetails' {awsWafWebAcl} -> awsWafWebAcl) (\s@ResourceDetails' {} a -> s {awsWafWebAcl = a} :: ResourceDetails)

-- | Details about a resource that are not available in a type-specific
-- details object. Use the @Other@ object in the following cases.
--
-- -   The type-specific object does not contain all of the fields that you
--     want to populate. In this case, first use the type-specific object
--     to populate those fields. Use the @Other@ object to populate the
--     fields that are missing from the type-specific object.
--
-- -   The resource type does not have a corresponding object. This
--     includes resources for which the type is @Other@.
resourceDetails_other :: Lens.Lens' ResourceDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resourceDetails_other = Lens.lens (\ResourceDetails' {other} -> other) (\s@ResourceDetails' {} a -> s {other = a} :: ResourceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Provides information about the state of a patch on an instance based on
-- the patch baseline that was used to patch the instance.
resourceDetails_awsSsmPatchCompliance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSsmPatchComplianceDetails)
resourceDetails_awsSsmPatchCompliance = Lens.lens (\ResourceDetails' {awsSsmPatchCompliance} -> awsSsmPatchCompliance) (\s@ResourceDetails' {} a -> s {awsSsmPatchCompliance = a} :: ResourceDetails)

-- | Details about an Amazon OpenSearch Service domain.
resourceDetails_awsOpenSearchServiceDomain :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsOpenSearchServiceDomainDetails)
resourceDetails_awsOpenSearchServiceDomain = Lens.lens (\ResourceDetails' {awsOpenSearchServiceDomain} -> awsOpenSearchServiceDomain) (\s@ResourceDetails' {} a -> s {awsOpenSearchServiceDomain = a} :: ResourceDetails)

-- | Details about a DynamoDB table.
resourceDetails_awsDynamoDbTable :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsDynamoDbTableDetails)
resourceDetails_awsDynamoDbTable = Lens.lens (\ResourceDetails' {awsDynamoDbTable} -> awsDynamoDbTable) (\s@ResourceDetails' {} a -> s {awsDynamoDbTable = a} :: ResourceDetails)

-- | Details for an Amazon EC2 volume.
resourceDetails_awsEc2Volume :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VolumeDetails)
resourceDetails_awsEc2Volume = Lens.lens (\ResourceDetails' {awsEc2Volume} -> awsEc2Volume) (\s@ResourceDetails' {} a -> s {awsEc2Volume = a} :: ResourceDetails)

-- | Provides details about an Certificate Manager certificate.
resourceDetails_awsCertificateManagerCertificate :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCertificateManagerCertificateDetails)
resourceDetails_awsCertificateManagerCertificate = Lens.lens (\ResourceDetails' {awsCertificateManagerCertificate} -> awsCertificateManagerCertificate) (\s@ResourceDetails' {} a -> s {awsCertificateManagerCertificate = a} :: ResourceDetails)

-- | Details about an Amazon EC2 VPN connection.
resourceDetails_awsEc2VpnConnection :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpnConnectionDetails)
resourceDetails_awsEc2VpnConnection = Lens.lens (\ResourceDetails' {awsEc2VpnConnection} -> awsEc2VpnConnection) (\s@ResourceDetails' {} a -> s {awsEc2VpnConnection = a} :: ResourceDetails)

-- | Details about a subnet in Amazon EC2.
resourceDetails_awsEc2Subnet :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2SubnetDetails)
resourceDetails_awsEc2Subnet = Lens.lens (\ResourceDetails' {awsEc2Subnet} -> awsEc2Subnet) (\s@ResourceDetails' {} a -> s {awsEc2Subnet = a} :: ResourceDetails)

-- | Contains details about an IAM group.
resourceDetails_awsIamGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamGroupDetails)
resourceDetails_awsIamGroup = Lens.lens (\ResourceDetails' {awsIamGroup} -> awsIamGroup) (\s@ResourceDetails' {} a -> s {awsIamGroup = a} :: ResourceDetails)

-- | Details about an IAM user.
resourceDetails_awsIamUser :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamUserDetails)
resourceDetails_awsIamUser = Lens.lens (\ResourceDetails' {awsIamUser} -> awsIamUser) (\s@ResourceDetails' {} a -> s {awsIamUser = a} :: ResourceDetails)

-- | Provides details about an Backup backup vault.
resourceDetails_awsBackupBackupVault :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsBackupBackupVaultDetails)
resourceDetails_awsBackupBackupVault = Lens.lens (\ResourceDetails' {awsBackupBackupVault} -> awsBackupBackupVault) (\s@ResourceDetails' {} a -> s {awsBackupBackupVault = a} :: ResourceDetails)

-- | Provides information about a version 2 API in Amazon API Gateway.
resourceDetails_awsApiGatewayV2Api :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayV2ApiDetails)
resourceDetails_awsApiGatewayV2Api = Lens.lens (\ResourceDetails' {awsApiGatewayV2Api} -> awsApiGatewayV2Api) (\s@ResourceDetails' {} a -> s {awsApiGatewayV2Api = a} :: ResourceDetails)

-- | Details about a task definition. A task definition describes the
-- container and volume definitions of an Amazon Elastic Container Service
-- task.
resourceDetails_awsEcsTaskDefinition :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsTaskDefinitionDetails)
resourceDetails_awsEcsTaskDefinition = Lens.lens (\ResourceDetails' {awsEcsTaskDefinition} -> awsEcsTaskDefinition) (\s@ResourceDetails' {} a -> s {awsEcsTaskDefinition = a} :: ResourceDetails)

-- | Details for an autoscaling group.
resourceDetails_awsAutoScalingAutoScalingGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupDetails)
resourceDetails_awsAutoScalingAutoScalingGroup = Lens.lens (\ResourceDetails' {awsAutoScalingAutoScalingGroup} -> awsAutoScalingAutoScalingGroup) (\s@ResourceDetails' {} a -> s {awsAutoScalingAutoScalingGroup = a} :: ResourceDetails)

-- | Details about an EC2 network access control list (ACL).
resourceDetails_awsEc2NetworkAcl :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2NetworkAclDetails)
resourceDetails_awsEc2NetworkAcl = Lens.lens (\ResourceDetails' {awsEc2NetworkAcl} -> awsEc2NetworkAcl) (\s@ResourceDetails' {} a -> s {awsEc2NetworkAcl = a} :: ResourceDetails)

-- | Details about an Amazon EFS access point. An access point is an
-- application-specific view into an EFS file system that applies an
-- operating system user and group, and a file system path, to any file
-- system request made through the access point.
resourceDetails_awsEfsAccessPoint :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEfsAccessPointDetails)
resourceDetails_awsEfsAccessPoint = Lens.lens (\ResourceDetails' {awsEfsAccessPoint} -> awsEfsAccessPoint) (\s@ResourceDetails' {} a -> s {awsEfsAccessPoint = a} :: ResourceDetails)

-- | Details about the service configuration for a VPC endpoint service.
resourceDetails_awsEc2VpcEndpointService :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpcEndpointServiceDetails)
resourceDetails_awsEc2VpcEndpointService = Lens.lens (\ResourceDetails' {awsEc2VpcEndpointService} -> awsEc2VpcEndpointService) (\s@ResourceDetails' {} a -> s {awsEc2VpcEndpointService = a} :: ResourceDetails)

-- | Details for an CodeBuild project.
resourceDetails_awsCodeBuildProject :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCodeBuildProjectDetails)
resourceDetails_awsCodeBuildProject = Lens.lens (\ResourceDetails' {awsCodeBuildProject} -> awsCodeBuildProject) (\s@ResourceDetails' {} a -> s {awsCodeBuildProject = a} :: ResourceDetails)

-- | Details about an Amazon RDS database instance.
resourceDetails_awsRdsDbInstance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbInstanceDetails)
resourceDetails_awsRdsDbInstance = Lens.lens (\ResourceDetails' {awsRdsDbInstance} -> awsRdsDbInstance) (\s@ResourceDetails' {} a -> s {awsRdsDbInstance = a} :: ResourceDetails)

-- | Details for an Elasticsearch domain.
resourceDetails_awsElasticsearchDomain :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElasticsearchDomainDetails)
resourceDetails_awsElasticsearchDomain = Lens.lens (\ResourceDetails' {awsElasticsearchDomain} -> awsElasticsearchDomain) (\s@ResourceDetails' {} a -> s {awsElasticsearchDomain = a} :: ResourceDetails)

-- | Information about an Amazon ECR image.
resourceDetails_awsEcrContainerImage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcrContainerImageDetails)
resourceDetails_awsEcrContainerImage = Lens.lens (\ResourceDetails' {awsEcrContainerImage} -> awsEcrContainerImage) (\s@ResourceDetails' {} a -> s {awsEcrContainerImage = a} :: ResourceDetails)

-- | Provides details about a launch configuration.
resourceDetails_awsAutoScalingLaunchConfiguration :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsAutoScalingLaunchConfigurationDetails)
resourceDetails_awsAutoScalingLaunchConfiguration = Lens.lens (\ResourceDetails' {awsAutoScalingLaunchConfiguration} -> awsAutoScalingLaunchConfiguration) (\s@ResourceDetails' {} a -> s {awsAutoScalingLaunchConfiguration = a} :: ResourceDetails)

-- | Details about an IAM access key related to a finding.
resourceDetails_awsIamAccessKey :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamAccessKeyDetails)
resourceDetails_awsIamAccessKey = Lens.lens (\ResourceDetails' {awsIamAccessKey} -> awsIamAccessKey) (\s@ResourceDetails' {} a -> s {awsIamAccessKey = a} :: ResourceDetails)

-- | Contains details about an Amazon Redshift cluster.
resourceDetails_awsRedshiftCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRedshiftClusterDetails)
resourceDetails_awsRedshiftCluster = Lens.lens (\ResourceDetails' {awsRedshiftCluster} -> awsRedshiftCluster) (\s@ResourceDetails' {} a -> s {awsRedshiftCluster = a} :: ResourceDetails)

-- | Details about an Network Firewall firewall policy.
resourceDetails_awsNetworkFirewallFirewallPolicy :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsNetworkFirewallFirewallPolicyDetails)
resourceDetails_awsNetworkFirewallFirewallPolicy = Lens.lens (\ResourceDetails' {awsNetworkFirewallFirewallPolicy} -> awsNetworkFirewallFirewallPolicy) (\s@ResourceDetails' {} a -> s {awsNetworkFirewallFirewallPolicy = a} :: ResourceDetails)

-- | Details for an Amazon EC2 VPC.
resourceDetails_awsEc2Vpc :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpcDetails)
resourceDetails_awsEc2Vpc = Lens.lens (\ResourceDetails' {awsEc2Vpc} -> awsEc2Vpc) (\s@ResourceDetails' {} a -> s {awsEc2Vpc = a} :: ResourceDetails)

-- | Information about an Amazon Elastic Container Registry repository.
resourceDetails_awsEcrRepository :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcrRepositoryDetails)
resourceDetails_awsEcrRepository = Lens.lens (\ResourceDetails' {awsEcrRepository} -> awsEcrRepository) (\s@ResourceDetails' {} a -> s {awsEcrRepository = a} :: ResourceDetails)

-- | Details about an WAF rule group for Regional resources.
resourceDetails_awsWafRegionalRuleGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRegionalRuleGroupDetails)
resourceDetails_awsWafRegionalRuleGroup = Lens.lens (\ResourceDetails' {awsWafRegionalRuleGroup} -> awsWafRegionalRuleGroup) (\s@ResourceDetails' {} a -> s {awsWafRegionalRuleGroup = a} :: ResourceDetails)

-- | Details about a rate-based rule for Regional resources.
resourceDetails_awsWafRegionalRateBasedRule :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRegionalRateBasedRuleDetails)
resourceDetails_awsWafRegionalRateBasedRule = Lens.lens (\ResourceDetails' {awsWafRegionalRateBasedRule} -> awsWafRegionalRateBasedRule) (\s@ResourceDetails' {} a -> s {awsWafRegionalRateBasedRule = a} :: ResourceDetails)

-- | Details about an WAF rule for Regional resources.
resourceDetails_awsWafRegionalRule :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRegionalRuleDetails)
resourceDetails_awsWafRegionalRule = Lens.lens (\ResourceDetails' {awsWafRegionalRule} -> awsWafRegionalRule) (\s@ResourceDetails' {} a -> s {awsWafRegionalRule = a} :: ResourceDetails)

-- | Provides information about a version 2 stage for Amazon API Gateway.
resourceDetails_awsApiGatewayV2Stage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayV2StageDetails)
resourceDetails_awsApiGatewayV2Stage = Lens.lens (\ResourceDetails' {awsApiGatewayV2Stage} -> awsApiGatewayV2Stage) (\s@ResourceDetails' {} a -> s {awsApiGatewayV2Stage = a} :: ResourceDetails)

-- | Details about an Amazon RDS database cluster.
resourceDetails_awsRdsDbCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbClusterDetails)
resourceDetails_awsRdsDbCluster = Lens.lens (\ResourceDetails' {awsRdsDbCluster} -> awsRdsDbCluster) (\s@ResourceDetails' {} a -> s {awsRdsDbCluster = a} :: ResourceDetails)

-- | Details about an S3 object related to a finding.
resourceDetails_awsS3Object :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsS3ObjectDetails)
resourceDetails_awsS3Object = Lens.lens (\ResourceDetails' {awsS3Object} -> awsS3Object) (\s@ResourceDetails' {} a -> s {awsS3Object = a} :: ResourceDetails)

-- | Details about an Network Firewall firewall.
resourceDetails_awsNetworkFirewallFirewall :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsNetworkFirewallFirewallDetails)
resourceDetails_awsNetworkFirewallFirewall = Lens.lens (\ResourceDetails' {awsNetworkFirewallFirewall} -> awsNetworkFirewallFirewall) (\s@ResourceDetails' {} a -> s {awsNetworkFirewallFirewall = a} :: ResourceDetails)

-- | Details about an WAF rule group for global resources.
resourceDetails_awsWafRuleGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRuleGroupDetails)
resourceDetails_awsWafRuleGroup = Lens.lens (\ResourceDetails' {awsWafRuleGroup} -> awsWafRuleGroup) (\s@ResourceDetails' {} a -> s {awsWafRuleGroup = a} :: ResourceDetails)

-- | Details about an Amazon EC2 VPC peering connection. A VPC peering
-- connection is a networking connection between two VPCs that enables you
-- to route traffic between them privately.
resourceDetails_awsEc2VpcPeeringConnection :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionDetails)
resourceDetails_awsEc2VpcPeeringConnection = Lens.lens (\ResourceDetails' {awsEc2VpcPeeringConnection} -> awsEc2VpcPeeringConnection) (\s@ResourceDetails' {} a -> s {awsEc2VpcPeeringConnection = a} :: ResourceDetails)

-- | Details about an Amazon EC2 transit gateway that interconnects your
-- virtual private clouds (VPC) and on-premises networks.
resourceDetails_awsEc2TransitGateway :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2TransitGatewayDetails)
resourceDetails_awsEc2TransitGateway = Lens.lens (\ResourceDetails' {awsEc2TransitGateway} -> awsEc2TransitGateway) (\s@ResourceDetails' {} a -> s {awsEc2TransitGateway = a} :: ResourceDetails)

-- | Details about an S3 bucket related to a finding.
resourceDetails_awsS3Bucket :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsS3BucketDetails)
resourceDetails_awsS3Bucket = Lens.lens (\ResourceDetails' {awsS3Bucket} -> awsS3Bucket) (\s@ResourceDetails' {} a -> s {awsS3Bucket = a} :: ResourceDetails)

-- | Details about an WAF web access control list (web ACL) for Regional
-- resources.
resourceDetails_awsWafRegionalWebAcl :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRegionalWebAclDetails)
resourceDetails_awsWafRegionalWebAcl = Lens.lens (\ResourceDetails' {awsWafRegionalWebAcl} -> awsWafRegionalWebAcl) (\s@ResourceDetails' {} a -> s {awsWafRegionalWebAcl = a} :: ResourceDetails)

-- | Details about an Elastic IP address.
resourceDetails_awsEc2Eip :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2EipDetails)
resourceDetails_awsEc2Eip = Lens.lens (\ResourceDetails' {awsEc2Eip} -> awsEc2Eip) (\s@ResourceDetails' {} a -> s {awsEc2Eip = a} :: ResourceDetails)

-- | Provides details about an Backup backup plan.
resourceDetails_awsBackupBackupPlan :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsBackupBackupPlanDetails)
resourceDetails_awsBackupBackupPlan = Lens.lens (\ResourceDetails' {awsBackupBackupPlan} -> awsBackupBackupPlan) (\s@ResourceDetails' {} a -> s {awsBackupBackupPlan = a} :: ResourceDetails)

-- | Details about a Lambda function.
resourceDetails_awsLambdaFunction :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsLambdaFunctionDetails)
resourceDetails_awsLambdaFunction = Lens.lens (\ResourceDetails' {awsLambdaFunction} -> awsLambdaFunction) (\s@ResourceDetails' {} a -> s {awsLambdaFunction = a} :: ResourceDetails)

-- | Details about a container resource related to a finding.
resourceDetails_container :: Lens.Lens' ResourceDetails (Prelude.Maybe ContainerDetails)
resourceDetails_container = Lens.lens (\ResourceDetails' {container} -> container) (\s@ResourceDetails' {} a -> s {container = a} :: ResourceDetails)

-- | Details about an Amazon RDS database cluster snapshot.
resourceDetails_awsRdsDbClusterSnapshot :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbClusterSnapshotDetails)
resourceDetails_awsRdsDbClusterSnapshot = Lens.lens (\ResourceDetails' {awsRdsDbClusterSnapshot} -> awsRdsDbClusterSnapshot) (\s@ResourceDetails' {} a -> s {awsRdsDbClusterSnapshot = a} :: ResourceDetails)

-- | Contains details about a Classic Load Balancer.
resourceDetails_awsElbLoadBalancer :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElbLoadBalancerDetails)
resourceDetails_awsElbLoadBalancer = Lens.lens (\ResourceDetails' {awsElbLoadBalancer} -> awsElbLoadBalancer) (\s@ResourceDetails' {} a -> s {awsElbLoadBalancer = a} :: ResourceDetails)

-- | Provides details about a CloudTrail trail.
resourceDetails_awsCloudTrailTrail :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCloudTrailTrailDetails)
resourceDetails_awsCloudTrailTrail = Lens.lens (\ResourceDetails' {awsCloudTrailTrail} -> awsCloudTrailTrail) (\s@ResourceDetails' {} a -> s {awsCloudTrailTrail = a} :: ResourceDetails)

-- | Details about a service within an ECS cluster.
resourceDetails_awsEcsService :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsServiceDetails)
resourceDetails_awsEcsService = Lens.lens (\ResourceDetails' {awsEcsService} -> awsEcsService) (\s@ResourceDetails' {} a -> s {awsEcsService = a} :: ResourceDetails)

-- | Provides information about a version 1 Amazon API Gateway stage.
resourceDetails_awsApiGatewayStage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayStageDetails)
resourceDetails_awsApiGatewayStage = Lens.lens (\ResourceDetails' {awsApiGatewayStage} -> awsApiGatewayStage) (\s@ResourceDetails' {} a -> s {awsApiGatewayStage = a} :: ResourceDetails)

-- | Details about an SNS topic.
resourceDetails_awsSnsTopic :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSnsTopicDetails)
resourceDetails_awsSnsTopic = Lens.lens (\ResourceDetails' {awsSnsTopic} -> awsSnsTopic) (\s@ResourceDetails' {} a -> s {awsSnsTopic = a} :: ResourceDetails)

-- | Details about an Amazon RDS DB security group.
resourceDetails_awsRdsDbSecurityGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbSecurityGroupDetails)
resourceDetails_awsRdsDbSecurityGroup = Lens.lens (\ResourceDetails' {awsRdsDbSecurityGroup} -> awsRdsDbSecurityGroup) (\s@ResourceDetails' {} a -> s {awsRdsDbSecurityGroup = a} :: ResourceDetails)

-- | Details about an Amazon Kinesis data stream.
resourceDetails_awsKinesisStream :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsKinesisStreamDetails)
resourceDetails_awsKinesisStream = Lens.lens (\ResourceDetails' {awsKinesisStream} -> awsKinesisStream) (\s@ResourceDetails' {} a -> s {awsKinesisStream = a} :: ResourceDetails)

-- | Details for an EC2 security group.
resourceDetails_awsEc2SecurityGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2SecurityGroupDetails)
resourceDetails_awsEc2SecurityGroup = Lens.lens (\ResourceDetails' {awsEc2SecurityGroup} -> awsEc2SecurityGroup) (\s@ResourceDetails' {} a -> s {awsEc2SecurityGroup = a} :: ResourceDetails)

-- | Details about an Amazon ECS cluster.
resourceDetails_awsEcsCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsClusterDetails)
resourceDetails_awsEcsCluster = Lens.lens (\ResourceDetails' {awsEcsCluster} -> awsEcsCluster) (\s@ResourceDetails' {} a -> s {awsEcsCluster = a} :: ResourceDetails)

-- | Provides information about a REST API in version 1 of Amazon API
-- Gateway.
resourceDetails_awsApiGatewayRestApi :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayRestApiDetails)
resourceDetails_awsApiGatewayRestApi = Lens.lens (\ResourceDetails' {awsApiGatewayRestApi} -> awsApiGatewayRestApi) (\s@ResourceDetails' {} a -> s {awsApiGatewayRestApi = a} :: ResourceDetails)

-- | Details about an WAF rule for global resources.
resourceDetails_awsWafRule :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRuleDetails)
resourceDetails_awsWafRule = Lens.lens (\ResourceDetails' {awsWafRule} -> awsWafRule) (\s@ResourceDetails' {} a -> s {awsWafRule = a} :: ResourceDetails)

-- | Provides details about an Backup backup, or recovery point.
resourceDetails_awsBackupRecoveryPoint :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsBackupRecoveryPointDetails)
resourceDetails_awsBackupRecoveryPoint = Lens.lens (\ResourceDetails' {awsBackupRecoveryPoint} -> awsBackupRecoveryPoint) (\s@ResourceDetails' {} a -> s {awsBackupRecoveryPoint = a} :: ResourceDetails)

-- | Details about an Amazon RDS database snapshot.
resourceDetails_awsRdsDbSnapshot :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbSnapshotDetails)
resourceDetails_awsRdsDbSnapshot = Lens.lens (\ResourceDetails' {awsRdsDbSnapshot} -> awsRdsDbSnapshot) (\s@ResourceDetails' {} a -> s {awsRdsDbSnapshot = a} :: ResourceDetails)

-- | Details about an Elastic Beanstalk environment.
resourceDetails_awsElasticBeanstalkEnvironment :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElasticBeanstalkEnvironmentDetails)
resourceDetails_awsElasticBeanstalkEnvironment = Lens.lens (\ResourceDetails' {awsElasticBeanstalkEnvironment} -> awsElasticBeanstalkEnvironment) (\s@ResourceDetails' {} a -> s {awsElasticBeanstalkEnvironment = a} :: ResourceDetails)

-- | Details about a task in a cluster.
resourceDetails_awsEcsTask :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsTaskDetails)
resourceDetails_awsEcsTask = Lens.lens (\ResourceDetails' {awsEcsTask} -> awsEcsTask) (\s@ResourceDetails' {} a -> s {awsEcsTask = a} :: ResourceDetails)

-- | Information about the encryption configuration for X-Ray.
resourceDetails_awsXrayEncryptionConfig :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsXrayEncryptionConfigDetails)
resourceDetails_awsXrayEncryptionConfig = Lens.lens (\ResourceDetails' {awsXrayEncryptionConfig} -> awsXrayEncryptionConfig) (\s@ResourceDetails' {} a -> s {awsXrayEncryptionConfig = a} :: ResourceDetails)

-- | Details for a Lambda layer version.
resourceDetails_awsLambdaLayerVersion :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsLambdaLayerVersionDetails)
resourceDetails_awsLambdaLayerVersion = Lens.lens (\ResourceDetails' {awsLambdaLayerVersion} -> awsLambdaLayerVersion) (\s@ResourceDetails' {} a -> s {awsLambdaLayerVersion = a} :: ResourceDetails)

-- | Details about an KMS key.
resourceDetails_awsKmsKey :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsKmsKeyDetails)
resourceDetails_awsKmsKey = Lens.lens (\ResourceDetails' {awsKmsKey} -> awsKmsKey) (\s@ResourceDetails' {} a -> s {awsKmsKey = a} :: ResourceDetails)

-- | Details about a Secrets Manager secret.
resourceDetails_awsSecretsManagerSecret :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSecretsManagerSecretDetails)
resourceDetails_awsSecretsManagerSecret = Lens.lens (\ResourceDetails' {awsSecretsManagerSecret} -> awsSecretsManagerSecret) (\s@ResourceDetails' {} a -> s {awsSecretsManagerSecret = a} :: ResourceDetails)

-- | Details about a CloudFront distribution.
resourceDetails_awsCloudFrontDistribution :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCloudFrontDistributionDetails)
resourceDetails_awsCloudFrontDistribution = Lens.lens (\ResourceDetails' {awsCloudFrontDistribution} -> awsCloudFrontDistribution) (\s@ResourceDetails' {} a -> s {awsCloudFrontDistribution = a} :: ResourceDetails)

instance Core.FromJSON ResourceDetails where
  parseJSON =
    Core.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Core..:? "AwsNetworkFirewallRuleGroup")
            Prelude.<*> (x Core..:? "AwsEc2Instance")
            Prelude.<*> (x Core..:? "AwsIamRole")
            Prelude.<*> (x Core..:? "AwsS3AccountPublicAccessBlock")
            Prelude.<*> (x Core..:? "AwsSqsQueue")
            Prelude.<*> (x Core..:? "AwsElbv2LoadBalancer")
            Prelude.<*> (x Core..:? "AwsCloudWatchAlarm")
            Prelude.<*> (x Core..:? "AwsIamPolicy")
            Prelude.<*> (x Core..:? "AwsEc2NetworkInterface")
            Prelude.<*> (x Core..:? "AwsEksCluster")
            Prelude.<*> (x Core..:? "AwsWafRateBasedRule")
            Prelude.<*> (x Core..:? "AwsEcsContainer")
            Prelude.<*> (x Core..:? "AwsRdsEventSubscription")
            Prelude.<*> (x Core..:? "AwsCloudFormationStack")
            Prelude.<*> (x Core..:? "AwsWafWebAcl")
            Prelude.<*> (x Core..:? "Other" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AwsSsmPatchCompliance")
            Prelude.<*> (x Core..:? "AwsOpenSearchServiceDomain")
            Prelude.<*> (x Core..:? "AwsDynamoDbTable")
            Prelude.<*> (x Core..:? "AwsEc2Volume")
            Prelude.<*> (x Core..:? "AwsCertificateManagerCertificate")
            Prelude.<*> (x Core..:? "AwsEc2VpnConnection")
            Prelude.<*> (x Core..:? "AwsEc2Subnet")
            Prelude.<*> (x Core..:? "AwsIamGroup")
            Prelude.<*> (x Core..:? "AwsIamUser")
            Prelude.<*> (x Core..:? "AwsBackupBackupVault")
            Prelude.<*> (x Core..:? "AwsApiGatewayV2Api")
            Prelude.<*> (x Core..:? "AwsEcsTaskDefinition")
            Prelude.<*> (x Core..:? "AwsAutoScalingAutoScalingGroup")
            Prelude.<*> (x Core..:? "AwsEc2NetworkAcl")
            Prelude.<*> (x Core..:? "AwsEfsAccessPoint")
            Prelude.<*> (x Core..:? "AwsEc2VpcEndpointService")
            Prelude.<*> (x Core..:? "AwsCodeBuildProject")
            Prelude.<*> (x Core..:? "AwsRdsDbInstance")
            Prelude.<*> (x Core..:? "AwsElasticsearchDomain")
            Prelude.<*> (x Core..:? "AwsEcrContainerImage")
            Prelude.<*> (x Core..:? "AwsAutoScalingLaunchConfiguration")
            Prelude.<*> (x Core..:? "AwsIamAccessKey")
            Prelude.<*> (x Core..:? "AwsRedshiftCluster")
            Prelude.<*> (x Core..:? "AwsNetworkFirewallFirewallPolicy")
            Prelude.<*> (x Core..:? "AwsEc2Vpc")
            Prelude.<*> (x Core..:? "AwsEcrRepository")
            Prelude.<*> (x Core..:? "AwsWafRegionalRuleGroup")
            Prelude.<*> (x Core..:? "AwsWafRegionalRateBasedRule")
            Prelude.<*> (x Core..:? "AwsWafRegionalRule")
            Prelude.<*> (x Core..:? "AwsApiGatewayV2Stage")
            Prelude.<*> (x Core..:? "AwsRdsDbCluster")
            Prelude.<*> (x Core..:? "AwsS3Object")
            Prelude.<*> (x Core..:? "AwsNetworkFirewallFirewall")
            Prelude.<*> (x Core..:? "AwsWafRuleGroup")
            Prelude.<*> (x Core..:? "AwsEc2VpcPeeringConnection")
            Prelude.<*> (x Core..:? "AwsEc2TransitGateway")
            Prelude.<*> (x Core..:? "AwsS3Bucket")
            Prelude.<*> (x Core..:? "AwsWafRegionalWebAcl")
            Prelude.<*> (x Core..:? "AwsEc2Eip")
            Prelude.<*> (x Core..:? "AwsBackupBackupPlan")
            Prelude.<*> (x Core..:? "AwsLambdaFunction")
            Prelude.<*> (x Core..:? "Container")
            Prelude.<*> (x Core..:? "AwsRdsDbClusterSnapshot")
            Prelude.<*> (x Core..:? "AwsElbLoadBalancer")
            Prelude.<*> (x Core..:? "AwsCloudTrailTrail")
            Prelude.<*> (x Core..:? "AwsEcsService")
            Prelude.<*> (x Core..:? "AwsApiGatewayStage")
            Prelude.<*> (x Core..:? "AwsSnsTopic")
            Prelude.<*> (x Core..:? "AwsRdsDbSecurityGroup")
            Prelude.<*> (x Core..:? "AwsKinesisStream")
            Prelude.<*> (x Core..:? "AwsEc2SecurityGroup")
            Prelude.<*> (x Core..:? "AwsEcsCluster")
            Prelude.<*> (x Core..:? "AwsApiGatewayRestApi")
            Prelude.<*> (x Core..:? "AwsWafRule")
            Prelude.<*> (x Core..:? "AwsBackupRecoveryPoint")
            Prelude.<*> (x Core..:? "AwsRdsDbSnapshot")
            Prelude.<*> (x Core..:? "AwsElasticBeanstalkEnvironment")
            Prelude.<*> (x Core..:? "AwsEcsTask")
            Prelude.<*> (x Core..:? "AwsXrayEncryptionConfig")
            Prelude.<*> (x Core..:? "AwsLambdaLayerVersion")
            Prelude.<*> (x Core..:? "AwsKmsKey")
            Prelude.<*> (x Core..:? "AwsSecretsManagerSecret")
            Prelude.<*> (x Core..:? "AwsCloudFrontDistribution")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt _salt ResourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` awsNetworkFirewallRuleGroup
      `Prelude.hashWithSalt` awsEc2Instance
      `Prelude.hashWithSalt` awsIamRole
      `Prelude.hashWithSalt` awsS3AccountPublicAccessBlock
      `Prelude.hashWithSalt` awsSqsQueue
      `Prelude.hashWithSalt` awsElbv2LoadBalancer
      `Prelude.hashWithSalt` awsCloudWatchAlarm
      `Prelude.hashWithSalt` awsIamPolicy
      `Prelude.hashWithSalt` awsEc2NetworkInterface
      `Prelude.hashWithSalt` awsEksCluster
      `Prelude.hashWithSalt` awsWafRateBasedRule
      `Prelude.hashWithSalt` awsEcsContainer
      `Prelude.hashWithSalt` awsRdsEventSubscription
      `Prelude.hashWithSalt` awsCloudFormationStack
      `Prelude.hashWithSalt` awsWafWebAcl
      `Prelude.hashWithSalt` other
      `Prelude.hashWithSalt` awsSsmPatchCompliance
      `Prelude.hashWithSalt` awsOpenSearchServiceDomain
      `Prelude.hashWithSalt` awsDynamoDbTable
      `Prelude.hashWithSalt` awsEc2Volume
      `Prelude.hashWithSalt` awsCertificateManagerCertificate
      `Prelude.hashWithSalt` awsEc2VpnConnection
      `Prelude.hashWithSalt` awsEc2Subnet
      `Prelude.hashWithSalt` awsIamGroup
      `Prelude.hashWithSalt` awsIamUser
      `Prelude.hashWithSalt` awsBackupBackupVault
      `Prelude.hashWithSalt` awsApiGatewayV2Api
      `Prelude.hashWithSalt` awsEcsTaskDefinition
      `Prelude.hashWithSalt` awsAutoScalingAutoScalingGroup
      `Prelude.hashWithSalt` awsEc2NetworkAcl
      `Prelude.hashWithSalt` awsEfsAccessPoint
      `Prelude.hashWithSalt` awsEc2VpcEndpointService
      `Prelude.hashWithSalt` awsCodeBuildProject
      `Prelude.hashWithSalt` awsRdsDbInstance
      `Prelude.hashWithSalt` awsElasticsearchDomain
      `Prelude.hashWithSalt` awsEcrContainerImage
      `Prelude.hashWithSalt` awsAutoScalingLaunchConfiguration
      `Prelude.hashWithSalt` awsIamAccessKey
      `Prelude.hashWithSalt` awsRedshiftCluster
      `Prelude.hashWithSalt` awsNetworkFirewallFirewallPolicy
      `Prelude.hashWithSalt` awsEc2Vpc
      `Prelude.hashWithSalt` awsEcrRepository
      `Prelude.hashWithSalt` awsWafRegionalRuleGroup
      `Prelude.hashWithSalt` awsWafRegionalRateBasedRule
      `Prelude.hashWithSalt` awsWafRegionalRule
      `Prelude.hashWithSalt` awsApiGatewayV2Stage
      `Prelude.hashWithSalt` awsRdsDbCluster
      `Prelude.hashWithSalt` awsS3Object
      `Prelude.hashWithSalt` awsNetworkFirewallFirewall
      `Prelude.hashWithSalt` awsWafRuleGroup
      `Prelude.hashWithSalt` awsEc2VpcPeeringConnection
      `Prelude.hashWithSalt` awsEc2TransitGateway
      `Prelude.hashWithSalt` awsS3Bucket
      `Prelude.hashWithSalt` awsWafRegionalWebAcl
      `Prelude.hashWithSalt` awsEc2Eip
      `Prelude.hashWithSalt` awsBackupBackupPlan
      `Prelude.hashWithSalt` awsLambdaFunction
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` awsRdsDbClusterSnapshot
      `Prelude.hashWithSalt` awsElbLoadBalancer
      `Prelude.hashWithSalt` awsCloudTrailTrail
      `Prelude.hashWithSalt` awsEcsService
      `Prelude.hashWithSalt` awsApiGatewayStage
      `Prelude.hashWithSalt` awsSnsTopic
      `Prelude.hashWithSalt` awsRdsDbSecurityGroup
      `Prelude.hashWithSalt` awsKinesisStream
      `Prelude.hashWithSalt` awsEc2SecurityGroup
      `Prelude.hashWithSalt` awsEcsCluster
      `Prelude.hashWithSalt` awsApiGatewayRestApi
      `Prelude.hashWithSalt` awsWafRule
      `Prelude.hashWithSalt` awsBackupRecoveryPoint
      `Prelude.hashWithSalt` awsRdsDbSnapshot
      `Prelude.hashWithSalt` awsElasticBeanstalkEnvironment
      `Prelude.hashWithSalt` awsEcsTask
      `Prelude.hashWithSalt` awsXrayEncryptionConfig
      `Prelude.hashWithSalt` awsLambdaLayerVersion
      `Prelude.hashWithSalt` awsKmsKey
      `Prelude.hashWithSalt` awsSecretsManagerSecret
      `Prelude.hashWithSalt` awsCloudFrontDistribution

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf awsNetworkFirewallRuleGroup
      `Prelude.seq` Prelude.rnf awsEc2Instance
      `Prelude.seq` Prelude.rnf awsIamRole
      `Prelude.seq` Prelude.rnf awsS3AccountPublicAccessBlock
      `Prelude.seq` Prelude.rnf awsSqsQueue
      `Prelude.seq` Prelude.rnf awsElbv2LoadBalancer
      `Prelude.seq` Prelude.rnf awsCloudWatchAlarm
      `Prelude.seq` Prelude.rnf awsIamPolicy
      `Prelude.seq` Prelude.rnf awsEc2NetworkInterface
      `Prelude.seq` Prelude.rnf awsEksCluster
      `Prelude.seq` Prelude.rnf awsWafRateBasedRule
      `Prelude.seq` Prelude.rnf awsEcsContainer
      `Prelude.seq` Prelude.rnf awsRdsEventSubscription
      `Prelude.seq` Prelude.rnf awsCloudFormationStack
      `Prelude.seq` Prelude.rnf awsWafWebAcl
      `Prelude.seq` Prelude.rnf other
      `Prelude.seq` Prelude.rnf awsSsmPatchCompliance
      `Prelude.seq` Prelude.rnf
        awsOpenSearchServiceDomain
      `Prelude.seq` Prelude.rnf awsDynamoDbTable
      `Prelude.seq` Prelude.rnf awsEc2Volume
      `Prelude.seq` Prelude.rnf
        awsCertificateManagerCertificate
      `Prelude.seq` Prelude.rnf
        awsEc2VpnConnection
      `Prelude.seq` Prelude.rnf awsEc2Subnet
      `Prelude.seq` Prelude.rnf
        awsIamGroup
      `Prelude.seq` Prelude.rnf
        awsIamUser
      `Prelude.seq` Prelude.rnf
        awsBackupBackupVault
      `Prelude.seq` Prelude.rnf
        awsApiGatewayV2Api
      `Prelude.seq` Prelude.rnf
        awsEcsTaskDefinition
      `Prelude.seq` Prelude.rnf
        awsAutoScalingAutoScalingGroup
      `Prelude.seq` Prelude.rnf
        awsEc2NetworkAcl
      `Prelude.seq` Prelude.rnf
        awsEfsAccessPoint
      `Prelude.seq` Prelude.rnf
        awsEc2VpcEndpointService
      `Prelude.seq` Prelude.rnf
        awsCodeBuildProject
      `Prelude.seq` Prelude.rnf
        awsRdsDbInstance
      `Prelude.seq` Prelude.rnf
        awsElasticsearchDomain
      `Prelude.seq` Prelude.rnf
        awsEcrContainerImage
      `Prelude.seq` Prelude.rnf
        awsAutoScalingLaunchConfiguration
      `Prelude.seq` Prelude.rnf
        awsIamAccessKey
      `Prelude.seq` Prelude.rnf
        awsRedshiftCluster
      `Prelude.seq` Prelude.rnf
        awsNetworkFirewallFirewallPolicy
      `Prelude.seq` Prelude.rnf
        awsEc2Vpc
      `Prelude.seq` Prelude.rnf
        awsEcrRepository
      `Prelude.seq` Prelude.rnf
        awsWafRegionalRuleGroup
      `Prelude.seq` Prelude.rnf
        awsWafRegionalRateBasedRule
      `Prelude.seq` Prelude.rnf
        awsWafRegionalRule
      `Prelude.seq` Prelude.rnf
        awsApiGatewayV2Stage
      `Prelude.seq` Prelude.rnf
        awsRdsDbCluster
      `Prelude.seq` Prelude.rnf
        awsS3Object
      `Prelude.seq` Prelude.rnf
        awsNetworkFirewallFirewall
      `Prelude.seq` Prelude.rnf
        awsWafRuleGroup
      `Prelude.seq` Prelude.rnf
        awsEc2VpcPeeringConnection
      `Prelude.seq` Prelude.rnf
        awsEc2TransitGateway
      `Prelude.seq` Prelude.rnf
        awsS3Bucket
      `Prelude.seq` Prelude.rnf
        awsWafRegionalWebAcl
      `Prelude.seq` Prelude.rnf
        awsEc2Eip
      `Prelude.seq` Prelude.rnf
        awsBackupBackupPlan
      `Prelude.seq` Prelude.rnf
        awsLambdaFunction
      `Prelude.seq` Prelude.rnf
        container
      `Prelude.seq` Prelude.rnf
        awsRdsDbClusterSnapshot
      `Prelude.seq` Prelude.rnf
        awsElbLoadBalancer
      `Prelude.seq` Prelude.rnf
        awsCloudTrailTrail
      `Prelude.seq` Prelude.rnf
        awsEcsService
      `Prelude.seq` Prelude.rnf
        awsApiGatewayStage
      `Prelude.seq` Prelude.rnf
        awsSnsTopic
      `Prelude.seq` Prelude.rnf
        awsRdsDbSecurityGroup
      `Prelude.seq` Prelude.rnf
        awsKinesisStream
      `Prelude.seq` Prelude.rnf
        awsEc2SecurityGroup
      `Prelude.seq` Prelude.rnf
        awsEcsCluster
      `Prelude.seq` Prelude.rnf
        awsApiGatewayRestApi
      `Prelude.seq` Prelude.rnf
        awsWafRule
      `Prelude.seq` Prelude.rnf
        awsBackupRecoveryPoint
      `Prelude.seq` Prelude.rnf
        awsRdsDbSnapshot
      `Prelude.seq` Prelude.rnf
        awsElasticBeanstalkEnvironment
      `Prelude.seq` Prelude.rnf
        awsEcsTask
      `Prelude.seq` Prelude.rnf
        awsXrayEncryptionConfig
      `Prelude.seq` Prelude.rnf
        awsLambdaLayerVersion
      `Prelude.seq` Prelude.rnf
        awsKmsKey
      `Prelude.seq` Prelude.rnf
        awsSecretsManagerSecret
      `Prelude.seq` Prelude.rnf
        awsCloudFrontDistribution

instance Core.ToJSON ResourceDetails where
  toJSON ResourceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AwsNetworkFirewallRuleGroup" Core..=)
              Prelude.<$> awsNetworkFirewallRuleGroup,
            ("AwsEc2Instance" Core..=)
              Prelude.<$> awsEc2Instance,
            ("AwsIamRole" Core..=) Prelude.<$> awsIamRole,
            ("AwsS3AccountPublicAccessBlock" Core..=)
              Prelude.<$> awsS3AccountPublicAccessBlock,
            ("AwsSqsQueue" Core..=) Prelude.<$> awsSqsQueue,
            ("AwsElbv2LoadBalancer" Core..=)
              Prelude.<$> awsElbv2LoadBalancer,
            ("AwsCloudWatchAlarm" Core..=)
              Prelude.<$> awsCloudWatchAlarm,
            ("AwsIamPolicy" Core..=) Prelude.<$> awsIamPolicy,
            ("AwsEc2NetworkInterface" Core..=)
              Prelude.<$> awsEc2NetworkInterface,
            ("AwsEksCluster" Core..=) Prelude.<$> awsEksCluster,
            ("AwsWafRateBasedRule" Core..=)
              Prelude.<$> awsWafRateBasedRule,
            ("AwsEcsContainer" Core..=)
              Prelude.<$> awsEcsContainer,
            ("AwsRdsEventSubscription" Core..=)
              Prelude.<$> awsRdsEventSubscription,
            ("AwsCloudFormationStack" Core..=)
              Prelude.<$> awsCloudFormationStack,
            ("AwsWafWebAcl" Core..=) Prelude.<$> awsWafWebAcl,
            ("Other" Core..=) Prelude.<$> other,
            ("AwsSsmPatchCompliance" Core..=)
              Prelude.<$> awsSsmPatchCompliance,
            ("AwsOpenSearchServiceDomain" Core..=)
              Prelude.<$> awsOpenSearchServiceDomain,
            ("AwsDynamoDbTable" Core..=)
              Prelude.<$> awsDynamoDbTable,
            ("AwsEc2Volume" Core..=) Prelude.<$> awsEc2Volume,
            ("AwsCertificateManagerCertificate" Core..=)
              Prelude.<$> awsCertificateManagerCertificate,
            ("AwsEc2VpnConnection" Core..=)
              Prelude.<$> awsEc2VpnConnection,
            ("AwsEc2Subnet" Core..=) Prelude.<$> awsEc2Subnet,
            ("AwsIamGroup" Core..=) Prelude.<$> awsIamGroup,
            ("AwsIamUser" Core..=) Prelude.<$> awsIamUser,
            ("AwsBackupBackupVault" Core..=)
              Prelude.<$> awsBackupBackupVault,
            ("AwsApiGatewayV2Api" Core..=)
              Prelude.<$> awsApiGatewayV2Api,
            ("AwsEcsTaskDefinition" Core..=)
              Prelude.<$> awsEcsTaskDefinition,
            ("AwsAutoScalingAutoScalingGroup" Core..=)
              Prelude.<$> awsAutoScalingAutoScalingGroup,
            ("AwsEc2NetworkAcl" Core..=)
              Prelude.<$> awsEc2NetworkAcl,
            ("AwsEfsAccessPoint" Core..=)
              Prelude.<$> awsEfsAccessPoint,
            ("AwsEc2VpcEndpointService" Core..=)
              Prelude.<$> awsEc2VpcEndpointService,
            ("AwsCodeBuildProject" Core..=)
              Prelude.<$> awsCodeBuildProject,
            ("AwsRdsDbInstance" Core..=)
              Prelude.<$> awsRdsDbInstance,
            ("AwsElasticsearchDomain" Core..=)
              Prelude.<$> awsElasticsearchDomain,
            ("AwsEcrContainerImage" Core..=)
              Prelude.<$> awsEcrContainerImage,
            ("AwsAutoScalingLaunchConfiguration" Core..=)
              Prelude.<$> awsAutoScalingLaunchConfiguration,
            ("AwsIamAccessKey" Core..=)
              Prelude.<$> awsIamAccessKey,
            ("AwsRedshiftCluster" Core..=)
              Prelude.<$> awsRedshiftCluster,
            ("AwsNetworkFirewallFirewallPolicy" Core..=)
              Prelude.<$> awsNetworkFirewallFirewallPolicy,
            ("AwsEc2Vpc" Core..=) Prelude.<$> awsEc2Vpc,
            ("AwsEcrRepository" Core..=)
              Prelude.<$> awsEcrRepository,
            ("AwsWafRegionalRuleGroup" Core..=)
              Prelude.<$> awsWafRegionalRuleGroup,
            ("AwsWafRegionalRateBasedRule" Core..=)
              Prelude.<$> awsWafRegionalRateBasedRule,
            ("AwsWafRegionalRule" Core..=)
              Prelude.<$> awsWafRegionalRule,
            ("AwsApiGatewayV2Stage" Core..=)
              Prelude.<$> awsApiGatewayV2Stage,
            ("AwsRdsDbCluster" Core..=)
              Prelude.<$> awsRdsDbCluster,
            ("AwsS3Object" Core..=) Prelude.<$> awsS3Object,
            ("AwsNetworkFirewallFirewall" Core..=)
              Prelude.<$> awsNetworkFirewallFirewall,
            ("AwsWafRuleGroup" Core..=)
              Prelude.<$> awsWafRuleGroup,
            ("AwsEc2VpcPeeringConnection" Core..=)
              Prelude.<$> awsEc2VpcPeeringConnection,
            ("AwsEc2TransitGateway" Core..=)
              Prelude.<$> awsEc2TransitGateway,
            ("AwsS3Bucket" Core..=) Prelude.<$> awsS3Bucket,
            ("AwsWafRegionalWebAcl" Core..=)
              Prelude.<$> awsWafRegionalWebAcl,
            ("AwsEc2Eip" Core..=) Prelude.<$> awsEc2Eip,
            ("AwsBackupBackupPlan" Core..=)
              Prelude.<$> awsBackupBackupPlan,
            ("AwsLambdaFunction" Core..=)
              Prelude.<$> awsLambdaFunction,
            ("Container" Core..=) Prelude.<$> container,
            ("AwsRdsDbClusterSnapshot" Core..=)
              Prelude.<$> awsRdsDbClusterSnapshot,
            ("AwsElbLoadBalancer" Core..=)
              Prelude.<$> awsElbLoadBalancer,
            ("AwsCloudTrailTrail" Core..=)
              Prelude.<$> awsCloudTrailTrail,
            ("AwsEcsService" Core..=) Prelude.<$> awsEcsService,
            ("AwsApiGatewayStage" Core..=)
              Prelude.<$> awsApiGatewayStage,
            ("AwsSnsTopic" Core..=) Prelude.<$> awsSnsTopic,
            ("AwsRdsDbSecurityGroup" Core..=)
              Prelude.<$> awsRdsDbSecurityGroup,
            ("AwsKinesisStream" Core..=)
              Prelude.<$> awsKinesisStream,
            ("AwsEc2SecurityGroup" Core..=)
              Prelude.<$> awsEc2SecurityGroup,
            ("AwsEcsCluster" Core..=) Prelude.<$> awsEcsCluster,
            ("AwsApiGatewayRestApi" Core..=)
              Prelude.<$> awsApiGatewayRestApi,
            ("AwsWafRule" Core..=) Prelude.<$> awsWafRule,
            ("AwsBackupRecoveryPoint" Core..=)
              Prelude.<$> awsBackupRecoveryPoint,
            ("AwsRdsDbSnapshot" Core..=)
              Prelude.<$> awsRdsDbSnapshot,
            ("AwsElasticBeanstalkEnvironment" Core..=)
              Prelude.<$> awsElasticBeanstalkEnvironment,
            ("AwsEcsTask" Core..=) Prelude.<$> awsEcsTask,
            ("AwsXrayEncryptionConfig" Core..=)
              Prelude.<$> awsXrayEncryptionConfig,
            ("AwsLambdaLayerVersion" Core..=)
              Prelude.<$> awsLambdaLayerVersion,
            ("AwsKmsKey" Core..=) Prelude.<$> awsKmsKey,
            ("AwsSecretsManagerSecret" Core..=)
              Prelude.<$> awsSecretsManagerSecret,
            ("AwsCloudFrontDistribution" Core..=)
              Prelude.<$> awsCloudFrontDistribution
          ]
      )
