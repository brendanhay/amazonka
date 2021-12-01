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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayStageDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2ApiDetails
import Amazonka.SecurityHub.Types.AwsApiGatewayV2StageDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationDetails
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDetails
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails
import Amazonka.SecurityHub.Types.AwsCloudTrailTrailDetails
import Amazonka.SecurityHub.Types.AwsCodeBuildProjectDetails
import Amazonka.SecurityHub.Types.AwsDynamoDbTableDetails
import Amazonka.SecurityHub.Types.AwsEc2EipDetails
import Amazonka.SecurityHub.Types.AwsEc2InstanceDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkAclDetails
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
import Amazonka.SecurityHub.Types.AwsEc2SecurityGroupDetails
import Amazonka.SecurityHub.Types.AwsEc2SubnetDetails
import Amazonka.SecurityHub.Types.AwsEc2VolumeDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionDetails
import Amazonka.SecurityHub.Types.AwsEcrContainerImageDetails
import Amazonka.SecurityHub.Types.AwsEcrRepositoryDetails
import Amazonka.SecurityHub.Types.AwsEcsClusterDetails
import Amazonka.SecurityHub.Types.AwsEcsServiceDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionDetails
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
import Amazonka.SecurityHub.Types.AwsKmsKeyDetails
import Amazonka.SecurityHub.Types.AwsLambdaFunctionDetails
import Amazonka.SecurityHub.Types.AwsLambdaLayerVersionDetails
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails
import Amazonka.SecurityHub.Types.AwsRdsDbClusterSnapshotDetails
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceDetails
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
  { -- | Provides details about a CloudTrail trail.
    awsCloudTrailTrail :: Prelude.Maybe AwsCloudTrailTrailDetails,
    -- | Details about an Amazon RDS database cluster.
    awsRdsDbCluster :: Prelude.Maybe AwsRdsDbClusterDetails,
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
    -- | Details about an IAM role.
    awsIamRole :: Prelude.Maybe AwsIamRoleDetails,
    -- | Contains details about a Classic Load Balancer.
    awsElbLoadBalancer :: Prelude.Maybe AwsElbLoadBalancerDetails,
    -- | Details about an EC2 VPN connection.
    awsEc2VpnConnection :: Prelude.Maybe AwsEc2VpnConnectionDetails,
    -- | Details about a CloudFront distribution.
    awsCloudFrontDistribution :: Prelude.Maybe AwsCloudFrontDistributionDetails,
    -- | Details about an KMS key.
    awsKmsKey :: Prelude.Maybe AwsKmsKeyDetails,
    -- | Details about an Amazon EKS cluster.
    awsEksCluster :: Prelude.Maybe AwsEksClusterDetails,
    -- | Provides details about a launch configuration.
    awsAutoScalingLaunchConfiguration :: Prelude.Maybe AwsAutoScalingLaunchConfigurationDetails,
    -- | Details about an Amazon OpenSearch Service domain.
    awsOpenSearchServiceDomain :: Prelude.Maybe AwsOpenSearchServiceDomainDetails,
    -- | Details about an Amazon RDS database snapshot.
    awsRdsDbSnapshot :: Prelude.Maybe AwsRdsDbSnapshotDetails,
    -- | Details about a rate-based rule for Regional resources.
    awsWafRegionalRateBasedRule :: Prelude.Maybe AwsWafRegionalRateBasedRuleDetails,
    -- | Provides information about a REST API in version 1 of Amazon API
    -- Gateway.
    awsApiGatewayRestApi :: Prelude.Maybe AwsApiGatewayRestApiDetails,
    -- | Details about an SQS queue.
    awsSqsQueue :: Prelude.Maybe AwsSqsQueueDetails,
    -- | Details about a Secrets Manager secret.
    awsSecretsManagerSecret :: Prelude.Maybe AwsSecretsManagerSecretDetails,
    -- | Details about a DynamoDB table.
    awsDynamoDbTable :: Prelude.Maybe AwsDynamoDbTableDetails,
    -- | Details about the Amazon S3 Public Access Block configuration for an
    -- account.
    awsS3AccountPublicAccessBlock :: Prelude.Maybe AwsS3AccountPublicAccessBlockDetails,
    -- | Details about an Amazon RDS database cluster snapshot.
    awsRdsDbClusterSnapshot :: Prelude.Maybe AwsRdsDbClusterSnapshotDetails,
    -- | Details for an EC2 security group.
    awsEc2SecurityGroup :: Prelude.Maybe AwsEc2SecurityGroupDetails,
    -- | Details about an EC2 instance related to a finding.
    awsEc2Instance :: Prelude.Maybe AwsEc2InstanceDetails,
    -- | Details about an IAM permissions policy.
    awsIamPolicy :: Prelude.Maybe AwsIamPolicyDetails,
    -- | Details about an S3 object related to a finding.
    awsS3Object :: Prelude.Maybe AwsS3ObjectDetails,
    -- | Details about a task definition. A task definition describes the
    -- container and volume definitions of an Amazon Elastic Container Service
    -- task.
    awsEcsTaskDefinition :: Prelude.Maybe AwsEcsTaskDefinitionDetails,
    -- | Details for a Lambda layer version.
    awsLambdaLayerVersion :: Prelude.Maybe AwsLambdaLayerVersionDetails,
    -- | Details about an S3 bucket related to a finding.
    awsS3Bucket :: Prelude.Maybe AwsS3BucketDetails,
    -- | Details about an IAM access key related to a finding.
    awsIamAccessKey :: Prelude.Maybe AwsIamAccessKeyDetails,
    -- | Details for an CodeBuild project.
    awsCodeBuildProject :: Prelude.Maybe AwsCodeBuildProjectDetails,
    -- | Provides information about a version 1 Amazon API Gateway stage.
    awsApiGatewayStage :: Prelude.Maybe AwsApiGatewayStageDetails,
    -- | Details about an EC2 network access control list (ACL).
    awsEc2NetworkAcl :: Prelude.Maybe AwsEc2NetworkAclDetails,
    -- | Provides information about a version 2 API in Amazon API Gateway.
    awsApiGatewayV2Api :: Prelude.Maybe AwsApiGatewayV2ApiDetails,
    -- | Details about an Elastic Beanstalk environment.
    awsElasticBeanstalkEnvironment :: Prelude.Maybe AwsElasticBeanstalkEnvironmentDetails,
    -- | Details for an EC2 VPC.
    awsEc2Vpc :: Prelude.Maybe AwsEc2VpcDetails,
    -- | Contains details about an Amazon Redshift cluster.
    awsRedshiftCluster :: Prelude.Maybe AwsRedshiftClusterDetails,
    -- | Details about an Amazon RDS database instance.
    awsRdsDbInstance :: Prelude.Maybe AwsRdsDbInstanceDetails,
    -- | Details about an RDS event notification subscription.
    awsRdsEventSubscription :: Prelude.Maybe AwsRdsEventSubscriptionDetails,
    -- | Details about a container resource related to a finding.
    container :: Prelude.Maybe ContainerDetails,
    -- | Contains details about an IAM group.
    awsIamGroup :: Prelude.Maybe AwsIamGroupDetails,
    -- | Information about the encryption configuration for X-Ray.
    awsXrayEncryptionConfig :: Prelude.Maybe AwsXrayEncryptionConfigDetails,
    -- | Details about a subnet in Amazon EC2.
    awsEc2Subnet :: Prelude.Maybe AwsEc2SubnetDetails,
    -- | Details about an ECS cluster.
    awsEcsCluster :: Prelude.Maybe AwsEcsClusterDetails,
    -- | Details about the service configuration for a VPC endpoint service.
    awsEc2VpcEndpointService :: Prelude.Maybe AwsEc2VpcEndpointServiceDetails,
    -- | Details for an autoscaling group.
    awsAutoScalingAutoScalingGroup :: Prelude.Maybe AwsAutoScalingAutoScalingGroupDetails,
    -- | Details about an SNS topic.
    awsSnsTopic :: Prelude.Maybe AwsSnsTopicDetails,
    -- | Details for an EC2 network interface.
    awsEc2NetworkInterface :: Prelude.Maybe AwsEc2NetworkInterfaceDetails,
    -- | Details about a service within an ECS cluster.
    awsEcsService :: Prelude.Maybe AwsEcsServiceDetails,
    -- | Details about an Elastic IP address.
    awsEc2Eip :: Prelude.Maybe AwsEc2EipDetails,
    -- | Provides information about a version 2 stage for Amazon API Gateway.
    awsApiGatewayV2Stage :: Prelude.Maybe AwsApiGatewayV2StageDetails,
    -- | Details about a Lambda function.
    awsLambdaFunction :: Prelude.Maybe AwsLambdaFunctionDetails,
    -- | Details about a load balancer.
    awsElbv2LoadBalancer :: Prelude.Maybe AwsElbv2LoadBalancerDetails,
    -- | Details about a rate-based rule for global resources.
    awsWafRateBasedRule :: Prelude.Maybe AwsWafRateBasedRuleDetails,
    -- | Information about an Amazon Elastic Container Registry repository.
    awsEcrRepository :: Prelude.Maybe AwsEcrRepositoryDetails,
    -- | Details for an WAF WebACL.
    awsWafWebAcl :: Prelude.Maybe AwsWafWebAclDetails,
    -- | Provides information about the state of a patch on an instance based on
    -- the patch baseline that was used to patch the instance.
    awsSsmPatchCompliance :: Prelude.Maybe AwsSsmPatchComplianceDetails,
    -- | Details for an Elasticsearch domain.
    awsElasticsearchDomain :: Prelude.Maybe AwsElasticsearchDomainDetails,
    -- | Provides details about an Certificate Manager certificate.
    awsCertificateManagerCertificate :: Prelude.Maybe AwsCertificateManagerCertificateDetails,
    -- | Information about an Amazon ECR image.
    awsEcrContainerImage :: Prelude.Maybe AwsEcrContainerImageDetails,
    -- | Details about an IAM user.
    awsIamUser :: Prelude.Maybe AwsIamUserDetails,
    -- | Details for an EC2 volume.
    awsEc2Volume :: Prelude.Maybe AwsEc2VolumeDetails
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
-- 'awsCloudTrailTrail', 'resourceDetails_awsCloudTrailTrail' - Provides details about a CloudTrail trail.
--
-- 'awsRdsDbCluster', 'resourceDetails_awsRdsDbCluster' - Details about an Amazon RDS database cluster.
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
-- 'awsIamRole', 'resourceDetails_awsIamRole' - Details about an IAM role.
--
-- 'awsElbLoadBalancer', 'resourceDetails_awsElbLoadBalancer' - Contains details about a Classic Load Balancer.
--
-- 'awsEc2VpnConnection', 'resourceDetails_awsEc2VpnConnection' - Details about an EC2 VPN connection.
--
-- 'awsCloudFrontDistribution', 'resourceDetails_awsCloudFrontDistribution' - Details about a CloudFront distribution.
--
-- 'awsKmsKey', 'resourceDetails_awsKmsKey' - Details about an KMS key.
--
-- 'awsEksCluster', 'resourceDetails_awsEksCluster' - Details about an Amazon EKS cluster.
--
-- 'awsAutoScalingLaunchConfiguration', 'resourceDetails_awsAutoScalingLaunchConfiguration' - Provides details about a launch configuration.
--
-- 'awsOpenSearchServiceDomain', 'resourceDetails_awsOpenSearchServiceDomain' - Details about an Amazon OpenSearch Service domain.
--
-- 'awsRdsDbSnapshot', 'resourceDetails_awsRdsDbSnapshot' - Details about an Amazon RDS database snapshot.
--
-- 'awsWafRegionalRateBasedRule', 'resourceDetails_awsWafRegionalRateBasedRule' - Details about a rate-based rule for Regional resources.
--
-- 'awsApiGatewayRestApi', 'resourceDetails_awsApiGatewayRestApi' - Provides information about a REST API in version 1 of Amazon API
-- Gateway.
--
-- 'awsSqsQueue', 'resourceDetails_awsSqsQueue' - Details about an SQS queue.
--
-- 'awsSecretsManagerSecret', 'resourceDetails_awsSecretsManagerSecret' - Details about a Secrets Manager secret.
--
-- 'awsDynamoDbTable', 'resourceDetails_awsDynamoDbTable' - Details about a DynamoDB table.
--
-- 'awsS3AccountPublicAccessBlock', 'resourceDetails_awsS3AccountPublicAccessBlock' - Details about the Amazon S3 Public Access Block configuration for an
-- account.
--
-- 'awsRdsDbClusterSnapshot', 'resourceDetails_awsRdsDbClusterSnapshot' - Details about an Amazon RDS database cluster snapshot.
--
-- 'awsEc2SecurityGroup', 'resourceDetails_awsEc2SecurityGroup' - Details for an EC2 security group.
--
-- 'awsEc2Instance', 'resourceDetails_awsEc2Instance' - Details about an EC2 instance related to a finding.
--
-- 'awsIamPolicy', 'resourceDetails_awsIamPolicy' - Details about an IAM permissions policy.
--
-- 'awsS3Object', 'resourceDetails_awsS3Object' - Details about an S3 object related to a finding.
--
-- 'awsEcsTaskDefinition', 'resourceDetails_awsEcsTaskDefinition' - Details about a task definition. A task definition describes the
-- container and volume definitions of an Amazon Elastic Container Service
-- task.
--
-- 'awsLambdaLayerVersion', 'resourceDetails_awsLambdaLayerVersion' - Details for a Lambda layer version.
--
-- 'awsS3Bucket', 'resourceDetails_awsS3Bucket' - Details about an S3 bucket related to a finding.
--
-- 'awsIamAccessKey', 'resourceDetails_awsIamAccessKey' - Details about an IAM access key related to a finding.
--
-- 'awsCodeBuildProject', 'resourceDetails_awsCodeBuildProject' - Details for an CodeBuild project.
--
-- 'awsApiGatewayStage', 'resourceDetails_awsApiGatewayStage' - Provides information about a version 1 Amazon API Gateway stage.
--
-- 'awsEc2NetworkAcl', 'resourceDetails_awsEc2NetworkAcl' - Details about an EC2 network access control list (ACL).
--
-- 'awsApiGatewayV2Api', 'resourceDetails_awsApiGatewayV2Api' - Provides information about a version 2 API in Amazon API Gateway.
--
-- 'awsElasticBeanstalkEnvironment', 'resourceDetails_awsElasticBeanstalkEnvironment' - Details about an Elastic Beanstalk environment.
--
-- 'awsEc2Vpc', 'resourceDetails_awsEc2Vpc' - Details for an EC2 VPC.
--
-- 'awsRedshiftCluster', 'resourceDetails_awsRedshiftCluster' - Contains details about an Amazon Redshift cluster.
--
-- 'awsRdsDbInstance', 'resourceDetails_awsRdsDbInstance' - Details about an Amazon RDS database instance.
--
-- 'awsRdsEventSubscription', 'resourceDetails_awsRdsEventSubscription' - Details about an RDS event notification subscription.
--
-- 'container', 'resourceDetails_container' - Details about a container resource related to a finding.
--
-- 'awsIamGroup', 'resourceDetails_awsIamGroup' - Contains details about an IAM group.
--
-- 'awsXrayEncryptionConfig', 'resourceDetails_awsXrayEncryptionConfig' - Information about the encryption configuration for X-Ray.
--
-- 'awsEc2Subnet', 'resourceDetails_awsEc2Subnet' - Details about a subnet in Amazon EC2.
--
-- 'awsEcsCluster', 'resourceDetails_awsEcsCluster' - Details about an ECS cluster.
--
-- 'awsEc2VpcEndpointService', 'resourceDetails_awsEc2VpcEndpointService' - Details about the service configuration for a VPC endpoint service.
--
-- 'awsAutoScalingAutoScalingGroup', 'resourceDetails_awsAutoScalingAutoScalingGroup' - Details for an autoscaling group.
--
-- 'awsSnsTopic', 'resourceDetails_awsSnsTopic' - Details about an SNS topic.
--
-- 'awsEc2NetworkInterface', 'resourceDetails_awsEc2NetworkInterface' - Details for an EC2 network interface.
--
-- 'awsEcsService', 'resourceDetails_awsEcsService' - Details about a service within an ECS cluster.
--
-- 'awsEc2Eip', 'resourceDetails_awsEc2Eip' - Details about an Elastic IP address.
--
-- 'awsApiGatewayV2Stage', 'resourceDetails_awsApiGatewayV2Stage' - Provides information about a version 2 stage for Amazon API Gateway.
--
-- 'awsLambdaFunction', 'resourceDetails_awsLambdaFunction' - Details about a Lambda function.
--
-- 'awsElbv2LoadBalancer', 'resourceDetails_awsElbv2LoadBalancer' - Details about a load balancer.
--
-- 'awsWafRateBasedRule', 'resourceDetails_awsWafRateBasedRule' - Details about a rate-based rule for global resources.
--
-- 'awsEcrRepository', 'resourceDetails_awsEcrRepository' - Information about an Amazon Elastic Container Registry repository.
--
-- 'awsWafWebAcl', 'resourceDetails_awsWafWebAcl' - Details for an WAF WebACL.
--
-- 'awsSsmPatchCompliance', 'resourceDetails_awsSsmPatchCompliance' - Provides information about the state of a patch on an instance based on
-- the patch baseline that was used to patch the instance.
--
-- 'awsElasticsearchDomain', 'resourceDetails_awsElasticsearchDomain' - Details for an Elasticsearch domain.
--
-- 'awsCertificateManagerCertificate', 'resourceDetails_awsCertificateManagerCertificate' - Provides details about an Certificate Manager certificate.
--
-- 'awsEcrContainerImage', 'resourceDetails_awsEcrContainerImage' - Information about an Amazon ECR image.
--
-- 'awsIamUser', 'resourceDetails_awsIamUser' - Details about an IAM user.
--
-- 'awsEc2Volume', 'resourceDetails_awsEc2Volume' - Details for an EC2 volume.
newResourceDetails ::
  ResourceDetails
newResourceDetails =
  ResourceDetails'
    { awsCloudTrailTrail =
        Prelude.Nothing,
      awsRdsDbCluster = Prelude.Nothing,
      other = Prelude.Nothing,
      awsIamRole = Prelude.Nothing,
      awsElbLoadBalancer = Prelude.Nothing,
      awsEc2VpnConnection = Prelude.Nothing,
      awsCloudFrontDistribution = Prelude.Nothing,
      awsKmsKey = Prelude.Nothing,
      awsEksCluster = Prelude.Nothing,
      awsAutoScalingLaunchConfiguration = Prelude.Nothing,
      awsOpenSearchServiceDomain = Prelude.Nothing,
      awsRdsDbSnapshot = Prelude.Nothing,
      awsWafRegionalRateBasedRule = Prelude.Nothing,
      awsApiGatewayRestApi = Prelude.Nothing,
      awsSqsQueue = Prelude.Nothing,
      awsSecretsManagerSecret = Prelude.Nothing,
      awsDynamoDbTable = Prelude.Nothing,
      awsS3AccountPublicAccessBlock = Prelude.Nothing,
      awsRdsDbClusterSnapshot = Prelude.Nothing,
      awsEc2SecurityGroup = Prelude.Nothing,
      awsEc2Instance = Prelude.Nothing,
      awsIamPolicy = Prelude.Nothing,
      awsS3Object = Prelude.Nothing,
      awsEcsTaskDefinition = Prelude.Nothing,
      awsLambdaLayerVersion = Prelude.Nothing,
      awsS3Bucket = Prelude.Nothing,
      awsIamAccessKey = Prelude.Nothing,
      awsCodeBuildProject = Prelude.Nothing,
      awsApiGatewayStage = Prelude.Nothing,
      awsEc2NetworkAcl = Prelude.Nothing,
      awsApiGatewayV2Api = Prelude.Nothing,
      awsElasticBeanstalkEnvironment = Prelude.Nothing,
      awsEc2Vpc = Prelude.Nothing,
      awsRedshiftCluster = Prelude.Nothing,
      awsRdsDbInstance = Prelude.Nothing,
      awsRdsEventSubscription = Prelude.Nothing,
      container = Prelude.Nothing,
      awsIamGroup = Prelude.Nothing,
      awsXrayEncryptionConfig = Prelude.Nothing,
      awsEc2Subnet = Prelude.Nothing,
      awsEcsCluster = Prelude.Nothing,
      awsEc2VpcEndpointService = Prelude.Nothing,
      awsAutoScalingAutoScalingGroup = Prelude.Nothing,
      awsSnsTopic = Prelude.Nothing,
      awsEc2NetworkInterface = Prelude.Nothing,
      awsEcsService = Prelude.Nothing,
      awsEc2Eip = Prelude.Nothing,
      awsApiGatewayV2Stage = Prelude.Nothing,
      awsLambdaFunction = Prelude.Nothing,
      awsElbv2LoadBalancer = Prelude.Nothing,
      awsWafRateBasedRule = Prelude.Nothing,
      awsEcrRepository = Prelude.Nothing,
      awsWafWebAcl = Prelude.Nothing,
      awsSsmPatchCompliance = Prelude.Nothing,
      awsElasticsearchDomain = Prelude.Nothing,
      awsCertificateManagerCertificate = Prelude.Nothing,
      awsEcrContainerImage = Prelude.Nothing,
      awsIamUser = Prelude.Nothing,
      awsEc2Volume = Prelude.Nothing
    }

-- | Provides details about a CloudTrail trail.
resourceDetails_awsCloudTrailTrail :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCloudTrailTrailDetails)
resourceDetails_awsCloudTrailTrail = Lens.lens (\ResourceDetails' {awsCloudTrailTrail} -> awsCloudTrailTrail) (\s@ResourceDetails' {} a -> s {awsCloudTrailTrail = a} :: ResourceDetails)

-- | Details about an Amazon RDS database cluster.
resourceDetails_awsRdsDbCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbClusterDetails)
resourceDetails_awsRdsDbCluster = Lens.lens (\ResourceDetails' {awsRdsDbCluster} -> awsRdsDbCluster) (\s@ResourceDetails' {} a -> s {awsRdsDbCluster = a} :: ResourceDetails)

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

-- | Details about an IAM role.
resourceDetails_awsIamRole :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamRoleDetails)
resourceDetails_awsIamRole = Lens.lens (\ResourceDetails' {awsIamRole} -> awsIamRole) (\s@ResourceDetails' {} a -> s {awsIamRole = a} :: ResourceDetails)

-- | Contains details about a Classic Load Balancer.
resourceDetails_awsElbLoadBalancer :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElbLoadBalancerDetails)
resourceDetails_awsElbLoadBalancer = Lens.lens (\ResourceDetails' {awsElbLoadBalancer} -> awsElbLoadBalancer) (\s@ResourceDetails' {} a -> s {awsElbLoadBalancer = a} :: ResourceDetails)

-- | Details about an EC2 VPN connection.
resourceDetails_awsEc2VpnConnection :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpnConnectionDetails)
resourceDetails_awsEc2VpnConnection = Lens.lens (\ResourceDetails' {awsEc2VpnConnection} -> awsEc2VpnConnection) (\s@ResourceDetails' {} a -> s {awsEc2VpnConnection = a} :: ResourceDetails)

-- | Details about a CloudFront distribution.
resourceDetails_awsCloudFrontDistribution :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCloudFrontDistributionDetails)
resourceDetails_awsCloudFrontDistribution = Lens.lens (\ResourceDetails' {awsCloudFrontDistribution} -> awsCloudFrontDistribution) (\s@ResourceDetails' {} a -> s {awsCloudFrontDistribution = a} :: ResourceDetails)

-- | Details about an KMS key.
resourceDetails_awsKmsKey :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsKmsKeyDetails)
resourceDetails_awsKmsKey = Lens.lens (\ResourceDetails' {awsKmsKey} -> awsKmsKey) (\s@ResourceDetails' {} a -> s {awsKmsKey = a} :: ResourceDetails)

-- | Details about an Amazon EKS cluster.
resourceDetails_awsEksCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEksClusterDetails)
resourceDetails_awsEksCluster = Lens.lens (\ResourceDetails' {awsEksCluster} -> awsEksCluster) (\s@ResourceDetails' {} a -> s {awsEksCluster = a} :: ResourceDetails)

-- | Provides details about a launch configuration.
resourceDetails_awsAutoScalingLaunchConfiguration :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsAutoScalingLaunchConfigurationDetails)
resourceDetails_awsAutoScalingLaunchConfiguration = Lens.lens (\ResourceDetails' {awsAutoScalingLaunchConfiguration} -> awsAutoScalingLaunchConfiguration) (\s@ResourceDetails' {} a -> s {awsAutoScalingLaunchConfiguration = a} :: ResourceDetails)

-- | Details about an Amazon OpenSearch Service domain.
resourceDetails_awsOpenSearchServiceDomain :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsOpenSearchServiceDomainDetails)
resourceDetails_awsOpenSearchServiceDomain = Lens.lens (\ResourceDetails' {awsOpenSearchServiceDomain} -> awsOpenSearchServiceDomain) (\s@ResourceDetails' {} a -> s {awsOpenSearchServiceDomain = a} :: ResourceDetails)

-- | Details about an Amazon RDS database snapshot.
resourceDetails_awsRdsDbSnapshot :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbSnapshotDetails)
resourceDetails_awsRdsDbSnapshot = Lens.lens (\ResourceDetails' {awsRdsDbSnapshot} -> awsRdsDbSnapshot) (\s@ResourceDetails' {} a -> s {awsRdsDbSnapshot = a} :: ResourceDetails)

-- | Details about a rate-based rule for Regional resources.
resourceDetails_awsWafRegionalRateBasedRule :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRegionalRateBasedRuleDetails)
resourceDetails_awsWafRegionalRateBasedRule = Lens.lens (\ResourceDetails' {awsWafRegionalRateBasedRule} -> awsWafRegionalRateBasedRule) (\s@ResourceDetails' {} a -> s {awsWafRegionalRateBasedRule = a} :: ResourceDetails)

-- | Provides information about a REST API in version 1 of Amazon API
-- Gateway.
resourceDetails_awsApiGatewayRestApi :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayRestApiDetails)
resourceDetails_awsApiGatewayRestApi = Lens.lens (\ResourceDetails' {awsApiGatewayRestApi} -> awsApiGatewayRestApi) (\s@ResourceDetails' {} a -> s {awsApiGatewayRestApi = a} :: ResourceDetails)

-- | Details about an SQS queue.
resourceDetails_awsSqsQueue :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSqsQueueDetails)
resourceDetails_awsSqsQueue = Lens.lens (\ResourceDetails' {awsSqsQueue} -> awsSqsQueue) (\s@ResourceDetails' {} a -> s {awsSqsQueue = a} :: ResourceDetails)

-- | Details about a Secrets Manager secret.
resourceDetails_awsSecretsManagerSecret :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSecretsManagerSecretDetails)
resourceDetails_awsSecretsManagerSecret = Lens.lens (\ResourceDetails' {awsSecretsManagerSecret} -> awsSecretsManagerSecret) (\s@ResourceDetails' {} a -> s {awsSecretsManagerSecret = a} :: ResourceDetails)

-- | Details about a DynamoDB table.
resourceDetails_awsDynamoDbTable :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsDynamoDbTableDetails)
resourceDetails_awsDynamoDbTable = Lens.lens (\ResourceDetails' {awsDynamoDbTable} -> awsDynamoDbTable) (\s@ResourceDetails' {} a -> s {awsDynamoDbTable = a} :: ResourceDetails)

-- | Details about the Amazon S3 Public Access Block configuration for an
-- account.
resourceDetails_awsS3AccountPublicAccessBlock :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsS3AccountPublicAccessBlockDetails)
resourceDetails_awsS3AccountPublicAccessBlock = Lens.lens (\ResourceDetails' {awsS3AccountPublicAccessBlock} -> awsS3AccountPublicAccessBlock) (\s@ResourceDetails' {} a -> s {awsS3AccountPublicAccessBlock = a} :: ResourceDetails)

-- | Details about an Amazon RDS database cluster snapshot.
resourceDetails_awsRdsDbClusterSnapshot :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbClusterSnapshotDetails)
resourceDetails_awsRdsDbClusterSnapshot = Lens.lens (\ResourceDetails' {awsRdsDbClusterSnapshot} -> awsRdsDbClusterSnapshot) (\s@ResourceDetails' {} a -> s {awsRdsDbClusterSnapshot = a} :: ResourceDetails)

-- | Details for an EC2 security group.
resourceDetails_awsEc2SecurityGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2SecurityGroupDetails)
resourceDetails_awsEc2SecurityGroup = Lens.lens (\ResourceDetails' {awsEc2SecurityGroup} -> awsEc2SecurityGroup) (\s@ResourceDetails' {} a -> s {awsEc2SecurityGroup = a} :: ResourceDetails)

-- | Details about an EC2 instance related to a finding.
resourceDetails_awsEc2Instance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2InstanceDetails)
resourceDetails_awsEc2Instance = Lens.lens (\ResourceDetails' {awsEc2Instance} -> awsEc2Instance) (\s@ResourceDetails' {} a -> s {awsEc2Instance = a} :: ResourceDetails)

-- | Details about an IAM permissions policy.
resourceDetails_awsIamPolicy :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamPolicyDetails)
resourceDetails_awsIamPolicy = Lens.lens (\ResourceDetails' {awsIamPolicy} -> awsIamPolicy) (\s@ResourceDetails' {} a -> s {awsIamPolicy = a} :: ResourceDetails)

-- | Details about an S3 object related to a finding.
resourceDetails_awsS3Object :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsS3ObjectDetails)
resourceDetails_awsS3Object = Lens.lens (\ResourceDetails' {awsS3Object} -> awsS3Object) (\s@ResourceDetails' {} a -> s {awsS3Object = a} :: ResourceDetails)

-- | Details about a task definition. A task definition describes the
-- container and volume definitions of an Amazon Elastic Container Service
-- task.
resourceDetails_awsEcsTaskDefinition :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsTaskDefinitionDetails)
resourceDetails_awsEcsTaskDefinition = Lens.lens (\ResourceDetails' {awsEcsTaskDefinition} -> awsEcsTaskDefinition) (\s@ResourceDetails' {} a -> s {awsEcsTaskDefinition = a} :: ResourceDetails)

-- | Details for a Lambda layer version.
resourceDetails_awsLambdaLayerVersion :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsLambdaLayerVersionDetails)
resourceDetails_awsLambdaLayerVersion = Lens.lens (\ResourceDetails' {awsLambdaLayerVersion} -> awsLambdaLayerVersion) (\s@ResourceDetails' {} a -> s {awsLambdaLayerVersion = a} :: ResourceDetails)

-- | Details about an S3 bucket related to a finding.
resourceDetails_awsS3Bucket :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsS3BucketDetails)
resourceDetails_awsS3Bucket = Lens.lens (\ResourceDetails' {awsS3Bucket} -> awsS3Bucket) (\s@ResourceDetails' {} a -> s {awsS3Bucket = a} :: ResourceDetails)

-- | Details about an IAM access key related to a finding.
resourceDetails_awsIamAccessKey :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamAccessKeyDetails)
resourceDetails_awsIamAccessKey = Lens.lens (\ResourceDetails' {awsIamAccessKey} -> awsIamAccessKey) (\s@ResourceDetails' {} a -> s {awsIamAccessKey = a} :: ResourceDetails)

-- | Details for an CodeBuild project.
resourceDetails_awsCodeBuildProject :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCodeBuildProjectDetails)
resourceDetails_awsCodeBuildProject = Lens.lens (\ResourceDetails' {awsCodeBuildProject} -> awsCodeBuildProject) (\s@ResourceDetails' {} a -> s {awsCodeBuildProject = a} :: ResourceDetails)

-- | Provides information about a version 1 Amazon API Gateway stage.
resourceDetails_awsApiGatewayStage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayStageDetails)
resourceDetails_awsApiGatewayStage = Lens.lens (\ResourceDetails' {awsApiGatewayStage} -> awsApiGatewayStage) (\s@ResourceDetails' {} a -> s {awsApiGatewayStage = a} :: ResourceDetails)

-- | Details about an EC2 network access control list (ACL).
resourceDetails_awsEc2NetworkAcl :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2NetworkAclDetails)
resourceDetails_awsEc2NetworkAcl = Lens.lens (\ResourceDetails' {awsEc2NetworkAcl} -> awsEc2NetworkAcl) (\s@ResourceDetails' {} a -> s {awsEc2NetworkAcl = a} :: ResourceDetails)

-- | Provides information about a version 2 API in Amazon API Gateway.
resourceDetails_awsApiGatewayV2Api :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayV2ApiDetails)
resourceDetails_awsApiGatewayV2Api = Lens.lens (\ResourceDetails' {awsApiGatewayV2Api} -> awsApiGatewayV2Api) (\s@ResourceDetails' {} a -> s {awsApiGatewayV2Api = a} :: ResourceDetails)

-- | Details about an Elastic Beanstalk environment.
resourceDetails_awsElasticBeanstalkEnvironment :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElasticBeanstalkEnvironmentDetails)
resourceDetails_awsElasticBeanstalkEnvironment = Lens.lens (\ResourceDetails' {awsElasticBeanstalkEnvironment} -> awsElasticBeanstalkEnvironment) (\s@ResourceDetails' {} a -> s {awsElasticBeanstalkEnvironment = a} :: ResourceDetails)

-- | Details for an EC2 VPC.
resourceDetails_awsEc2Vpc :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpcDetails)
resourceDetails_awsEc2Vpc = Lens.lens (\ResourceDetails' {awsEc2Vpc} -> awsEc2Vpc) (\s@ResourceDetails' {} a -> s {awsEc2Vpc = a} :: ResourceDetails)

-- | Contains details about an Amazon Redshift cluster.
resourceDetails_awsRedshiftCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRedshiftClusterDetails)
resourceDetails_awsRedshiftCluster = Lens.lens (\ResourceDetails' {awsRedshiftCluster} -> awsRedshiftCluster) (\s@ResourceDetails' {} a -> s {awsRedshiftCluster = a} :: ResourceDetails)

-- | Details about an Amazon RDS database instance.
resourceDetails_awsRdsDbInstance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsDbInstanceDetails)
resourceDetails_awsRdsDbInstance = Lens.lens (\ResourceDetails' {awsRdsDbInstance} -> awsRdsDbInstance) (\s@ResourceDetails' {} a -> s {awsRdsDbInstance = a} :: ResourceDetails)

-- | Details about an RDS event notification subscription.
resourceDetails_awsRdsEventSubscription :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsRdsEventSubscriptionDetails)
resourceDetails_awsRdsEventSubscription = Lens.lens (\ResourceDetails' {awsRdsEventSubscription} -> awsRdsEventSubscription) (\s@ResourceDetails' {} a -> s {awsRdsEventSubscription = a} :: ResourceDetails)

-- | Details about a container resource related to a finding.
resourceDetails_container :: Lens.Lens' ResourceDetails (Prelude.Maybe ContainerDetails)
resourceDetails_container = Lens.lens (\ResourceDetails' {container} -> container) (\s@ResourceDetails' {} a -> s {container = a} :: ResourceDetails)

-- | Contains details about an IAM group.
resourceDetails_awsIamGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamGroupDetails)
resourceDetails_awsIamGroup = Lens.lens (\ResourceDetails' {awsIamGroup} -> awsIamGroup) (\s@ResourceDetails' {} a -> s {awsIamGroup = a} :: ResourceDetails)

-- | Information about the encryption configuration for X-Ray.
resourceDetails_awsXrayEncryptionConfig :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsXrayEncryptionConfigDetails)
resourceDetails_awsXrayEncryptionConfig = Lens.lens (\ResourceDetails' {awsXrayEncryptionConfig} -> awsXrayEncryptionConfig) (\s@ResourceDetails' {} a -> s {awsXrayEncryptionConfig = a} :: ResourceDetails)

-- | Details about a subnet in Amazon EC2.
resourceDetails_awsEc2Subnet :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2SubnetDetails)
resourceDetails_awsEc2Subnet = Lens.lens (\ResourceDetails' {awsEc2Subnet} -> awsEc2Subnet) (\s@ResourceDetails' {} a -> s {awsEc2Subnet = a} :: ResourceDetails)

-- | Details about an ECS cluster.
resourceDetails_awsEcsCluster :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsClusterDetails)
resourceDetails_awsEcsCluster = Lens.lens (\ResourceDetails' {awsEcsCluster} -> awsEcsCluster) (\s@ResourceDetails' {} a -> s {awsEcsCluster = a} :: ResourceDetails)

-- | Details about the service configuration for a VPC endpoint service.
resourceDetails_awsEc2VpcEndpointService :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VpcEndpointServiceDetails)
resourceDetails_awsEc2VpcEndpointService = Lens.lens (\ResourceDetails' {awsEc2VpcEndpointService} -> awsEc2VpcEndpointService) (\s@ResourceDetails' {} a -> s {awsEc2VpcEndpointService = a} :: ResourceDetails)

-- | Details for an autoscaling group.
resourceDetails_awsAutoScalingAutoScalingGroup :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsAutoScalingAutoScalingGroupDetails)
resourceDetails_awsAutoScalingAutoScalingGroup = Lens.lens (\ResourceDetails' {awsAutoScalingAutoScalingGroup} -> awsAutoScalingAutoScalingGroup) (\s@ResourceDetails' {} a -> s {awsAutoScalingAutoScalingGroup = a} :: ResourceDetails)

-- | Details about an SNS topic.
resourceDetails_awsSnsTopic :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSnsTopicDetails)
resourceDetails_awsSnsTopic = Lens.lens (\ResourceDetails' {awsSnsTopic} -> awsSnsTopic) (\s@ResourceDetails' {} a -> s {awsSnsTopic = a} :: ResourceDetails)

-- | Details for an EC2 network interface.
resourceDetails_awsEc2NetworkInterface :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2NetworkInterfaceDetails)
resourceDetails_awsEc2NetworkInterface = Lens.lens (\ResourceDetails' {awsEc2NetworkInterface} -> awsEc2NetworkInterface) (\s@ResourceDetails' {} a -> s {awsEc2NetworkInterface = a} :: ResourceDetails)

-- | Details about a service within an ECS cluster.
resourceDetails_awsEcsService :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcsServiceDetails)
resourceDetails_awsEcsService = Lens.lens (\ResourceDetails' {awsEcsService} -> awsEcsService) (\s@ResourceDetails' {} a -> s {awsEcsService = a} :: ResourceDetails)

-- | Details about an Elastic IP address.
resourceDetails_awsEc2Eip :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2EipDetails)
resourceDetails_awsEc2Eip = Lens.lens (\ResourceDetails' {awsEc2Eip} -> awsEc2Eip) (\s@ResourceDetails' {} a -> s {awsEc2Eip = a} :: ResourceDetails)

-- | Provides information about a version 2 stage for Amazon API Gateway.
resourceDetails_awsApiGatewayV2Stage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsApiGatewayV2StageDetails)
resourceDetails_awsApiGatewayV2Stage = Lens.lens (\ResourceDetails' {awsApiGatewayV2Stage} -> awsApiGatewayV2Stage) (\s@ResourceDetails' {} a -> s {awsApiGatewayV2Stage = a} :: ResourceDetails)

-- | Details about a Lambda function.
resourceDetails_awsLambdaFunction :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsLambdaFunctionDetails)
resourceDetails_awsLambdaFunction = Lens.lens (\ResourceDetails' {awsLambdaFunction} -> awsLambdaFunction) (\s@ResourceDetails' {} a -> s {awsLambdaFunction = a} :: ResourceDetails)

-- | Details about a load balancer.
resourceDetails_awsElbv2LoadBalancer :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElbv2LoadBalancerDetails)
resourceDetails_awsElbv2LoadBalancer = Lens.lens (\ResourceDetails' {awsElbv2LoadBalancer} -> awsElbv2LoadBalancer) (\s@ResourceDetails' {} a -> s {awsElbv2LoadBalancer = a} :: ResourceDetails)

-- | Details about a rate-based rule for global resources.
resourceDetails_awsWafRateBasedRule :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafRateBasedRuleDetails)
resourceDetails_awsWafRateBasedRule = Lens.lens (\ResourceDetails' {awsWafRateBasedRule} -> awsWafRateBasedRule) (\s@ResourceDetails' {} a -> s {awsWafRateBasedRule = a} :: ResourceDetails)

-- | Information about an Amazon Elastic Container Registry repository.
resourceDetails_awsEcrRepository :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcrRepositoryDetails)
resourceDetails_awsEcrRepository = Lens.lens (\ResourceDetails' {awsEcrRepository} -> awsEcrRepository) (\s@ResourceDetails' {} a -> s {awsEcrRepository = a} :: ResourceDetails)

-- | Details for an WAF WebACL.
resourceDetails_awsWafWebAcl :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsWafWebAclDetails)
resourceDetails_awsWafWebAcl = Lens.lens (\ResourceDetails' {awsWafWebAcl} -> awsWafWebAcl) (\s@ResourceDetails' {} a -> s {awsWafWebAcl = a} :: ResourceDetails)

-- | Provides information about the state of a patch on an instance based on
-- the patch baseline that was used to patch the instance.
resourceDetails_awsSsmPatchCompliance :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsSsmPatchComplianceDetails)
resourceDetails_awsSsmPatchCompliance = Lens.lens (\ResourceDetails' {awsSsmPatchCompliance} -> awsSsmPatchCompliance) (\s@ResourceDetails' {} a -> s {awsSsmPatchCompliance = a} :: ResourceDetails)

-- | Details for an Elasticsearch domain.
resourceDetails_awsElasticsearchDomain :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsElasticsearchDomainDetails)
resourceDetails_awsElasticsearchDomain = Lens.lens (\ResourceDetails' {awsElasticsearchDomain} -> awsElasticsearchDomain) (\s@ResourceDetails' {} a -> s {awsElasticsearchDomain = a} :: ResourceDetails)

-- | Provides details about an Certificate Manager certificate.
resourceDetails_awsCertificateManagerCertificate :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsCertificateManagerCertificateDetails)
resourceDetails_awsCertificateManagerCertificate = Lens.lens (\ResourceDetails' {awsCertificateManagerCertificate} -> awsCertificateManagerCertificate) (\s@ResourceDetails' {} a -> s {awsCertificateManagerCertificate = a} :: ResourceDetails)

-- | Information about an Amazon ECR image.
resourceDetails_awsEcrContainerImage :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEcrContainerImageDetails)
resourceDetails_awsEcrContainerImage = Lens.lens (\ResourceDetails' {awsEcrContainerImage} -> awsEcrContainerImage) (\s@ResourceDetails' {} a -> s {awsEcrContainerImage = a} :: ResourceDetails)

-- | Details about an IAM user.
resourceDetails_awsIamUser :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsIamUserDetails)
resourceDetails_awsIamUser = Lens.lens (\ResourceDetails' {awsIamUser} -> awsIamUser) (\s@ResourceDetails' {} a -> s {awsIamUser = a} :: ResourceDetails)

-- | Details for an EC2 volume.
resourceDetails_awsEc2Volume :: Lens.Lens' ResourceDetails (Prelude.Maybe AwsEc2VolumeDetails)
resourceDetails_awsEc2Volume = Lens.lens (\ResourceDetails' {awsEc2Volume} -> awsEc2Volume) (\s@ResourceDetails' {} a -> s {awsEc2Volume = a} :: ResourceDetails)

instance Core.FromJSON ResourceDetails where
  parseJSON =
    Core.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails'
            Prelude.<$> (x Core..:? "AwsCloudTrailTrail")
            Prelude.<*> (x Core..:? "AwsRdsDbCluster")
            Prelude.<*> (x Core..:? "Other" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AwsIamRole")
            Prelude.<*> (x Core..:? "AwsElbLoadBalancer")
            Prelude.<*> (x Core..:? "AwsEc2VpnConnection")
            Prelude.<*> (x Core..:? "AwsCloudFrontDistribution")
            Prelude.<*> (x Core..:? "AwsKmsKey")
            Prelude.<*> (x Core..:? "AwsEksCluster")
            Prelude.<*> (x Core..:? "AwsAutoScalingLaunchConfiguration")
            Prelude.<*> (x Core..:? "AwsOpenSearchServiceDomain")
            Prelude.<*> (x Core..:? "AwsRdsDbSnapshot")
            Prelude.<*> (x Core..:? "AwsWafRegionalRateBasedRule")
            Prelude.<*> (x Core..:? "AwsApiGatewayRestApi")
            Prelude.<*> (x Core..:? "AwsSqsQueue")
            Prelude.<*> (x Core..:? "AwsSecretsManagerSecret")
            Prelude.<*> (x Core..:? "AwsDynamoDbTable")
            Prelude.<*> (x Core..:? "AwsS3AccountPublicAccessBlock")
            Prelude.<*> (x Core..:? "AwsRdsDbClusterSnapshot")
            Prelude.<*> (x Core..:? "AwsEc2SecurityGroup")
            Prelude.<*> (x Core..:? "AwsEc2Instance")
            Prelude.<*> (x Core..:? "AwsIamPolicy")
            Prelude.<*> (x Core..:? "AwsS3Object")
            Prelude.<*> (x Core..:? "AwsEcsTaskDefinition")
            Prelude.<*> (x Core..:? "AwsLambdaLayerVersion")
            Prelude.<*> (x Core..:? "AwsS3Bucket")
            Prelude.<*> (x Core..:? "AwsIamAccessKey")
            Prelude.<*> (x Core..:? "AwsCodeBuildProject")
            Prelude.<*> (x Core..:? "AwsApiGatewayStage")
            Prelude.<*> (x Core..:? "AwsEc2NetworkAcl")
            Prelude.<*> (x Core..:? "AwsApiGatewayV2Api")
            Prelude.<*> (x Core..:? "AwsElasticBeanstalkEnvironment")
            Prelude.<*> (x Core..:? "AwsEc2Vpc")
            Prelude.<*> (x Core..:? "AwsRedshiftCluster")
            Prelude.<*> (x Core..:? "AwsRdsDbInstance")
            Prelude.<*> (x Core..:? "AwsRdsEventSubscription")
            Prelude.<*> (x Core..:? "Container")
            Prelude.<*> (x Core..:? "AwsIamGroup")
            Prelude.<*> (x Core..:? "AwsXrayEncryptionConfig")
            Prelude.<*> (x Core..:? "AwsEc2Subnet")
            Prelude.<*> (x Core..:? "AwsEcsCluster")
            Prelude.<*> (x Core..:? "AwsEc2VpcEndpointService")
            Prelude.<*> (x Core..:? "AwsAutoScalingAutoScalingGroup")
            Prelude.<*> (x Core..:? "AwsSnsTopic")
            Prelude.<*> (x Core..:? "AwsEc2NetworkInterface")
            Prelude.<*> (x Core..:? "AwsEcsService")
            Prelude.<*> (x Core..:? "AwsEc2Eip")
            Prelude.<*> (x Core..:? "AwsApiGatewayV2Stage")
            Prelude.<*> (x Core..:? "AwsLambdaFunction")
            Prelude.<*> (x Core..:? "AwsElbv2LoadBalancer")
            Prelude.<*> (x Core..:? "AwsWafRateBasedRule")
            Prelude.<*> (x Core..:? "AwsEcrRepository")
            Prelude.<*> (x Core..:? "AwsWafWebAcl")
            Prelude.<*> (x Core..:? "AwsSsmPatchCompliance")
            Prelude.<*> (x Core..:? "AwsElasticsearchDomain")
            Prelude.<*> (x Core..:? "AwsCertificateManagerCertificate")
            Prelude.<*> (x Core..:? "AwsEcrContainerImage")
            Prelude.<*> (x Core..:? "AwsIamUser")
            Prelude.<*> (x Core..:? "AwsEc2Volume")
      )

instance Prelude.Hashable ResourceDetails where
  hashWithSalt salt' ResourceDetails' {..} =
    salt' `Prelude.hashWithSalt` awsEc2Volume
      `Prelude.hashWithSalt` awsIamUser
      `Prelude.hashWithSalt` awsEcrContainerImage
      `Prelude.hashWithSalt` awsCertificateManagerCertificate
      `Prelude.hashWithSalt` awsElasticsearchDomain
      `Prelude.hashWithSalt` awsSsmPatchCompliance
      `Prelude.hashWithSalt` awsWafWebAcl
      `Prelude.hashWithSalt` awsEcrRepository
      `Prelude.hashWithSalt` awsWafRateBasedRule
      `Prelude.hashWithSalt` awsElbv2LoadBalancer
      `Prelude.hashWithSalt` awsLambdaFunction
      `Prelude.hashWithSalt` awsApiGatewayV2Stage
      `Prelude.hashWithSalt` awsEc2Eip
      `Prelude.hashWithSalt` awsEcsService
      `Prelude.hashWithSalt` awsEc2NetworkInterface
      `Prelude.hashWithSalt` awsSnsTopic
      `Prelude.hashWithSalt` awsAutoScalingAutoScalingGroup
      `Prelude.hashWithSalt` awsEc2VpcEndpointService
      `Prelude.hashWithSalt` awsEcsCluster
      `Prelude.hashWithSalt` awsEc2Subnet
      `Prelude.hashWithSalt` awsXrayEncryptionConfig
      `Prelude.hashWithSalt` awsIamGroup
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` awsRdsEventSubscription
      `Prelude.hashWithSalt` awsRdsDbInstance
      `Prelude.hashWithSalt` awsRedshiftCluster
      `Prelude.hashWithSalt` awsEc2Vpc
      `Prelude.hashWithSalt` awsElasticBeanstalkEnvironment
      `Prelude.hashWithSalt` awsApiGatewayV2Api
      `Prelude.hashWithSalt` awsEc2NetworkAcl
      `Prelude.hashWithSalt` awsApiGatewayStage
      `Prelude.hashWithSalt` awsCodeBuildProject
      `Prelude.hashWithSalt` awsIamAccessKey
      `Prelude.hashWithSalt` awsS3Bucket
      `Prelude.hashWithSalt` awsLambdaLayerVersion
      `Prelude.hashWithSalt` awsEcsTaskDefinition
      `Prelude.hashWithSalt` awsS3Object
      `Prelude.hashWithSalt` awsIamPolicy
      `Prelude.hashWithSalt` awsEc2Instance
      `Prelude.hashWithSalt` awsEc2SecurityGroup
      `Prelude.hashWithSalt` awsRdsDbClusterSnapshot
      `Prelude.hashWithSalt` awsS3AccountPublicAccessBlock
      `Prelude.hashWithSalt` awsDynamoDbTable
      `Prelude.hashWithSalt` awsSecretsManagerSecret
      `Prelude.hashWithSalt` awsSqsQueue
      `Prelude.hashWithSalt` awsApiGatewayRestApi
      `Prelude.hashWithSalt` awsWafRegionalRateBasedRule
      `Prelude.hashWithSalt` awsRdsDbSnapshot
      `Prelude.hashWithSalt` awsOpenSearchServiceDomain
      `Prelude.hashWithSalt` awsAutoScalingLaunchConfiguration
      `Prelude.hashWithSalt` awsEksCluster
      `Prelude.hashWithSalt` awsKmsKey
      `Prelude.hashWithSalt` awsCloudFrontDistribution
      `Prelude.hashWithSalt` awsEc2VpnConnection
      `Prelude.hashWithSalt` awsElbLoadBalancer
      `Prelude.hashWithSalt` awsIamRole
      `Prelude.hashWithSalt` other
      `Prelude.hashWithSalt` awsRdsDbCluster
      `Prelude.hashWithSalt` awsCloudTrailTrail

instance Prelude.NFData ResourceDetails where
  rnf ResourceDetails' {..} =
    Prelude.rnf awsCloudTrailTrail
      `Prelude.seq` Prelude.rnf awsEc2Volume
      `Prelude.seq` Prelude.rnf awsIamUser
      `Prelude.seq` Prelude.rnf awsEcrContainerImage
      `Prelude.seq` Prelude.rnf awsCertificateManagerCertificate
      `Prelude.seq` Prelude.rnf awsElasticsearchDomain
      `Prelude.seq` Prelude.rnf awsSsmPatchCompliance
      `Prelude.seq` Prelude.rnf awsWafWebAcl
      `Prelude.seq` Prelude.rnf awsEcrRepository
      `Prelude.seq` Prelude.rnf awsWafRateBasedRule
      `Prelude.seq` Prelude.rnf awsElbv2LoadBalancer
      `Prelude.seq` Prelude.rnf awsLambdaFunction
      `Prelude.seq` Prelude.rnf awsApiGatewayV2Stage
      `Prelude.seq` Prelude.rnf awsEc2Eip
      `Prelude.seq` Prelude.rnf awsEcsService
      `Prelude.seq` Prelude.rnf awsEc2NetworkInterface
      `Prelude.seq` Prelude.rnf awsSnsTopic
      `Prelude.seq` Prelude.rnf awsAutoScalingAutoScalingGroup
      `Prelude.seq` Prelude.rnf awsEc2VpcEndpointService
      `Prelude.seq` Prelude.rnf awsEcsCluster
      `Prelude.seq` Prelude.rnf awsEc2Subnet
      `Prelude.seq` Prelude.rnf awsXrayEncryptionConfig
      `Prelude.seq` Prelude.rnf awsIamGroup
      `Prelude.seq` Prelude.rnf container
      `Prelude.seq` Prelude.rnf awsRdsEventSubscription
      `Prelude.seq` Prelude.rnf awsRdsDbInstance
      `Prelude.seq` Prelude.rnf awsRedshiftCluster
      `Prelude.seq` Prelude.rnf awsEc2Vpc
      `Prelude.seq` Prelude.rnf awsElasticBeanstalkEnvironment
      `Prelude.seq` Prelude.rnf awsApiGatewayV2Api
      `Prelude.seq` Prelude.rnf awsEc2NetworkAcl
      `Prelude.seq` Prelude.rnf awsApiGatewayStage
      `Prelude.seq` Prelude.rnf awsCodeBuildProject
      `Prelude.seq` Prelude.rnf awsIamAccessKey
      `Prelude.seq` Prelude.rnf awsS3Bucket
      `Prelude.seq` Prelude.rnf awsLambdaLayerVersion
      `Prelude.seq` Prelude.rnf awsEcsTaskDefinition
      `Prelude.seq` Prelude.rnf awsS3Object
      `Prelude.seq` Prelude.rnf awsIamPolicy
      `Prelude.seq` Prelude.rnf awsEc2Instance
      `Prelude.seq` Prelude.rnf awsEc2SecurityGroup
      `Prelude.seq` Prelude.rnf awsRdsDbClusterSnapshot
      `Prelude.seq` Prelude.rnf awsS3AccountPublicAccessBlock
      `Prelude.seq` Prelude.rnf awsDynamoDbTable
      `Prelude.seq` Prelude.rnf awsSecretsManagerSecret
      `Prelude.seq` Prelude.rnf awsSqsQueue
      `Prelude.seq` Prelude.rnf awsApiGatewayRestApi
      `Prelude.seq` Prelude.rnf awsWafRegionalRateBasedRule
      `Prelude.seq` Prelude.rnf awsRdsDbSnapshot
      `Prelude.seq` Prelude.rnf awsOpenSearchServiceDomain
      `Prelude.seq` Prelude.rnf awsAutoScalingLaunchConfiguration
      `Prelude.seq` Prelude.rnf awsEksCluster
      `Prelude.seq` Prelude.rnf awsKmsKey
      `Prelude.seq` Prelude.rnf awsCloudFrontDistribution
      `Prelude.seq` Prelude.rnf awsEc2VpnConnection
      `Prelude.seq` Prelude.rnf awsElbLoadBalancer
      `Prelude.seq` Prelude.rnf awsIamRole
      `Prelude.seq` Prelude.rnf other
      `Prelude.seq` Prelude.rnf awsRdsDbCluster

instance Core.ToJSON ResourceDetails where
  toJSON ResourceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AwsCloudTrailTrail" Core..=)
              Prelude.<$> awsCloudTrailTrail,
            ("AwsRdsDbCluster" Core..=)
              Prelude.<$> awsRdsDbCluster,
            ("Other" Core..=) Prelude.<$> other,
            ("AwsIamRole" Core..=) Prelude.<$> awsIamRole,
            ("AwsElbLoadBalancer" Core..=)
              Prelude.<$> awsElbLoadBalancer,
            ("AwsEc2VpnConnection" Core..=)
              Prelude.<$> awsEc2VpnConnection,
            ("AwsCloudFrontDistribution" Core..=)
              Prelude.<$> awsCloudFrontDistribution,
            ("AwsKmsKey" Core..=) Prelude.<$> awsKmsKey,
            ("AwsEksCluster" Core..=) Prelude.<$> awsEksCluster,
            ("AwsAutoScalingLaunchConfiguration" Core..=)
              Prelude.<$> awsAutoScalingLaunchConfiguration,
            ("AwsOpenSearchServiceDomain" Core..=)
              Prelude.<$> awsOpenSearchServiceDomain,
            ("AwsRdsDbSnapshot" Core..=)
              Prelude.<$> awsRdsDbSnapshot,
            ("AwsWafRegionalRateBasedRule" Core..=)
              Prelude.<$> awsWafRegionalRateBasedRule,
            ("AwsApiGatewayRestApi" Core..=)
              Prelude.<$> awsApiGatewayRestApi,
            ("AwsSqsQueue" Core..=) Prelude.<$> awsSqsQueue,
            ("AwsSecretsManagerSecret" Core..=)
              Prelude.<$> awsSecretsManagerSecret,
            ("AwsDynamoDbTable" Core..=)
              Prelude.<$> awsDynamoDbTable,
            ("AwsS3AccountPublicAccessBlock" Core..=)
              Prelude.<$> awsS3AccountPublicAccessBlock,
            ("AwsRdsDbClusterSnapshot" Core..=)
              Prelude.<$> awsRdsDbClusterSnapshot,
            ("AwsEc2SecurityGroup" Core..=)
              Prelude.<$> awsEc2SecurityGroup,
            ("AwsEc2Instance" Core..=)
              Prelude.<$> awsEc2Instance,
            ("AwsIamPolicy" Core..=) Prelude.<$> awsIamPolicy,
            ("AwsS3Object" Core..=) Prelude.<$> awsS3Object,
            ("AwsEcsTaskDefinition" Core..=)
              Prelude.<$> awsEcsTaskDefinition,
            ("AwsLambdaLayerVersion" Core..=)
              Prelude.<$> awsLambdaLayerVersion,
            ("AwsS3Bucket" Core..=) Prelude.<$> awsS3Bucket,
            ("AwsIamAccessKey" Core..=)
              Prelude.<$> awsIamAccessKey,
            ("AwsCodeBuildProject" Core..=)
              Prelude.<$> awsCodeBuildProject,
            ("AwsApiGatewayStage" Core..=)
              Prelude.<$> awsApiGatewayStage,
            ("AwsEc2NetworkAcl" Core..=)
              Prelude.<$> awsEc2NetworkAcl,
            ("AwsApiGatewayV2Api" Core..=)
              Prelude.<$> awsApiGatewayV2Api,
            ("AwsElasticBeanstalkEnvironment" Core..=)
              Prelude.<$> awsElasticBeanstalkEnvironment,
            ("AwsEc2Vpc" Core..=) Prelude.<$> awsEc2Vpc,
            ("AwsRedshiftCluster" Core..=)
              Prelude.<$> awsRedshiftCluster,
            ("AwsRdsDbInstance" Core..=)
              Prelude.<$> awsRdsDbInstance,
            ("AwsRdsEventSubscription" Core..=)
              Prelude.<$> awsRdsEventSubscription,
            ("Container" Core..=) Prelude.<$> container,
            ("AwsIamGroup" Core..=) Prelude.<$> awsIamGroup,
            ("AwsXrayEncryptionConfig" Core..=)
              Prelude.<$> awsXrayEncryptionConfig,
            ("AwsEc2Subnet" Core..=) Prelude.<$> awsEc2Subnet,
            ("AwsEcsCluster" Core..=) Prelude.<$> awsEcsCluster,
            ("AwsEc2VpcEndpointService" Core..=)
              Prelude.<$> awsEc2VpcEndpointService,
            ("AwsAutoScalingAutoScalingGroup" Core..=)
              Prelude.<$> awsAutoScalingAutoScalingGroup,
            ("AwsSnsTopic" Core..=) Prelude.<$> awsSnsTopic,
            ("AwsEc2NetworkInterface" Core..=)
              Prelude.<$> awsEc2NetworkInterface,
            ("AwsEcsService" Core..=) Prelude.<$> awsEcsService,
            ("AwsEc2Eip" Core..=) Prelude.<$> awsEc2Eip,
            ("AwsApiGatewayV2Stage" Core..=)
              Prelude.<$> awsApiGatewayV2Stage,
            ("AwsLambdaFunction" Core..=)
              Prelude.<$> awsLambdaFunction,
            ("AwsElbv2LoadBalancer" Core..=)
              Prelude.<$> awsElbv2LoadBalancer,
            ("AwsWafRateBasedRule" Core..=)
              Prelude.<$> awsWafRateBasedRule,
            ("AwsEcrRepository" Core..=)
              Prelude.<$> awsEcrRepository,
            ("AwsWafWebAcl" Core..=) Prelude.<$> awsWafWebAcl,
            ("AwsSsmPatchCompliance" Core..=)
              Prelude.<$> awsSsmPatchCompliance,
            ("AwsElasticsearchDomain" Core..=)
              Prelude.<$> awsElasticsearchDomain,
            ("AwsCertificateManagerCertificate" Core..=)
              Prelude.<$> awsCertificateManagerCertificate,
            ("AwsEcrContainerImage" Core..=)
              Prelude.<$> awsEcrContainerImage,
            ("AwsIamUser" Core..=) Prelude.<$> awsIamUser,
            ("AwsEc2Volume" Core..=) Prelude.<$> awsEc2Volume
          ]
      )
