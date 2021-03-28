{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Resource Groups Tagging API__ 
--
-- This guide describes the API operations for the resource groups tagging.
-- A tag is a label that you assign to an AWS resource. A tag consists of a key and a value, both of which you define. For example, if you have two Amazon EC2 instances, you might assign both a tag key of "Stack." But the value of "Stack" might be "Testing" for one and "Production" for the other.
-- /Important:/ Do not store personally identifiable information (PII) or other confidential or sensitive information in tags. We use tags to provide you with billing and administration services. Tags are not intended to be used for private or sensitive data.
-- Tagging can help you organize your resources and enables you to simplify resource management, access management and cost allocation. 
-- You can use the resource groups tagging API operations to complete the following tasks:
--
--     * Tag and untag supported resources located in the specified Region for the AWS account.
--
--
--     * Use tag-based filters to search for resources located in the specified Region for the AWS account.
--
--
--     * List all existing tag keys in the specified Region for the AWS account.
--
--
--     * List all existing values for the specified key in the specified Region for the AWS account.
--
--
-- To use resource groups tagging API operations, you must add the following permissions to your IAM policy:
--
--     * @tag:GetResources@ 
--
--
--     * @tag:TagResources@ 
--
--
--     * @tag:UntagResources@ 
--
--
--     * @tag:GetTagKeys@ 
--
--
--     * @tag:GetTagValues@ 
--
--
-- You'll also need permissions to access the resources of individual services so that you can tag and untag those resources.
-- For more information on IAM policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage.html Managing IAM Policies> in the /IAM User Guide/ .
-- /__Services that support the Resource Groups Tagging API__ / 
-- You can use the Resource Groups Tagging API to tag resources for the following AWS services.
--
--     * <https://docs.aws.amazon.com/a4b Alexa for Business (a4b)> 
--
--
--     * <https://docs.aws.amazon.com/apigateway API Gateway> 
--
--
--     * <https://docs.aws.amazon.com/appstream2 Amazon AppStream> 
--
--
--     * <https://docs.aws.amazon.com/appsync AWS AppSync> 
--
--
--     * <https://docs.aws.amazon.com/app-mesh AWS App Mesh> 
--
--
--     * <https://docs.aws.amazon.com/athena Amazon Athena> 
--
--
--     * <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide Amazon Aurora> 
--
--
--     * <https://docs.aws.amazon.com/aws-backup AWS Backup> 
--
--
--     * <https://docs.aws.amazon.com/acm AWS Certificate Manager> 
--
--
--     * <https://docs.aws.amazon.com/acm AWS Certificate Manager Private CA> 
--
--
--     * <https://docs.aws.amazon.com/clouddirectory Amazon Cloud Directory> 
--
--
--     * <https://docs.aws.amazon.com/cloud-map AWS Cloud Map> 
--
--
--     * <https://docs.aws.amazon.com/cloudformation AWS CloudFormation> 
--
--
--     * <https://docs.aws.amazon.com/cloudfront Amazon CloudFront> 
--
--
--     * <https://docs.aws.amazon.com/cloudhsm AWS CloudHSM> 
--
--
--     * <https://docs.aws.amazon.com/cloudtrail AWS CloudTrail> 
--
--
--     * <https://docs.aws.amazon.com/cloudwatch Amazon CloudWatch (alarms only)> 
--
--
--     * <https://docs.aws.amazon.com/cloudwatch/?id=docs_gateway#amazon-cloudwatch-events Amazon CloudWatch Events> 
--
--
--     * <https://docs.aws.amazon.com/cloudwatch/?id=docs_gateway#amazon-cloudwatch-logs Amazon CloudWatch Logs> 
--
--
--     * <https://docs.aws.amazon.com/cloudwatch Amazon Cloudwatch Synthetics> 
--
--
--     * <https://docs.aws.amazon.com/codebuild AWS CodeBuild> 
--
--
--     * <https://docs.aws.amazon.com/codecommit AWS CodeCommit> 
--
--
--     * <https://docs.aws.amazon.com/codeguru/latest/profiler-ug/ AWS CodeGuru Profiler> 
--
--
--     * <https://docs.aws.amazon.com/codepipeline AWS CodePipeline> 
--
--
--     * <https://docs.aws.amazon.com/codestar AWS CodeStar> 
--
--
--     * <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/ AWS CodeStar Connections> 
--
--
--     * <https://docs.aws.amazon.com/cognito Amazon Cognito Identity> 
--
--
--     * <https://docs.aws.amazon.com/cognito Amazon Cognito User Pools> 
--
--
--     * <https://docs.aws.amazon.com/comprehend Amazon Comprehend> 
--
--
--     * <https://docs.aws.amazon.com/config AWS Config> 
--
--
--     * <http://aws.amazon.com/connect/resources/?whats-new-cards#Documentation Amazon Connect> 
--
--
--     * <https://docs.aws.amazon.com/data-exchange AWS Data Exchange> 
--
--
--     * <https://docs.aws.amazon.com/data-pipeline AWS Data Pipeline> 
--
--
--     * <https://docs.aws.amazon.com/dms AWS Database Migration Service> 
--
--
--     * <https://docs.aws.amazon.com/datasync AWS DataSync> 
--
--
--     * <https://docs.aws.amazon.com/devicefarm AWS Device Farm> 
--
--
--     * <https://docs.aws.amazon.com/directconnect AWS Direct Connect> 
--
--
--     * <https://docs.aws.amazon.com/directory-service AWS Directory Service> 
--
--
--     * <https://docs.aws.amazon.com/dynamodb Amazon DynamoDB> 
--
--
--     * <https://docs.aws.amazon.com/ebs Amazon EBS> 
--
--
--     * <https://docs.aws.amazon.com/ec2 Amazon EC2> 
--
--
--     * <https://docs.aws.amazon.com/imagebuilder EC2 Image Builder> 
--
--
--     * <https://docs.aws.amazon.com/ecr Amazon ECR> 
--
--
--     * <https://docs.aws.amazon.com/ecs Amazon ECS> 
--
--
--     * <https://docs.aws.amazon.com/eks Amazon EKS> 
--
--
--     * <https://docs.aws.amazon.com/elastic-beanstalk AWS Elastic Beanstalk> 
--
--
--     * <https://docs.aws.amazon.com/efs Amazon Elastic File System> 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing Elastic Load Balancing> 
--
--
--     * <https://docs.aws.amazon.com/elastic-inference Amazon Elastic Inference> 
--
--
--     * <https://docs.aws.amazon.com/elasticache Amazon ElastiCache> 
--
--
--     * <https://docs.aws.amazon.com/elasticsearch-service Amazon Elasticsearch Service> 
--
--
--     * <https://docs.aws.amazon.com/medialive AWS Elemental MediaLive> 
--
--
--     * <https://docs.aws.amazon.com/mediapackage AWS Elemental MediaPackage> 
--
--
--     * <https://docs.aws.amazon.com/mediapackage AWS Elemental MediaPackage VoD> 
--
--
--     * <https://docs.aws.amazon.com/mediatailor AWS Elemental MediaTailor> 
--
--
--     * <https://docs.aws.amazon.com/emr Amazon EMR> 
--
--
--     * <https://docs.aws.amazon.com/eventbridge Amazon EventBridge Schema> 
--
--
--     * <https://docs.aws.amazon.com/firewall-manager AWS Firewall Manager> 
--
--
--     * <https://docs.aws.amazon.com/forecast Amazon Forecast> 
--
--
--     * <https://docs.aws.amazon.com/frauddetector Amazon Fraud Detector> 
--
--
--     * <https://docs.aws.amazon.com/fsx Amazon FSx> 
--
--
--     * <https://docs.aws.amazon.com/s3/?id=docs_gateway#amazon-s3-glacier Amazon S3 Glacier> 
--
--
--     * <https://docs.aws.amazon.com/global-accelerator AWS Global Accelerator> 
--
--
--     * <https://docs.aws.amazon.com/ground-station AWS Ground Station> 
--
--
--     * <https://docs.aws.amazon.com/glue AWS Glue> 
--
--
--     * <https://docs.aws.amazon.com/guardduty Amazon GuardDuty> 
--
--
--     * <https://docs.aws.amazon.com/inspector Amazon Inspector> 
--
--
--     * <https://docs.aws.amazon.com/ivs Amazon Interactive Video Service> 
--
--
--     * <https://docs.aws.amazon.com/iotanalytics AWS IoT Analytics> 
--
--
--     * <https://docs.aws.amazon.com/iot AWS IoT Core> 
--
--
--     * <https://docs.aws.amazon.com/iot-device-defender AWS IoT Device Defender> 
--
--
--     * <https://docs.aws.amazon.com/iot-device-management AWS IoT Device Management> 
--
--
--     * <https://docs.aws.amazon.com/iotevents AWS IoT Events> 
--
--
--     * <https://docs.aws.amazon.com/greengrass AWS IoT Greengrass> 
--
--
--     * <https://docs.aws.amazon.com/iot-1-click AWS IoT 1-Click> 
--
--
--     * <https://docs.aws.amazon.com/iot-sitewise AWS IoT Sitewise> 
--
--
--     * <https://docs.aws.amazon.com/thingsgraph AWS IoT Things Graph> 
--
--
--     * <https://docs.aws.amazon.com/kendra Amazon Kendra> 
--
--
--     * <https://docs.aws.amazon.com/kms AWS Key Management Service> 
--
--
--     * <https://docs.aws.amazon.com/kinesis Amazon Kinesis> 
--
--
--     * <https://docs.aws.amazon.com/kinesis/?id=docs_gateway#amazon-kinesis-data-analytics Amazon Kinesis Data Analytics> 
--
--
--     * <https://docs.aws.amazon.com/kinesis/?id=docs_gateway#amazon-kinesis-data-firehose Amazon Kinesis Data Firehose> 
--
--
--     * <https://docs.aws.amazon.com/lambda AWS Lambda> 
--
--
--     * <https://docs.aws.amazon.com/lex Amazon Lex> 
--
--
--     * <https://docs.aws.amazon.com/license-manager AWS License Manager> 
--
--
--     * <https://docs.aws.amazon.com/lightsail Amazon Lightsail> 
--
--
--     * <https://docs.aws.amazon.com/macie Amazon Macie> 
--
--
--     * <https://docs.aws.amazon.com/machine-learning Amazon Machine Learning> 
--
--
--     * <https://docs.aws.amazon.com/amazon-mq Amazon MQ> 
--
--
--     * <https://docs.aws.amazon.com/msk Amazon MSK> 
--
--
--     * <https://docs.aws.amazon.com/msk Amazon MSK> 
--
--
--     * <https://docs.aws.amazon.com/neptune Amazon Neptune> 
--
--
--     * <https://docs.aws.amazon.com/vpc/latest/tgw/what-is-network-manager.html AWS Network Manager> 
--
--
--     * <https://docs.aws.amazon.com/opsworks AWS OpsWorks> 
--
--
--     * <https://docs.aws.amazon.com/opsworks AWS OpsWorks CM> 
--
--
--     * <https://docs.aws.amazon.com/organizations AWS Organizations> 
--
--
--     * <https://docs.aws.amazon.com/pinpoint Amazon Pinpoint> 
--
--
--     * <https://docs.aws.amazon.com/qldb Amazon Quantum Ledger Database (QLDB)> 
--
--
--     * <https://docs.aws.amazon.com/rds Amazon RDS> 
--
--
--     * <https://docs.aws.amazon.com/redshift Amazon Redshift> 
--
--
--     * <https://docs.aws.amazon.com/ram AWS Resource Access Manager> 
--
--
--     * <https://docs.aws.amazon.com/ARG AWS Resource Groups> 
--
--
--     * <https://docs.aws.amazon.com/robomaker AWS RoboMaker> 
--
--
--     * <https://docs.aws.amazon.com/route53 Amazon Route 53> 
--
--
--     * <https://docs.aws.amazon.com/route53 Amazon Route 53 Resolver> 
--
--
--     * <https://docs.aws.amazon.com/s3 Amazon S3 (buckets only)> 
--
--
--     * <https://docs.aws.amazon.com/sagemaker Amazon SageMaker> 
--
--
--     * <https://docs.aws.amazon.com/savingsplans Savings Plans> 
--
--
--     * <https://docs.aws.amazon.com/secretsmanager AWS Secrets Manager> 
--
--
--     * <https://docs.aws.amazon.com/securityhub AWS Security Hub> 
--
--
--     * <https://docs.aws.amazon.com/servicecatalog AWS Service Catalog> 
--
--
--     * <https://docs.aws.amazon.com/ses Amazon Simple Email Service (SES)> 
--
--
--     * <https://docs.aws.amazon.com/sns Amazon Simple Notification Service (SNS)> 
--
--
--     * <https://docs.aws.amazon.com/sqs Amazon Simple Queue Service (SQS)> 
--
--
--     * <https://docs.aws.amazon.com/swf Amazon Simple Workflow Service> 
--
--
--     * <https://docs.aws.amazon.com/step-functions AWS Step Functions> 
--
--
--     * <https://docs.aws.amazon.com/storagegateway AWS Storage Gateway> 
--
--
--     * <https://docs.aws.amazon.com/systems-manager AWS Systems Manager> 
--
--
--     * <https://docs.aws.amazon.com/transfer AWS Transfer for SFTP> 
--
--
--     * <https://docs.aws.amazon.com/vpc Amazon VPC> 
--
--
--     * <https://docs.aws.amazon.com/waf AWS WAF> 
--
--
--     * <https://docs.aws.amazon.com/waf AWS WAF Regional> 
--
--
--     * <https://docs.aws.amazon.com/worklink Amazon WorkLink> 
--
--
--     * <https://docs.aws.amazon.com/workspaces Amazon WorkSpaces> 
--
--
module Network.AWS.ResourceGroupsTagging
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ConstraintViolationException
    , _ConstraintViolationException

    -- ** ThrottledException
    , _ThrottledException

    -- ** PaginationTokenExpiredException
    , _PaginationTokenExpiredException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InternalServiceException
    , _InternalServiceException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetComplianceSummary (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetComplianceSummary

    -- ** DescribeReportCreation 
    , module Network.AWS.ResourceGroupsTagging.DescribeReportCreation

    -- ** GetTagKeys (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetTagKeys

    -- ** TagResources 
    , module Network.AWS.ResourceGroupsTagging.TagResources

    -- ** GetTagValues (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetTagValues

    -- ** StartReportCreation 
    , module Network.AWS.ResourceGroupsTagging.StartReportCreation

    -- ** GetResources (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetResources

    -- ** UntagResources 
    , module Network.AWS.ResourceGroupsTagging.UntagResources

    -- * Types

    -- ** ResourceTagMapping
    , ResourceTagMapping (..)
    , mkResourceTagMapping
    , rtmComplianceDetails
    , rtmResourceARN
    , rtmTags

    -- ** Status
    , Status (..)

    -- ** TargetId
    , TargetId (..)

    -- ** Summary
    , Summary (..)
    , mkSummary
    , sLastUpdated
    , sNonCompliantResources
    , sRegion
    , sResourceType
    , sTargetId
    , sTargetIdType

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** ComplianceDetails
    , ComplianceDetails (..)
    , mkComplianceDetails
    , cdComplianceStatus
    , cdKeysWithNoncompliantValues
    , cdNoncompliantKeys

    -- ** LastUpdated
    , LastUpdated (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** GroupByAttribute
    , GroupByAttribute (..)

    -- ** FailureInfo
    , FailureInfo (..)
    , mkFailureInfo
    , fiErrorCode
    , fiErrorMessage
    , fiStatusCode

    -- ** TagValue
    , TagValue (..)

    -- ** AmazonResourceType
    , AmazonResourceType (..)

    -- ** TargetIdType
    , TargetIdType (..)

    -- ** ResourceARN
    , ResourceARN (..)

    -- ** TagFilter
    , TagFilter (..)
    , mkTagFilter
    , tfKey
    , tfValues

    -- ** ResourceErrorCode
    , ResourceErrorCode (..)

    -- ** TagKey
    , TagKey (..)

    -- ** S3Location
    , S3Location (..)

    -- ** Region
    , Region (..)

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** S3Bucket
    , S3Bucket (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.Waiters
import Network.AWS.ResourceGroupsTagging.GetComplianceSummary
import Network.AWS.ResourceGroupsTagging.DescribeReportCreation
import Network.AWS.ResourceGroupsTagging.GetTagKeys
import Network.AWS.ResourceGroupsTagging.TagResources
import Network.AWS.ResourceGroupsTagging.GetTagValues
import Network.AWS.ResourceGroupsTagging.StartReportCreation
import Network.AWS.ResourceGroupsTagging.GetResources
import Network.AWS.ResourceGroupsTagging.UntagResources
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ResourceGroupsTagging'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
