{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy where

import Network.AWS.ApplicationAutoScaling.Types.Alarm
import Network.AWS.ApplicationAutoScaling.Types.PolicyType
import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a scaling policy to use with Application Auto Scaling.
--
--
-- For more information about configuring scaling policies for a specific service, see <https://docs.aws.amazon.com/autoscaling/application/userguide/getting-started.html Getting started with Application Auto Scaling> in the /Application Auto Scaling User Guide/ .
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { _spTargetTrackingScalingPolicyConfiguration ::
      !(Maybe TargetTrackingScalingPolicyConfiguration),
    _spStepScalingPolicyConfiguration ::
      !(Maybe StepScalingPolicyConfiguration),
    _spAlarms :: !(Maybe [Alarm]),
    _spPolicyARN :: !Text,
    _spPolicyName :: !Text,
    _spServiceNamespace :: !ServiceNamespace,
    _spResourceId :: !Text,
    _spScalableDimension :: !ScalableDimension,
    _spPolicyType :: !PolicyType,
    _spCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spTargetTrackingScalingPolicyConfiguration' - A target tracking scaling policy.
--
-- * 'spStepScalingPolicyConfiguration' - A step scaling policy.
--
-- * 'spAlarms' - The CloudWatch alarms associated with the scaling policy.
--
-- * 'spPolicyARN' - The Amazon Resource Name (ARN) of the scaling policy.
--
-- * 'spPolicyName' - The name of the scaling policy.
--
-- * 'spServiceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- * 'spResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
-- * 'spScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
-- * 'spPolicyType' - The scaling policy type.
--
-- * 'spCreationTime' - The Unix timestamp for when the scaling policy was created.
scalingPolicy ::
  -- | 'spPolicyARN'
  Text ->
  -- | 'spPolicyName'
  Text ->
  -- | 'spServiceNamespace'
  ServiceNamespace ->
  -- | 'spResourceId'
  Text ->
  -- | 'spScalableDimension'
  ScalableDimension ->
  -- | 'spPolicyType'
  PolicyType ->
  -- | 'spCreationTime'
  UTCTime ->
  ScalingPolicy
scalingPolicy
  pPolicyARN_
  pPolicyName_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pPolicyType_
  pCreationTime_ =
    ScalingPolicy'
      { _spTargetTrackingScalingPolicyConfiguration =
          Nothing,
        _spStepScalingPolicyConfiguration = Nothing,
        _spAlarms = Nothing,
        _spPolicyARN = pPolicyARN_,
        _spPolicyName = pPolicyName_,
        _spServiceNamespace = pServiceNamespace_,
        _spResourceId = pResourceId_,
        _spScalableDimension = pScalableDimension_,
        _spPolicyType = pPolicyType_,
        _spCreationTime = _Time # pCreationTime_
      }

-- | A target tracking scaling policy.
spTargetTrackingScalingPolicyConfiguration :: Lens' ScalingPolicy (Maybe TargetTrackingScalingPolicyConfiguration)
spTargetTrackingScalingPolicyConfiguration = lens _spTargetTrackingScalingPolicyConfiguration (\s a -> s {_spTargetTrackingScalingPolicyConfiguration = a})

-- | A step scaling policy.
spStepScalingPolicyConfiguration :: Lens' ScalingPolicy (Maybe StepScalingPolicyConfiguration)
spStepScalingPolicyConfiguration = lens _spStepScalingPolicyConfiguration (\s a -> s {_spStepScalingPolicyConfiguration = a})

-- | The CloudWatch alarms associated with the scaling policy.
spAlarms :: Lens' ScalingPolicy [Alarm]
spAlarms = lens _spAlarms (\s a -> s {_spAlarms = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the scaling policy.
spPolicyARN :: Lens' ScalingPolicy Text
spPolicyARN = lens _spPolicyARN (\s a -> s {_spPolicyARN = a})

-- | The name of the scaling policy.
spPolicyName :: Lens' ScalingPolicy Text
spPolicyName = lens _spPolicyName (\s a -> s {_spPolicyName = a})

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
spServiceNamespace :: Lens' ScalingPolicy ServiceNamespace
spServiceNamespace = lens _spServiceNamespace (\s a -> s {_spServiceNamespace = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
spResourceId :: Lens' ScalingPolicy Text
spResourceId = lens _spResourceId (\s a -> s {_spResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
spScalableDimension :: Lens' ScalingPolicy ScalableDimension
spScalableDimension = lens _spScalableDimension (\s a -> s {_spScalableDimension = a})

-- | The scaling policy type.
spPolicyType :: Lens' ScalingPolicy PolicyType
spPolicyType = lens _spPolicyType (\s a -> s {_spPolicyType = a})

-- | The Unix timestamp for when the scaling policy was created.
spCreationTime :: Lens' ScalingPolicy UTCTime
spCreationTime = lens _spCreationTime (\s a -> s {_spCreationTime = a}) . _Time

instance FromJSON ScalingPolicy where
  parseJSON =
    withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            <$> (x .:? "TargetTrackingScalingPolicyConfiguration")
            <*> (x .:? "StepScalingPolicyConfiguration")
            <*> (x .:? "Alarms" .!= mempty)
            <*> (x .: "PolicyARN")
            <*> (x .: "PolicyName")
            <*> (x .: "ServiceNamespace")
            <*> (x .: "ResourceId")
            <*> (x .: "ScalableDimension")
            <*> (x .: "PolicyType")
            <*> (x .: "CreationTime")
      )

instance Hashable ScalingPolicy

instance NFData ScalingPolicy
