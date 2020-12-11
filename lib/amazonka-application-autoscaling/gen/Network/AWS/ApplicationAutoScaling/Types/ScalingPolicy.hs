-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy
  ( ScalingPolicy (..),

    -- * Smart constructor
    mkScalingPolicy,

    -- * Lenses
    spTargetTrackingScalingPolicyConfiguration,
    spStepScalingPolicyConfiguration,
    spAlarms,
    spPolicyARN,
    spPolicyName,
    spServiceNamespace,
    spResourceId,
    spScalableDimension,
    spPolicyType,
    spCreationTime,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.Alarm
import Network.AWS.ApplicationAutoScaling.Types.PolicyType
import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a scaling policy to use with Application Auto Scaling.
--
-- For more information about configuring scaling policies for a specific service, see <https://docs.aws.amazon.com/autoscaling/application/userguide/getting-started.html Getting started with Application Auto Scaling> in the /Application Auto Scaling User Guide/ .
--
-- /See:/ 'mkScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { targetTrackingScalingPolicyConfiguration ::
      Lude.Maybe TargetTrackingScalingPolicyConfiguration,
    stepScalingPolicyConfiguration ::
      Lude.Maybe StepScalingPolicyConfiguration,
    alarms :: Lude.Maybe [Alarm],
    policyARN :: Lude.Text,
    policyName :: Lude.Text,
    serviceNamespace :: ServiceNamespace,
    resourceId :: Lude.Text,
    scalableDimension :: ScalableDimension,
    policyType :: PolicyType,
    creationTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- * 'alarms' - The CloudWatch alarms associated with the scaling policy.
-- * 'creationTime' - The Unix timestamp for when the scaling policy was created.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the scaling policy.
-- * 'policyName' - The name of the scaling policy.
-- * 'policyType' - The scaling policy type.
-- * 'resourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
--
--     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
--
--     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
--
--
--     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .
--
--
--     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .
--
--
--     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .
--
--
--     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
--
--     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
--
-- * 'scalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
--
--     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
--     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
--
--     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
--
--
--     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.
--
--
--     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.
--
--
--     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.
--
--
--     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.
--
--
--     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
--
--     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
--
-- * 'serviceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
-- * 'stepScalingPolicyConfiguration' - A step scaling policy.
-- * 'targetTrackingScalingPolicyConfiguration' - A target tracking scaling policy.
mkScalingPolicy ::
  -- | 'policyARN'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'policyType'
  PolicyType ->
  -- | 'creationTime'
  Lude.Timestamp ->
  ScalingPolicy
mkScalingPolicy
  pPolicyARN_
  pPolicyName_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pPolicyType_
  pCreationTime_ =
    ScalingPolicy'
      { targetTrackingScalingPolicyConfiguration =
          Lude.Nothing,
        stepScalingPolicyConfiguration = Lude.Nothing,
        alarms = Lude.Nothing,
        policyARN = pPolicyARN_,
        policyName = pPolicyName_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        policyType = pPolicyType_,
        creationTime = pCreationTime_
      }

-- | A target tracking scaling policy.
--
-- /Note:/ Consider using 'targetTrackingScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTargetTrackingScalingPolicyConfiguration :: Lens.Lens' ScalingPolicy (Lude.Maybe TargetTrackingScalingPolicyConfiguration)
spTargetTrackingScalingPolicyConfiguration = Lens.lens (targetTrackingScalingPolicyConfiguration :: ScalingPolicy -> Lude.Maybe TargetTrackingScalingPolicyConfiguration) (\s a -> s {targetTrackingScalingPolicyConfiguration = a} :: ScalingPolicy)
{-# DEPRECATED spTargetTrackingScalingPolicyConfiguration "Use generic-lens or generic-optics with 'targetTrackingScalingPolicyConfiguration' instead." #-}

-- | A step scaling policy.
--
-- /Note:/ Consider using 'stepScalingPolicyConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStepScalingPolicyConfiguration :: Lens.Lens' ScalingPolicy (Lude.Maybe StepScalingPolicyConfiguration)
spStepScalingPolicyConfiguration = Lens.lens (stepScalingPolicyConfiguration :: ScalingPolicy -> Lude.Maybe StepScalingPolicyConfiguration) (\s a -> s {stepScalingPolicyConfiguration = a} :: ScalingPolicy)
{-# DEPRECATED spStepScalingPolicyConfiguration "Use generic-lens or generic-optics with 'stepScalingPolicyConfiguration' instead." #-}

-- | The CloudWatch alarms associated with the scaling policy.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAlarms :: Lens.Lens' ScalingPolicy (Lude.Maybe [Alarm])
spAlarms = Lens.lens (alarms :: ScalingPolicy -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: ScalingPolicy)
{-# DEPRECATED spAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

-- | The Amazon Resource Name (ARN) of the scaling policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyARN :: Lens.Lens' ScalingPolicy Lude.Text
spPolicyARN = Lens.lens (policyARN :: ScalingPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The name of the scaling policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyName :: Lens.Lens' ScalingPolicy Lude.Text
spPolicyName = Lens.lens (policyName :: ScalingPolicy -> Lude.Text) (\s a -> s {policyName = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spServiceNamespace :: Lens.Lens' ScalingPolicy ServiceNamespace
spServiceNamespace = Lens.lens (serviceNamespace :: ScalingPolicy -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: ScalingPolicy)
{-# DEPRECATED spServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .
--
--
--     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .
--
--
--     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .
--
--
--     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .
--
--
--     * Amazon Comprehend entity recognizer endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE@ .
--
--
--     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .
--
--
--     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
--
--     * Amazon MSK cluster - The resource type and unique identifier are specified using the cluster ARN. Example: @arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@ .
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spResourceId :: Lens.Lens' ScalingPolicy Lude.Text
spResourceId = Lens.lens (resourceId :: ScalingPolicy -> Lude.Text) (\s a -> s {resourceId = a} :: ScalingPolicy)
{-# DEPRECATED spResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.
--
--
--     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
--     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.
--
--
--     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.
--
--
--     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.
--
--
--     * @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend entity recognizer endpoint.
--
--
--     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.
--
--
--     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.
--
--
--     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
--
--     * @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in GiB) for brokers in an Amazon MSK cluster.
--
--
--
-- /Note:/ Consider using 'scalableDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spScalableDimension :: Lens.Lens' ScalingPolicy ScalableDimension
spScalableDimension = Lens.lens (scalableDimension :: ScalingPolicy -> ScalableDimension) (\s a -> s {scalableDimension = a} :: ScalingPolicy)
{-# DEPRECATED spScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The scaling policy type.
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPolicyType :: Lens.Lens' ScalingPolicy PolicyType
spPolicyType = Lens.lens (policyType :: ScalingPolicy -> PolicyType) (\s a -> s {policyType = a} :: ScalingPolicy)
{-# DEPRECATED spPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The Unix timestamp for when the scaling policy was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spCreationTime :: Lens.Lens' ScalingPolicy Lude.Timestamp
spCreationTime = Lens.lens (creationTime :: ScalingPolicy -> Lude.Timestamp) (\s a -> s {creationTime = a} :: ScalingPolicy)
{-# DEPRECATED spCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Lude.FromJSON ScalingPolicy where
  parseJSON =
    Lude.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Lude.<$> (x Lude..:? "TargetTrackingScalingPolicyConfiguration")
            Lude.<*> (x Lude..:? "StepScalingPolicyConfiguration")
            Lude.<*> (x Lude..:? "Alarms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "PolicyARN")
            Lude.<*> (x Lude..: "PolicyName")
            Lude.<*> (x Lude..: "ServiceNamespace")
            Lude.<*> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ScalableDimension")
            Lude.<*> (x Lude..: "PolicyType")
            Lude.<*> (x Lude..: "CreationTime")
      )
