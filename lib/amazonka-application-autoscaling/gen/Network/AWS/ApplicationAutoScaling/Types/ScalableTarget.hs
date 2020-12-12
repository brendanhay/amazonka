{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalableTarget
  ( ScalableTarget (..),

    -- * Smart constructor
    mkScalableTarget,

    -- * Lenses
    stSuspendedState,
    stServiceNamespace,
    stResourceId,
    stScalableDimension,
    stMinCapacity,
    stMaxCapacity,
    stRoleARN,
    stCreationTime,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import Network.AWS.ApplicationAutoScaling.Types.SuspendedState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a scalable target.
--
-- /See:/ 'mkScalableTarget' smart constructor.
data ScalableTarget = ScalableTarget'
  { suspendedState ::
      Lude.Maybe SuspendedState,
    serviceNamespace :: ServiceNamespace,
    resourceId :: Lude.Text,
    scalableDimension :: ScalableDimension,
    minCapacity :: Lude.Int,
    maxCapacity :: Lude.Int,
    roleARN :: Lude.Text,
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

-- | Creates a value of 'ScalableTarget' with the minimum fields required to make a request.
--
-- * 'creationTime' - The Unix timestamp for when the scalable target was created.
-- * 'maxCapacity' - The maximum value to scale to in response to a scale-out activity.
-- * 'minCapacity' - The minimum value to scale to in response to a scale-in activity.
-- * 'resourceId' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.
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
-- * 'roleARN' - The ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
-- * 'scalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.
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
-- * 'suspendedState' - Undocumented field.
mkScalableTarget ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'minCapacity'
  Lude.Int ->
  -- | 'maxCapacity'
  Lude.Int ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  ScalableTarget
mkScalableTarget
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pMinCapacity_
  pMaxCapacity_
  pRoleARN_
  pCreationTime_ =
    ScalableTarget'
      { suspendedState = Lude.Nothing,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        minCapacity = pMinCapacity_,
        maxCapacity = pMaxCapacity_,
        roleARN = pRoleARN_,
        creationTime = pCreationTime_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'suspendedState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSuspendedState :: Lens.Lens' ScalableTarget (Lude.Maybe SuspendedState)
stSuspendedState = Lens.lens (suspendedState :: ScalableTarget -> Lude.Maybe SuspendedState) (\s a -> s {suspendedState = a} :: ScalableTarget)
{-# DEPRECATED stSuspendedState "Use generic-lens or generic-optics with 'suspendedState' instead." #-}

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stServiceNamespace :: Lens.Lens' ScalableTarget ServiceNamespace
stServiceNamespace = Lens.lens (serviceNamespace :: ScalableTarget -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: ScalableTarget)
{-# DEPRECATED stServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.
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
stResourceId :: Lens.Lens' ScalableTarget Lude.Text
stResourceId = Lens.lens (resourceId :: ScalableTarget -> Lude.Text) (\s a -> s {resourceId = a} :: ScalableTarget)
{-# DEPRECATED stResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.
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
stScalableDimension :: Lens.Lens' ScalableTarget ScalableDimension
stScalableDimension = Lens.lens (scalableDimension :: ScalableTarget -> ScalableDimension) (\s a -> s {scalableDimension = a} :: ScalableTarget)
{-# DEPRECATED stScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The minimum value to scale to in response to a scale-in activity.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stMinCapacity :: Lens.Lens' ScalableTarget Lude.Int
stMinCapacity = Lens.lens (minCapacity :: ScalableTarget -> Lude.Int) (\s a -> s {minCapacity = a} :: ScalableTarget)
{-# DEPRECATED stMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

-- | The maximum value to scale to in response to a scale-out activity.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stMaxCapacity :: Lens.Lens' ScalableTarget Lude.Int
stMaxCapacity = Lens.lens (maxCapacity :: ScalableTarget -> Lude.Int) (\s a -> s {maxCapacity = a} :: ScalableTarget)
{-# DEPRECATED stMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stRoleARN :: Lens.Lens' ScalableTarget Lude.Text
stRoleARN = Lens.lens (roleARN :: ScalableTarget -> Lude.Text) (\s a -> s {roleARN = a} :: ScalableTarget)
{-# DEPRECATED stRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Unix timestamp for when the scalable target was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCreationTime :: Lens.Lens' ScalableTarget Lude.Timestamp
stCreationTime = Lens.lens (creationTime :: ScalableTarget -> Lude.Timestamp) (\s a -> s {creationTime = a} :: ScalableTarget)
{-# DEPRECATED stCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Lude.FromJSON ScalableTarget where
  parseJSON =
    Lude.withObject
      "ScalableTarget"
      ( \x ->
          ScalableTarget'
            Lude.<$> (x Lude..:? "SuspendedState")
            Lude.<*> (x Lude..: "ServiceNamespace")
            Lude.<*> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ScalableDimension")
            Lude.<*> (x Lude..: "MinCapacity")
            Lude.<*> (x Lude..: "MaxCapacity")
            Lude.<*> (x Lude..: "RoleARN")
            Lude.<*> (x Lude..: "CreationTime")
      )
