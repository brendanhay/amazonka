{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalingActivity
  ( ScalingActivity (..),

    -- * Smart constructor
    mkScalingActivity,

    -- * Lenses
    sScalableDimension,
    sResourceId,
    sStartTime,
    sActivityId,
    sServiceNamespace,
    sCause,
    sStatusMessage,
    sEndTime,
    sDetails,
    sDescription,
    sStatusCode,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a scaling activity.
--
-- /See:/ 'mkScalingActivity' smart constructor.
data ScalingActivity = ScalingActivity'
  { -- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.
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
    scalableDimension :: ScalableDimension,
    -- | The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.
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
    resourceId :: Lude.Text,
    -- | The Unix timestamp for when the scaling activity began.
    startTime :: Lude.Timestamp,
    -- | The unique identifier of the scaling activity.
    activityId :: Lude.Text,
    -- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
    serviceNamespace :: ServiceNamespace,
    -- | A simple description of what caused the scaling activity to happen.
    cause :: Lude.Text,
    -- | A simple message about the current status of the scaling activity.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The Unix timestamp for when the scaling activity ended.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The details about the scaling activity.
    details :: Lude.Maybe Lude.Text,
    -- | A simple description of what action the scaling activity intends to accomplish.
    description :: Lude.Text,
    -- | Indicates the status of the scaling activity.
    statusCode :: ScalingActivityStatusCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingActivity' with the minimum fields required to make a request.
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
-- * 'resourceId' - The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.
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
-- * 'startTime' - The Unix timestamp for when the scaling activity began.
-- * 'activityId' - The unique identifier of the scaling activity.
-- * 'serviceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
-- * 'cause' - A simple description of what caused the scaling activity to happen.
-- * 'statusMessage' - A simple message about the current status of the scaling activity.
-- * 'endTime' - The Unix timestamp for when the scaling activity ended.
-- * 'details' - The details about the scaling activity.
-- * 'description' - A simple description of what action the scaling activity intends to accomplish.
-- * 'statusCode' - Indicates the status of the scaling activity.
mkScalingActivity ::
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'activityId'
  Lude.Text ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'cause'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'statusCode'
  ScalingActivityStatusCode ->
  ScalingActivity
mkScalingActivity
  pScalableDimension_
  pResourceId_
  pStartTime_
  pActivityId_
  pServiceNamespace_
  pCause_
  pDescription_
  pStatusCode_ =
    ScalingActivity'
      { scalableDimension = pScalableDimension_,
        resourceId = pResourceId_,
        startTime = pStartTime_,
        activityId = pActivityId_,
        serviceNamespace = pServiceNamespace_,
        cause = pCause_,
        statusMessage = Lude.Nothing,
        endTime = Lude.Nothing,
        details = Lude.Nothing,
        description = pDescription_,
        statusCode = pStatusCode_
      }

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
sScalableDimension :: Lens.Lens' ScalingActivity ScalableDimension
sScalableDimension = Lens.lens (scalableDimension :: ScalingActivity -> ScalableDimension) (\s a -> s {scalableDimension = a} :: ScalingActivity)
{-# DEPRECATED sScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.
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
sResourceId :: Lens.Lens' ScalingActivity Lude.Text
sResourceId = Lens.lens (resourceId :: ScalingActivity -> Lude.Text) (\s a -> s {resourceId = a} :: ScalingActivity)
{-# DEPRECATED sResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The Unix timestamp for when the scaling activity began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartTime :: Lens.Lens' ScalingActivity Lude.Timestamp
sStartTime = Lens.lens (startTime :: ScalingActivity -> Lude.Timestamp) (\s a -> s {startTime = a} :: ScalingActivity)
{-# DEPRECATED sStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier of the scaling activity.
--
-- /Note:/ Consider using 'activityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActivityId :: Lens.Lens' ScalingActivity Lude.Text
sActivityId = Lens.lens (activityId :: ScalingActivity -> Lude.Text) (\s a -> s {activityId = a} :: ScalingActivity)
{-# DEPRECATED sActivityId "Use generic-lens or generic-optics with 'activityId' instead." #-}

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServiceNamespace :: Lens.Lens' ScalingActivity ServiceNamespace
sServiceNamespace = Lens.lens (serviceNamespace :: ScalingActivity -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: ScalingActivity)
{-# DEPRECATED sServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | A simple description of what caused the scaling activity to happen.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCause :: Lens.Lens' ScalingActivity Lude.Text
sCause = Lens.lens (cause :: ScalingActivity -> Lude.Text) (\s a -> s {cause = a} :: ScalingActivity)
{-# DEPRECATED sCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | A simple message about the current status of the scaling activity.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatusMessage :: Lens.Lens' ScalingActivity (Lude.Maybe Lude.Text)
sStatusMessage = Lens.lens (statusMessage :: ScalingActivity -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ScalingActivity)
{-# DEPRECATED sStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The Unix timestamp for when the scaling activity ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndTime :: Lens.Lens' ScalingActivity (Lude.Maybe Lude.Timestamp)
sEndTime = Lens.lens (endTime :: ScalingActivity -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: ScalingActivity)
{-# DEPRECATED sEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The details about the scaling activity.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDetails :: Lens.Lens' ScalingActivity (Lude.Maybe Lude.Text)
sDetails = Lens.lens (details :: ScalingActivity -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: ScalingActivity)
{-# DEPRECATED sDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | A simple description of what action the scaling activity intends to accomplish.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDescription :: Lens.Lens' ScalingActivity Lude.Text
sDescription = Lens.lens (description :: ScalingActivity -> Lude.Text) (\s a -> s {description = a} :: ScalingActivity)
{-# DEPRECATED sDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates the status of the scaling activity.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatusCode :: Lens.Lens' ScalingActivity ScalingActivityStatusCode
sStatusCode = Lens.lens (statusCode :: ScalingActivity -> ScalingActivityStatusCode) (\s a -> s {statusCode = a} :: ScalingActivity)
{-# DEPRECATED sStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON ScalingActivity where
  parseJSON =
    Lude.withObject
      "ScalingActivity"
      ( \x ->
          ScalingActivity'
            Lude.<$> (x Lude..: "ScalableDimension")
            Lude.<*> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "StartTime")
            Lude.<*> (x Lude..: "ActivityId")
            Lude.<*> (x Lude..: "ServiceNamespace")
            Lude.<*> (x Lude..: "Cause")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "Details")
            Lude.<*> (x Lude..: "Description")
            Lude.<*> (x Lude..: "StatusCode")
      )
