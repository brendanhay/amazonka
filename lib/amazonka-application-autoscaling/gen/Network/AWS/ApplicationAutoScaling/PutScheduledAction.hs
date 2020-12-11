{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.PutScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled action for an Application Auto Scaling scalable target.
--
-- Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scheduled action applies to the scalable target identified by those three attributes. You cannot create a scheduled action until you have registered the resource as a scalable target.
-- When start and end times are specified with a recurring schedule using a cron expression or rates, they form the boundaries of when the recurring action starts and stops.
-- To update a scheduled action, specify the parameters that you want to change. If you don't specify start and end times, the old values are deleted.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
module Network.AWS.ApplicationAutoScaling.PutScheduledAction
  ( -- * Creating a request
    PutScheduledAction (..),
    mkPutScheduledAction,

    -- ** Request lenses
    psaStartTime,
    psaSchedule,
    psaEndTime,
    psaScalableTargetAction,
    psaServiceNamespace,
    psaScheduledActionName,
    psaResourceId,
    psaScalableDimension,

    -- * Destructuring the response
    PutScheduledActionResponse (..),
    mkPutScheduledActionResponse,

    -- ** Response lenses
    psarsResponseStatus,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutScheduledAction' smart constructor.
data PutScheduledAction = PutScheduledAction'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    schedule :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    scalableTargetAction ::
      Lude.Maybe ScalableTargetAction,
    serviceNamespace :: ServiceNamespace,
    scheduledActionName :: Lude.Text,
    resourceId :: Lude.Text,
    scalableDimension :: ScalableDimension
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScheduledAction' with the minimum fields required to make a request.
--
-- * 'endTime' - The date and time for the recurring schedule to end.
-- * 'resourceId' - The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.
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
-- * 'scalableTargetAction' - The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
-- * 'schedule' - The schedule for this action. The following formats are supported:
--
--
--     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "
--
--
--     * Rate expressions - "@rate(/value/ /unit/ )@ "
--
--
--     * Cron expressions - "@cron(/fields/ )@ "
--
--
-- At expressions are useful for one-time schedules. Specify the time in UTC.
-- For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ .
-- For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
-- For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
-- * 'scheduledActionName' - The name of the scheduled action. This name must be unique among all other scheduled actions on the specified scalable target.
-- * 'serviceNamespace' - The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
-- * 'startTime' - The date and time for this scheduled action to start.
mkPutScheduledAction ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'scheduledActionName'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  PutScheduledAction
mkPutScheduledAction
  pServiceNamespace_
  pScheduledActionName_
  pResourceId_
  pScalableDimension_ =
    PutScheduledAction'
      { startTime = Lude.Nothing,
        schedule = Lude.Nothing,
        endTime = Lude.Nothing,
        scalableTargetAction = Lude.Nothing,
        serviceNamespace = pServiceNamespace_,
        scheduledActionName = pScheduledActionName_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_
      }

-- | The date and time for this scheduled action to start.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaStartTime :: Lens.Lens' PutScheduledAction (Lude.Maybe Lude.Timestamp)
psaStartTime = Lens.lens (startTime :: PutScheduledAction -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: PutScheduledAction)
{-# DEPRECATED psaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The schedule for this action. The following formats are supported:
--
--
--     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "
--
--
--     * Rate expressions - "@rate(/value/ /unit/ )@ "
--
--
--     * Cron expressions - "@cron(/fields/ )@ "
--
--
-- At expressions are useful for one-time schedules. Specify the time in UTC.
-- For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ .
-- For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
-- For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaSchedule :: Lens.Lens' PutScheduledAction (Lude.Maybe Lude.Text)
psaSchedule = Lens.lens (schedule :: PutScheduledAction -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: PutScheduledAction)
{-# DEPRECATED psaSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The date and time for the recurring schedule to end.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaEndTime :: Lens.Lens' PutScheduledAction (Lude.Maybe Lude.Timestamp)
psaEndTime = Lens.lens (endTime :: PutScheduledAction -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: PutScheduledAction)
{-# DEPRECATED psaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
--
-- /Note:/ Consider using 'scalableTargetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaScalableTargetAction :: Lens.Lens' PutScheduledAction (Lude.Maybe ScalableTargetAction)
psaScalableTargetAction = Lens.lens (scalableTargetAction :: PutScheduledAction -> Lude.Maybe ScalableTargetAction) (\s a -> s {scalableTargetAction = a} :: PutScheduledAction)
{-# DEPRECATED psaScalableTargetAction "Use generic-lens or generic-optics with 'scalableTargetAction' instead." #-}

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaServiceNamespace :: Lens.Lens' PutScheduledAction ServiceNamespace
psaServiceNamespace = Lens.lens (serviceNamespace :: PutScheduledAction -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: PutScheduledAction)
{-# DEPRECATED psaServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The name of the scheduled action. This name must be unique among all other scheduled actions on the specified scalable target.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psaScheduledActionName :: Lens.Lens' PutScheduledAction Lude.Text
psaScheduledActionName = Lens.lens (scheduledActionName :: PutScheduledAction -> Lude.Text) (\s a -> s {scheduledActionName = a} :: PutScheduledAction)
{-# DEPRECATED psaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The identifier of the resource associated with the scheduled action. This string consists of the resource type and unique identifier.
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
psaResourceId :: Lens.Lens' PutScheduledAction Lude.Text
psaResourceId = Lens.lens (resourceId :: PutScheduledAction -> Lude.Text) (\s a -> s {resourceId = a} :: PutScheduledAction)
{-# DEPRECATED psaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

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
psaScalableDimension :: Lens.Lens' PutScheduledAction ScalableDimension
psaScalableDimension = Lens.lens (scalableDimension :: PutScheduledAction -> ScalableDimension) (\s a -> s {scalableDimension = a} :: PutScheduledAction)
{-# DEPRECATED psaScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

instance Lude.AWSRequest PutScheduledAction where
  type Rs PutScheduledAction = PutScheduledActionResponse
  request = Req.postJSON applicationAutoScalingService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutScheduledActionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutScheduledAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AnyScaleFrontendService.PutScheduledAction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutScheduledAction where
  toJSON PutScheduledAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("ScalableTargetAction" Lude..=) Lude.<$> scalableTargetAction,
            Lude.Just ("ServiceNamespace" Lude..= serviceNamespace),
            Lude.Just ("ScheduledActionName" Lude..= scheduledActionName),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ScalableDimension" Lude..= scalableDimension)
          ]
      )

instance Lude.ToPath PutScheduledAction where
  toPath = Lude.const "/"

instance Lude.ToQuery PutScheduledAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutScheduledActionResponse' smart constructor.
newtype PutScheduledActionResponse = PutScheduledActionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutScheduledActionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutScheduledActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutScheduledActionResponse
mkPutScheduledActionResponse pResponseStatus_ =
  PutScheduledActionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psarsResponseStatus :: Lens.Lens' PutScheduledActionResponse Lude.Int
psarsResponseStatus = Lens.lens (responseStatus :: PutScheduledActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutScheduledActionResponse)
{-# DEPRECATED psarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
