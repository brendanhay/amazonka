{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScheduledAction where

import Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
import Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
import Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a scheduled action.
--
-- /See:/ 'newScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { -- | The date and time that the action is scheduled to begin, in UTC.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time that the action is scheduled to end, in UTC.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The scalable dimension. This string consists of the service namespace,
    -- resource type, and scaling property.
    --
    -- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
    --     service.
    --
    -- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
    --     Spot Fleet request.
    --
    -- -   @elasticmapreduce:instancegroup:InstanceCount@ - The instance count
    --     of an EMR Instance Group.
    --
    -- -   @appstream:fleet:DesiredCapacity@ - The desired capacity of an
    --     AppStream 2.0 fleet.
    --
    -- -   @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity
    --     for a DynamoDB table.
    --
    -- -   @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity
    --     for a DynamoDB table.
    --
    -- -   @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity
    --     for a DynamoDB global secondary index.
    --
    -- -   @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity
    --     for a DynamoDB global secondary index.
    --
    -- -   @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an
    --     Aurora DB cluster. Available for Aurora MySQL-compatible edition and
    --     Aurora PostgreSQL-compatible edition.
    --
    -- -   @sagemaker:variant:DesiredInstanceCount@ - The number of EC2
    --     instances for an Amazon SageMaker model endpoint variant.
    --
    -- -   @custom-resource:ResourceType:Property@ - The scalable dimension for
    --     a custom resource provided by your own application or service.
    --
    -- -   @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ -
    --     The number of inference units for an Amazon Comprehend document
    --     classification endpoint.
    --
    -- -   @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The
    --     number of inference units for an Amazon Comprehend entity recognizer
    --     endpoint.
    --
    -- -   @lambda:function:ProvisionedConcurrency@ - The provisioned
    --     concurrency for a Lambda function.
    --
    -- -   @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity
    --     for an Amazon Keyspaces table.
    --
    -- -   @cassandra:table:WriteCapacityUnits@ - The provisioned write
    --     capacity for an Amazon Keyspaces table.
    --
    -- -   @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in
    --     GiB) for brokers in an Amazon MSK cluster.
    scalableDimension :: Prelude.Maybe ScalableDimension,
    -- | The time zone used when referring to the date and time of a scheduled
    -- action, when the scheduled action uses an at or cron expression.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The new minimum and maximum capacity. You can set both values or just
    -- one. At the scheduled time, if the current capacity is below the minimum
    -- capacity, Application Auto Scaling scales out to the minimum capacity.
    -- If the current capacity is above the maximum capacity, Application Auto
    -- Scaling scales in to the maximum capacity.
    scalableTargetAction :: Prelude.Maybe ScalableTargetAction,
    -- | The name of the scheduled action.
    scheduledActionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the scheduled action.
    scheduledActionARN :: Prelude.Text,
    -- | The namespace of the AWS service that provides the resource, or a
    -- @custom-resource@.
    serviceNamespace :: ServiceNamespace,
    -- | The schedule for this action. The following formats are supported:
    --
    -- -   At expressions - \"@at(yyyy-mm-ddThh:mm:ss)@\"
    --
    -- -   Rate expressions - \"@rate(value unit)@\"
    --
    -- -   Cron expressions - \"@cron(fields)@\"
    --
    -- At expressions are useful for one-time schedules. Cron expressions are
    -- useful for scheduled actions that run periodically at a specified date
    -- and time, and rate expressions are useful for scheduled actions that run
    -- at a regular interval.
    --
    -- At and cron expressions use Universal Coordinated Time (UTC) by default.
    --
    -- The cron format consists of six fields separated by white spaces:
    -- [Minutes] [Hours] [Day_of_Month] [Month] [Day_of_Week] [Year].
    --
    -- For rate expressions, /value/ is a positive integer and /unit/ is
    -- @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@.
    --
    -- For more information and examples, see
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/examples-scheduled-actions.html Example scheduled actions for Application Auto Scaling>
    -- in the /Application Auto Scaling User Guide/.
    schedule :: Prelude.Text,
    -- | The identifier of the resource associated with the scaling policy. This
    -- string consists of the resource type and unique identifier.
    --
    -- -   ECS service - The resource type is @service@ and the unique
    --     identifier is the cluster name and service name. Example:
    --     @service\/default\/sample-webapp@.
    --
    -- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
    --     the unique identifier is the Spot Fleet request ID. Example:
    --     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
    --
    -- -   EMR cluster - The resource type is @instancegroup@ and the unique
    --     identifier is the cluster ID and instance group ID. Example:
    --     @instancegroup\/j-2EEZNYKUA1NTV\/ig-1791Y4E1L8YI0@.
    --
    -- -   AppStream 2.0 fleet - The resource type is @fleet@ and the unique
    --     identifier is the fleet name. Example: @fleet\/sample-fleet@.
    --
    -- -   DynamoDB table - The resource type is @table@ and the unique
    --     identifier is the table name. Example: @table\/my-table@.
    --
    -- -   DynamoDB global secondary index - The resource type is @index@ and
    --     the unique identifier is the index name. Example:
    --     @table\/my-table\/index\/my-table-index@.
    --
    -- -   Aurora DB cluster - The resource type is @cluster@ and the unique
    --     identifier is the cluster name. Example: @cluster:my-db-cluster@.
    --
    -- -   Amazon SageMaker endpoint variant - The resource type is @variant@
    --     and the unique identifier is the resource ID. Example:
    --     @endpoint\/my-end-point\/variant\/KMeansClustering@.
    --
    -- -   Custom resources are not supported with a resource type. This
    --     parameter must specify the @OutputValue@ from the CloudFormation
    --     template stack used to access the resources. The unique identifier
    --     is defined by the service provider. More information is available in
    --     our
    --     <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository>.
    --
    -- -   Amazon Comprehend document classification endpoint - The resource
    --     type and unique identifier are specified using the endpoint ARN.
    --     Example:
    --     @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint\/EXAMPLE@.
    --
    -- -   Amazon Comprehend entity recognizer endpoint - The resource type and
    --     unique identifier are specified using the endpoint ARN. Example:
    --     @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint\/EXAMPLE@.
    --
    -- -   Lambda provisioned concurrency - The resource type is @function@ and
    --     the unique identifier is the function name with a function version
    --     or alias name suffix that is not @$LATEST@. Example:
    --     @function:my-function:prod@ or @function:my-function:1@.
    --
    -- -   Amazon Keyspaces table - The resource type is @table@ and the unique
    --     identifier is the table name. Example:
    --     @keyspace\/mykeyspace\/table\/mytable@.
    --
    -- -   Amazon MSK cluster - The resource type and unique identifier are
    --     specified using the cluster ARN. Example:
    --     @arn:aws:kafka:us-east-1:123456789012:cluster\/demo-cluster-1\/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@.
    resourceId :: Prelude.Text,
    -- | The date and time that the scheduled action was created.
    creationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'scheduledAction_startTime' - The date and time that the action is scheduled to begin, in UTC.
--
-- 'endTime', 'scheduledAction_endTime' - The date and time that the action is scheduled to end, in UTC.
--
-- 'scalableDimension', 'scheduledAction_scalableDimension' - The scalable dimension. This string consists of the service namespace,
-- resource type, and scaling property.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet request.
--
-- -   @elasticmapreduce:instancegroup:InstanceCount@ - The instance count
--     of an EMR Instance Group.
--
-- -   @appstream:fleet:DesiredCapacity@ - The desired capacity of an
--     AppStream 2.0 fleet.
--
-- -   @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB global secondary index.
--
-- -   @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB global secondary index.
--
-- -   @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an
--     Aurora DB cluster. Available for Aurora MySQL-compatible edition and
--     Aurora PostgreSQL-compatible edition.
--
-- -   @sagemaker:variant:DesiredInstanceCount@ - The number of EC2
--     instances for an Amazon SageMaker model endpoint variant.
--
-- -   @custom-resource:ResourceType:Property@ - The scalable dimension for
--     a custom resource provided by your own application or service.
--
-- -   @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ -
--     The number of inference units for an Amazon Comprehend document
--     classification endpoint.
--
-- -   @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The
--     number of inference units for an Amazon Comprehend entity recognizer
--     endpoint.
--
-- -   @lambda:function:ProvisionedConcurrency@ - The provisioned
--     concurrency for a Lambda function.
--
-- -   @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity
--     for an Amazon Keyspaces table.
--
-- -   @cassandra:table:WriteCapacityUnits@ - The provisioned write
--     capacity for an Amazon Keyspaces table.
--
-- -   @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in
--     GiB) for brokers in an Amazon MSK cluster.
--
-- 'timezone', 'scheduledAction_timezone' - The time zone used when referring to the date and time of a scheduled
-- action, when the scheduled action uses an at or cron expression.
--
-- 'scalableTargetAction', 'scheduledAction_scalableTargetAction' - The new minimum and maximum capacity. You can set both values or just
-- one. At the scheduled time, if the current capacity is below the minimum
-- capacity, Application Auto Scaling scales out to the minimum capacity.
-- If the current capacity is above the maximum capacity, Application Auto
-- Scaling scales in to the maximum capacity.
--
-- 'scheduledActionName', 'scheduledAction_scheduledActionName' - The name of the scheduled action.
--
-- 'scheduledActionARN', 'scheduledAction_scheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
--
-- 'serviceNamespace', 'scheduledAction_serviceNamespace' - The namespace of the AWS service that provides the resource, or a
-- @custom-resource@.
--
-- 'schedule', 'scheduledAction_schedule' - The schedule for this action. The following formats are supported:
--
-- -   At expressions - \"@at(yyyy-mm-ddThh:mm:ss)@\"
--
-- -   Rate expressions - \"@rate(value unit)@\"
--
-- -   Cron expressions - \"@cron(fields)@\"
--
-- At expressions are useful for one-time schedules. Cron expressions are
-- useful for scheduled actions that run periodically at a specified date
-- and time, and rate expressions are useful for scheduled actions that run
-- at a regular interval.
--
-- At and cron expressions use Universal Coordinated Time (UTC) by default.
--
-- The cron format consists of six fields separated by white spaces:
-- [Minutes] [Hours] [Day_of_Month] [Month] [Day_of_Week] [Year].
--
-- For rate expressions, /value/ is a positive integer and /unit/ is
-- @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@.
--
-- For more information and examples, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/examples-scheduled-actions.html Example scheduled actions for Application Auto Scaling>
-- in the /Application Auto Scaling User Guide/.
--
-- 'resourceId', 'scheduledAction_resourceId' - The identifier of the resource associated with the scaling policy. This
-- string consists of the resource type and unique identifier.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
--     the unique identifier is the Spot Fleet request ID. Example:
--     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
--
-- -   EMR cluster - The resource type is @instancegroup@ and the unique
--     identifier is the cluster ID and instance group ID. Example:
--     @instancegroup\/j-2EEZNYKUA1NTV\/ig-1791Y4E1L8YI0@.
--
-- -   AppStream 2.0 fleet - The resource type is @fleet@ and the unique
--     identifier is the fleet name. Example: @fleet\/sample-fleet@.
--
-- -   DynamoDB table - The resource type is @table@ and the unique
--     identifier is the table name. Example: @table\/my-table@.
--
-- -   DynamoDB global secondary index - The resource type is @index@ and
--     the unique identifier is the index name. Example:
--     @table\/my-table\/index\/my-table-index@.
--
-- -   Aurora DB cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:my-db-cluster@.
--
-- -   Amazon SageMaker endpoint variant - The resource type is @variant@
--     and the unique identifier is the resource ID. Example:
--     @endpoint\/my-end-point\/variant\/KMeansClustering@.
--
-- -   Custom resources are not supported with a resource type. This
--     parameter must specify the @OutputValue@ from the CloudFormation
--     template stack used to access the resources. The unique identifier
--     is defined by the service provider. More information is available in
--     our
--     <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository>.
--
-- -   Amazon Comprehend document classification endpoint - The resource
--     type and unique identifier are specified using the endpoint ARN.
--     Example:
--     @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint\/EXAMPLE@.
--
-- -   Amazon Comprehend entity recognizer endpoint - The resource type and
--     unique identifier are specified using the endpoint ARN. Example:
--     @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint\/EXAMPLE@.
--
-- -   Lambda provisioned concurrency - The resource type is @function@ and
--     the unique identifier is the function name with a function version
--     or alias name suffix that is not @$LATEST@. Example:
--     @function:my-function:prod@ or @function:my-function:1@.
--
-- -   Amazon Keyspaces table - The resource type is @table@ and the unique
--     identifier is the table name. Example:
--     @keyspace\/mykeyspace\/table\/mytable@.
--
-- -   Amazon MSK cluster - The resource type and unique identifier are
--     specified using the cluster ARN. Example:
--     @arn:aws:kafka:us-east-1:123456789012:cluster\/demo-cluster-1\/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@.
--
-- 'creationTime', 'scheduledAction_creationTime' - The date and time that the scheduled action was created.
newScheduledAction ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  -- | 'scheduledActionARN'
  Prelude.Text ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'schedule'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ScheduledAction
newScheduledAction
  pScheduledActionName_
  pScheduledActionARN_
  pServiceNamespace_
  pSchedule_
  pResourceId_
  pCreationTime_ =
    ScheduledAction'
      { startTime = Prelude.Nothing,
        endTime = Prelude.Nothing,
        scalableDimension = Prelude.Nothing,
        timezone = Prelude.Nothing,
        scalableTargetAction = Prelude.Nothing,
        scheduledActionName = pScheduledActionName_,
        scheduledActionARN = pScheduledActionARN_,
        serviceNamespace = pServiceNamespace_,
        schedule = pSchedule_,
        resourceId = pResourceId_,
        creationTime = Prelude._Time Lens.# pCreationTime_
      }

-- | The date and time that the action is scheduled to begin, in UTC.
scheduledAction_startTime :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.UTCTime)
scheduledAction_startTime = Lens.lens (\ScheduledAction' {startTime} -> startTime) (\s@ScheduledAction' {} a -> s {startTime = a} :: ScheduledAction) Prelude.. Lens.mapping Prelude._Time

-- | The date and time that the action is scheduled to end, in UTC.
scheduledAction_endTime :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.UTCTime)
scheduledAction_endTime = Lens.lens (\ScheduledAction' {endTime} -> endTime) (\s@ScheduledAction' {} a -> s {endTime = a} :: ScheduledAction) Prelude.. Lens.mapping Prelude._Time

-- | The scalable dimension. This string consists of the service namespace,
-- resource type, and scaling property.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet request.
--
-- -   @elasticmapreduce:instancegroup:InstanceCount@ - The instance count
--     of an EMR Instance Group.
--
-- -   @appstream:fleet:DesiredCapacity@ - The desired capacity of an
--     AppStream 2.0 fleet.
--
-- -   @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB global secondary index.
--
-- -   @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB global secondary index.
--
-- -   @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an
--     Aurora DB cluster. Available for Aurora MySQL-compatible edition and
--     Aurora PostgreSQL-compatible edition.
--
-- -   @sagemaker:variant:DesiredInstanceCount@ - The number of EC2
--     instances for an Amazon SageMaker model endpoint variant.
--
-- -   @custom-resource:ResourceType:Property@ - The scalable dimension for
--     a custom resource provided by your own application or service.
--
-- -   @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ -
--     The number of inference units for an Amazon Comprehend document
--     classification endpoint.
--
-- -   @comprehend:entity-recognizer-endpoint:DesiredInferenceUnits@ - The
--     number of inference units for an Amazon Comprehend entity recognizer
--     endpoint.
--
-- -   @lambda:function:ProvisionedConcurrency@ - The provisioned
--     concurrency for a Lambda function.
--
-- -   @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity
--     for an Amazon Keyspaces table.
--
-- -   @cassandra:table:WriteCapacityUnits@ - The provisioned write
--     capacity for an Amazon Keyspaces table.
--
-- -   @kafka:broker-storage:VolumeSize@ - The provisioned volume size (in
--     GiB) for brokers in an Amazon MSK cluster.
scheduledAction_scalableDimension :: Lens.Lens' ScheduledAction (Prelude.Maybe ScalableDimension)
scheduledAction_scalableDimension = Lens.lens (\ScheduledAction' {scalableDimension} -> scalableDimension) (\s@ScheduledAction' {} a -> s {scalableDimension = a} :: ScheduledAction)

-- | The time zone used when referring to the date and time of a scheduled
-- action, when the scheduled action uses an at or cron expression.
scheduledAction_timezone :: Lens.Lens' ScheduledAction (Prelude.Maybe Prelude.Text)
scheduledAction_timezone = Lens.lens (\ScheduledAction' {timezone} -> timezone) (\s@ScheduledAction' {} a -> s {timezone = a} :: ScheduledAction)

-- | The new minimum and maximum capacity. You can set both values or just
-- one. At the scheduled time, if the current capacity is below the minimum
-- capacity, Application Auto Scaling scales out to the minimum capacity.
-- If the current capacity is above the maximum capacity, Application Auto
-- Scaling scales in to the maximum capacity.
scheduledAction_scalableTargetAction :: Lens.Lens' ScheduledAction (Prelude.Maybe ScalableTargetAction)
scheduledAction_scalableTargetAction = Lens.lens (\ScheduledAction' {scalableTargetAction} -> scalableTargetAction) (\s@ScheduledAction' {} a -> s {scalableTargetAction = a} :: ScheduledAction)

-- | The name of the scheduled action.
scheduledAction_scheduledActionName :: Lens.Lens' ScheduledAction Prelude.Text
scheduledAction_scheduledActionName = Lens.lens (\ScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@ScheduledAction' {} a -> s {scheduledActionName = a} :: ScheduledAction)

-- | The Amazon Resource Name (ARN) of the scheduled action.
scheduledAction_scheduledActionARN :: Lens.Lens' ScheduledAction Prelude.Text
scheduledAction_scheduledActionARN = Lens.lens (\ScheduledAction' {scheduledActionARN} -> scheduledActionARN) (\s@ScheduledAction' {} a -> s {scheduledActionARN = a} :: ScheduledAction)

-- | The namespace of the AWS service that provides the resource, or a
-- @custom-resource@.
scheduledAction_serviceNamespace :: Lens.Lens' ScheduledAction ServiceNamespace
scheduledAction_serviceNamespace = Lens.lens (\ScheduledAction' {serviceNamespace} -> serviceNamespace) (\s@ScheduledAction' {} a -> s {serviceNamespace = a} :: ScheduledAction)

-- | The schedule for this action. The following formats are supported:
--
-- -   At expressions - \"@at(yyyy-mm-ddThh:mm:ss)@\"
--
-- -   Rate expressions - \"@rate(value unit)@\"
--
-- -   Cron expressions - \"@cron(fields)@\"
--
-- At expressions are useful for one-time schedules. Cron expressions are
-- useful for scheduled actions that run periodically at a specified date
-- and time, and rate expressions are useful for scheduled actions that run
-- at a regular interval.
--
-- At and cron expressions use Universal Coordinated Time (UTC) by default.
--
-- The cron format consists of six fields separated by white spaces:
-- [Minutes] [Hours] [Day_of_Month] [Month] [Day_of_Week] [Year].
--
-- For rate expressions, /value/ is a positive integer and /unit/ is
-- @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@.
--
-- For more information and examples, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/examples-scheduled-actions.html Example scheduled actions for Application Auto Scaling>
-- in the /Application Auto Scaling User Guide/.
scheduledAction_schedule :: Lens.Lens' ScheduledAction Prelude.Text
scheduledAction_schedule = Lens.lens (\ScheduledAction' {schedule} -> schedule) (\s@ScheduledAction' {} a -> s {schedule = a} :: ScheduledAction)

-- | The identifier of the resource associated with the scaling policy. This
-- string consists of the resource type and unique identifier.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
--     the unique identifier is the Spot Fleet request ID. Example:
--     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
--
-- -   EMR cluster - The resource type is @instancegroup@ and the unique
--     identifier is the cluster ID and instance group ID. Example:
--     @instancegroup\/j-2EEZNYKUA1NTV\/ig-1791Y4E1L8YI0@.
--
-- -   AppStream 2.0 fleet - The resource type is @fleet@ and the unique
--     identifier is the fleet name. Example: @fleet\/sample-fleet@.
--
-- -   DynamoDB table - The resource type is @table@ and the unique
--     identifier is the table name. Example: @table\/my-table@.
--
-- -   DynamoDB global secondary index - The resource type is @index@ and
--     the unique identifier is the index name. Example:
--     @table\/my-table\/index\/my-table-index@.
--
-- -   Aurora DB cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:my-db-cluster@.
--
-- -   Amazon SageMaker endpoint variant - The resource type is @variant@
--     and the unique identifier is the resource ID. Example:
--     @endpoint\/my-end-point\/variant\/KMeansClustering@.
--
-- -   Custom resources are not supported with a resource type. This
--     parameter must specify the @OutputValue@ from the CloudFormation
--     template stack used to access the resources. The unique identifier
--     is defined by the service provider. More information is available in
--     our
--     <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository>.
--
-- -   Amazon Comprehend document classification endpoint - The resource
--     type and unique identifier are specified using the endpoint ARN.
--     Example:
--     @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint\/EXAMPLE@.
--
-- -   Amazon Comprehend entity recognizer endpoint - The resource type and
--     unique identifier are specified using the endpoint ARN. Example:
--     @arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint\/EXAMPLE@.
--
-- -   Lambda provisioned concurrency - The resource type is @function@ and
--     the unique identifier is the function name with a function version
--     or alias name suffix that is not @$LATEST@. Example:
--     @function:my-function:prod@ or @function:my-function:1@.
--
-- -   Amazon Keyspaces table - The resource type is @table@ and the unique
--     identifier is the table name. Example:
--     @keyspace\/mykeyspace\/table\/mytable@.
--
-- -   Amazon MSK cluster - The resource type and unique identifier are
--     specified using the cluster ARN. Example:
--     @arn:aws:kafka:us-east-1:123456789012:cluster\/demo-cluster-1\/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5@.
scheduledAction_resourceId :: Lens.Lens' ScheduledAction Prelude.Text
scheduledAction_resourceId = Lens.lens (\ScheduledAction' {resourceId} -> resourceId) (\s@ScheduledAction' {} a -> s {resourceId = a} :: ScheduledAction)

-- | The date and time that the scheduled action was created.
scheduledAction_creationTime :: Lens.Lens' ScheduledAction Prelude.UTCTime
scheduledAction_creationTime = Lens.lens (\ScheduledAction' {creationTime} -> creationTime) (\s@ScheduledAction' {} a -> s {creationTime = a} :: ScheduledAction) Prelude.. Prelude._Time

instance Prelude.FromJSON ScheduledAction where
  parseJSON =
    Prelude.withObject
      "ScheduledAction"
      ( \x ->
          ScheduledAction'
            Prelude.<$> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "ScalableDimension")
            Prelude.<*> (x Prelude..:? "Timezone")
            Prelude.<*> (x Prelude..:? "ScalableTargetAction")
            Prelude.<*> (x Prelude..: "ScheduledActionName")
            Prelude.<*> (x Prelude..: "ScheduledActionARN")
            Prelude.<*> (x Prelude..: "ServiceNamespace")
            Prelude.<*> (x Prelude..: "Schedule")
            Prelude.<*> (x Prelude..: "ResourceId")
            Prelude.<*> (x Prelude..: "CreationTime")
      )

instance Prelude.Hashable ScheduledAction

instance Prelude.NFData ScheduledAction
