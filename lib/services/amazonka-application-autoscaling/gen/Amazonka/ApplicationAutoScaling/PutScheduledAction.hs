{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationAutoScaling.PutScheduledAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scheduled action for an Application Auto Scaling
-- scalable target.
--
-- Each scalable target is identified by a service namespace, resource ID,
-- and scalable dimension. A scheduled action applies to the scalable
-- target identified by those three attributes. You cannot create a
-- scheduled action until you have registered the resource as a scalable
-- target.
--
-- When start and end times are specified with a recurring schedule using a
-- cron expression or rates, they form the boundaries for when the
-- recurring action starts and stops.
--
-- To update a scheduled action, specify the parameters that you want to
-- change. If you don\'t specify start and end times, the old values are
-- deleted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled scaling>
-- in the /Application Auto Scaling User Guide/.
--
-- If a scalable target is deregistered, the scalable target is no longer
-- available to run scheduled actions. Any scheduled actions that were
-- specified for the scalable target are deleted.
module Amazonka.ApplicationAutoScaling.PutScheduledAction
  ( -- * Creating a Request
    PutScheduledAction (..),
    newPutScheduledAction,

    -- * Request Lenses
    putScheduledAction_endTime,
    putScheduledAction_scalableTargetAction,
    putScheduledAction_schedule,
    putScheduledAction_startTime,
    putScheduledAction_timezone,
    putScheduledAction_serviceNamespace,
    putScheduledAction_scheduledActionName,
    putScheduledAction_resourceId,
    putScheduledAction_scalableDimension,

    -- * Destructuring the Response
    PutScheduledActionResponse (..),
    newPutScheduledActionResponse,

    -- * Response Lenses
    putScheduledActionResponse_httpStatus,
  )
where

import Amazonka.ApplicationAutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutScheduledAction' smart constructor.
data PutScheduledAction = PutScheduledAction'
  { -- | The date and time for the recurring schedule to end, in UTC.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The new minimum and maximum capacity. You can set both values or just
    -- one. At the scheduled time, if the current capacity is below the minimum
    -- capacity, Application Auto Scaling scales out to the minimum capacity.
    -- If the current capacity is above the maximum capacity, Application Auto
    -- Scaling scales in to the maximum capacity.
    scalableTargetAction :: Prelude.Maybe ScalableTargetAction,
    -- | The schedule for this action. The following formats are supported:
    --
    -- -   At expressions -
    --     \"@at(@/@yyyy@/@-@/@mm@/@-@/@dd@/@T@/@hh@/@:@/@mm@/@:@/@ss@/@)@\"
    --
    -- -   Rate expressions - \"@rate(@/@value@/@ @/@unit@/@)@\"
    --
    -- -   Cron expressions - \"@cron(@/@fields@/@)@\"
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
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The date and time for this scheduled action to start, in UTC.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the time zone used when setting a scheduled action by using an
    -- at or cron expression. If a time zone is not provided, UTC is used by
    -- default.
    --
    -- Valid values are the canonical names of the IANA time zones supported by
    -- Joda-Time (such as @Etc\/GMT+9@ or @Pacific\/Tahiti@). For more
    -- information, see <https://www.joda.org/joda-time/timezones.html>.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the Amazon Web Services service that provides the
    -- resource. For a resource provided by your own application or service,
    -- use @custom-resource@ instead.
    serviceNamespace :: ServiceNamespace,
    -- | The name of the scheduled action. This name must be unique among all
    -- other scheduled actions on the specified scalable target.
    scheduledActionName :: Prelude.Text,
    -- | The identifier of the resource associated with the scheduled action.
    -- This string consists of the resource type and unique identifier.
    --
    -- -   ECS service - The resource type is @service@ and the unique
    --     identifier is the cluster name and service name. Example:
    --     @service\/default\/sample-webapp@.
    --
    -- -   Spot Fleet - The resource type is @spot-fleet-request@ and the
    --     unique identifier is the Spot Fleet request ID. Example:
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
    -- -   SageMaker endpoint variant - The resource type is @variant@ and the
    --     unique identifier is the resource ID. Example:
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
    -- -   Amazon ElastiCache replication group - The resource type is
    --     @replication-group@ and the unique identifier is the replication
    --     group name. Example: @replication-group\/mycluster@.
    --
    -- -   Neptune cluster - The resource type is @cluster@ and the unique
    --     identifier is the cluster name. Example: @cluster:mycluster@.
    resourceId :: Prelude.Text,
    -- | The scalable dimension. This string consists of the service namespace,
    -- resource type, and scaling property.
    --
    -- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
    --     service.
    --
    -- -   @elasticmapreduce:instancegroup:InstanceCount@ - The instance count
    --     of an EMR Instance Group.
    --
    -- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
    --     Spot Fleet.
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
    --     instances for a SageMaker model endpoint variant.
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
    -- -   @elasticache:replication-group:NodeGroups@ - The number of node
    --     groups for an Amazon ElastiCache replication group.
    --
    -- -   @elasticache:replication-group:Replicas@ - The number of replicas
    --     per node group for an Amazon ElastiCache replication group.
    --
    -- -   @neptune:cluster:ReadReplicaCount@ - The count of read replicas in
    --     an Amazon Neptune DB cluster.
    scalableDimension :: ScalableDimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'putScheduledAction_endTime' - The date and time for the recurring schedule to end, in UTC.
--
-- 'scalableTargetAction', 'putScheduledAction_scalableTargetAction' - The new minimum and maximum capacity. You can set both values or just
-- one. At the scheduled time, if the current capacity is below the minimum
-- capacity, Application Auto Scaling scales out to the minimum capacity.
-- If the current capacity is above the maximum capacity, Application Auto
-- Scaling scales in to the maximum capacity.
--
-- 'schedule', 'putScheduledAction_schedule' - The schedule for this action. The following formats are supported:
--
-- -   At expressions -
--     \"@at(@/@yyyy@/@-@/@mm@/@-@/@dd@/@T@/@hh@/@:@/@mm@/@:@/@ss@/@)@\"
--
-- -   Rate expressions - \"@rate(@/@value@/@ @/@unit@/@)@\"
--
-- -   Cron expressions - \"@cron(@/@fields@/@)@\"
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
-- 'startTime', 'putScheduledAction_startTime' - The date and time for this scheduled action to start, in UTC.
--
-- 'timezone', 'putScheduledAction_timezone' - Specifies the time zone used when setting a scheduled action by using an
-- at or cron expression. If a time zone is not provided, UTC is used by
-- default.
--
-- Valid values are the canonical names of the IANA time zones supported by
-- Joda-Time (such as @Etc\/GMT+9@ or @Pacific\/Tahiti@). For more
-- information, see <https://www.joda.org/joda-time/timezones.html>.
--
-- 'serviceNamespace', 'putScheduledAction_serviceNamespace' - The namespace of the Amazon Web Services service that provides the
-- resource. For a resource provided by your own application or service,
-- use @custom-resource@ instead.
--
-- 'scheduledActionName', 'putScheduledAction_scheduledActionName' - The name of the scheduled action. This name must be unique among all
-- other scheduled actions on the specified scalable target.
--
-- 'resourceId', 'putScheduledAction_resourceId' - The identifier of the resource associated with the scheduled action.
-- This string consists of the resource type and unique identifier.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet - The resource type is @spot-fleet-request@ and the
--     unique identifier is the Spot Fleet request ID. Example:
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
-- -   SageMaker endpoint variant - The resource type is @variant@ and the
--     unique identifier is the resource ID. Example:
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
-- -   Amazon ElastiCache replication group - The resource type is
--     @replication-group@ and the unique identifier is the replication
--     group name. Example: @replication-group\/mycluster@.
--
-- -   Neptune cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:mycluster@.
--
-- 'scalableDimension', 'putScheduledAction_scalableDimension' - The scalable dimension. This string consists of the service namespace,
-- resource type, and scaling property.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @elasticmapreduce:instancegroup:InstanceCount@ - The instance count
--     of an EMR Instance Group.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet.
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
--     instances for a SageMaker model endpoint variant.
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
-- -   @elasticache:replication-group:NodeGroups@ - The number of node
--     groups for an Amazon ElastiCache replication group.
--
-- -   @elasticache:replication-group:Replicas@ - The number of replicas
--     per node group for an Amazon ElastiCache replication group.
--
-- -   @neptune:cluster:ReadReplicaCount@ - The count of read replicas in
--     an Amazon Neptune DB cluster.
newPutScheduledAction ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'scheduledActionName'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  PutScheduledAction
newPutScheduledAction
  pServiceNamespace_
  pScheduledActionName_
  pResourceId_
  pScalableDimension_ =
    PutScheduledAction'
      { endTime = Prelude.Nothing,
        scalableTargetAction = Prelude.Nothing,
        schedule = Prelude.Nothing,
        startTime = Prelude.Nothing,
        timezone = Prelude.Nothing,
        serviceNamespace = pServiceNamespace_,
        scheduledActionName = pScheduledActionName_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_
      }

-- | The date and time for the recurring schedule to end, in UTC.
putScheduledAction_endTime :: Lens.Lens' PutScheduledAction (Prelude.Maybe Prelude.UTCTime)
putScheduledAction_endTime = Lens.lens (\PutScheduledAction' {endTime} -> endTime) (\s@PutScheduledAction' {} a -> s {endTime = a} :: PutScheduledAction) Prelude.. Lens.mapping Data._Time

-- | The new minimum and maximum capacity. You can set both values or just
-- one. At the scheduled time, if the current capacity is below the minimum
-- capacity, Application Auto Scaling scales out to the minimum capacity.
-- If the current capacity is above the maximum capacity, Application Auto
-- Scaling scales in to the maximum capacity.
putScheduledAction_scalableTargetAction :: Lens.Lens' PutScheduledAction (Prelude.Maybe ScalableTargetAction)
putScheduledAction_scalableTargetAction = Lens.lens (\PutScheduledAction' {scalableTargetAction} -> scalableTargetAction) (\s@PutScheduledAction' {} a -> s {scalableTargetAction = a} :: PutScheduledAction)

-- | The schedule for this action. The following formats are supported:
--
-- -   At expressions -
--     \"@at(@/@yyyy@/@-@/@mm@/@-@/@dd@/@T@/@hh@/@:@/@mm@/@:@/@ss@/@)@\"
--
-- -   Rate expressions - \"@rate(@/@value@/@ @/@unit@/@)@\"
--
-- -   Cron expressions - \"@cron(@/@fields@/@)@\"
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
putScheduledAction_schedule :: Lens.Lens' PutScheduledAction (Prelude.Maybe Prelude.Text)
putScheduledAction_schedule = Lens.lens (\PutScheduledAction' {schedule} -> schedule) (\s@PutScheduledAction' {} a -> s {schedule = a} :: PutScheduledAction)

-- | The date and time for this scheduled action to start, in UTC.
putScheduledAction_startTime :: Lens.Lens' PutScheduledAction (Prelude.Maybe Prelude.UTCTime)
putScheduledAction_startTime = Lens.lens (\PutScheduledAction' {startTime} -> startTime) (\s@PutScheduledAction' {} a -> s {startTime = a} :: PutScheduledAction) Prelude.. Lens.mapping Data._Time

-- | Specifies the time zone used when setting a scheduled action by using an
-- at or cron expression. If a time zone is not provided, UTC is used by
-- default.
--
-- Valid values are the canonical names of the IANA time zones supported by
-- Joda-Time (such as @Etc\/GMT+9@ or @Pacific\/Tahiti@). For more
-- information, see <https://www.joda.org/joda-time/timezones.html>.
putScheduledAction_timezone :: Lens.Lens' PutScheduledAction (Prelude.Maybe Prelude.Text)
putScheduledAction_timezone = Lens.lens (\PutScheduledAction' {timezone} -> timezone) (\s@PutScheduledAction' {} a -> s {timezone = a} :: PutScheduledAction)

-- | The namespace of the Amazon Web Services service that provides the
-- resource. For a resource provided by your own application or service,
-- use @custom-resource@ instead.
putScheduledAction_serviceNamespace :: Lens.Lens' PutScheduledAction ServiceNamespace
putScheduledAction_serviceNamespace = Lens.lens (\PutScheduledAction' {serviceNamespace} -> serviceNamespace) (\s@PutScheduledAction' {} a -> s {serviceNamespace = a} :: PutScheduledAction)

-- | The name of the scheduled action. This name must be unique among all
-- other scheduled actions on the specified scalable target.
putScheduledAction_scheduledActionName :: Lens.Lens' PutScheduledAction Prelude.Text
putScheduledAction_scheduledActionName = Lens.lens (\PutScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@PutScheduledAction' {} a -> s {scheduledActionName = a} :: PutScheduledAction)

-- | The identifier of the resource associated with the scheduled action.
-- This string consists of the resource type and unique identifier.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet - The resource type is @spot-fleet-request@ and the
--     unique identifier is the Spot Fleet request ID. Example:
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
-- -   SageMaker endpoint variant - The resource type is @variant@ and the
--     unique identifier is the resource ID. Example:
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
-- -   Amazon ElastiCache replication group - The resource type is
--     @replication-group@ and the unique identifier is the replication
--     group name. Example: @replication-group\/mycluster@.
--
-- -   Neptune cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:mycluster@.
putScheduledAction_resourceId :: Lens.Lens' PutScheduledAction Prelude.Text
putScheduledAction_resourceId = Lens.lens (\PutScheduledAction' {resourceId} -> resourceId) (\s@PutScheduledAction' {} a -> s {resourceId = a} :: PutScheduledAction)

-- | The scalable dimension. This string consists of the service namespace,
-- resource type, and scaling property.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @elasticmapreduce:instancegroup:InstanceCount@ - The instance count
--     of an EMR Instance Group.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet.
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
--     instances for a SageMaker model endpoint variant.
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
-- -   @elasticache:replication-group:NodeGroups@ - The number of node
--     groups for an Amazon ElastiCache replication group.
--
-- -   @elasticache:replication-group:Replicas@ - The number of replicas
--     per node group for an Amazon ElastiCache replication group.
--
-- -   @neptune:cluster:ReadReplicaCount@ - The count of read replicas in
--     an Amazon Neptune DB cluster.
putScheduledAction_scalableDimension :: Lens.Lens' PutScheduledAction ScalableDimension
putScheduledAction_scalableDimension = Lens.lens (\PutScheduledAction' {scalableDimension} -> scalableDimension) (\s@PutScheduledAction' {} a -> s {scalableDimension = a} :: PutScheduledAction)

instance Core.AWSRequest PutScheduledAction where
  type
    AWSResponse PutScheduledAction =
      PutScheduledActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutScheduledActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutScheduledAction where
  hashWithSalt _salt PutScheduledAction' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` scalableTargetAction
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` serviceNamespace
      `Prelude.hashWithSalt` scheduledActionName
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` scalableDimension

instance Prelude.NFData PutScheduledAction where
  rnf PutScheduledAction' {..} =
    Prelude.rnf endTime `Prelude.seq`
      Prelude.rnf scalableTargetAction `Prelude.seq`
        Prelude.rnf schedule `Prelude.seq`
          Prelude.rnf startTime `Prelude.seq`
            Prelude.rnf timezone `Prelude.seq`
              Prelude.rnf serviceNamespace `Prelude.seq`
                Prelude.rnf scheduledActionName `Prelude.seq`
                  Prelude.rnf resourceId `Prelude.seq`
                    Prelude.rnf scalableDimension

instance Data.ToHeaders PutScheduledAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AnyScaleFrontendService.PutScheduledAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutScheduledAction where
  toJSON PutScheduledAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("ScalableTargetAction" Data..=)
              Prelude.<$> scalableTargetAction,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("StartTime" Data..=) Prelude.<$> startTime,
            ("Timezone" Data..=) Prelude.<$> timezone,
            Prelude.Just
              ("ServiceNamespace" Data..= serviceNamespace),
            Prelude.Just
              ("ScheduledActionName" Data..= scheduledActionName),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ("ScalableDimension" Data..= scalableDimension)
          ]
      )

instance Data.ToPath PutScheduledAction where
  toPath = Prelude.const "/"

instance Data.ToQuery PutScheduledAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutScheduledActionResponse' smart constructor.
data PutScheduledActionResponse = PutScheduledActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScheduledActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putScheduledActionResponse_httpStatus' - The response's http status code.
newPutScheduledActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutScheduledActionResponse
newPutScheduledActionResponse pHttpStatus_ =
  PutScheduledActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putScheduledActionResponse_httpStatus :: Lens.Lens' PutScheduledActionResponse Prelude.Int
putScheduledActionResponse_httpStatus = Lens.lens (\PutScheduledActionResponse' {httpStatus} -> httpStatus) (\s@PutScheduledActionResponse' {} a -> s {httpStatus = a} :: PutScheduledActionResponse)

instance Prelude.NFData PutScheduledActionResponse where
  rnf PutScheduledActionResponse' {..} =
    Prelude.rnf httpStatus
