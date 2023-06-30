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
-- Module      : Amazonka.ApplicationAutoScaling.Types.ScalingActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.ScalingActivity where

import Amazonka.ApplicationAutoScaling.Types.NotScaledReason
import Amazonka.ApplicationAutoScaling.Types.ScalableDimension
import Amazonka.ApplicationAutoScaling.Types.ScalingActivityStatusCode
import Amazonka.ApplicationAutoScaling.Types.ServiceNamespace
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a scaling activity.
--
-- /See:/ 'newScalingActivity' smart constructor.
data ScalingActivity = ScalingActivity'
  { -- | The details about the scaling activity.
    details :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the scaling activity ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Machine-readable data that describes the reason for a not scaled
    -- activity. Only available when
    -- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalingActivities.html DescribeScalingActivities>
    -- includes not scaled activities.
    notScaledReasons :: Prelude.Maybe [NotScaledReason],
    -- | A simple message about the current status of the scaling activity.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the scaling activity.
    activityId :: Prelude.Text,
    -- | The namespace of the Amazon Web Services service that provides the
    -- resource, or a @custom-resource@.
    serviceNamespace :: ServiceNamespace,
    -- | The identifier of the resource associated with the scaling activity.
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
    scalableDimension :: ScalableDimension,
    -- | A simple description of what action the scaling activity intends to
    -- accomplish.
    description :: Prelude.Text,
    -- | A simple description of what caused the scaling activity to happen.
    cause :: Prelude.Text,
    -- | The Unix timestamp for when the scaling activity began.
    startTime :: Data.POSIX,
    -- | Indicates the status of the scaling activity.
    statusCode :: ScalingActivityStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'scalingActivity_details' - The details about the scaling activity.
--
-- 'endTime', 'scalingActivity_endTime' - The Unix timestamp for when the scaling activity ended.
--
-- 'notScaledReasons', 'scalingActivity_notScaledReasons' - Machine-readable data that describes the reason for a not scaled
-- activity. Only available when
-- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalingActivities.html DescribeScalingActivities>
-- includes not scaled activities.
--
-- 'statusMessage', 'scalingActivity_statusMessage' - A simple message about the current status of the scaling activity.
--
-- 'activityId', 'scalingActivity_activityId' - The unique identifier of the scaling activity.
--
-- 'serviceNamespace', 'scalingActivity_serviceNamespace' - The namespace of the Amazon Web Services service that provides the
-- resource, or a @custom-resource@.
--
-- 'resourceId', 'scalingActivity_resourceId' - The identifier of the resource associated with the scaling activity.
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
-- 'scalableDimension', 'scalingActivity_scalableDimension' - The scalable dimension. This string consists of the service namespace,
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
--
-- 'description', 'scalingActivity_description' - A simple description of what action the scaling activity intends to
-- accomplish.
--
-- 'cause', 'scalingActivity_cause' - A simple description of what caused the scaling activity to happen.
--
-- 'startTime', 'scalingActivity_startTime' - The Unix timestamp for when the scaling activity began.
--
-- 'statusCode', 'scalingActivity_statusCode' - Indicates the status of the scaling activity.
newScalingActivity ::
  -- | 'activityId'
  Prelude.Text ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'description'
  Prelude.Text ->
  -- | 'cause'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'statusCode'
  ScalingActivityStatusCode ->
  ScalingActivity
newScalingActivity
  pActivityId_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pDescription_
  pCause_
  pStartTime_
  pStatusCode_ =
    ScalingActivity'
      { details = Prelude.Nothing,
        endTime = Prelude.Nothing,
        notScaledReasons = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        activityId = pActivityId_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        description = pDescription_,
        cause = pCause_,
        startTime = Data._Time Lens.# pStartTime_,
        statusCode = pStatusCode_
      }

-- | The details about the scaling activity.
scalingActivity_details :: Lens.Lens' ScalingActivity (Prelude.Maybe Prelude.Text)
scalingActivity_details = Lens.lens (\ScalingActivity' {details} -> details) (\s@ScalingActivity' {} a -> s {details = a} :: ScalingActivity)

-- | The Unix timestamp for when the scaling activity ended.
scalingActivity_endTime :: Lens.Lens' ScalingActivity (Prelude.Maybe Prelude.UTCTime)
scalingActivity_endTime = Lens.lens (\ScalingActivity' {endTime} -> endTime) (\s@ScalingActivity' {} a -> s {endTime = a} :: ScalingActivity) Prelude.. Lens.mapping Data._Time

-- | Machine-readable data that describes the reason for a not scaled
-- activity. Only available when
-- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalingActivities.html DescribeScalingActivities>
-- includes not scaled activities.
scalingActivity_notScaledReasons :: Lens.Lens' ScalingActivity (Prelude.Maybe [NotScaledReason])
scalingActivity_notScaledReasons = Lens.lens (\ScalingActivity' {notScaledReasons} -> notScaledReasons) (\s@ScalingActivity' {} a -> s {notScaledReasons = a} :: ScalingActivity) Prelude.. Lens.mapping Lens.coerced

-- | A simple message about the current status of the scaling activity.
scalingActivity_statusMessage :: Lens.Lens' ScalingActivity (Prelude.Maybe Prelude.Text)
scalingActivity_statusMessage = Lens.lens (\ScalingActivity' {statusMessage} -> statusMessage) (\s@ScalingActivity' {} a -> s {statusMessage = a} :: ScalingActivity)

-- | The unique identifier of the scaling activity.
scalingActivity_activityId :: Lens.Lens' ScalingActivity Prelude.Text
scalingActivity_activityId = Lens.lens (\ScalingActivity' {activityId} -> activityId) (\s@ScalingActivity' {} a -> s {activityId = a} :: ScalingActivity)

-- | The namespace of the Amazon Web Services service that provides the
-- resource, or a @custom-resource@.
scalingActivity_serviceNamespace :: Lens.Lens' ScalingActivity ServiceNamespace
scalingActivity_serviceNamespace = Lens.lens (\ScalingActivity' {serviceNamespace} -> serviceNamespace) (\s@ScalingActivity' {} a -> s {serviceNamespace = a} :: ScalingActivity)

-- | The identifier of the resource associated with the scaling activity.
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
scalingActivity_resourceId :: Lens.Lens' ScalingActivity Prelude.Text
scalingActivity_resourceId = Lens.lens (\ScalingActivity' {resourceId} -> resourceId) (\s@ScalingActivity' {} a -> s {resourceId = a} :: ScalingActivity)

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
scalingActivity_scalableDimension :: Lens.Lens' ScalingActivity ScalableDimension
scalingActivity_scalableDimension = Lens.lens (\ScalingActivity' {scalableDimension} -> scalableDimension) (\s@ScalingActivity' {} a -> s {scalableDimension = a} :: ScalingActivity)

-- | A simple description of what action the scaling activity intends to
-- accomplish.
scalingActivity_description :: Lens.Lens' ScalingActivity Prelude.Text
scalingActivity_description = Lens.lens (\ScalingActivity' {description} -> description) (\s@ScalingActivity' {} a -> s {description = a} :: ScalingActivity)

-- | A simple description of what caused the scaling activity to happen.
scalingActivity_cause :: Lens.Lens' ScalingActivity Prelude.Text
scalingActivity_cause = Lens.lens (\ScalingActivity' {cause} -> cause) (\s@ScalingActivity' {} a -> s {cause = a} :: ScalingActivity)

-- | The Unix timestamp for when the scaling activity began.
scalingActivity_startTime :: Lens.Lens' ScalingActivity Prelude.UTCTime
scalingActivity_startTime = Lens.lens (\ScalingActivity' {startTime} -> startTime) (\s@ScalingActivity' {} a -> s {startTime = a} :: ScalingActivity) Prelude.. Data._Time

-- | Indicates the status of the scaling activity.
scalingActivity_statusCode :: Lens.Lens' ScalingActivity ScalingActivityStatusCode
scalingActivity_statusCode = Lens.lens (\ScalingActivity' {statusCode} -> statusCode) (\s@ScalingActivity' {} a -> s {statusCode = a} :: ScalingActivity)

instance Data.FromJSON ScalingActivity where
  parseJSON =
    Data.withObject
      "ScalingActivity"
      ( \x ->
          ScalingActivity'
            Prelude.<$> (x Data..:? "Details")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> ( x
                            Data..:? "NotScaledReasons"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..: "ActivityId")
            Prelude.<*> (x Data..: "ServiceNamespace")
            Prelude.<*> (x Data..: "ResourceId")
            Prelude.<*> (x Data..: "ScalableDimension")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "Cause")
            Prelude.<*> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "StatusCode")
      )

instance Prelude.Hashable ScalingActivity where
  hashWithSalt _salt ScalingActivity' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` notScaledReasons
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` activityId
      `Prelude.hashWithSalt` serviceNamespace
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` scalableDimension
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData ScalingActivity where
  rnf ScalingActivity' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf notScaledReasons
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf activityId
      `Prelude.seq` Prelude.rnf serviceNamespace
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf scalableDimension
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf cause
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusCode
