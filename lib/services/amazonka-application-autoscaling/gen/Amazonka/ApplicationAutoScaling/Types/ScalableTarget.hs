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
-- Module      : Amazonka.ApplicationAutoScaling.Types.ScalableTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.ScalableTarget where

import Amazonka.ApplicationAutoScaling.Types.ScalableDimension
import Amazonka.ApplicationAutoScaling.Types.ServiceNamespace
import Amazonka.ApplicationAutoScaling.Types.SuspendedState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a scalable target.
--
-- /See:/ 'newScalableTarget' smart constructor.
data ScalableTarget = ScalableTarget'
  { suspendedState :: Prelude.Maybe SuspendedState,
    -- | The namespace of the Amazon Web Services service that provides the
    -- resource, or a @custom-resource@.
    serviceNamespace :: ServiceNamespace,
    -- | The identifier of the resource associated with the scalable target. This
    -- string consists of the resource type and unique identifier.
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
    -- | The scalable dimension associated with the scalable target. This string
    -- consists of the service namespace, resource type, and scaling property.
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
    -- | The minimum value to scale to in response to a scale-in activity.
    minCapacity :: Prelude.Int,
    -- | The maximum value to scale to in response to a scale-out activity.
    maxCapacity :: Prelude.Int,
    -- | The ARN of an IAM role that allows Application Auto Scaling to modify
    -- the scalable target on your behalf.
    roleARN :: Prelude.Text,
    -- | The Unix timestamp for when the scalable target was created.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalableTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suspendedState', 'scalableTarget_suspendedState' - Undocumented member.
--
-- 'serviceNamespace', 'scalableTarget_serviceNamespace' - The namespace of the Amazon Web Services service that provides the
-- resource, or a @custom-resource@.
--
-- 'resourceId', 'scalableTarget_resourceId' - The identifier of the resource associated with the scalable target. This
-- string consists of the resource type and unique identifier.
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
-- 'scalableDimension', 'scalableTarget_scalableDimension' - The scalable dimension associated with the scalable target. This string
-- consists of the service namespace, resource type, and scaling property.
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
-- 'minCapacity', 'scalableTarget_minCapacity' - The minimum value to scale to in response to a scale-in activity.
--
-- 'maxCapacity', 'scalableTarget_maxCapacity' - The maximum value to scale to in response to a scale-out activity.
--
-- 'roleARN', 'scalableTarget_roleARN' - The ARN of an IAM role that allows Application Auto Scaling to modify
-- the scalable target on your behalf.
--
-- 'creationTime', 'scalableTarget_creationTime' - The Unix timestamp for when the scalable target was created.
newScalableTarget ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'minCapacity'
  Prelude.Int ->
  -- | 'maxCapacity'
  Prelude.Int ->
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ScalableTarget
newScalableTarget
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pMinCapacity_
  pMaxCapacity_
  pRoleARN_
  pCreationTime_ =
    ScalableTarget'
      { suspendedState = Prelude.Nothing,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        minCapacity = pMinCapacity_,
        maxCapacity = pMaxCapacity_,
        roleARN = pRoleARN_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | Undocumented member.
scalableTarget_suspendedState :: Lens.Lens' ScalableTarget (Prelude.Maybe SuspendedState)
scalableTarget_suspendedState = Lens.lens (\ScalableTarget' {suspendedState} -> suspendedState) (\s@ScalableTarget' {} a -> s {suspendedState = a} :: ScalableTarget)

-- | The namespace of the Amazon Web Services service that provides the
-- resource, or a @custom-resource@.
scalableTarget_serviceNamespace :: Lens.Lens' ScalableTarget ServiceNamespace
scalableTarget_serviceNamespace = Lens.lens (\ScalableTarget' {serviceNamespace} -> serviceNamespace) (\s@ScalableTarget' {} a -> s {serviceNamespace = a} :: ScalableTarget)

-- | The identifier of the resource associated with the scalable target. This
-- string consists of the resource type and unique identifier.
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
scalableTarget_resourceId :: Lens.Lens' ScalableTarget Prelude.Text
scalableTarget_resourceId = Lens.lens (\ScalableTarget' {resourceId} -> resourceId) (\s@ScalableTarget' {} a -> s {resourceId = a} :: ScalableTarget)

-- | The scalable dimension associated with the scalable target. This string
-- consists of the service namespace, resource type, and scaling property.
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
scalableTarget_scalableDimension :: Lens.Lens' ScalableTarget ScalableDimension
scalableTarget_scalableDimension = Lens.lens (\ScalableTarget' {scalableDimension} -> scalableDimension) (\s@ScalableTarget' {} a -> s {scalableDimension = a} :: ScalableTarget)

-- | The minimum value to scale to in response to a scale-in activity.
scalableTarget_minCapacity :: Lens.Lens' ScalableTarget Prelude.Int
scalableTarget_minCapacity = Lens.lens (\ScalableTarget' {minCapacity} -> minCapacity) (\s@ScalableTarget' {} a -> s {minCapacity = a} :: ScalableTarget)

-- | The maximum value to scale to in response to a scale-out activity.
scalableTarget_maxCapacity :: Lens.Lens' ScalableTarget Prelude.Int
scalableTarget_maxCapacity = Lens.lens (\ScalableTarget' {maxCapacity} -> maxCapacity) (\s@ScalableTarget' {} a -> s {maxCapacity = a} :: ScalableTarget)

-- | The ARN of an IAM role that allows Application Auto Scaling to modify
-- the scalable target on your behalf.
scalableTarget_roleARN :: Lens.Lens' ScalableTarget Prelude.Text
scalableTarget_roleARN = Lens.lens (\ScalableTarget' {roleARN} -> roleARN) (\s@ScalableTarget' {} a -> s {roleARN = a} :: ScalableTarget)

-- | The Unix timestamp for when the scalable target was created.
scalableTarget_creationTime :: Lens.Lens' ScalableTarget Prelude.UTCTime
scalableTarget_creationTime = Lens.lens (\ScalableTarget' {creationTime} -> creationTime) (\s@ScalableTarget' {} a -> s {creationTime = a} :: ScalableTarget) Prelude.. Data._Time

instance Data.FromJSON ScalableTarget where
  parseJSON =
    Data.withObject
      "ScalableTarget"
      ( \x ->
          ScalableTarget'
            Prelude.<$> (x Data..:? "SuspendedState")
            Prelude.<*> (x Data..: "ServiceNamespace")
            Prelude.<*> (x Data..: "ResourceId")
            Prelude.<*> (x Data..: "ScalableDimension")
            Prelude.<*> (x Data..: "MinCapacity")
            Prelude.<*> (x Data..: "MaxCapacity")
            Prelude.<*> (x Data..: "RoleARN")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance Prelude.Hashable ScalableTarget where
  hashWithSalt _salt ScalableTarget' {..} =
    _salt
      `Prelude.hashWithSalt` suspendedState
      `Prelude.hashWithSalt` serviceNamespace
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` scalableDimension
      `Prelude.hashWithSalt` minCapacity
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ScalableTarget where
  rnf ScalableTarget' {..} =
    Prelude.rnf suspendedState `Prelude.seq`
      Prelude.rnf serviceNamespace `Prelude.seq`
        Prelude.rnf resourceId `Prelude.seq`
          Prelude.rnf scalableDimension `Prelude.seq`
            Prelude.rnf minCapacity `Prelude.seq`
              Prelude.rnf maxCapacity `Prelude.seq`
                Prelude.rnf roleARN `Prelude.seq`
                  Prelude.rnf creationTime
