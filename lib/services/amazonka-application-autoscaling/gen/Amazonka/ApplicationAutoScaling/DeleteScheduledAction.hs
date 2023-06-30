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
-- Module      : Amazonka.ApplicationAutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scheduled action for an Application Auto Scaling
-- scalable target.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/scheduled-scaling-additional-cli-commands.html#delete-scheduled-action Delete a scheduled action>
-- in the /Application Auto Scaling User Guide/.
module Amazonka.ApplicationAutoScaling.DeleteScheduledAction
  ( -- * Creating a Request
    DeleteScheduledAction (..),
    newDeleteScheduledAction,

    -- * Request Lenses
    deleteScheduledAction_serviceNamespace,
    deleteScheduledAction_scheduledActionName,
    deleteScheduledAction_resourceId,
    deleteScheduledAction_scalableDimension,

    -- * Destructuring the Response
    DeleteScheduledActionResponse (..),
    newDeleteScheduledActionResponse,

    -- * Response Lenses
    deleteScheduledActionResponse_httpStatus,
  )
where

import Amazonka.ApplicationAutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
  { -- | The namespace of the Amazon Web Services service that provides the
    -- resource. For a resource provided by your own application or service,
    -- use @custom-resource@ instead.
    serviceNamespace :: ServiceNamespace,
    -- | The name of the scheduled action.
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
-- Create a value of 'DeleteScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNamespace', 'deleteScheduledAction_serviceNamespace' - The namespace of the Amazon Web Services service that provides the
-- resource. For a resource provided by your own application or service,
-- use @custom-resource@ instead.
--
-- 'scheduledActionName', 'deleteScheduledAction_scheduledActionName' - The name of the scheduled action.
--
-- 'resourceId', 'deleteScheduledAction_resourceId' - The identifier of the resource associated with the scheduled action.
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
-- 'scalableDimension', 'deleteScheduledAction_scalableDimension' - The scalable dimension. This string consists of the service namespace,
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
newDeleteScheduledAction ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'scheduledActionName'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  DeleteScheduledAction
newDeleteScheduledAction
  pServiceNamespace_
  pScheduledActionName_
  pResourceId_
  pScalableDimension_ =
    DeleteScheduledAction'
      { serviceNamespace =
          pServiceNamespace_,
        scheduledActionName = pScheduledActionName_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_
      }

-- | The namespace of the Amazon Web Services service that provides the
-- resource. For a resource provided by your own application or service,
-- use @custom-resource@ instead.
deleteScheduledAction_serviceNamespace :: Lens.Lens' DeleteScheduledAction ServiceNamespace
deleteScheduledAction_serviceNamespace = Lens.lens (\DeleteScheduledAction' {serviceNamespace} -> serviceNamespace) (\s@DeleteScheduledAction' {} a -> s {serviceNamespace = a} :: DeleteScheduledAction)

-- | The name of the scheduled action.
deleteScheduledAction_scheduledActionName :: Lens.Lens' DeleteScheduledAction Prelude.Text
deleteScheduledAction_scheduledActionName = Lens.lens (\DeleteScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@DeleteScheduledAction' {} a -> s {scheduledActionName = a} :: DeleteScheduledAction)

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
deleteScheduledAction_resourceId :: Lens.Lens' DeleteScheduledAction Prelude.Text
deleteScheduledAction_resourceId = Lens.lens (\DeleteScheduledAction' {resourceId} -> resourceId) (\s@DeleteScheduledAction' {} a -> s {resourceId = a} :: DeleteScheduledAction)

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
deleteScheduledAction_scalableDimension :: Lens.Lens' DeleteScheduledAction ScalableDimension
deleteScheduledAction_scalableDimension = Lens.lens (\DeleteScheduledAction' {scalableDimension} -> scalableDimension) (\s@DeleteScheduledAction' {} a -> s {scalableDimension = a} :: DeleteScheduledAction)

instance Core.AWSRequest DeleteScheduledAction where
  type
    AWSResponse DeleteScheduledAction =
      DeleteScheduledActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduledActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteScheduledAction where
  hashWithSalt _salt DeleteScheduledAction' {..} =
    _salt
      `Prelude.hashWithSalt` serviceNamespace
      `Prelude.hashWithSalt` scheduledActionName
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` scalableDimension

instance Prelude.NFData DeleteScheduledAction where
  rnf DeleteScheduledAction' {..} =
    Prelude.rnf serviceNamespace
      `Prelude.seq` Prelude.rnf scheduledActionName
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf scalableDimension

instance Data.ToHeaders DeleteScheduledAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AnyScaleFrontendService.DeleteScheduledAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteScheduledAction where
  toJSON DeleteScheduledAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ServiceNamespace" Data..= serviceNamespace),
            Prelude.Just
              ("ScheduledActionName" Data..= scheduledActionName),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ("ScalableDimension" Data..= scalableDimension)
          ]
      )

instance Data.ToPath DeleteScheduledAction where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteScheduledAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScheduledActionResponse_httpStatus' - The response's http status code.
newDeleteScheduledActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteScheduledActionResponse
newDeleteScheduledActionResponse pHttpStatus_ =
  DeleteScheduledActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteScheduledActionResponse_httpStatus :: Lens.Lens' DeleteScheduledActionResponse Prelude.Int
deleteScheduledActionResponse_httpStatus = Lens.lens (\DeleteScheduledActionResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduledActionResponse' {} a -> s {httpStatus = a} :: DeleteScheduledActionResponse)

instance Prelude.NFData DeleteScheduledActionResponse where
  rnf DeleteScheduledActionResponse' {..} =
    Prelude.rnf httpStatus
