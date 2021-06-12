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
-- Module      : Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Application Auto Scaling scalable target when you have
-- finished using it. To see which resources have been registered, use
-- <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalableTargets.html DescribeScalableTargets>.
--
-- Deregistering a scalable target deletes the scaling policies and the
-- scheduled actions that are associated with it.
module Network.AWS.ApplicationAutoScaling.DeregisterScalableTarget
  ( -- * Creating a Request
    DeregisterScalableTarget (..),
    newDeregisterScalableTarget,

    -- * Request Lenses
    deregisterScalableTarget_serviceNamespace,
    deregisterScalableTarget_resourceId,
    deregisterScalableTarget_scalableDimension,

    -- * Destructuring the Response
    DeregisterScalableTargetResponse (..),
    newDeregisterScalableTargetResponse,

    -- * Response Lenses
    deregisterScalableTargetResponse_httpStatus,
  )
where

import Network.AWS.ApplicationAutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterScalableTarget' smart constructor.
data DeregisterScalableTarget = DeregisterScalableTarget'
  { -- | The namespace of the AWS service that provides the resource. For a
    -- resource provided by your own application or service, use
    -- @custom-resource@ instead.
    serviceNamespace :: ServiceNamespace,
    -- | The identifier of the resource associated with the scalable target. This
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
    resourceId :: Core.Text,
    -- | The scalable dimension associated with the scalable target. This string
    -- consists of the service namespace, resource type, and scaling property.
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
    scalableDimension :: ScalableDimension
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterScalableTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNamespace', 'deregisterScalableTarget_serviceNamespace' - The namespace of the AWS service that provides the resource. For a
-- resource provided by your own application or service, use
-- @custom-resource@ instead.
--
-- 'resourceId', 'deregisterScalableTarget_resourceId' - The identifier of the resource associated with the scalable target. This
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
-- 'scalableDimension', 'deregisterScalableTarget_scalableDimension' - The scalable dimension associated with the scalable target. This string
-- consists of the service namespace, resource type, and scaling property.
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
newDeregisterScalableTarget ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  DeregisterScalableTarget
newDeregisterScalableTarget
  pServiceNamespace_
  pResourceId_
  pScalableDimension_ =
    DeregisterScalableTarget'
      { serviceNamespace =
          pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_
      }

-- | The namespace of the AWS service that provides the resource. For a
-- resource provided by your own application or service, use
-- @custom-resource@ instead.
deregisterScalableTarget_serviceNamespace :: Lens.Lens' DeregisterScalableTarget ServiceNamespace
deregisterScalableTarget_serviceNamespace = Lens.lens (\DeregisterScalableTarget' {serviceNamespace} -> serviceNamespace) (\s@DeregisterScalableTarget' {} a -> s {serviceNamespace = a} :: DeregisterScalableTarget)

-- | The identifier of the resource associated with the scalable target. This
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
deregisterScalableTarget_resourceId :: Lens.Lens' DeregisterScalableTarget Core.Text
deregisterScalableTarget_resourceId = Lens.lens (\DeregisterScalableTarget' {resourceId} -> resourceId) (\s@DeregisterScalableTarget' {} a -> s {resourceId = a} :: DeregisterScalableTarget)

-- | The scalable dimension associated with the scalable target. This string
-- consists of the service namespace, resource type, and scaling property.
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
deregisterScalableTarget_scalableDimension :: Lens.Lens' DeregisterScalableTarget ScalableDimension
deregisterScalableTarget_scalableDimension = Lens.lens (\DeregisterScalableTarget' {scalableDimension} -> scalableDimension) (\s@DeregisterScalableTarget' {} a -> s {scalableDimension = a} :: DeregisterScalableTarget)

instance Core.AWSRequest DeregisterScalableTarget where
  type
    AWSResponse DeregisterScalableTarget =
      DeregisterScalableTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterScalableTargetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterScalableTarget

instance Core.NFData DeregisterScalableTarget

instance Core.ToHeaders DeregisterScalableTarget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AnyScaleFrontendService.DeregisterScalableTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterScalableTarget where
  toJSON DeregisterScalableTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ServiceNamespace" Core..= serviceNamespace),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just
              ("ScalableDimension" Core..= scalableDimension)
          ]
      )

instance Core.ToPath DeregisterScalableTarget where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterScalableTarget where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterScalableTargetResponse' smart constructor.
data DeregisterScalableTargetResponse = DeregisterScalableTargetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterScalableTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterScalableTargetResponse_httpStatus' - The response's http status code.
newDeregisterScalableTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterScalableTargetResponse
newDeregisterScalableTargetResponse pHttpStatus_ =
  DeregisterScalableTargetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterScalableTargetResponse_httpStatus :: Lens.Lens' DeregisterScalableTargetResponse Core.Int
deregisterScalableTargetResponse_httpStatus = Lens.lens (\DeregisterScalableTargetResponse' {httpStatus} -> httpStatus) (\s@DeregisterScalableTargetResponse' {} a -> s {httpStatus = a} :: DeregisterScalableTargetResponse)

instance Core.NFData DeregisterScalableTargetResponse
