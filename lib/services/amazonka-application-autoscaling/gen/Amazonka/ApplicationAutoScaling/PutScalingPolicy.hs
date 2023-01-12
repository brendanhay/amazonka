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
-- Module      : Amazonka.ApplicationAutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for an Application Auto Scaling
-- scalable target.
--
-- Each scalable target is identified by a service namespace, resource ID,
-- and scalable dimension. A scaling policy applies to the scalable target
-- identified by those three attributes. You cannot create a scaling policy
-- until you have registered the resource as a scalable target.
--
-- Multiple scaling policies can be in force at the same time for the same
-- scalable target. You can have one or more target tracking scaling
-- policies, one or more step scaling policies, or both. However, there is
-- a chance that multiple policies could conflict, instructing the scalable
-- target to scale out or in at the same time. Application Auto Scaling
-- gives precedence to the policy that provides the largest capacity for
-- both scale out and scale in. For example, if one policy increases
-- capacity by 3, another policy increases capacity by 200 percent, and the
-- current capacity is 10, Application Auto Scaling uses the policy with
-- the highest calculated capacity (200% of 10 = 20) and scales out to 30.
--
-- We recommend caution, however, when using target tracking scaling
-- policies with step scaling policies because conflicts between these
-- policies can cause undesirable behavior. For example, if the step
-- scaling policy initiates a scale-in activity before the target tracking
-- policy is ready to scale in, the scale-in activity will not be blocked.
-- After the scale-in activity completes, the target tracking policy could
-- instruct the scalable target to scale out again.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies>
-- in the /Application Auto Scaling User Guide/.
--
-- If a scalable target is deregistered, the scalable target is no longer
-- available to execute scaling policies. Any scaling policies that were
-- specified for the scalable target are deleted.
module Amazonka.ApplicationAutoScaling.PutScalingPolicy
  ( -- * Creating a Request
    PutScalingPolicy (..),
    newPutScalingPolicy,

    -- * Request Lenses
    putScalingPolicy_policyType,
    putScalingPolicy_stepScalingPolicyConfiguration,
    putScalingPolicy_targetTrackingScalingPolicyConfiguration,
    putScalingPolicy_policyName,
    putScalingPolicy_serviceNamespace,
    putScalingPolicy_resourceId,
    putScalingPolicy_scalableDimension,

    -- * Destructuring the Response
    PutScalingPolicyResponse (..),
    newPutScalingPolicyResponse,

    -- * Response Lenses
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_httpStatus,
    putScalingPolicyResponse_policyARN,
  )
where

import Amazonka.ApplicationAutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { -- | The scaling policy type. This parameter is required if you are creating
    -- a scaling policy.
    --
    -- The following policy types are supported:
    --
    -- @TargetTrackingScaling@—Not supported for Amazon EMR
    --
    -- @StepScaling@—Not supported for DynamoDB, Amazon Comprehend, Lambda,
    -- Amazon Keyspaces, Amazon MSK, Amazon ElastiCache, or Neptune.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies>
    -- and
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies>
    -- in the /Application Auto Scaling User Guide/.
    policyType :: Prelude.Maybe PolicyType,
    -- | A step scaling policy.
    --
    -- This parameter is required if you are creating a policy and the policy
    -- type is @StepScaling@.
    stepScalingPolicyConfiguration :: Prelude.Maybe StepScalingPolicyConfiguration,
    -- | A target tracking scaling policy. Includes support for predefined or
    -- customized metrics.
    --
    -- This parameter is required if you are creating a policy and the policy
    -- type is @TargetTrackingScaling@.
    targetTrackingScalingPolicyConfiguration :: Prelude.Maybe TargetTrackingScalingPolicyConfiguration,
    -- | The name of the scaling policy.
    --
    -- You cannot change the name of a scaling policy, but you can delete the
    -- original scaling policy and create a new scaling policy with the same
    -- settings and a different name.
    policyName :: Prelude.Text,
    -- | The namespace of the Amazon Web Services service that provides the
    -- resource. For a resource provided by your own application or service,
    -- use @custom-resource@ instead.
    serviceNamespace :: ServiceNamespace,
    -- | The identifier of the resource associated with the scaling policy. This
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
-- Create a value of 'PutScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyType', 'putScalingPolicy_policyType' - The scaling policy type. This parameter is required if you are creating
-- a scaling policy.
--
-- The following policy types are supported:
--
-- @TargetTrackingScaling@—Not supported for Amazon EMR
--
-- @StepScaling@—Not supported for DynamoDB, Amazon Comprehend, Lambda,
-- Amazon Keyspaces, Amazon MSK, Amazon ElastiCache, or Neptune.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies>
-- in the /Application Auto Scaling User Guide/.
--
-- 'stepScalingPolicyConfiguration', 'putScalingPolicy_stepScalingPolicyConfiguration' - A step scaling policy.
--
-- This parameter is required if you are creating a policy and the policy
-- type is @StepScaling@.
--
-- 'targetTrackingScalingPolicyConfiguration', 'putScalingPolicy_targetTrackingScalingPolicyConfiguration' - A target tracking scaling policy. Includes support for predefined or
-- customized metrics.
--
-- This parameter is required if you are creating a policy and the policy
-- type is @TargetTrackingScaling@.
--
-- 'policyName', 'putScalingPolicy_policyName' - The name of the scaling policy.
--
-- You cannot change the name of a scaling policy, but you can delete the
-- original scaling policy and create a new scaling policy with the same
-- settings and a different name.
--
-- 'serviceNamespace', 'putScalingPolicy_serviceNamespace' - The namespace of the Amazon Web Services service that provides the
-- resource. For a resource provided by your own application or service,
-- use @custom-resource@ instead.
--
-- 'resourceId', 'putScalingPolicy_resourceId' - The identifier of the resource associated with the scaling policy. This
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
-- 'scalableDimension', 'putScalingPolicy_scalableDimension' - The scalable dimension. This string consists of the service namespace,
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
newPutScalingPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  PutScalingPolicy
newPutScalingPolicy
  pPolicyName_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_ =
    PutScalingPolicy'
      { policyType = Prelude.Nothing,
        stepScalingPolicyConfiguration = Prelude.Nothing,
        targetTrackingScalingPolicyConfiguration =
          Prelude.Nothing,
        policyName = pPolicyName_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_
      }

-- | The scaling policy type. This parameter is required if you are creating
-- a scaling policy.
--
-- The following policy types are supported:
--
-- @TargetTrackingScaling@—Not supported for Amazon EMR
--
-- @StepScaling@—Not supported for DynamoDB, Amazon Comprehend, Lambda,
-- Amazon Keyspaces, Amazon MSK, Amazon ElastiCache, or Neptune.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies>
-- in the /Application Auto Scaling User Guide/.
putScalingPolicy_policyType :: Lens.Lens' PutScalingPolicy (Prelude.Maybe PolicyType)
putScalingPolicy_policyType = Lens.lens (\PutScalingPolicy' {policyType} -> policyType) (\s@PutScalingPolicy' {} a -> s {policyType = a} :: PutScalingPolicy)

-- | A step scaling policy.
--
-- This parameter is required if you are creating a policy and the policy
-- type is @StepScaling@.
putScalingPolicy_stepScalingPolicyConfiguration :: Lens.Lens' PutScalingPolicy (Prelude.Maybe StepScalingPolicyConfiguration)
putScalingPolicy_stepScalingPolicyConfiguration = Lens.lens (\PutScalingPolicy' {stepScalingPolicyConfiguration} -> stepScalingPolicyConfiguration) (\s@PutScalingPolicy' {} a -> s {stepScalingPolicyConfiguration = a} :: PutScalingPolicy)

-- | A target tracking scaling policy. Includes support for predefined or
-- customized metrics.
--
-- This parameter is required if you are creating a policy and the policy
-- type is @TargetTrackingScaling@.
putScalingPolicy_targetTrackingScalingPolicyConfiguration :: Lens.Lens' PutScalingPolicy (Prelude.Maybe TargetTrackingScalingPolicyConfiguration)
putScalingPolicy_targetTrackingScalingPolicyConfiguration = Lens.lens (\PutScalingPolicy' {targetTrackingScalingPolicyConfiguration} -> targetTrackingScalingPolicyConfiguration) (\s@PutScalingPolicy' {} a -> s {targetTrackingScalingPolicyConfiguration = a} :: PutScalingPolicy)

-- | The name of the scaling policy.
--
-- You cannot change the name of a scaling policy, but you can delete the
-- original scaling policy and create a new scaling policy with the same
-- settings and a different name.
putScalingPolicy_policyName :: Lens.Lens' PutScalingPolicy Prelude.Text
putScalingPolicy_policyName = Lens.lens (\PutScalingPolicy' {policyName} -> policyName) (\s@PutScalingPolicy' {} a -> s {policyName = a} :: PutScalingPolicy)

-- | The namespace of the Amazon Web Services service that provides the
-- resource. For a resource provided by your own application or service,
-- use @custom-resource@ instead.
putScalingPolicy_serviceNamespace :: Lens.Lens' PutScalingPolicy ServiceNamespace
putScalingPolicy_serviceNamespace = Lens.lens (\PutScalingPolicy' {serviceNamespace} -> serviceNamespace) (\s@PutScalingPolicy' {} a -> s {serviceNamespace = a} :: PutScalingPolicy)

-- | The identifier of the resource associated with the scaling policy. This
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
putScalingPolicy_resourceId :: Lens.Lens' PutScalingPolicy Prelude.Text
putScalingPolicy_resourceId = Lens.lens (\PutScalingPolicy' {resourceId} -> resourceId) (\s@PutScalingPolicy' {} a -> s {resourceId = a} :: PutScalingPolicy)

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
putScalingPolicy_scalableDimension :: Lens.Lens' PutScalingPolicy ScalableDimension
putScalingPolicy_scalableDimension = Lens.lens (\PutScalingPolicy' {scalableDimension} -> scalableDimension) (\s@PutScalingPolicy' {} a -> s {scalableDimension = a} :: PutScalingPolicy)

instance Core.AWSRequest PutScalingPolicy where
  type
    AWSResponse PutScalingPolicy =
      PutScalingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutScalingPolicyResponse'
            Prelude.<$> (x Data..?> "Alarms" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "PolicyARN")
      )

instance Prelude.Hashable PutScalingPolicy where
  hashWithSalt _salt PutScalingPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` stepScalingPolicyConfiguration
      `Prelude.hashWithSalt` targetTrackingScalingPolicyConfiguration
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` serviceNamespace
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` scalableDimension

instance Prelude.NFData PutScalingPolicy where
  rnf PutScalingPolicy' {..} =
    Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf stepScalingPolicyConfiguration
      `Prelude.seq` Prelude.rnf targetTrackingScalingPolicyConfiguration
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf serviceNamespace
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf scalableDimension

instance Data.ToHeaders PutScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AnyScaleFrontendService.PutScalingPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutScalingPolicy where
  toJSON PutScalingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyType" Data..=) Prelude.<$> policyType,
            ("StepScalingPolicyConfiguration" Data..=)
              Prelude.<$> stepScalingPolicyConfiguration,
            ("TargetTrackingScalingPolicyConfiguration" Data..=)
              Prelude.<$> targetTrackingScalingPolicyConfiguration,
            Prelude.Just ("PolicyName" Data..= policyName),
            Prelude.Just
              ("ServiceNamespace" Data..= serviceNamespace),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ("ScalableDimension" Data..= scalableDimension)
          ]
      )

instance Data.ToPath PutScalingPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { -- | The CloudWatch alarms created for the target tracking scaling policy.
    alarms :: Prelude.Maybe [Alarm],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the resulting scaling policy.
    policyARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarms', 'putScalingPolicyResponse_alarms' - The CloudWatch alarms created for the target tracking scaling policy.
--
-- 'httpStatus', 'putScalingPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policyARN', 'putScalingPolicyResponse_policyARN' - The Amazon Resource Name (ARN) of the resulting scaling policy.
newPutScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyARN'
  Prelude.Text ->
  PutScalingPolicyResponse
newPutScalingPolicyResponse pHttpStatus_ pPolicyARN_ =
  PutScalingPolicyResponse'
    { alarms = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      policyARN = pPolicyARN_
    }

-- | The CloudWatch alarms created for the target tracking scaling policy.
putScalingPolicyResponse_alarms :: Lens.Lens' PutScalingPolicyResponse (Prelude.Maybe [Alarm])
putScalingPolicyResponse_alarms = Lens.lens (\PutScalingPolicyResponse' {alarms} -> alarms) (\s@PutScalingPolicyResponse' {} a -> s {alarms = a} :: PutScalingPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putScalingPolicyResponse_httpStatus :: Lens.Lens' PutScalingPolicyResponse Prelude.Int
putScalingPolicyResponse_httpStatus = Lens.lens (\PutScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@PutScalingPolicyResponse' {} a -> s {httpStatus = a} :: PutScalingPolicyResponse)

-- | The Amazon Resource Name (ARN) of the resulting scaling policy.
putScalingPolicyResponse_policyARN :: Lens.Lens' PutScalingPolicyResponse Prelude.Text
putScalingPolicyResponse_policyARN = Lens.lens (\PutScalingPolicyResponse' {policyARN} -> policyARN) (\s@PutScalingPolicyResponse' {} a -> s {policyARN = a} :: PutScalingPolicyResponse)

instance Prelude.NFData PutScalingPolicyResponse where
  rnf PutScalingPolicyResponse' {..} =
    Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyARN
