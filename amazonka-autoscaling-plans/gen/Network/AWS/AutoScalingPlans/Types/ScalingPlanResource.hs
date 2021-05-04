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
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPlanResource where

import Network.AWS.AutoScalingPlans.Types.ScalableDimension
import Network.AWS.AutoScalingPlans.Types.ScalingPolicy
import Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a scalable resource.
--
-- /See:/ 'newScalingPlanResource' smart constructor.
data ScalingPlanResource = ScalingPlanResource'
  { -- | The scaling policies.
    scalingPolicies :: Prelude.Maybe [ScalingPolicy],
    -- | A simple message about the current scaling status of the resource.
    scalingStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the scaling plan.
    scalingPlanName :: Prelude.Text,
    -- | The version number of the scaling plan.
    scalingPlanVersion :: Prelude.Integer,
    -- | The namespace of the AWS service.
    serviceNamespace :: ServiceNamespace,
    -- | The ID of the resource. This string consists of the resource type and
    -- unique identifier.
    --
    -- -   Auto Scaling group - The resource type is @autoScalingGroup@ and the
    --     unique identifier is the name of the Auto Scaling group. Example:
    --     @autoScalingGroup\/my-asg@.
    --
    -- -   ECS service - The resource type is @service@ and the unique
    --     identifier is the cluster name and service name. Example:
    --     @service\/default\/sample-webapp@.
    --
    -- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
    --     the unique identifier is the Spot Fleet request ID. Example:
    --     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
    --
    -- -   DynamoDB table - The resource type is @table@ and the unique
    --     identifier is the resource ID. Example: @table\/my-table@.
    --
    -- -   DynamoDB global secondary index - The resource type is @index@ and
    --     the unique identifier is the resource ID. Example:
    --     @table\/my-table\/index\/my-table-index@.
    --
    -- -   Aurora DB cluster - The resource type is @cluster@ and the unique
    --     identifier is the cluster name. Example: @cluster:my-db-cluster@.
    resourceId :: Prelude.Text,
    -- | The scalable dimension for the resource.
    --
    -- -   @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired
    --     capacity of an Auto Scaling group.
    --
    -- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
    --     service.
    --
    -- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
    --     Spot Fleet request.
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
    scalableDimension :: ScalableDimension,
    -- | The scaling status of the resource.
    --
    -- -   @Active@ - The scaling configuration is active.
    --
    -- -   @Inactive@ - The scaling configuration is not active because the
    --     scaling plan is being created or the scaling configuration could not
    --     be applied. Check the status message for more information.
    --
    -- -   @PartiallyActive@ - The scaling configuration is partially active
    --     because the scaling plan is being created or deleted or the scaling
    --     configuration could not be fully applied. Check the status message
    --     for more information.
    scalingStatusCode :: ScalingStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScalingPlanResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingPolicies', 'scalingPlanResource_scalingPolicies' - The scaling policies.
--
-- 'scalingStatusMessage', 'scalingPlanResource_scalingStatusMessage' - A simple message about the current scaling status of the resource.
--
-- 'scalingPlanName', 'scalingPlanResource_scalingPlanName' - The name of the scaling plan.
--
-- 'scalingPlanVersion', 'scalingPlanResource_scalingPlanVersion' - The version number of the scaling plan.
--
-- 'serviceNamespace', 'scalingPlanResource_serviceNamespace' - The namespace of the AWS service.
--
-- 'resourceId', 'scalingPlanResource_resourceId' - The ID of the resource. This string consists of the resource type and
-- unique identifier.
--
-- -   Auto Scaling group - The resource type is @autoScalingGroup@ and the
--     unique identifier is the name of the Auto Scaling group. Example:
--     @autoScalingGroup\/my-asg@.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
--     the unique identifier is the Spot Fleet request ID. Example:
--     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
--
-- -   DynamoDB table - The resource type is @table@ and the unique
--     identifier is the resource ID. Example: @table\/my-table@.
--
-- -   DynamoDB global secondary index - The resource type is @index@ and
--     the unique identifier is the resource ID. Example:
--     @table\/my-table\/index\/my-table-index@.
--
-- -   Aurora DB cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:my-db-cluster@.
--
-- 'scalableDimension', 'scalingPlanResource_scalableDimension' - The scalable dimension for the resource.
--
-- -   @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired
--     capacity of an Auto Scaling group.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet request.
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
-- 'scalingStatusCode', 'scalingPlanResource_scalingStatusCode' - The scaling status of the resource.
--
-- -   @Active@ - The scaling configuration is active.
--
-- -   @Inactive@ - The scaling configuration is not active because the
--     scaling plan is being created or the scaling configuration could not
--     be applied. Check the status message for more information.
--
-- -   @PartiallyActive@ - The scaling configuration is partially active
--     because the scaling plan is being created or deleted or the scaling
--     configuration could not be fully applied. Check the status message
--     for more information.
newScalingPlanResource ::
  -- | 'scalingPlanName'
  Prelude.Text ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'scalingStatusCode'
  ScalingStatusCode ->
  ScalingPlanResource
newScalingPlanResource
  pScalingPlanName_
  pScalingPlanVersion_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pScalingStatusCode_ =
    ScalingPlanResource'
      { scalingPolicies =
          Prelude.Nothing,
        scalingStatusMessage = Prelude.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        scalingStatusCode = pScalingStatusCode_
      }

-- | The scaling policies.
scalingPlanResource_scalingPolicies :: Lens.Lens' ScalingPlanResource (Prelude.Maybe [ScalingPolicy])
scalingPlanResource_scalingPolicies = Lens.lens (\ScalingPlanResource' {scalingPolicies} -> scalingPolicies) (\s@ScalingPlanResource' {} a -> s {scalingPolicies = a} :: ScalingPlanResource) Prelude.. Lens.mapping Prelude._Coerce

-- | A simple message about the current scaling status of the resource.
scalingPlanResource_scalingStatusMessage :: Lens.Lens' ScalingPlanResource (Prelude.Maybe Prelude.Text)
scalingPlanResource_scalingStatusMessage = Lens.lens (\ScalingPlanResource' {scalingStatusMessage} -> scalingStatusMessage) (\s@ScalingPlanResource' {} a -> s {scalingStatusMessage = a} :: ScalingPlanResource)

-- | The name of the scaling plan.
scalingPlanResource_scalingPlanName :: Lens.Lens' ScalingPlanResource Prelude.Text
scalingPlanResource_scalingPlanName = Lens.lens (\ScalingPlanResource' {scalingPlanName} -> scalingPlanName) (\s@ScalingPlanResource' {} a -> s {scalingPlanName = a} :: ScalingPlanResource)

-- | The version number of the scaling plan.
scalingPlanResource_scalingPlanVersion :: Lens.Lens' ScalingPlanResource Prelude.Integer
scalingPlanResource_scalingPlanVersion = Lens.lens (\ScalingPlanResource' {scalingPlanVersion} -> scalingPlanVersion) (\s@ScalingPlanResource' {} a -> s {scalingPlanVersion = a} :: ScalingPlanResource)

-- | The namespace of the AWS service.
scalingPlanResource_serviceNamespace :: Lens.Lens' ScalingPlanResource ServiceNamespace
scalingPlanResource_serviceNamespace = Lens.lens (\ScalingPlanResource' {serviceNamespace} -> serviceNamespace) (\s@ScalingPlanResource' {} a -> s {serviceNamespace = a} :: ScalingPlanResource)

-- | The ID of the resource. This string consists of the resource type and
-- unique identifier.
--
-- -   Auto Scaling group - The resource type is @autoScalingGroup@ and the
--     unique identifier is the name of the Auto Scaling group. Example:
--     @autoScalingGroup\/my-asg@.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
--     the unique identifier is the Spot Fleet request ID. Example:
--     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
--
-- -   DynamoDB table - The resource type is @table@ and the unique
--     identifier is the resource ID. Example: @table\/my-table@.
--
-- -   DynamoDB global secondary index - The resource type is @index@ and
--     the unique identifier is the resource ID. Example:
--     @table\/my-table\/index\/my-table-index@.
--
-- -   Aurora DB cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:my-db-cluster@.
scalingPlanResource_resourceId :: Lens.Lens' ScalingPlanResource Prelude.Text
scalingPlanResource_resourceId = Lens.lens (\ScalingPlanResource' {resourceId} -> resourceId) (\s@ScalingPlanResource' {} a -> s {resourceId = a} :: ScalingPlanResource)

-- | The scalable dimension for the resource.
--
-- -   @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired
--     capacity of an Auto Scaling group.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet request.
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
scalingPlanResource_scalableDimension :: Lens.Lens' ScalingPlanResource ScalableDimension
scalingPlanResource_scalableDimension = Lens.lens (\ScalingPlanResource' {scalableDimension} -> scalableDimension) (\s@ScalingPlanResource' {} a -> s {scalableDimension = a} :: ScalingPlanResource)

-- | The scaling status of the resource.
--
-- -   @Active@ - The scaling configuration is active.
--
-- -   @Inactive@ - The scaling configuration is not active because the
--     scaling plan is being created or the scaling configuration could not
--     be applied. Check the status message for more information.
--
-- -   @PartiallyActive@ - The scaling configuration is partially active
--     because the scaling plan is being created or deleted or the scaling
--     configuration could not be fully applied. Check the status message
--     for more information.
scalingPlanResource_scalingStatusCode :: Lens.Lens' ScalingPlanResource ScalingStatusCode
scalingPlanResource_scalingStatusCode = Lens.lens (\ScalingPlanResource' {scalingStatusCode} -> scalingStatusCode) (\s@ScalingPlanResource' {} a -> s {scalingStatusCode = a} :: ScalingPlanResource)

instance Prelude.FromJSON ScalingPlanResource where
  parseJSON =
    Prelude.withObject
      "ScalingPlanResource"
      ( \x ->
          ScalingPlanResource'
            Prelude.<$> ( x Prelude..:? "ScalingPolicies"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ScalingStatusMessage")
            Prelude.<*> (x Prelude..: "ScalingPlanName")
            Prelude.<*> (x Prelude..: "ScalingPlanVersion")
            Prelude.<*> (x Prelude..: "ServiceNamespace")
            Prelude.<*> (x Prelude..: "ResourceId")
            Prelude.<*> (x Prelude..: "ScalableDimension")
            Prelude.<*> (x Prelude..: "ScalingStatusCode")
      )

instance Prelude.Hashable ScalingPlanResource

instance Prelude.NFData ScalingPlanResource
