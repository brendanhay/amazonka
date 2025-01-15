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
-- Module      : Amazonka.AutoScalingPlans.Types.ScalingPlanResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ScalingPlanResource where

import Amazonka.AutoScalingPlans.Types.ScalableDimension
import Amazonka.AutoScalingPlans.Types.ScalingPolicy
import Amazonka.AutoScalingPlans.Types.ScalingStatusCode
import Amazonka.AutoScalingPlans.Types.ServiceNamespace
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
scalingPlanResource_scalingPolicies = Lens.lens (\ScalingPlanResource' {scalingPolicies} -> scalingPolicies) (\s@ScalingPlanResource' {} a -> s {scalingPolicies = a} :: ScalingPlanResource) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromJSON ScalingPlanResource where
  parseJSON =
    Data.withObject
      "ScalingPlanResource"
      ( \x ->
          ScalingPlanResource'
            Prelude.<$> ( x
                            Data..:? "ScalingPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ScalingStatusMessage")
            Prelude.<*> (x Data..: "ScalingPlanName")
            Prelude.<*> (x Data..: "ScalingPlanVersion")
            Prelude.<*> (x Data..: "ServiceNamespace")
            Prelude.<*> (x Data..: "ResourceId")
            Prelude.<*> (x Data..: "ScalableDimension")
            Prelude.<*> (x Data..: "ScalingStatusCode")
      )

instance Prelude.Hashable ScalingPlanResource where
  hashWithSalt _salt ScalingPlanResource' {..} =
    _salt
      `Prelude.hashWithSalt` scalingPolicies
      `Prelude.hashWithSalt` scalingStatusMessage
      `Prelude.hashWithSalt` scalingPlanName
      `Prelude.hashWithSalt` scalingPlanVersion
      `Prelude.hashWithSalt` serviceNamespace
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` scalableDimension
      `Prelude.hashWithSalt` scalingStatusCode

instance Prelude.NFData ScalingPlanResource where
  rnf ScalingPlanResource' {..} =
    Prelude.rnf scalingPolicies `Prelude.seq`
      Prelude.rnf scalingStatusMessage `Prelude.seq`
        Prelude.rnf scalingPlanName `Prelude.seq`
          Prelude.rnf scalingPlanVersion `Prelude.seq`
            Prelude.rnf serviceNamespace `Prelude.seq`
              Prelude.rnf resourceId `Prelude.seq`
                Prelude.rnf scalableDimension `Prelude.seq`
                  Prelude.rnf scalingStatusCode
