{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
  ( ScalingPlanResource (..)
  -- * Smart constructor
  , mkScalingPlanResource
  -- * Lenses
  , sprScalingPlanName
  , sprScalingPlanVersion
  , sprServiceNamespace
  , sprResourceId
  , sprScalableDimension
  , sprScalingStatusCode
  , sprScalingPolicies
  , sprScalingStatusMessage
  ) where

import qualified Network.AWS.AutoScalingPlans.Types.ResourceId as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalableDimension as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalingPlanName as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalingPolicy as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalingStatusCode as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalingStatusMessage as Types
import qualified Network.AWS.AutoScalingPlans.Types.ServiceNamespace as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a scalable resource.
--
-- /See:/ 'mkScalingPlanResource' smart constructor.
data ScalingPlanResource = ScalingPlanResource'
  { scalingPlanName :: Types.ScalingPlanName
    -- ^ The name of the scaling plan.
  , scalingPlanVersion :: Core.Integer
    -- ^ The version number of the scaling plan.
  , serviceNamespace :: Types.ServiceNamespace
    -- ^ The namespace of the AWS service.
  , resourceId :: Types.ResourceId
    -- ^ The ID of the resource. This string consists of the resource type and unique identifier.
--
--
--     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
  , scalableDimension :: Types.ScalableDimension
    -- ^ The scalable dimension for the resource.
--
--
--     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
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
  , scalingStatusCode :: Types.ScalingStatusCode
    -- ^ The scaling status of the resource.
--
--
--     * @Active@ - The scaling configuration is active.
--
--
--     * @Inactive@ - The scaling configuration is not active because the scaling plan is being created or the scaling configuration could not be applied. Check the status message for more information.
--
--
--     * @PartiallyActive@ - The scaling configuration is partially active because the scaling plan is being created or deleted or the scaling configuration could not be fully applied. Check the status message for more information.
--
--
  , scalingPolicies :: Core.Maybe [Types.ScalingPolicy]
    -- ^ The scaling policies.
  , scalingStatusMessage :: Core.Maybe Types.ScalingStatusMessage
    -- ^ A simple message about the current scaling status of the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingPlanResource' value with any optional fields omitted.
mkScalingPlanResource
    :: Types.ScalingPlanName -- ^ 'scalingPlanName'
    -> Core.Integer -- ^ 'scalingPlanVersion'
    -> Types.ServiceNamespace -- ^ 'serviceNamespace'
    -> Types.ResourceId -- ^ 'resourceId'
    -> Types.ScalableDimension -- ^ 'scalableDimension'
    -> Types.ScalingStatusCode -- ^ 'scalingStatusCode'
    -> ScalingPlanResource
mkScalingPlanResource scalingPlanName scalingPlanVersion
  serviceNamespace resourceId scalableDimension scalingStatusCode
  = ScalingPlanResource'{scalingPlanName, scalingPlanVersion,
                         serviceNamespace, resourceId, scalableDimension, scalingStatusCode,
                         scalingPolicies = Core.Nothing,
                         scalingStatusMessage = Core.Nothing}

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingPlanName :: Lens.Lens' ScalingPlanResource Types.ScalingPlanName
sprScalingPlanName = Lens.field @"scalingPlanName"
{-# INLINEABLE sprScalingPlanName #-}
{-# DEPRECATED scalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead"  #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingPlanVersion :: Lens.Lens' ScalingPlanResource Core.Integer
sprScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# INLINEABLE sprScalingPlanVersion #-}
{-# DEPRECATED scalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead"  #-}

-- | The namespace of the AWS service.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprServiceNamespace :: Lens.Lens' ScalingPlanResource Types.ServiceNamespace
sprServiceNamespace = Lens.field @"serviceNamespace"
{-# INLINEABLE sprServiceNamespace #-}
{-# DEPRECATED serviceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead"  #-}

-- | The ID of the resource. This string consists of the resource type and unique identifier.
--
--
--     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprResourceId :: Lens.Lens' ScalingPlanResource Types.ResourceId
sprResourceId = Lens.field @"resourceId"
{-# INLINEABLE sprResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The scalable dimension for the resource.
--
--
--     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
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
--
-- /Note:/ Consider using 'scalableDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalableDimension :: Lens.Lens' ScalingPlanResource Types.ScalableDimension
sprScalableDimension = Lens.field @"scalableDimension"
{-# INLINEABLE sprScalableDimension #-}
{-# DEPRECATED scalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead"  #-}

-- | The scaling status of the resource.
--
--
--     * @Active@ - The scaling configuration is active.
--
--
--     * @Inactive@ - The scaling configuration is not active because the scaling plan is being created or the scaling configuration could not be applied. Check the status message for more information.
--
--
--     * @PartiallyActive@ - The scaling configuration is partially active because the scaling plan is being created or deleted or the scaling configuration could not be fully applied. Check the status message for more information.
--
--
--
-- /Note:/ Consider using 'scalingStatusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingStatusCode :: Lens.Lens' ScalingPlanResource Types.ScalingStatusCode
sprScalingStatusCode = Lens.field @"scalingStatusCode"
{-# INLINEABLE sprScalingStatusCode #-}
{-# DEPRECATED scalingStatusCode "Use generic-lens or generic-optics with 'scalingStatusCode' instead"  #-}

-- | The scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingPolicies :: Lens.Lens' ScalingPlanResource (Core.Maybe [Types.ScalingPolicy])
sprScalingPolicies = Lens.field @"scalingPolicies"
{-# INLINEABLE sprScalingPolicies #-}
{-# DEPRECATED scalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead"  #-}

-- | A simple message about the current scaling status of the resource.
--
-- /Note:/ Consider using 'scalingStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingStatusMessage :: Lens.Lens' ScalingPlanResource (Core.Maybe Types.ScalingStatusMessage)
sprScalingStatusMessage = Lens.field @"scalingStatusMessage"
{-# INLINEABLE sprScalingStatusMessage #-}
{-# DEPRECATED scalingStatusMessage "Use generic-lens or generic-optics with 'scalingStatusMessage' instead"  #-}

instance Core.FromJSON ScalingPlanResource where
        parseJSON
          = Core.withObject "ScalingPlanResource" Core.$
              \ x ->
                ScalingPlanResource' Core.<$>
                  (x Core..: "ScalingPlanName") Core.<*>
                    x Core..: "ScalingPlanVersion"
                    Core.<*> x Core..: "ServiceNamespace"
                    Core.<*> x Core..: "ResourceId"
                    Core.<*> x Core..: "ScalableDimension"
                    Core.<*> x Core..: "ScalingStatusCode"
                    Core.<*> x Core..:? "ScalingPolicies"
                    Core.<*> x Core..:? "ScalingStatusMessage"
