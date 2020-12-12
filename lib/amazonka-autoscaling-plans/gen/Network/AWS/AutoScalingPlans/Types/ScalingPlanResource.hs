{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
  ( ScalingPlanResource (..),

    -- * Smart constructor
    mkScalingPlanResource,

    -- * Lenses
    sprScalingStatusMessage,
    sprScalingPolicies,
    sprScalingPlanName,
    sprScalingPlanVersion,
    sprServiceNamespace,
    sprResourceId,
    sprScalableDimension,
    sprScalingStatusCode,
  )
where

import Network.AWS.AutoScalingPlans.Types.ScalableDimension
import Network.AWS.AutoScalingPlans.Types.ScalingPolicy
import Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a scalable resource.
--
-- /See:/ 'mkScalingPlanResource' smart constructor.
data ScalingPlanResource = ScalingPlanResource'
  { scalingStatusMessage ::
      Lude.Maybe Lude.Text,
    scalingPolicies :: Lude.Maybe [ScalingPolicy],
    scalingPlanName :: Lude.Text,
    scalingPlanVersion :: Lude.Integer,
    serviceNamespace :: ServiceNamespace,
    resourceId :: Lude.Text,
    scalableDimension :: ScalableDimension,
    scalingStatusCode :: ScalingStatusCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingPlanResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource. This string consists of the resource type and unique identifier.
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
-- * 'scalableDimension' - The scalable dimension for the resource.
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
-- * 'scalingPlanName' - The name of the scaling plan.
-- * 'scalingPlanVersion' - The version number of the scaling plan.
-- * 'scalingPolicies' - The scaling policies.
-- * 'scalingStatusCode' - The scaling status of the resource.
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
-- * 'scalingStatusMessage' - A simple message about the current scaling status of the resource.
-- * 'serviceNamespace' - The namespace of the AWS service.
mkScalingPlanResource ::
  -- | 'scalingPlanName'
  Lude.Text ->
  -- | 'scalingPlanVersion'
  Lude.Integer ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'scalingStatusCode'
  ScalingStatusCode ->
  ScalingPlanResource
mkScalingPlanResource
  pScalingPlanName_
  pScalingPlanVersion_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pScalingStatusCode_ =
    ScalingPlanResource'
      { scalingStatusMessage = Lude.Nothing,
        scalingPolicies = Lude.Nothing,
        scalingPlanName = pScalingPlanName_,
        scalingPlanVersion = pScalingPlanVersion_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        scalingStatusCode = pScalingStatusCode_
      }

-- | A simple message about the current scaling status of the resource.
--
-- /Note:/ Consider using 'scalingStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingStatusMessage :: Lens.Lens' ScalingPlanResource (Lude.Maybe Lude.Text)
sprScalingStatusMessage = Lens.lens (scalingStatusMessage :: ScalingPlanResource -> Lude.Maybe Lude.Text) (\s a -> s {scalingStatusMessage = a} :: ScalingPlanResource)
{-# DEPRECATED sprScalingStatusMessage "Use generic-lens or generic-optics with 'scalingStatusMessage' instead." #-}

-- | The scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingPolicies :: Lens.Lens' ScalingPlanResource (Lude.Maybe [ScalingPolicy])
sprScalingPolicies = Lens.lens (scalingPolicies :: ScalingPlanResource -> Lude.Maybe [ScalingPolicy]) (\s a -> s {scalingPolicies = a} :: ScalingPlanResource)
{-# DEPRECATED sprScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingPlanName :: Lens.Lens' ScalingPlanResource Lude.Text
sprScalingPlanName = Lens.lens (scalingPlanName :: ScalingPlanResource -> Lude.Text) (\s a -> s {scalingPlanName = a} :: ScalingPlanResource)
{-# DEPRECATED sprScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprScalingPlanVersion :: Lens.Lens' ScalingPlanResource Lude.Integer
sprScalingPlanVersion = Lens.lens (scalingPlanVersion :: ScalingPlanResource -> Lude.Integer) (\s a -> s {scalingPlanVersion = a} :: ScalingPlanResource)
{-# DEPRECATED sprScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

-- | The namespace of the AWS service.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprServiceNamespace :: Lens.Lens' ScalingPlanResource ServiceNamespace
sprServiceNamespace = Lens.lens (serviceNamespace :: ScalingPlanResource -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: ScalingPlanResource)
{-# DEPRECATED sprServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

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
sprResourceId :: Lens.Lens' ScalingPlanResource Lude.Text
sprResourceId = Lens.lens (resourceId :: ScalingPlanResource -> Lude.Text) (\s a -> s {resourceId = a} :: ScalingPlanResource)
{-# DEPRECATED sprResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

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
sprScalableDimension :: Lens.Lens' ScalingPlanResource ScalableDimension
sprScalableDimension = Lens.lens (scalableDimension :: ScalingPlanResource -> ScalableDimension) (\s a -> s {scalableDimension = a} :: ScalingPlanResource)
{-# DEPRECATED sprScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

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
sprScalingStatusCode :: Lens.Lens' ScalingPlanResource ScalingStatusCode
sprScalingStatusCode = Lens.lens (scalingStatusCode :: ScalingPlanResource -> ScalingStatusCode) (\s a -> s {scalingStatusCode = a} :: ScalingPlanResource)
{-# DEPRECATED sprScalingStatusCode "Use generic-lens or generic-optics with 'scalingStatusCode' instead." #-}

instance Lude.FromJSON ScalingPlanResource where
  parseJSON =
    Lude.withObject
      "ScalingPlanResource"
      ( \x ->
          ScalingPlanResource'
            Lude.<$> (x Lude..:? "ScalingStatusMessage")
            Lude.<*> (x Lude..:? "ScalingPolicies" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ScalingPlanName")
            Lude.<*> (x Lude..: "ScalingPlanVersion")
            Lude.<*> (x Lude..: "ServiceNamespace")
            Lude.<*> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ScalableDimension")
            Lude.<*> (x Lude..: "ScalingStatusCode")
      )
