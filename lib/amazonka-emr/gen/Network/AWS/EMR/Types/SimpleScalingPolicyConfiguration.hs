{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
  ( SimpleScalingPolicyConfiguration (..)
  -- * Smart constructor
  , mkSimpleScalingPolicyConfiguration
  -- * Lenses
  , sspcScalingAdjustment
  , sspcAdjustmentType
  , sspcCoolDown
  ) where

import qualified Network.AWS.EMR.Types.AdjustmentType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An automatic scaling configuration, which describes how the policy adds or removes instances, the cooldown period, and the number of EC2 instances that will be added each time the CloudWatch metric alarm condition is satisfied.
--
-- /See:/ 'mkSimpleScalingPolicyConfiguration' smart constructor.
data SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration'
  { scalingAdjustment :: Core.Int
    -- ^ The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
  , adjustmentType :: Core.Maybe Types.AdjustmentType
    -- ^ The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
  , coolDown :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SimpleScalingPolicyConfiguration' value with any optional fields omitted.
mkSimpleScalingPolicyConfiguration
    :: Core.Int -- ^ 'scalingAdjustment'
    -> SimpleScalingPolicyConfiguration
mkSimpleScalingPolicyConfiguration scalingAdjustment
  = SimpleScalingPolicyConfiguration'{scalingAdjustment,
                                      adjustmentType = Core.Nothing, coolDown = Core.Nothing}

-- | The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcScalingAdjustment :: Lens.Lens' SimpleScalingPolicyConfiguration Core.Int
sspcScalingAdjustment = Lens.field @"scalingAdjustment"
{-# INLINEABLE sspcScalingAdjustment #-}
{-# DEPRECATED scalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead"  #-}

-- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcAdjustmentType :: Lens.Lens' SimpleScalingPolicyConfiguration (Core.Maybe Types.AdjustmentType)
sspcAdjustmentType = Lens.field @"adjustmentType"
{-# INLINEABLE sspcAdjustmentType #-}
{-# DEPRECATED adjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead"  #-}

-- | The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
--
-- /Note:/ Consider using 'coolDown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcCoolDown :: Lens.Lens' SimpleScalingPolicyConfiguration (Core.Maybe Core.Int)
sspcCoolDown = Lens.field @"coolDown"
{-# INLINEABLE sspcCoolDown #-}
{-# DEPRECATED coolDown "Use generic-lens or generic-optics with 'coolDown' instead"  #-}

instance Core.FromJSON SimpleScalingPolicyConfiguration where
        toJSON SimpleScalingPolicyConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScalingAdjustment" Core..= scalingAdjustment),
                  ("AdjustmentType" Core..=) Core.<$> adjustmentType,
                  ("CoolDown" Core..=) Core.<$> coolDown])

instance Core.FromJSON SimpleScalingPolicyConfiguration where
        parseJSON
          = Core.withObject "SimpleScalingPolicyConfiguration" Core.$
              \ x ->
                SimpleScalingPolicyConfiguration' Core.<$>
                  (x Core..: "ScalingAdjustment") Core.<*>
                    x Core..:? "AdjustmentType"
                    Core.<*> x Core..:? "CoolDown"
