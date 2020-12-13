{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
  ( SimpleScalingPolicyConfiguration (..),

    -- * Smart constructor
    mkSimpleScalingPolicyConfiguration,

    -- * Lenses
    sspcAdjustmentType,
    sspcScalingAdjustment,
    sspcCoolDown,
  )
where

import Network.AWS.EMR.Types.AdjustmentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An automatic scaling configuration, which describes how the policy adds or removes instances, the cooldown period, and the number of EC2 instances that will be added each time the CloudWatch metric alarm condition is satisfied.
--
-- /See:/ 'mkSimpleScalingPolicyConfiguration' smart constructor.
data SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration'
  { -- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
    adjustmentType :: Lude.Maybe AdjustmentType,
    -- | The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
    scalingAdjustment :: Lude.Int,
    -- | The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
    coolDown :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimpleScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- * 'adjustmentType' - The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
-- * 'scalingAdjustment' - The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
-- * 'coolDown' - The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
mkSimpleScalingPolicyConfiguration ::
  -- | 'scalingAdjustment'
  Lude.Int ->
  SimpleScalingPolicyConfiguration
mkSimpleScalingPolicyConfiguration pScalingAdjustment_ =
  SimpleScalingPolicyConfiguration'
    { adjustmentType = Lude.Nothing,
      scalingAdjustment = pScalingAdjustment_,
      coolDown = Lude.Nothing
    }

-- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcAdjustmentType :: Lens.Lens' SimpleScalingPolicyConfiguration (Lude.Maybe AdjustmentType)
sspcAdjustmentType = Lens.lens (adjustmentType :: SimpleScalingPolicyConfiguration -> Lude.Maybe AdjustmentType) (\s a -> s {adjustmentType = a} :: SimpleScalingPolicyConfiguration)
{-# DEPRECATED sspcAdjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead." #-}

-- | The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
--
-- /Note:/ Consider using 'scalingAdjustment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcScalingAdjustment :: Lens.Lens' SimpleScalingPolicyConfiguration Lude.Int
sspcScalingAdjustment = Lens.lens (scalingAdjustment :: SimpleScalingPolicyConfiguration -> Lude.Int) (\s a -> s {scalingAdjustment = a} :: SimpleScalingPolicyConfiguration)
{-# DEPRECATED sspcScalingAdjustment "Use generic-lens or generic-optics with 'scalingAdjustment' instead." #-}

-- | The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
--
-- /Note:/ Consider using 'coolDown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspcCoolDown :: Lens.Lens' SimpleScalingPolicyConfiguration (Lude.Maybe Lude.Int)
sspcCoolDown = Lens.lens (coolDown :: SimpleScalingPolicyConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {coolDown = a} :: SimpleScalingPolicyConfiguration)
{-# DEPRECATED sspcCoolDown "Use generic-lens or generic-optics with 'coolDown' instead." #-}

instance Lude.FromJSON SimpleScalingPolicyConfiguration where
  parseJSON =
    Lude.withObject
      "SimpleScalingPolicyConfiguration"
      ( \x ->
          SimpleScalingPolicyConfiguration'
            Lude.<$> (x Lude..:? "AdjustmentType")
            Lude.<*> (x Lude..: "ScalingAdjustment")
            Lude.<*> (x Lude..:? "CoolDown")
      )

instance Lude.ToJSON SimpleScalingPolicyConfiguration where
  toJSON SimpleScalingPolicyConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AdjustmentType" Lude..=) Lude.<$> adjustmentType,
            Lude.Just ("ScalingAdjustment" Lude..= scalingAdjustment),
            ("CoolDown" Lude..=) Lude.<$> coolDown
          ]
      )
