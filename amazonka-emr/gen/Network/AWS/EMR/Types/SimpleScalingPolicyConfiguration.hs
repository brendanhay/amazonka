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
-- Module      : Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.AdjustmentType
import qualified Network.AWS.Lens as Lens

-- | An automatic scaling configuration, which describes how the policy adds
-- or removes instances, the cooldown period, and the number of EC2
-- instances that will be added each time the CloudWatch metric alarm
-- condition is satisfied.
--
-- /See:/ 'newSimpleScalingPolicyConfiguration' smart constructor.
data SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration'
  { -- | The amount of time, in seconds, after a scaling activity completes
    -- before any further trigger-related scaling activities can start. The
    -- default value is 0.
    coolDown :: Core.Maybe Core.Int,
    -- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a
    -- positive number) or terminated (if @ScalingAdjustment@ is a negative
    -- number) each time the scaling activity is triggered.
    -- @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that
    -- the EC2 instance count increments or decrements by @ScalingAdjustment@,
    -- which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@
    -- indicates the instance count increments or decrements by the percentage
    -- specified by @ScalingAdjustment@, which should be expressed as an
    -- integer. For example, 20 indicates an increase in 20% increments of
    -- cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity
    -- results in an instance group with the number of EC2 instances specified
    -- by @ScalingAdjustment@, which should be expressed as a positive integer.
    adjustmentType :: Core.Maybe AdjustmentType,
    -- | The amount by which to scale in or scale out, based on the specified
    -- @AdjustmentType@. A positive value adds to the instance group\'s EC2
    -- instance count while a negative number removes instances. If
    -- @AdjustmentType@ is set to @EXACT_CAPACITY@, the number should only be a
    -- positive integer. If @AdjustmentType@ is set to
    -- @PERCENT_CHANGE_IN_CAPACITY@, the value should express the percentage as
    -- an integer. For example, -20 indicates a decrease in 20% increments of
    -- cluster capacity.
    scalingAdjustment :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SimpleScalingPolicyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coolDown', 'simpleScalingPolicyConfiguration_coolDown' - The amount of time, in seconds, after a scaling activity completes
-- before any further trigger-related scaling activities can start. The
-- default value is 0.
--
-- 'adjustmentType', 'simpleScalingPolicyConfiguration_adjustmentType' - The way in which EC2 instances are added (if @ScalingAdjustment@ is a
-- positive number) or terminated (if @ScalingAdjustment@ is a negative
-- number) each time the scaling activity is triggered.
-- @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that
-- the EC2 instance count increments or decrements by @ScalingAdjustment@,
-- which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@
-- indicates the instance count increments or decrements by the percentage
-- specified by @ScalingAdjustment@, which should be expressed as an
-- integer. For example, 20 indicates an increase in 20% increments of
-- cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity
-- results in an instance group with the number of EC2 instances specified
-- by @ScalingAdjustment@, which should be expressed as a positive integer.
--
-- 'scalingAdjustment', 'simpleScalingPolicyConfiguration_scalingAdjustment' - The amount by which to scale in or scale out, based on the specified
-- @AdjustmentType@. A positive value adds to the instance group\'s EC2
-- instance count while a negative number removes instances. If
-- @AdjustmentType@ is set to @EXACT_CAPACITY@, the number should only be a
-- positive integer. If @AdjustmentType@ is set to
-- @PERCENT_CHANGE_IN_CAPACITY@, the value should express the percentage as
-- an integer. For example, -20 indicates a decrease in 20% increments of
-- cluster capacity.
newSimpleScalingPolicyConfiguration ::
  -- | 'scalingAdjustment'
  Core.Int ->
  SimpleScalingPolicyConfiguration
newSimpleScalingPolicyConfiguration
  pScalingAdjustment_ =
    SimpleScalingPolicyConfiguration'
      { coolDown =
          Core.Nothing,
        adjustmentType = Core.Nothing,
        scalingAdjustment = pScalingAdjustment_
      }

-- | The amount of time, in seconds, after a scaling activity completes
-- before any further trigger-related scaling activities can start. The
-- default value is 0.
simpleScalingPolicyConfiguration_coolDown :: Lens.Lens' SimpleScalingPolicyConfiguration (Core.Maybe Core.Int)
simpleScalingPolicyConfiguration_coolDown = Lens.lens (\SimpleScalingPolicyConfiguration' {coolDown} -> coolDown) (\s@SimpleScalingPolicyConfiguration' {} a -> s {coolDown = a} :: SimpleScalingPolicyConfiguration)

-- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a
-- positive number) or terminated (if @ScalingAdjustment@ is a negative
-- number) each time the scaling activity is triggered.
-- @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that
-- the EC2 instance count increments or decrements by @ScalingAdjustment@,
-- which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@
-- indicates the instance count increments or decrements by the percentage
-- specified by @ScalingAdjustment@, which should be expressed as an
-- integer. For example, 20 indicates an increase in 20% increments of
-- cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity
-- results in an instance group with the number of EC2 instances specified
-- by @ScalingAdjustment@, which should be expressed as a positive integer.
simpleScalingPolicyConfiguration_adjustmentType :: Lens.Lens' SimpleScalingPolicyConfiguration (Core.Maybe AdjustmentType)
simpleScalingPolicyConfiguration_adjustmentType = Lens.lens (\SimpleScalingPolicyConfiguration' {adjustmentType} -> adjustmentType) (\s@SimpleScalingPolicyConfiguration' {} a -> s {adjustmentType = a} :: SimpleScalingPolicyConfiguration)

-- | The amount by which to scale in or scale out, based on the specified
-- @AdjustmentType@. A positive value adds to the instance group\'s EC2
-- instance count while a negative number removes instances. If
-- @AdjustmentType@ is set to @EXACT_CAPACITY@, the number should only be a
-- positive integer. If @AdjustmentType@ is set to
-- @PERCENT_CHANGE_IN_CAPACITY@, the value should express the percentage as
-- an integer. For example, -20 indicates a decrease in 20% increments of
-- cluster capacity.
simpleScalingPolicyConfiguration_scalingAdjustment :: Lens.Lens' SimpleScalingPolicyConfiguration Core.Int
simpleScalingPolicyConfiguration_scalingAdjustment = Lens.lens (\SimpleScalingPolicyConfiguration' {scalingAdjustment} -> scalingAdjustment) (\s@SimpleScalingPolicyConfiguration' {} a -> s {scalingAdjustment = a} :: SimpleScalingPolicyConfiguration)

instance
  Core.FromJSON
    SimpleScalingPolicyConfiguration
  where
  parseJSON =
    Core.withObject
      "SimpleScalingPolicyConfiguration"
      ( \x ->
          SimpleScalingPolicyConfiguration'
            Core.<$> (x Core..:? "CoolDown")
            Core.<*> (x Core..:? "AdjustmentType")
            Core.<*> (x Core..: "ScalingAdjustment")
      )

instance
  Core.Hashable
    SimpleScalingPolicyConfiguration

instance Core.NFData SimpleScalingPolicyConfiguration

instance Core.ToJSON SimpleScalingPolicyConfiguration where
  toJSON SimpleScalingPolicyConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CoolDown" Core..=) Core.<$> coolDown,
            ("AdjustmentType" Core..=) Core.<$> adjustmentType,
            Core.Just
              ("ScalingAdjustment" Core..= scalingAdjustment)
          ]
      )
