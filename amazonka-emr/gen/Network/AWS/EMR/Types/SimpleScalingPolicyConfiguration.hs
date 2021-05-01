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
-- Module      : Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration where

import Network.AWS.EMR.Types.AdjustmentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    coolDown :: Prelude.Maybe Prelude.Int,
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
    adjustmentType :: Prelude.Maybe AdjustmentType,
    -- | The amount by which to scale in or scale out, based on the specified
    -- @AdjustmentType@. A positive value adds to the instance group\'s EC2
    -- instance count while a negative number removes instances. If
    -- @AdjustmentType@ is set to @EXACT_CAPACITY@, the number should only be a
    -- positive integer. If @AdjustmentType@ is set to
    -- @PERCENT_CHANGE_IN_CAPACITY@, the value should express the percentage as
    -- an integer. For example, -20 indicates a decrease in 20% increments of
    -- cluster capacity.
    scalingAdjustment :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  SimpleScalingPolicyConfiguration
newSimpleScalingPolicyConfiguration
  pScalingAdjustment_ =
    SimpleScalingPolicyConfiguration'
      { coolDown =
          Prelude.Nothing,
        adjustmentType = Prelude.Nothing,
        scalingAdjustment = pScalingAdjustment_
      }

-- | The amount of time, in seconds, after a scaling activity completes
-- before any further trigger-related scaling activities can start. The
-- default value is 0.
simpleScalingPolicyConfiguration_coolDown :: Lens.Lens' SimpleScalingPolicyConfiguration (Prelude.Maybe Prelude.Int)
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
simpleScalingPolicyConfiguration_adjustmentType :: Lens.Lens' SimpleScalingPolicyConfiguration (Prelude.Maybe AdjustmentType)
simpleScalingPolicyConfiguration_adjustmentType = Lens.lens (\SimpleScalingPolicyConfiguration' {adjustmentType} -> adjustmentType) (\s@SimpleScalingPolicyConfiguration' {} a -> s {adjustmentType = a} :: SimpleScalingPolicyConfiguration)

-- | The amount by which to scale in or scale out, based on the specified
-- @AdjustmentType@. A positive value adds to the instance group\'s EC2
-- instance count while a negative number removes instances. If
-- @AdjustmentType@ is set to @EXACT_CAPACITY@, the number should only be a
-- positive integer. If @AdjustmentType@ is set to
-- @PERCENT_CHANGE_IN_CAPACITY@, the value should express the percentage as
-- an integer. For example, -20 indicates a decrease in 20% increments of
-- cluster capacity.
simpleScalingPolicyConfiguration_scalingAdjustment :: Lens.Lens' SimpleScalingPolicyConfiguration Prelude.Int
simpleScalingPolicyConfiguration_scalingAdjustment = Lens.lens (\SimpleScalingPolicyConfiguration' {scalingAdjustment} -> scalingAdjustment) (\s@SimpleScalingPolicyConfiguration' {} a -> s {scalingAdjustment = a} :: SimpleScalingPolicyConfiguration)

instance
  Prelude.FromJSON
    SimpleScalingPolicyConfiguration
  where
  parseJSON =
    Prelude.withObject
      "SimpleScalingPolicyConfiguration"
      ( \x ->
          SimpleScalingPolicyConfiguration'
            Prelude.<$> (x Prelude..:? "CoolDown")
            Prelude.<*> (x Prelude..:? "AdjustmentType")
            Prelude.<*> (x Prelude..: "ScalingAdjustment")
      )

instance
  Prelude.Hashable
    SimpleScalingPolicyConfiguration

instance
  Prelude.NFData
    SimpleScalingPolicyConfiguration

instance
  Prelude.ToJSON
    SimpleScalingPolicyConfiguration
  where
  toJSON SimpleScalingPolicyConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CoolDown" Prelude..=) Prelude.<$> coolDown,
            ("AdjustmentType" Prelude..=)
              Prelude.<$> adjustmentType,
            Prelude.Just
              ("ScalingAdjustment" Prelude..= scalingAdjustment)
          ]
      )
