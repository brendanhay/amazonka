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
-- Module      : Amazonka.IoT.Types.BehaviorCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.BehaviorCriteria where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.ComparisonOperator
import Amazonka.IoT.Types.MachineLearningDetectionConfig
import Amazonka.IoT.Types.MetricValue
import Amazonka.IoT.Types.StatisticalThreshold
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The criteria by which the behavior is determined to be normal.
--
-- /See:/ 'newBehaviorCriteria' smart constructor.
data BehaviorCriteria = BehaviorCriteria'
  { -- | The configuration of an ML Detect
    mlDetectionConfig :: Prelude.Maybe MachineLearningDetectionConfig,
    -- | The value to be compared with the @metric@.
    value :: Prelude.Maybe MetricValue,
    -- | If a device is in violation of the behavior for the specified number of
    -- consecutive datapoints, an alarm occurs. If not specified, the default
    -- is 1.
    consecutiveDatapointsToAlarm :: Prelude.Maybe Prelude.Natural,
    -- | The operator that relates the thing measured (@metric@) to the criteria
    -- (containing a @value@ or @statisticalThreshold@). Valid operators
    -- include:
    --
    -- -   @string-list@: @in-set@ and @not-in-set@
    --
    -- -   @number-list@: @in-set@ and @not-in-set@
    --
    -- -   @ip-address-list@: @in-cidr-set@ and @not-in-cidr-set@
    --
    -- -   @number@: @less-than@, @less-than-equals@, @greater-than@, and
    --     @greater-than-equals@
    comparisonOperator :: Prelude.Maybe ComparisonOperator,
    -- | A statistical ranking (percentile)that indicates a threshold value by
    -- which a behavior is determined to be in compliance or in violation of
    -- the behavior.
    statisticalThreshold :: Prelude.Maybe StatisticalThreshold,
    -- | Use this to specify the time duration over which the behavior is
    -- evaluated, for those criteria that have a time dimension (for example,
    -- @NUM_MESSAGES_SENT@). For a @statisticalThreshhold@ metric comparison,
    -- measurements from all devices are accumulated over this time duration
    -- before being used to calculate percentiles, and later, measurements from
    -- an individual device are also accumulated over this time duration before
    -- being given a percentile rank. Cannot be used with list-based metric
    -- datatypes.
    durationSeconds :: Prelude.Maybe Prelude.Int,
    -- | If an alarm has occurred and the offending device is no longer in
    -- violation of the behavior for the specified number of consecutive
    -- datapoints, the alarm is cleared. If not specified, the default is 1.
    consecutiveDatapointsToClear :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BehaviorCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mlDetectionConfig', 'behaviorCriteria_mlDetectionConfig' - The configuration of an ML Detect
--
-- 'value', 'behaviorCriteria_value' - The value to be compared with the @metric@.
--
-- 'consecutiveDatapointsToAlarm', 'behaviorCriteria_consecutiveDatapointsToAlarm' - If a device is in violation of the behavior for the specified number of
-- consecutive datapoints, an alarm occurs. If not specified, the default
-- is 1.
--
-- 'comparisonOperator', 'behaviorCriteria_comparisonOperator' - The operator that relates the thing measured (@metric@) to the criteria
-- (containing a @value@ or @statisticalThreshold@). Valid operators
-- include:
--
-- -   @string-list@: @in-set@ and @not-in-set@
--
-- -   @number-list@: @in-set@ and @not-in-set@
--
-- -   @ip-address-list@: @in-cidr-set@ and @not-in-cidr-set@
--
-- -   @number@: @less-than@, @less-than-equals@, @greater-than@, and
--     @greater-than-equals@
--
-- 'statisticalThreshold', 'behaviorCriteria_statisticalThreshold' - A statistical ranking (percentile)that indicates a threshold value by
-- which a behavior is determined to be in compliance or in violation of
-- the behavior.
--
-- 'durationSeconds', 'behaviorCriteria_durationSeconds' - Use this to specify the time duration over which the behavior is
-- evaluated, for those criteria that have a time dimension (for example,
-- @NUM_MESSAGES_SENT@). For a @statisticalThreshhold@ metric comparison,
-- measurements from all devices are accumulated over this time duration
-- before being used to calculate percentiles, and later, measurements from
-- an individual device are also accumulated over this time duration before
-- being given a percentile rank. Cannot be used with list-based metric
-- datatypes.
--
-- 'consecutiveDatapointsToClear', 'behaviorCriteria_consecutiveDatapointsToClear' - If an alarm has occurred and the offending device is no longer in
-- violation of the behavior for the specified number of consecutive
-- datapoints, the alarm is cleared. If not specified, the default is 1.
newBehaviorCriteria ::
  BehaviorCriteria
newBehaviorCriteria =
  BehaviorCriteria'
    { mlDetectionConfig =
        Prelude.Nothing,
      value = Prelude.Nothing,
      consecutiveDatapointsToAlarm = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      statisticalThreshold = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      consecutiveDatapointsToClear = Prelude.Nothing
    }

-- | The configuration of an ML Detect
behaviorCriteria_mlDetectionConfig :: Lens.Lens' BehaviorCriteria (Prelude.Maybe MachineLearningDetectionConfig)
behaviorCriteria_mlDetectionConfig = Lens.lens (\BehaviorCriteria' {mlDetectionConfig} -> mlDetectionConfig) (\s@BehaviorCriteria' {} a -> s {mlDetectionConfig = a} :: BehaviorCriteria)

-- | The value to be compared with the @metric@.
behaviorCriteria_value :: Lens.Lens' BehaviorCriteria (Prelude.Maybe MetricValue)
behaviorCriteria_value = Lens.lens (\BehaviorCriteria' {value} -> value) (\s@BehaviorCriteria' {} a -> s {value = a} :: BehaviorCriteria)

-- | If a device is in violation of the behavior for the specified number of
-- consecutive datapoints, an alarm occurs. If not specified, the default
-- is 1.
behaviorCriteria_consecutiveDatapointsToAlarm :: Lens.Lens' BehaviorCriteria (Prelude.Maybe Prelude.Natural)
behaviorCriteria_consecutiveDatapointsToAlarm = Lens.lens (\BehaviorCriteria' {consecutiveDatapointsToAlarm} -> consecutiveDatapointsToAlarm) (\s@BehaviorCriteria' {} a -> s {consecutiveDatapointsToAlarm = a} :: BehaviorCriteria)

-- | The operator that relates the thing measured (@metric@) to the criteria
-- (containing a @value@ or @statisticalThreshold@). Valid operators
-- include:
--
-- -   @string-list@: @in-set@ and @not-in-set@
--
-- -   @number-list@: @in-set@ and @not-in-set@
--
-- -   @ip-address-list@: @in-cidr-set@ and @not-in-cidr-set@
--
-- -   @number@: @less-than@, @less-than-equals@, @greater-than@, and
--     @greater-than-equals@
behaviorCriteria_comparisonOperator :: Lens.Lens' BehaviorCriteria (Prelude.Maybe ComparisonOperator)
behaviorCriteria_comparisonOperator = Lens.lens (\BehaviorCriteria' {comparisonOperator} -> comparisonOperator) (\s@BehaviorCriteria' {} a -> s {comparisonOperator = a} :: BehaviorCriteria)

-- | A statistical ranking (percentile)that indicates a threshold value by
-- which a behavior is determined to be in compliance or in violation of
-- the behavior.
behaviorCriteria_statisticalThreshold :: Lens.Lens' BehaviorCriteria (Prelude.Maybe StatisticalThreshold)
behaviorCriteria_statisticalThreshold = Lens.lens (\BehaviorCriteria' {statisticalThreshold} -> statisticalThreshold) (\s@BehaviorCriteria' {} a -> s {statisticalThreshold = a} :: BehaviorCriteria)

-- | Use this to specify the time duration over which the behavior is
-- evaluated, for those criteria that have a time dimension (for example,
-- @NUM_MESSAGES_SENT@). For a @statisticalThreshhold@ metric comparison,
-- measurements from all devices are accumulated over this time duration
-- before being used to calculate percentiles, and later, measurements from
-- an individual device are also accumulated over this time duration before
-- being given a percentile rank. Cannot be used with list-based metric
-- datatypes.
behaviorCriteria_durationSeconds :: Lens.Lens' BehaviorCriteria (Prelude.Maybe Prelude.Int)
behaviorCriteria_durationSeconds = Lens.lens (\BehaviorCriteria' {durationSeconds} -> durationSeconds) (\s@BehaviorCriteria' {} a -> s {durationSeconds = a} :: BehaviorCriteria)

-- | If an alarm has occurred and the offending device is no longer in
-- violation of the behavior for the specified number of consecutive
-- datapoints, the alarm is cleared. If not specified, the default is 1.
behaviorCriteria_consecutiveDatapointsToClear :: Lens.Lens' BehaviorCriteria (Prelude.Maybe Prelude.Natural)
behaviorCriteria_consecutiveDatapointsToClear = Lens.lens (\BehaviorCriteria' {consecutiveDatapointsToClear} -> consecutiveDatapointsToClear) (\s@BehaviorCriteria' {} a -> s {consecutiveDatapointsToClear = a} :: BehaviorCriteria)

instance Core.FromJSON BehaviorCriteria where
  parseJSON =
    Core.withObject
      "BehaviorCriteria"
      ( \x ->
          BehaviorCriteria'
            Prelude.<$> (x Core..:? "mlDetectionConfig")
            Prelude.<*> (x Core..:? "value")
            Prelude.<*> (x Core..:? "consecutiveDatapointsToAlarm")
            Prelude.<*> (x Core..:? "comparisonOperator")
            Prelude.<*> (x Core..:? "statisticalThreshold")
            Prelude.<*> (x Core..:? "durationSeconds")
            Prelude.<*> (x Core..:? "consecutiveDatapointsToClear")
      )

instance Prelude.Hashable BehaviorCriteria where
  hashWithSalt salt' BehaviorCriteria' {..} =
    salt'
      `Prelude.hashWithSalt` consecutiveDatapointsToClear
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` statisticalThreshold
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` consecutiveDatapointsToAlarm
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` mlDetectionConfig

instance Prelude.NFData BehaviorCriteria where
  rnf BehaviorCriteria' {..} =
    Prelude.rnf mlDetectionConfig
      `Prelude.seq` Prelude.rnf consecutiveDatapointsToClear
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf statisticalThreshold
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf consecutiveDatapointsToAlarm
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON BehaviorCriteria where
  toJSON BehaviorCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("mlDetectionConfig" Core..=)
              Prelude.<$> mlDetectionConfig,
            ("value" Core..=) Prelude.<$> value,
            ("consecutiveDatapointsToAlarm" Core..=)
              Prelude.<$> consecutiveDatapointsToAlarm,
            ("comparisonOperator" Core..=)
              Prelude.<$> comparisonOperator,
            ("statisticalThreshold" Core..=)
              Prelude.<$> statisticalThreshold,
            ("durationSeconds" Core..=)
              Prelude.<$> durationSeconds,
            ("consecutiveDatapointsToClear" Core..=)
              Prelude.<$> consecutiveDatapointsToClear
          ]
      )
