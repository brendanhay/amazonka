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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.BehaviorCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ComparisonOperator
import Amazonka.IoT.Types.MachineLearningDetectionConfig
import Amazonka.IoT.Types.MetricValue
import Amazonka.IoT.Types.StatisticalThreshold
import qualified Amazonka.Prelude as Prelude

-- | The criteria by which the behavior is determined to be normal.
--
-- /See:/ 'newBehaviorCriteria' smart constructor.
data BehaviorCriteria = BehaviorCriteria'
  { -- | The operator that relates the thing measured (@metric@) to the criteria
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
    -- | If a device is in violation of the behavior for the specified number of
    -- consecutive datapoints, an alarm occurs. If not specified, the default
    -- is 1.
    consecutiveDatapointsToAlarm :: Prelude.Maybe Prelude.Natural,
    -- | If an alarm has occurred and the offending device is no longer in
    -- violation of the behavior for the specified number of consecutive
    -- datapoints, the alarm is cleared. If not specified, the default is 1.
    consecutiveDatapointsToClear :: Prelude.Maybe Prelude.Natural,
    -- | Use this to specify the time duration over which the behavior is
    -- evaluated, for those criteria that have a time dimension (for example,
    -- @NUM_MESSAGES_SENT@). For a @statisticalThreshhold@ metric comparison,
    -- measurements from all devices are accumulated over this time duration
    -- before being used to calculate percentiles, and later, measurements from
    -- an individual device are also accumulated over this time duration before
    -- being given a percentile rank. Cannot be used with list-based metric
    -- datatypes.
    durationSeconds :: Prelude.Maybe Prelude.Int,
    -- | The configuration of an ML Detect
    mlDetectionConfig :: Prelude.Maybe MachineLearningDetectionConfig,
    -- | A statistical ranking (percentile)that indicates a threshold value by
    -- which a behavior is determined to be in compliance or in violation of
    -- the behavior.
    statisticalThreshold :: Prelude.Maybe StatisticalThreshold,
    -- | The value to be compared with the @metric@.
    value :: Prelude.Maybe MetricValue
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
-- 'consecutiveDatapointsToAlarm', 'behaviorCriteria_consecutiveDatapointsToAlarm' - If a device is in violation of the behavior for the specified number of
-- consecutive datapoints, an alarm occurs. If not specified, the default
-- is 1.
--
-- 'consecutiveDatapointsToClear', 'behaviorCriteria_consecutiveDatapointsToClear' - If an alarm has occurred and the offending device is no longer in
-- violation of the behavior for the specified number of consecutive
-- datapoints, the alarm is cleared. If not specified, the default is 1.
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
-- 'mlDetectionConfig', 'behaviorCriteria_mlDetectionConfig' - The configuration of an ML Detect
--
-- 'statisticalThreshold', 'behaviorCriteria_statisticalThreshold' - A statistical ranking (percentile)that indicates a threshold value by
-- which a behavior is determined to be in compliance or in violation of
-- the behavior.
--
-- 'value', 'behaviorCriteria_value' - The value to be compared with the @metric@.
newBehaviorCriteria ::
  BehaviorCriteria
newBehaviorCriteria =
  BehaviorCriteria'
    { comparisonOperator =
        Prelude.Nothing,
      consecutiveDatapointsToAlarm = Prelude.Nothing,
      consecutiveDatapointsToClear = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      mlDetectionConfig = Prelude.Nothing,
      statisticalThreshold = Prelude.Nothing,
      value = Prelude.Nothing
    }

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

-- | If a device is in violation of the behavior for the specified number of
-- consecutive datapoints, an alarm occurs. If not specified, the default
-- is 1.
behaviorCriteria_consecutiveDatapointsToAlarm :: Lens.Lens' BehaviorCriteria (Prelude.Maybe Prelude.Natural)
behaviorCriteria_consecutiveDatapointsToAlarm = Lens.lens (\BehaviorCriteria' {consecutiveDatapointsToAlarm} -> consecutiveDatapointsToAlarm) (\s@BehaviorCriteria' {} a -> s {consecutiveDatapointsToAlarm = a} :: BehaviorCriteria)

-- | If an alarm has occurred and the offending device is no longer in
-- violation of the behavior for the specified number of consecutive
-- datapoints, the alarm is cleared. If not specified, the default is 1.
behaviorCriteria_consecutiveDatapointsToClear :: Lens.Lens' BehaviorCriteria (Prelude.Maybe Prelude.Natural)
behaviorCriteria_consecutiveDatapointsToClear = Lens.lens (\BehaviorCriteria' {consecutiveDatapointsToClear} -> consecutiveDatapointsToClear) (\s@BehaviorCriteria' {} a -> s {consecutiveDatapointsToClear = a} :: BehaviorCriteria)

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

-- | The configuration of an ML Detect
behaviorCriteria_mlDetectionConfig :: Lens.Lens' BehaviorCriteria (Prelude.Maybe MachineLearningDetectionConfig)
behaviorCriteria_mlDetectionConfig = Lens.lens (\BehaviorCriteria' {mlDetectionConfig} -> mlDetectionConfig) (\s@BehaviorCriteria' {} a -> s {mlDetectionConfig = a} :: BehaviorCriteria)

-- | A statistical ranking (percentile)that indicates a threshold value by
-- which a behavior is determined to be in compliance or in violation of
-- the behavior.
behaviorCriteria_statisticalThreshold :: Lens.Lens' BehaviorCriteria (Prelude.Maybe StatisticalThreshold)
behaviorCriteria_statisticalThreshold = Lens.lens (\BehaviorCriteria' {statisticalThreshold} -> statisticalThreshold) (\s@BehaviorCriteria' {} a -> s {statisticalThreshold = a} :: BehaviorCriteria)

-- | The value to be compared with the @metric@.
behaviorCriteria_value :: Lens.Lens' BehaviorCriteria (Prelude.Maybe MetricValue)
behaviorCriteria_value = Lens.lens (\BehaviorCriteria' {value} -> value) (\s@BehaviorCriteria' {} a -> s {value = a} :: BehaviorCriteria)

instance Data.FromJSON BehaviorCriteria where
  parseJSON =
    Data.withObject
      "BehaviorCriteria"
      ( \x ->
          BehaviorCriteria'
            Prelude.<$> (x Data..:? "comparisonOperator")
            Prelude.<*> (x Data..:? "consecutiveDatapointsToAlarm")
            Prelude.<*> (x Data..:? "consecutiveDatapointsToClear")
            Prelude.<*> (x Data..:? "durationSeconds")
            Prelude.<*> (x Data..:? "mlDetectionConfig")
            Prelude.<*> (x Data..:? "statisticalThreshold")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable BehaviorCriteria where
  hashWithSalt _salt BehaviorCriteria' {..} =
    _salt `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` consecutiveDatapointsToAlarm
      `Prelude.hashWithSalt` consecutiveDatapointsToClear
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` mlDetectionConfig
      `Prelude.hashWithSalt` statisticalThreshold
      `Prelude.hashWithSalt` value

instance Prelude.NFData BehaviorCriteria where
  rnf BehaviorCriteria' {..} =
    Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf consecutiveDatapointsToAlarm
      `Prelude.seq` Prelude.rnf consecutiveDatapointsToClear
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf mlDetectionConfig
      `Prelude.seq` Prelude.rnf statisticalThreshold
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON BehaviorCriteria where
  toJSON BehaviorCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comparisonOperator" Data..=)
              Prelude.<$> comparisonOperator,
            ("consecutiveDatapointsToAlarm" Data..=)
              Prelude.<$> consecutiveDatapointsToAlarm,
            ("consecutiveDatapointsToClear" Data..=)
              Prelude.<$> consecutiveDatapointsToClear,
            ("durationSeconds" Data..=)
              Prelude.<$> durationSeconds,
            ("mlDetectionConfig" Data..=)
              Prelude.<$> mlDetectionConfig,
            ("statisticalThreshold" Data..=)
              Prelude.<$> statisticalThreshold,
            ("value" Data..=) Prelude.<$> value
          ]
      )
