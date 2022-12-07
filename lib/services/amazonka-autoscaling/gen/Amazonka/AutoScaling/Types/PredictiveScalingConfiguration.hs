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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingConfiguration where

import Amazonka.AutoScaling.Types.PredictiveScalingMaxCapacityBreachBehavior
import Amazonka.AutoScaling.Types.PredictiveScalingMetricSpecification
import Amazonka.AutoScaling.Types.PredictiveScalingMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a predictive scaling policy configuration to use with Amazon
-- EC2 Auto Scaling.
--
-- /See:/ 'newPredictiveScalingConfiguration' smart constructor.
data PredictiveScalingConfiguration = PredictiveScalingConfiguration'
  { -- | The size of the capacity buffer to use when the forecast capacity is
    -- close to or exceeds the maximum capacity. The value is specified as a
    -- percentage relative to the forecast capacity. For example, if the buffer
    -- is 10, this means a 10 percent buffer, such that if the forecast
    -- capacity is 50, and the maximum capacity is 40, then the effective
    -- maximum capacity is 55.
    --
    -- If set to 0, Amazon EC2 Auto Scaling may scale capacity higher than the
    -- maximum capacity to equal but not exceed forecast capacity.
    --
    -- Required if the @MaxCapacityBreachBehavior@ property is set to
    -- @IncreaseMaxCapacity@, and cannot be used otherwise.
    maxCapacityBuffer :: Prelude.Maybe Prelude.Natural,
    -- | Defines the behavior that should be applied if the forecast capacity
    -- approaches or exceeds the maximum capacity of the Auto Scaling group.
    -- Defaults to @HonorMaxCapacity@ if not specified.
    --
    -- The following are possible values:
    --
    -- -   @HonorMaxCapacity@ - Amazon EC2 Auto Scaling cannot scale out
    --     capacity higher than the maximum capacity. The maximum capacity is
    --     enforced as a hard limit.
    --
    -- -   @IncreaseMaxCapacity@ - Amazon EC2 Auto Scaling can scale out
    --     capacity higher than the maximum capacity when the forecast capacity
    --     is close to or exceeds the maximum capacity. The upper limit is
    --     determined by the forecasted capacity and the value for
    --     @MaxCapacityBuffer@.
    maxCapacityBreachBehavior :: Prelude.Maybe PredictiveScalingMaxCapacityBreachBehavior,
    -- | The predictive scaling mode. Defaults to @ForecastOnly@ if not
    -- specified.
    mode :: Prelude.Maybe PredictiveScalingMode,
    -- | The amount of time, in seconds, by which the instance launch time can be
    -- advanced. For example, the forecast says to add capacity at 10:00 AM,
    -- and you choose to pre-launch instances by 5 minutes. In that case, the
    -- instances will be launched at 9:55 AM. The intention is to give
    -- resources time to be provisioned. It can take a few minutes to launch an
    -- EC2 instance. The actual amount of time required depends on several
    -- factors, such as the size of the instance and whether there are startup
    -- scripts to complete.
    --
    -- The value must be less than the forecast interval duration of 3600
    -- seconds (60 minutes). Defaults to 300 seconds if not specified.
    schedulingBufferTime :: Prelude.Maybe Prelude.Natural,
    -- | This structure includes the metrics and target utilization to use for
    -- predictive scaling.
    --
    -- This is an array, but we currently only support a single metric
    -- specification. That is, you can specify a target value and a single
    -- metric pair, or a target value and one scaling metric and one load
    -- metric.
    metricSpecifications :: [PredictiveScalingMetricSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCapacityBuffer', 'predictiveScalingConfiguration_maxCapacityBuffer' - The size of the capacity buffer to use when the forecast capacity is
-- close to or exceeds the maximum capacity. The value is specified as a
-- percentage relative to the forecast capacity. For example, if the buffer
-- is 10, this means a 10 percent buffer, such that if the forecast
-- capacity is 50, and the maximum capacity is 40, then the effective
-- maximum capacity is 55.
--
-- If set to 0, Amazon EC2 Auto Scaling may scale capacity higher than the
-- maximum capacity to equal but not exceed forecast capacity.
--
-- Required if the @MaxCapacityBreachBehavior@ property is set to
-- @IncreaseMaxCapacity@, and cannot be used otherwise.
--
-- 'maxCapacityBreachBehavior', 'predictiveScalingConfiguration_maxCapacityBreachBehavior' - Defines the behavior that should be applied if the forecast capacity
-- approaches or exceeds the maximum capacity of the Auto Scaling group.
-- Defaults to @HonorMaxCapacity@ if not specified.
--
-- The following are possible values:
--
-- -   @HonorMaxCapacity@ - Amazon EC2 Auto Scaling cannot scale out
--     capacity higher than the maximum capacity. The maximum capacity is
--     enforced as a hard limit.
--
-- -   @IncreaseMaxCapacity@ - Amazon EC2 Auto Scaling can scale out
--     capacity higher than the maximum capacity when the forecast capacity
--     is close to or exceeds the maximum capacity. The upper limit is
--     determined by the forecasted capacity and the value for
--     @MaxCapacityBuffer@.
--
-- 'mode', 'predictiveScalingConfiguration_mode' - The predictive scaling mode. Defaults to @ForecastOnly@ if not
-- specified.
--
-- 'schedulingBufferTime', 'predictiveScalingConfiguration_schedulingBufferTime' - The amount of time, in seconds, by which the instance launch time can be
-- advanced. For example, the forecast says to add capacity at 10:00 AM,
-- and you choose to pre-launch instances by 5 minutes. In that case, the
-- instances will be launched at 9:55 AM. The intention is to give
-- resources time to be provisioned. It can take a few minutes to launch an
-- EC2 instance. The actual amount of time required depends on several
-- factors, such as the size of the instance and whether there are startup
-- scripts to complete.
--
-- The value must be less than the forecast interval duration of 3600
-- seconds (60 minutes). Defaults to 300 seconds if not specified.
--
-- 'metricSpecifications', 'predictiveScalingConfiguration_metricSpecifications' - This structure includes the metrics and target utilization to use for
-- predictive scaling.
--
-- This is an array, but we currently only support a single metric
-- specification. That is, you can specify a target value and a single
-- metric pair, or a target value and one scaling metric and one load
-- metric.
newPredictiveScalingConfiguration ::
  PredictiveScalingConfiguration
newPredictiveScalingConfiguration =
  PredictiveScalingConfiguration'
    { maxCapacityBuffer =
        Prelude.Nothing,
      maxCapacityBreachBehavior = Prelude.Nothing,
      mode = Prelude.Nothing,
      schedulingBufferTime = Prelude.Nothing,
      metricSpecifications = Prelude.mempty
    }

-- | The size of the capacity buffer to use when the forecast capacity is
-- close to or exceeds the maximum capacity. The value is specified as a
-- percentage relative to the forecast capacity. For example, if the buffer
-- is 10, this means a 10 percent buffer, such that if the forecast
-- capacity is 50, and the maximum capacity is 40, then the effective
-- maximum capacity is 55.
--
-- If set to 0, Amazon EC2 Auto Scaling may scale capacity higher than the
-- maximum capacity to equal but not exceed forecast capacity.
--
-- Required if the @MaxCapacityBreachBehavior@ property is set to
-- @IncreaseMaxCapacity@, and cannot be used otherwise.
predictiveScalingConfiguration_maxCapacityBuffer :: Lens.Lens' PredictiveScalingConfiguration (Prelude.Maybe Prelude.Natural)
predictiveScalingConfiguration_maxCapacityBuffer = Lens.lens (\PredictiveScalingConfiguration' {maxCapacityBuffer} -> maxCapacityBuffer) (\s@PredictiveScalingConfiguration' {} a -> s {maxCapacityBuffer = a} :: PredictiveScalingConfiguration)

-- | Defines the behavior that should be applied if the forecast capacity
-- approaches or exceeds the maximum capacity of the Auto Scaling group.
-- Defaults to @HonorMaxCapacity@ if not specified.
--
-- The following are possible values:
--
-- -   @HonorMaxCapacity@ - Amazon EC2 Auto Scaling cannot scale out
--     capacity higher than the maximum capacity. The maximum capacity is
--     enforced as a hard limit.
--
-- -   @IncreaseMaxCapacity@ - Amazon EC2 Auto Scaling can scale out
--     capacity higher than the maximum capacity when the forecast capacity
--     is close to or exceeds the maximum capacity. The upper limit is
--     determined by the forecasted capacity and the value for
--     @MaxCapacityBuffer@.
predictiveScalingConfiguration_maxCapacityBreachBehavior :: Lens.Lens' PredictiveScalingConfiguration (Prelude.Maybe PredictiveScalingMaxCapacityBreachBehavior)
predictiveScalingConfiguration_maxCapacityBreachBehavior = Lens.lens (\PredictiveScalingConfiguration' {maxCapacityBreachBehavior} -> maxCapacityBreachBehavior) (\s@PredictiveScalingConfiguration' {} a -> s {maxCapacityBreachBehavior = a} :: PredictiveScalingConfiguration)

-- | The predictive scaling mode. Defaults to @ForecastOnly@ if not
-- specified.
predictiveScalingConfiguration_mode :: Lens.Lens' PredictiveScalingConfiguration (Prelude.Maybe PredictiveScalingMode)
predictiveScalingConfiguration_mode = Lens.lens (\PredictiveScalingConfiguration' {mode} -> mode) (\s@PredictiveScalingConfiguration' {} a -> s {mode = a} :: PredictiveScalingConfiguration)

-- | The amount of time, in seconds, by which the instance launch time can be
-- advanced. For example, the forecast says to add capacity at 10:00 AM,
-- and you choose to pre-launch instances by 5 minutes. In that case, the
-- instances will be launched at 9:55 AM. The intention is to give
-- resources time to be provisioned. It can take a few minutes to launch an
-- EC2 instance. The actual amount of time required depends on several
-- factors, such as the size of the instance and whether there are startup
-- scripts to complete.
--
-- The value must be less than the forecast interval duration of 3600
-- seconds (60 minutes). Defaults to 300 seconds if not specified.
predictiveScalingConfiguration_schedulingBufferTime :: Lens.Lens' PredictiveScalingConfiguration (Prelude.Maybe Prelude.Natural)
predictiveScalingConfiguration_schedulingBufferTime = Lens.lens (\PredictiveScalingConfiguration' {schedulingBufferTime} -> schedulingBufferTime) (\s@PredictiveScalingConfiguration' {} a -> s {schedulingBufferTime = a} :: PredictiveScalingConfiguration)

-- | This structure includes the metrics and target utilization to use for
-- predictive scaling.
--
-- This is an array, but we currently only support a single metric
-- specification. That is, you can specify a target value and a single
-- metric pair, or a target value and one scaling metric and one load
-- metric.
predictiveScalingConfiguration_metricSpecifications :: Lens.Lens' PredictiveScalingConfiguration [PredictiveScalingMetricSpecification]
predictiveScalingConfiguration_metricSpecifications = Lens.lens (\PredictiveScalingConfiguration' {metricSpecifications} -> metricSpecifications) (\s@PredictiveScalingConfiguration' {} a -> s {metricSpecifications = a} :: PredictiveScalingConfiguration) Prelude.. Lens.coerced

instance Data.FromXML PredictiveScalingConfiguration where
  parseXML x =
    PredictiveScalingConfiguration'
      Prelude.<$> (x Data..@? "MaxCapacityBuffer")
      Prelude.<*> (x Data..@? "MaxCapacityBreachBehavior")
      Prelude.<*> (x Data..@? "Mode")
      Prelude.<*> (x Data..@? "SchedulingBufferTime")
      Prelude.<*> ( x Data..@? "MetricSpecifications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )

instance
  Prelude.Hashable
    PredictiveScalingConfiguration
  where
  hashWithSalt
    _salt
    PredictiveScalingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` maxCapacityBuffer
        `Prelude.hashWithSalt` maxCapacityBreachBehavior
        `Prelude.hashWithSalt` mode
        `Prelude.hashWithSalt` schedulingBufferTime
        `Prelude.hashWithSalt` metricSpecifications

instance
  Prelude.NFData
    PredictiveScalingConfiguration
  where
  rnf PredictiveScalingConfiguration' {..} =
    Prelude.rnf maxCapacityBuffer
      `Prelude.seq` Prelude.rnf maxCapacityBreachBehavior
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf schedulingBufferTime
      `Prelude.seq` Prelude.rnf metricSpecifications

instance Data.ToQuery PredictiveScalingConfiguration where
  toQuery PredictiveScalingConfiguration' {..} =
    Prelude.mconcat
      [ "MaxCapacityBuffer" Data.=: maxCapacityBuffer,
        "MaxCapacityBreachBehavior"
          Data.=: maxCapacityBreachBehavior,
        "Mode" Data.=: mode,
        "SchedulingBufferTime" Data.=: schedulingBufferTime,
        "MetricSpecifications"
          Data.=: Data.toQueryList "member" metricSpecifications
      ]
