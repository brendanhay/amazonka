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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceProjectedUtilizationMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceProjectedUtilizationMetric where

import Amazonka.ComputeOptimizer.Types.ECSServiceMetricName
import Amazonka.ComputeOptimizer.Types.ECSServiceMetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the projected utilization metrics of an Amazon ECS service
-- recommendation option.
--
-- To determine the performance difference between your current Amazon ECS
-- service and the recommended option, compare the utilization metric data
-- of your service against its projected utilization metric data.
--
-- /See:/ 'newECSServiceProjectedUtilizationMetric' smart constructor.
data ECSServiceProjectedUtilizationMetric = ECSServiceProjectedUtilizationMetric'
  { -- | The lower bound values for the projected utilization metrics.
    lowerBoundValue :: Prelude.Maybe Prelude.Double,
    -- | The name of the projected utilization metric.
    --
    -- The following utilization metrics are available:
    --
    -- -   @Cpu@ — The percentage of allocated compute units that are currently
    --     in use on the service tasks.
    --
    -- -   @Memory@ — The percentage of memory that\'s currently in use on the
    --     service tasks.
    name :: Prelude.Maybe ECSServiceMetricName,
    -- | The statistic of the projected utilization metric.
    --
    -- The Compute Optimizer API, Command Line Interface (CLI), and SDKs return
    -- utilization metrics using only the @Maximum@ statistic, which is the
    -- highest value observed during the specified period.
    --
    -- The Compute Optimizer console displays graphs for some utilization
    -- metrics using the @Average@ statistic, which is the value of @Sum@ \/
    -- @SampleCount@ during the specified period. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/viewing-recommendations.html Viewing resource recommendations>
    -- in the /Compute Optimizer User Guide/. You can also get averaged
    -- utilization metric data for your resources using Amazon CloudWatch. For
    -- more information, see the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
    statistic :: Prelude.Maybe ECSServiceMetricStatistic,
    -- | The upper bound values for the projected utilization metrics.
    upperBoundValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceProjectedUtilizationMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBoundValue', 'eCSServiceProjectedUtilizationMetric_lowerBoundValue' - The lower bound values for the projected utilization metrics.
--
-- 'name', 'eCSServiceProjectedUtilizationMetric_name' - The name of the projected utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Cpu@ — The percentage of allocated compute units that are currently
--     in use on the service tasks.
--
-- -   @Memory@ — The percentage of memory that\'s currently in use on the
--     service tasks.
--
-- 'statistic', 'eCSServiceProjectedUtilizationMetric_statistic' - The statistic of the projected utilization metric.
--
-- The Compute Optimizer API, Command Line Interface (CLI), and SDKs return
-- utilization metrics using only the @Maximum@ statistic, which is the
-- highest value observed during the specified period.
--
-- The Compute Optimizer console displays graphs for some utilization
-- metrics using the @Average@ statistic, which is the value of @Sum@ \/
-- @SampleCount@ during the specified period. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/viewing-recommendations.html Viewing resource recommendations>
-- in the /Compute Optimizer User Guide/. You can also get averaged
-- utilization metric data for your resources using Amazon CloudWatch. For
-- more information, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
--
-- 'upperBoundValue', 'eCSServiceProjectedUtilizationMetric_upperBoundValue' - The upper bound values for the projected utilization metrics.
newECSServiceProjectedUtilizationMetric ::
  ECSServiceProjectedUtilizationMetric
newECSServiceProjectedUtilizationMetric =
  ECSServiceProjectedUtilizationMetric'
    { lowerBoundValue =
        Prelude.Nothing,
      name = Prelude.Nothing,
      statistic = Prelude.Nothing,
      upperBoundValue = Prelude.Nothing
    }

-- | The lower bound values for the projected utilization metrics.
eCSServiceProjectedUtilizationMetric_lowerBoundValue :: Lens.Lens' ECSServiceProjectedUtilizationMetric (Prelude.Maybe Prelude.Double)
eCSServiceProjectedUtilizationMetric_lowerBoundValue = Lens.lens (\ECSServiceProjectedUtilizationMetric' {lowerBoundValue} -> lowerBoundValue) (\s@ECSServiceProjectedUtilizationMetric' {} a -> s {lowerBoundValue = a} :: ECSServiceProjectedUtilizationMetric)

-- | The name of the projected utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Cpu@ — The percentage of allocated compute units that are currently
--     in use on the service tasks.
--
-- -   @Memory@ — The percentage of memory that\'s currently in use on the
--     service tasks.
eCSServiceProjectedUtilizationMetric_name :: Lens.Lens' ECSServiceProjectedUtilizationMetric (Prelude.Maybe ECSServiceMetricName)
eCSServiceProjectedUtilizationMetric_name = Lens.lens (\ECSServiceProjectedUtilizationMetric' {name} -> name) (\s@ECSServiceProjectedUtilizationMetric' {} a -> s {name = a} :: ECSServiceProjectedUtilizationMetric)

-- | The statistic of the projected utilization metric.
--
-- The Compute Optimizer API, Command Line Interface (CLI), and SDKs return
-- utilization metrics using only the @Maximum@ statistic, which is the
-- highest value observed during the specified period.
--
-- The Compute Optimizer console displays graphs for some utilization
-- metrics using the @Average@ statistic, which is the value of @Sum@ \/
-- @SampleCount@ during the specified period. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/viewing-recommendations.html Viewing resource recommendations>
-- in the /Compute Optimizer User Guide/. You can also get averaged
-- utilization metric data for your resources using Amazon CloudWatch. For
-- more information, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/WhatIsCloudWatch.html Amazon CloudWatch User Guide>.
eCSServiceProjectedUtilizationMetric_statistic :: Lens.Lens' ECSServiceProjectedUtilizationMetric (Prelude.Maybe ECSServiceMetricStatistic)
eCSServiceProjectedUtilizationMetric_statistic = Lens.lens (\ECSServiceProjectedUtilizationMetric' {statistic} -> statistic) (\s@ECSServiceProjectedUtilizationMetric' {} a -> s {statistic = a} :: ECSServiceProjectedUtilizationMetric)

-- | The upper bound values for the projected utilization metrics.
eCSServiceProjectedUtilizationMetric_upperBoundValue :: Lens.Lens' ECSServiceProjectedUtilizationMetric (Prelude.Maybe Prelude.Double)
eCSServiceProjectedUtilizationMetric_upperBoundValue = Lens.lens (\ECSServiceProjectedUtilizationMetric' {upperBoundValue} -> upperBoundValue) (\s@ECSServiceProjectedUtilizationMetric' {} a -> s {upperBoundValue = a} :: ECSServiceProjectedUtilizationMetric)

instance
  Data.FromJSON
    ECSServiceProjectedUtilizationMetric
  where
  parseJSON =
    Data.withObject
      "ECSServiceProjectedUtilizationMetric"
      ( \x ->
          ECSServiceProjectedUtilizationMetric'
            Prelude.<$> (x Data..:? "lowerBoundValue")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "statistic")
            Prelude.<*> (x Data..:? "upperBoundValue")
      )

instance
  Prelude.Hashable
    ECSServiceProjectedUtilizationMetric
  where
  hashWithSalt
    _salt
    ECSServiceProjectedUtilizationMetric' {..} =
      _salt
        `Prelude.hashWithSalt` lowerBoundValue
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` statistic
        `Prelude.hashWithSalt` upperBoundValue

instance
  Prelude.NFData
    ECSServiceProjectedUtilizationMetric
  where
  rnf ECSServiceProjectedUtilizationMetric' {..} =
    Prelude.rnf lowerBoundValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf upperBoundValue
