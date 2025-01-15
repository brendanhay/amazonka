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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceUtilizationMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceUtilizationMetric where

import Amazonka.ComputeOptimizer.Types.ECSServiceMetricName
import Amazonka.ComputeOptimizer.Types.ECSServiceMetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the utilization metric of an Amazon ECS service.
--
-- To determine the performance difference between your current ECS service
-- and the recommended option, compare the utilization metric data of your
-- service against its projected utilization metric data.
--
-- /See:/ 'newECSServiceUtilizationMetric' smart constructor.
data ECSServiceUtilizationMetric = ECSServiceUtilizationMetric'
  { -- | The name of the utilization metric.
    --
    -- The following utilization metrics are available:
    --
    -- -   @Cpu@ — The amount of CPU units that are used in the service.
    --
    -- -   @Memory@ — The amount of memory that is used in the service.
    name :: Prelude.Maybe ECSServiceMetricName,
    -- | The statistic of the utilization metric.
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
    -- | The value of the utilization metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceUtilizationMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eCSServiceUtilizationMetric_name' - The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Cpu@ — The amount of CPU units that are used in the service.
--
-- -   @Memory@ — The amount of memory that is used in the service.
--
-- 'statistic', 'eCSServiceUtilizationMetric_statistic' - The statistic of the utilization metric.
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
-- 'value', 'eCSServiceUtilizationMetric_value' - The value of the utilization metric.
newECSServiceUtilizationMetric ::
  ECSServiceUtilizationMetric
newECSServiceUtilizationMetric =
  ECSServiceUtilizationMetric'
    { name =
        Prelude.Nothing,
      statistic = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Cpu@ — The amount of CPU units that are used in the service.
--
-- -   @Memory@ — The amount of memory that is used in the service.
eCSServiceUtilizationMetric_name :: Lens.Lens' ECSServiceUtilizationMetric (Prelude.Maybe ECSServiceMetricName)
eCSServiceUtilizationMetric_name = Lens.lens (\ECSServiceUtilizationMetric' {name} -> name) (\s@ECSServiceUtilizationMetric' {} a -> s {name = a} :: ECSServiceUtilizationMetric)

-- | The statistic of the utilization metric.
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
eCSServiceUtilizationMetric_statistic :: Lens.Lens' ECSServiceUtilizationMetric (Prelude.Maybe ECSServiceMetricStatistic)
eCSServiceUtilizationMetric_statistic = Lens.lens (\ECSServiceUtilizationMetric' {statistic} -> statistic) (\s@ECSServiceUtilizationMetric' {} a -> s {statistic = a} :: ECSServiceUtilizationMetric)

-- | The value of the utilization metric.
eCSServiceUtilizationMetric_value :: Lens.Lens' ECSServiceUtilizationMetric (Prelude.Maybe Prelude.Double)
eCSServiceUtilizationMetric_value = Lens.lens (\ECSServiceUtilizationMetric' {value} -> value) (\s@ECSServiceUtilizationMetric' {} a -> s {value = a} :: ECSServiceUtilizationMetric)

instance Data.FromJSON ECSServiceUtilizationMetric where
  parseJSON =
    Data.withObject
      "ECSServiceUtilizationMetric"
      ( \x ->
          ECSServiceUtilizationMetric'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "statistic")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ECSServiceUtilizationMetric where
  hashWithSalt _salt ECSServiceUtilizationMetric' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` value

instance Prelude.NFData ECSServiceUtilizationMetric where
  rnf ECSServiceUtilizationMetric' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf statistic `Prelude.seq`
        Prelude.rnf value
