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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric where

import Amazonka.ComputeOptimizer.Types.LambdaFunctionMetricName
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a utilization metric of an Lambda function.
--
-- /See:/ 'newLambdaFunctionUtilizationMetric' smart constructor.
data LambdaFunctionUtilizationMetric = LambdaFunctionUtilizationMetric'
  { -- | The name of the utilization metric.
    --
    -- The following utilization metrics are available:
    --
    -- -   @Duration@ - The amount of time that your function code spends
    --     processing an event.
    --
    -- -   @Memory@ - The amount of memory used per invocation.
    name :: Prelude.Maybe LambdaFunctionMetricName,
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
    statistic :: Prelude.Maybe LambdaFunctionMetricStatistic,
    -- | The value of the utilization metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionUtilizationMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'lambdaFunctionUtilizationMetric_name' - The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Duration@ - The amount of time that your function code spends
--     processing an event.
--
-- -   @Memory@ - The amount of memory used per invocation.
--
-- 'statistic', 'lambdaFunctionUtilizationMetric_statistic' - The statistic of the utilization metric.
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
-- 'value', 'lambdaFunctionUtilizationMetric_value' - The value of the utilization metric.
newLambdaFunctionUtilizationMetric ::
  LambdaFunctionUtilizationMetric
newLambdaFunctionUtilizationMetric =
  LambdaFunctionUtilizationMetric'
    { name =
        Prelude.Nothing,
      statistic = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @Duration@ - The amount of time that your function code spends
--     processing an event.
--
-- -   @Memory@ - The amount of memory used per invocation.
lambdaFunctionUtilizationMetric_name :: Lens.Lens' LambdaFunctionUtilizationMetric (Prelude.Maybe LambdaFunctionMetricName)
lambdaFunctionUtilizationMetric_name = Lens.lens (\LambdaFunctionUtilizationMetric' {name} -> name) (\s@LambdaFunctionUtilizationMetric' {} a -> s {name = a} :: LambdaFunctionUtilizationMetric)

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
lambdaFunctionUtilizationMetric_statistic :: Lens.Lens' LambdaFunctionUtilizationMetric (Prelude.Maybe LambdaFunctionMetricStatistic)
lambdaFunctionUtilizationMetric_statistic = Lens.lens (\LambdaFunctionUtilizationMetric' {statistic} -> statistic) (\s@LambdaFunctionUtilizationMetric' {} a -> s {statistic = a} :: LambdaFunctionUtilizationMetric)

-- | The value of the utilization metric.
lambdaFunctionUtilizationMetric_value :: Lens.Lens' LambdaFunctionUtilizationMetric (Prelude.Maybe Prelude.Double)
lambdaFunctionUtilizationMetric_value = Lens.lens (\LambdaFunctionUtilizationMetric' {value} -> value) (\s@LambdaFunctionUtilizationMetric' {} a -> s {value = a} :: LambdaFunctionUtilizationMetric)

instance
  Data.FromJSON
    LambdaFunctionUtilizationMetric
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionUtilizationMetric"
      ( \x ->
          LambdaFunctionUtilizationMetric'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "statistic")
            Prelude.<*> (x Data..:? "value")
      )

instance
  Prelude.Hashable
    LambdaFunctionUtilizationMetric
  where
  hashWithSalt
    _salt
    LambdaFunctionUtilizationMetric' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` statistic
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    LambdaFunctionUtilizationMetric
  where
  rnf LambdaFunctionUtilizationMetric' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf value
