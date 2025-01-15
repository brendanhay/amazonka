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
-- Module      : Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric where

import Amazonka.ComputeOptimizer.Types.EBSMetricName
import Amazonka.ComputeOptimizer.Types.MetricStatistic
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a utilization metric of an Amazon Elastic Block Store (Amazon
-- EBS) volume.
--
-- Compare the utilization metric data of your resource against its
-- projected utilization metric data to determine the performance
-- difference between your current resource and the recommended option.
--
-- /See:/ 'newEBSUtilizationMetric' smart constructor.
data EBSUtilizationMetric = EBSUtilizationMetric'
  { -- | The name of the utilization metric.
    --
    -- The following utilization metrics are available:
    --
    -- -   @VolumeReadOpsPerSecond@ - The completed read operations per second
    --     from the volume in a specified period of time.
    --
    --     Unit: Count
    --
    -- -   @VolumeWriteOpsPerSecond@ - The completed write operations per
    --     second to the volume in a specified period of time.
    --
    --     Unit: Count
    --
    -- -   @VolumeReadBytesPerSecond@ - The bytes read per second from the
    --     volume in a specified period of time.
    --
    --     Unit: Bytes
    --
    -- -   @VolumeWriteBytesPerSecond@ - The bytes written to the volume in a
    --     specified period of time.
    --
    --     Unit: Bytes
    name :: Prelude.Maybe EBSMetricName,
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
    statistic :: Prelude.Maybe MetricStatistic,
    -- | The value of the utilization metric.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSUtilizationMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eBSUtilizationMetric_name' - The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @VolumeReadOpsPerSecond@ - The completed read operations per second
--     from the volume in a specified period of time.
--
--     Unit: Count
--
-- -   @VolumeWriteOpsPerSecond@ - The completed write operations per
--     second to the volume in a specified period of time.
--
--     Unit: Count
--
-- -   @VolumeReadBytesPerSecond@ - The bytes read per second from the
--     volume in a specified period of time.
--
--     Unit: Bytes
--
-- -   @VolumeWriteBytesPerSecond@ - The bytes written to the volume in a
--     specified period of time.
--
--     Unit: Bytes
--
-- 'statistic', 'eBSUtilizationMetric_statistic' - The statistic of the utilization metric.
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
-- 'value', 'eBSUtilizationMetric_value' - The value of the utilization metric.
newEBSUtilizationMetric ::
  EBSUtilizationMetric
newEBSUtilizationMetric =
  EBSUtilizationMetric'
    { name = Prelude.Nothing,
      statistic = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the utilization metric.
--
-- The following utilization metrics are available:
--
-- -   @VolumeReadOpsPerSecond@ - The completed read operations per second
--     from the volume in a specified period of time.
--
--     Unit: Count
--
-- -   @VolumeWriteOpsPerSecond@ - The completed write operations per
--     second to the volume in a specified period of time.
--
--     Unit: Count
--
-- -   @VolumeReadBytesPerSecond@ - The bytes read per second from the
--     volume in a specified period of time.
--
--     Unit: Bytes
--
-- -   @VolumeWriteBytesPerSecond@ - The bytes written to the volume in a
--     specified period of time.
--
--     Unit: Bytes
eBSUtilizationMetric_name :: Lens.Lens' EBSUtilizationMetric (Prelude.Maybe EBSMetricName)
eBSUtilizationMetric_name = Lens.lens (\EBSUtilizationMetric' {name} -> name) (\s@EBSUtilizationMetric' {} a -> s {name = a} :: EBSUtilizationMetric)

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
eBSUtilizationMetric_statistic :: Lens.Lens' EBSUtilizationMetric (Prelude.Maybe MetricStatistic)
eBSUtilizationMetric_statistic = Lens.lens (\EBSUtilizationMetric' {statistic} -> statistic) (\s@EBSUtilizationMetric' {} a -> s {statistic = a} :: EBSUtilizationMetric)

-- | The value of the utilization metric.
eBSUtilizationMetric_value :: Lens.Lens' EBSUtilizationMetric (Prelude.Maybe Prelude.Double)
eBSUtilizationMetric_value = Lens.lens (\EBSUtilizationMetric' {value} -> value) (\s@EBSUtilizationMetric' {} a -> s {value = a} :: EBSUtilizationMetric)

instance Data.FromJSON EBSUtilizationMetric where
  parseJSON =
    Data.withObject
      "EBSUtilizationMetric"
      ( \x ->
          EBSUtilizationMetric'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "statistic")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable EBSUtilizationMetric where
  hashWithSalt _salt EBSUtilizationMetric' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` value

instance Prelude.NFData EBSUtilizationMetric where
  rnf EBSUtilizationMetric' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf statistic `Prelude.seq`
        Prelude.rnf value
