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
-- Module      : Amazonka.DevOpsGuru.Types.CloudWatchMetricsDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudWatchMetricsDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsDataSummary
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsDimension
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsStat
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon CloudWatch metric.
--
-- /See:/ 'newCloudWatchMetricsDetail' smart constructor.
data CloudWatchMetricsDetail = CloudWatchMetricsDetail'
  { -- | An array of CloudWatch dimensions associated with
    dimensions :: Prelude.Maybe [CloudWatchMetricsDimension],
    -- | This object returns anomaly metric data.
    metricDataSummary :: Prelude.Maybe CloudWatchMetricsDataSummary,
    -- | The name of the CloudWatch metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the CloudWatch metric. A namespace is a container for
    -- CloudWatch metrics.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The length of time associated with the CloudWatch metric in number of
    -- seconds.
    period :: Prelude.Maybe Prelude.Int,
    -- | The type of statistic associated with the CloudWatch metric. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
    -- in the /Amazon CloudWatch User Guide/.
    stat :: Prelude.Maybe CloudWatchMetricsStat,
    -- | The unit of measure used for the CloudWatch metric. For example,
    -- @Bytes@, @Seconds@, @Count@, and @Percent@.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchMetricsDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'cloudWatchMetricsDetail_dimensions' - An array of CloudWatch dimensions associated with
--
-- 'metricDataSummary', 'cloudWatchMetricsDetail_metricDataSummary' - This object returns anomaly metric data.
--
-- 'metricName', 'cloudWatchMetricsDetail_metricName' - The name of the CloudWatch metric.
--
-- 'namespace', 'cloudWatchMetricsDetail_namespace' - The namespace of the CloudWatch metric. A namespace is a container for
-- CloudWatch metrics.
--
-- 'period', 'cloudWatchMetricsDetail_period' - The length of time associated with the CloudWatch metric in number of
-- seconds.
--
-- 'stat', 'cloudWatchMetricsDetail_stat' - The type of statistic associated with the CloudWatch metric. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'unit', 'cloudWatchMetricsDetail_unit' - The unit of measure used for the CloudWatch metric. For example,
-- @Bytes@, @Seconds@, @Count@, and @Percent@.
newCloudWatchMetricsDetail ::
  CloudWatchMetricsDetail
newCloudWatchMetricsDetail =
  CloudWatchMetricsDetail'
    { dimensions =
        Prelude.Nothing,
      metricDataSummary = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      period = Prelude.Nothing,
      stat = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | An array of CloudWatch dimensions associated with
cloudWatchMetricsDetail_dimensions :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe [CloudWatchMetricsDimension])
cloudWatchMetricsDetail_dimensions = Lens.lens (\CloudWatchMetricsDetail' {dimensions} -> dimensions) (\s@CloudWatchMetricsDetail' {} a -> s {dimensions = a} :: CloudWatchMetricsDetail) Prelude.. Lens.mapping Lens.coerced

-- | This object returns anomaly metric data.
cloudWatchMetricsDetail_metricDataSummary :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe CloudWatchMetricsDataSummary)
cloudWatchMetricsDetail_metricDataSummary = Lens.lens (\CloudWatchMetricsDetail' {metricDataSummary} -> metricDataSummary) (\s@CloudWatchMetricsDetail' {} a -> s {metricDataSummary = a} :: CloudWatchMetricsDetail)

-- | The name of the CloudWatch metric.
cloudWatchMetricsDetail_metricName :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDetail_metricName = Lens.lens (\CloudWatchMetricsDetail' {metricName} -> metricName) (\s@CloudWatchMetricsDetail' {} a -> s {metricName = a} :: CloudWatchMetricsDetail)

-- | The namespace of the CloudWatch metric. A namespace is a container for
-- CloudWatch metrics.
cloudWatchMetricsDetail_namespace :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDetail_namespace = Lens.lens (\CloudWatchMetricsDetail' {namespace} -> namespace) (\s@CloudWatchMetricsDetail' {} a -> s {namespace = a} :: CloudWatchMetricsDetail)

-- | The length of time associated with the CloudWatch metric in number of
-- seconds.
cloudWatchMetricsDetail_period :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Int)
cloudWatchMetricsDetail_period = Lens.lens (\CloudWatchMetricsDetail' {period} -> period) (\s@CloudWatchMetricsDetail' {} a -> s {period = a} :: CloudWatchMetricsDetail)

-- | The type of statistic associated with the CloudWatch metric. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
cloudWatchMetricsDetail_stat :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe CloudWatchMetricsStat)
cloudWatchMetricsDetail_stat = Lens.lens (\CloudWatchMetricsDetail' {stat} -> stat) (\s@CloudWatchMetricsDetail' {} a -> s {stat = a} :: CloudWatchMetricsDetail)

-- | The unit of measure used for the CloudWatch metric. For example,
-- @Bytes@, @Seconds@, @Count@, and @Percent@.
cloudWatchMetricsDetail_unit :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDetail_unit = Lens.lens (\CloudWatchMetricsDetail' {unit} -> unit) (\s@CloudWatchMetricsDetail' {} a -> s {unit = a} :: CloudWatchMetricsDetail)

instance Data.FromJSON CloudWatchMetricsDetail where
  parseJSON =
    Data.withObject
      "CloudWatchMetricsDetail"
      ( \x ->
          CloudWatchMetricsDetail'
            Prelude.<$> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MetricDataSummary")
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Namespace")
            Prelude.<*> (x Data..:? "Period")
            Prelude.<*> (x Data..:? "Stat")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable CloudWatchMetricsDetail where
  hashWithSalt _salt CloudWatchMetricsDetail' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricDataSummary
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` stat
      `Prelude.hashWithSalt` unit

instance Prelude.NFData CloudWatchMetricsDetail where
  rnf CloudWatchMetricsDetail' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricDataSummary
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf stat
      `Prelude.seq` Prelude.rnf unit
