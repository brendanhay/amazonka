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
-- Module      : Network.AWS.DevOpsGuru.Types.CloudWatchMetricsDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.CloudWatchMetricsDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.CloudWatchMetricsDimension
import Network.AWS.DevOpsGuru.Types.CloudWatchMetricsStat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Amazon CloudWatch metric.
--
-- /See:/ 'newCloudWatchMetricsDetail' smart constructor.
data CloudWatchMetricsDetail = CloudWatchMetricsDetail'
  { -- | The length of time associated with the CloudWatch metric in number of
    -- seconds.
    period :: Prelude.Maybe Prelude.Int,
    -- | The name of the CloudWatch metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the CloudWatch metric. A namespace is a container for
    -- CloudWatch metrics.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The type of statistic associated with the CloudWatch metric. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
    -- in the /Amazon CloudWatch User Guide/.
    stat :: Prelude.Maybe CloudWatchMetricsStat,
    -- | An array of CloudWatch dimensions associated with
    dimensions :: Prelude.Maybe [CloudWatchMetricsDimension],
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
-- 'period', 'cloudWatchMetricsDetail_period' - The length of time associated with the CloudWatch metric in number of
-- seconds.
--
-- 'metricName', 'cloudWatchMetricsDetail_metricName' - The name of the CloudWatch metric.
--
-- 'namespace', 'cloudWatchMetricsDetail_namespace' - The namespace of the CloudWatch metric. A namespace is a container for
-- CloudWatch metrics.
--
-- 'stat', 'cloudWatchMetricsDetail_stat' - The type of statistic associated with the CloudWatch metric. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'dimensions', 'cloudWatchMetricsDetail_dimensions' - An array of CloudWatch dimensions associated with
--
-- 'unit', 'cloudWatchMetricsDetail_unit' - The unit of measure used for the CloudWatch metric. For example,
-- @Bytes@, @Seconds@, @Count@, and @Percent@.
newCloudWatchMetricsDetail ::
  CloudWatchMetricsDetail
newCloudWatchMetricsDetail =
  CloudWatchMetricsDetail'
    { period = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      stat = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The length of time associated with the CloudWatch metric in number of
-- seconds.
cloudWatchMetricsDetail_period :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Int)
cloudWatchMetricsDetail_period = Lens.lens (\CloudWatchMetricsDetail' {period} -> period) (\s@CloudWatchMetricsDetail' {} a -> s {period = a} :: CloudWatchMetricsDetail)

-- | The name of the CloudWatch metric.
cloudWatchMetricsDetail_metricName :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDetail_metricName = Lens.lens (\CloudWatchMetricsDetail' {metricName} -> metricName) (\s@CloudWatchMetricsDetail' {} a -> s {metricName = a} :: CloudWatchMetricsDetail)

-- | The namespace of the CloudWatch metric. A namespace is a container for
-- CloudWatch metrics.
cloudWatchMetricsDetail_namespace :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDetail_namespace = Lens.lens (\CloudWatchMetricsDetail' {namespace} -> namespace) (\s@CloudWatchMetricsDetail' {} a -> s {namespace = a} :: CloudWatchMetricsDetail)

-- | The type of statistic associated with the CloudWatch metric. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#Statistic Statistics>
-- in the /Amazon CloudWatch User Guide/.
cloudWatchMetricsDetail_stat :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe CloudWatchMetricsStat)
cloudWatchMetricsDetail_stat = Lens.lens (\CloudWatchMetricsDetail' {stat} -> stat) (\s@CloudWatchMetricsDetail' {} a -> s {stat = a} :: CloudWatchMetricsDetail)

-- | An array of CloudWatch dimensions associated with
cloudWatchMetricsDetail_dimensions :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe [CloudWatchMetricsDimension])
cloudWatchMetricsDetail_dimensions = Lens.lens (\CloudWatchMetricsDetail' {dimensions} -> dimensions) (\s@CloudWatchMetricsDetail' {} a -> s {dimensions = a} :: CloudWatchMetricsDetail) Prelude.. Lens.mapping Lens.coerced

-- | The unit of measure used for the CloudWatch metric. For example,
-- @Bytes@, @Seconds@, @Count@, and @Percent@.
cloudWatchMetricsDetail_unit :: Lens.Lens' CloudWatchMetricsDetail (Prelude.Maybe Prelude.Text)
cloudWatchMetricsDetail_unit = Lens.lens (\CloudWatchMetricsDetail' {unit} -> unit) (\s@CloudWatchMetricsDetail' {} a -> s {unit = a} :: CloudWatchMetricsDetail)

instance Core.FromJSON CloudWatchMetricsDetail where
  parseJSON =
    Core.withObject
      "CloudWatchMetricsDetail"
      ( \x ->
          CloudWatchMetricsDetail'
            Prelude.<$> (x Core..:? "Period")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "Namespace")
            Prelude.<*> (x Core..:? "Stat")
            Prelude.<*> (x Core..:? "Dimensions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Unit")
      )

instance Prelude.Hashable CloudWatchMetricsDetail

instance Prelude.NFData CloudWatchMetricsDetail
