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
-- Module      : Amazonka.CloudWatch.Types.MetricStreamStatisticsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricStreamStatisticsConfiguration where

import Amazonka.CloudWatch.Types.MetricStreamStatisticsMetric
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | By default, a metric stream always sends the @MAX@, @MIN@, @SUM@, and
-- @SAMPLECOUNT@ statistics for each metric that is streamed. This
-- structure contains information for one metric that includes additional
-- statistics in the stream. For more information about statistics, see
-- CloudWatch, listed in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
--
-- /See:/ 'newMetricStreamStatisticsConfiguration' smart constructor.
data MetricStreamStatisticsConfiguration = MetricStreamStatisticsConfiguration'
  { -- | An array of metric name and namespace pairs that stream the additional
    -- statistics listed in the value of the @AdditionalStatistics@ parameter.
    -- There can be as many as 100 pairs in the array.
    --
    -- All metrics that match the combination of metric name and namespace will
    -- be streamed with the additional statistics, no matter their dimensions.
    includeMetrics :: [MetricStreamStatisticsMetric],
    -- | The list of additional statistics that are to be streamed for the
    -- metrics listed in the @IncludeMetrics@ array in this structure. This
    -- list can include as many as 20 statistics.
    --
    -- If the @OutputFormat@ for the stream is @opentelemetry0.7@, the only
    -- valid values are @p@/@??@/@ @ percentile statistics such as @p90@, @p99@
    -- and so on.
    --
    -- If the @OutputFormat@ for the stream is @json@, the valid values include
    -- the abbreviations for all of the statistics listed in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
    -- For example, this includes @tm98, @ @wm90@, @PR(:300)@, and so on.
    additionalStatistics :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricStreamStatisticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeMetrics', 'metricStreamStatisticsConfiguration_includeMetrics' - An array of metric name and namespace pairs that stream the additional
-- statistics listed in the value of the @AdditionalStatistics@ parameter.
-- There can be as many as 100 pairs in the array.
--
-- All metrics that match the combination of metric name and namespace will
-- be streamed with the additional statistics, no matter their dimensions.
--
-- 'additionalStatistics', 'metricStreamStatisticsConfiguration_additionalStatistics' - The list of additional statistics that are to be streamed for the
-- metrics listed in the @IncludeMetrics@ array in this structure. This
-- list can include as many as 20 statistics.
--
-- If the @OutputFormat@ for the stream is @opentelemetry0.7@, the only
-- valid values are @p@/@??@/@ @ percentile statistics such as @p90@, @p99@
-- and so on.
--
-- If the @OutputFormat@ for the stream is @json@, the valid values include
-- the abbreviations for all of the statistics listed in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
-- For example, this includes @tm98, @ @wm90@, @PR(:300)@, and so on.
newMetricStreamStatisticsConfiguration ::
  MetricStreamStatisticsConfiguration
newMetricStreamStatisticsConfiguration =
  MetricStreamStatisticsConfiguration'
    { includeMetrics =
        Prelude.mempty,
      additionalStatistics = Prelude.mempty
    }

-- | An array of metric name and namespace pairs that stream the additional
-- statistics listed in the value of the @AdditionalStatistics@ parameter.
-- There can be as many as 100 pairs in the array.
--
-- All metrics that match the combination of metric name and namespace will
-- be streamed with the additional statistics, no matter their dimensions.
metricStreamStatisticsConfiguration_includeMetrics :: Lens.Lens' MetricStreamStatisticsConfiguration [MetricStreamStatisticsMetric]
metricStreamStatisticsConfiguration_includeMetrics = Lens.lens (\MetricStreamStatisticsConfiguration' {includeMetrics} -> includeMetrics) (\s@MetricStreamStatisticsConfiguration' {} a -> s {includeMetrics = a} :: MetricStreamStatisticsConfiguration) Prelude.. Lens.coerced

-- | The list of additional statistics that are to be streamed for the
-- metrics listed in the @IncludeMetrics@ array in this structure. This
-- list can include as many as 20 statistics.
--
-- If the @OutputFormat@ for the stream is @opentelemetry0.7@, the only
-- valid values are @p@/@??@/@ @ percentile statistics such as @p90@, @p99@
-- and so on.
--
-- If the @OutputFormat@ for the stream is @json@, the valid values include
-- the abbreviations for all of the statistics listed in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
-- For example, this includes @tm98, @ @wm90@, @PR(:300)@, and so on.
metricStreamStatisticsConfiguration_additionalStatistics :: Lens.Lens' MetricStreamStatisticsConfiguration [Prelude.Text]
metricStreamStatisticsConfiguration_additionalStatistics = Lens.lens (\MetricStreamStatisticsConfiguration' {additionalStatistics} -> additionalStatistics) (\s@MetricStreamStatisticsConfiguration' {} a -> s {additionalStatistics = a} :: MetricStreamStatisticsConfiguration) Prelude.. Lens.coerced

instance
  Data.FromXML
    MetricStreamStatisticsConfiguration
  where
  parseXML x =
    MetricStreamStatisticsConfiguration'
      Prelude.<$> ( x
                      Data..@? "IncludeMetrics"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )
      Prelude.<*> ( x
                      Data..@? "AdditionalStatistics"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )

instance
  Prelude.Hashable
    MetricStreamStatisticsConfiguration
  where
  hashWithSalt
    _salt
    MetricStreamStatisticsConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` includeMetrics
        `Prelude.hashWithSalt` additionalStatistics

instance
  Prelude.NFData
    MetricStreamStatisticsConfiguration
  where
  rnf MetricStreamStatisticsConfiguration' {..} =
    Prelude.rnf includeMetrics
      `Prelude.seq` Prelude.rnf additionalStatistics

instance
  Data.ToQuery
    MetricStreamStatisticsConfiguration
  where
  toQuery MetricStreamStatisticsConfiguration' {..} =
    Prelude.mconcat
      [ "IncludeMetrics"
          Data.=: Data.toQueryList "member" includeMetrics,
        "AdditionalStatistics"
          Data.=: Data.toQueryList "member" additionalStatistics
      ]
