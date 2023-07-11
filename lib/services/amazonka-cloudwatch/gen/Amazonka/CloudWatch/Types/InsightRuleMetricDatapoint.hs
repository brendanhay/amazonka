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
-- Module      : Amazonka.CloudWatch.Types.InsightRuleMetricDatapoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.InsightRuleMetricDatapoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | One data point from the metric time series returned in a Contributor
-- Insights rule report.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetInsightRuleReport.html GetInsightRuleReport>.
--
-- /See:/ 'newInsightRuleMetricDatapoint' smart constructor.
data InsightRuleMetricDatapoint = InsightRuleMetricDatapoint'
  { -- | The average value from all contributors during the time period
    -- represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    average :: Prelude.Maybe Prelude.Double,
    -- | The maximum value provided by one contributor during this timestamp.
    -- Each timestamp is evaluated separately, so the identity of the max
    -- contributor could be different for each timestamp.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    maxContributorValue :: Prelude.Maybe Prelude.Double,
    -- | The maximum value from a single occurence from a single contributor
    -- during the time period represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | The minimum value from a single contributor during the time period
    -- represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | The number of occurrences that matched the rule during this data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    sampleCount :: Prelude.Maybe Prelude.Double,
    -- | The sum of the values from all contributors during the time period
    -- represented by that data point.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The number of unique contributors who published data during this
    -- timestamp.
    --
    -- This statistic is returned only if you included it in the @Metrics@
    -- array in your request.
    uniqueContributors :: Prelude.Maybe Prelude.Double,
    -- | The timestamp of the data point.
    timestamp :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightRuleMetricDatapoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'average', 'insightRuleMetricDatapoint_average' - The average value from all contributors during the time period
-- represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'maxContributorValue', 'insightRuleMetricDatapoint_maxContributorValue' - The maximum value provided by one contributor during this timestamp.
-- Each timestamp is evaluated separately, so the identity of the max
-- contributor could be different for each timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'maximum', 'insightRuleMetricDatapoint_maximum' - The maximum value from a single occurence from a single contributor
-- during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'minimum', 'insightRuleMetricDatapoint_minimum' - The minimum value from a single contributor during the time period
-- represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'sampleCount', 'insightRuleMetricDatapoint_sampleCount' - The number of occurrences that matched the rule during this data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'sum', 'insightRuleMetricDatapoint_sum' - The sum of the values from all contributors during the time period
-- represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'uniqueContributors', 'insightRuleMetricDatapoint_uniqueContributors' - The number of unique contributors who published data during this
-- timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
--
-- 'timestamp', 'insightRuleMetricDatapoint_timestamp' - The timestamp of the data point.
newInsightRuleMetricDatapoint ::
  -- | 'timestamp'
  Prelude.UTCTime ->
  InsightRuleMetricDatapoint
newInsightRuleMetricDatapoint pTimestamp_ =
  InsightRuleMetricDatapoint'
    { average =
        Prelude.Nothing,
      maxContributorValue = Prelude.Nothing,
      maximum = Prelude.Nothing,
      minimum = Prelude.Nothing,
      sampleCount = Prelude.Nothing,
      sum = Prelude.Nothing,
      uniqueContributors = Prelude.Nothing,
      timestamp = Data._Time Lens.# pTimestamp_
    }

-- | The average value from all contributors during the time period
-- represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_average :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_average = Lens.lens (\InsightRuleMetricDatapoint' {average} -> average) (\s@InsightRuleMetricDatapoint' {} a -> s {average = a} :: InsightRuleMetricDatapoint)

-- | The maximum value provided by one contributor during this timestamp.
-- Each timestamp is evaluated separately, so the identity of the max
-- contributor could be different for each timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_maxContributorValue :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_maxContributorValue = Lens.lens (\InsightRuleMetricDatapoint' {maxContributorValue} -> maxContributorValue) (\s@InsightRuleMetricDatapoint' {} a -> s {maxContributorValue = a} :: InsightRuleMetricDatapoint)

-- | The maximum value from a single occurence from a single contributor
-- during the time period represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_maximum :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_maximum = Lens.lens (\InsightRuleMetricDatapoint' {maximum} -> maximum) (\s@InsightRuleMetricDatapoint' {} a -> s {maximum = a} :: InsightRuleMetricDatapoint)

-- | The minimum value from a single contributor during the time period
-- represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_minimum :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_minimum = Lens.lens (\InsightRuleMetricDatapoint' {minimum} -> minimum) (\s@InsightRuleMetricDatapoint' {} a -> s {minimum = a} :: InsightRuleMetricDatapoint)

-- | The number of occurrences that matched the rule during this data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_sampleCount :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_sampleCount = Lens.lens (\InsightRuleMetricDatapoint' {sampleCount} -> sampleCount) (\s@InsightRuleMetricDatapoint' {} a -> s {sampleCount = a} :: InsightRuleMetricDatapoint)

-- | The sum of the values from all contributors during the time period
-- represented by that data point.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_sum :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_sum = Lens.lens (\InsightRuleMetricDatapoint' {sum} -> sum) (\s@InsightRuleMetricDatapoint' {} a -> s {sum = a} :: InsightRuleMetricDatapoint)

-- | The number of unique contributors who published data during this
-- timestamp.
--
-- This statistic is returned only if you included it in the @Metrics@
-- array in your request.
insightRuleMetricDatapoint_uniqueContributors :: Lens.Lens' InsightRuleMetricDatapoint (Prelude.Maybe Prelude.Double)
insightRuleMetricDatapoint_uniqueContributors = Lens.lens (\InsightRuleMetricDatapoint' {uniqueContributors} -> uniqueContributors) (\s@InsightRuleMetricDatapoint' {} a -> s {uniqueContributors = a} :: InsightRuleMetricDatapoint)

-- | The timestamp of the data point.
insightRuleMetricDatapoint_timestamp :: Lens.Lens' InsightRuleMetricDatapoint Prelude.UTCTime
insightRuleMetricDatapoint_timestamp = Lens.lens (\InsightRuleMetricDatapoint' {timestamp} -> timestamp) (\s@InsightRuleMetricDatapoint' {} a -> s {timestamp = a} :: InsightRuleMetricDatapoint) Prelude.. Data._Time

instance Data.FromXML InsightRuleMetricDatapoint where
  parseXML x =
    InsightRuleMetricDatapoint'
      Prelude.<$> (x Data..@? "Average")
      Prelude.<*> (x Data..@? "MaxContributorValue")
      Prelude.<*> (x Data..@? "Maximum")
      Prelude.<*> (x Data..@? "Minimum")
      Prelude.<*> (x Data..@? "SampleCount")
      Prelude.<*> (x Data..@? "Sum")
      Prelude.<*> (x Data..@? "UniqueContributors")
      Prelude.<*> (x Data..@ "Timestamp")

instance Prelude.Hashable InsightRuleMetricDatapoint where
  hashWithSalt _salt InsightRuleMetricDatapoint' {..} =
    _salt
      `Prelude.hashWithSalt` average
      `Prelude.hashWithSalt` maxContributorValue
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` sampleCount
      `Prelude.hashWithSalt` sum
      `Prelude.hashWithSalt` uniqueContributors
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData InsightRuleMetricDatapoint where
  rnf InsightRuleMetricDatapoint' {..} =
    Prelude.rnf average
      `Prelude.seq` Prelude.rnf maxContributorValue
      `Prelude.seq` Prelude.rnf maximum
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf sampleCount
      `Prelude.seq` Prelude.rnf sum
      `Prelude.seq` Prelude.rnf uniqueContributors
      `Prelude.seq` Prelude.rnf timestamp
