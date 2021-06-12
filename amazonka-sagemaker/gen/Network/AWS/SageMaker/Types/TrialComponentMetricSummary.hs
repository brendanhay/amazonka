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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentMetricSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentMetricSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of the metrics of a trial component.
--
-- /See:/ 'newTrialComponentMetricSummary' smart constructor.
data TrialComponentMetricSummary = TrialComponentMetricSummary'
  { -- | The name of the metric.
    metricName :: Core.Maybe Core.Text,
    -- | The minimum value of the metric.
    min :: Core.Maybe Core.Double,
    -- | The standard deviation of the metric.
    stdDev :: Core.Maybe Core.Double,
    -- | The maximum value of the metric.
    max :: Core.Maybe Core.Double,
    -- | When the metric was last updated.
    timeStamp :: Core.Maybe Core.POSIX,
    -- | The number of samples used to generate the metric.
    count :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Core.Maybe Core.Text,
    -- | The average value of the metric.
    avg :: Core.Maybe Core.Double,
    -- | The most recent value of the metric.
    last :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrialComponentMetricSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'trialComponentMetricSummary_metricName' - The name of the metric.
--
-- 'min', 'trialComponentMetricSummary_min' - The minimum value of the metric.
--
-- 'stdDev', 'trialComponentMetricSummary_stdDev' - The standard deviation of the metric.
--
-- 'max', 'trialComponentMetricSummary_max' - The maximum value of the metric.
--
-- 'timeStamp', 'trialComponentMetricSummary_timeStamp' - When the metric was last updated.
--
-- 'count', 'trialComponentMetricSummary_count' - The number of samples used to generate the metric.
--
-- 'sourceArn', 'trialComponentMetricSummary_sourceArn' - The Amazon Resource Name (ARN) of the source.
--
-- 'avg', 'trialComponentMetricSummary_avg' - The average value of the metric.
--
-- 'last', 'trialComponentMetricSummary_last' - The most recent value of the metric.
newTrialComponentMetricSummary ::
  TrialComponentMetricSummary
newTrialComponentMetricSummary =
  TrialComponentMetricSummary'
    { metricName =
        Core.Nothing,
      min = Core.Nothing,
      stdDev = Core.Nothing,
      max = Core.Nothing,
      timeStamp = Core.Nothing,
      count = Core.Nothing,
      sourceArn = Core.Nothing,
      avg = Core.Nothing,
      last = Core.Nothing
    }

-- | The name of the metric.
trialComponentMetricSummary_metricName :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Text)
trialComponentMetricSummary_metricName = Lens.lens (\TrialComponentMetricSummary' {metricName} -> metricName) (\s@TrialComponentMetricSummary' {} a -> s {metricName = a} :: TrialComponentMetricSummary)

-- | The minimum value of the metric.
trialComponentMetricSummary_min :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
trialComponentMetricSummary_min = Lens.lens (\TrialComponentMetricSummary' {min} -> min) (\s@TrialComponentMetricSummary' {} a -> s {min = a} :: TrialComponentMetricSummary)

-- | The standard deviation of the metric.
trialComponentMetricSummary_stdDev :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
trialComponentMetricSummary_stdDev = Lens.lens (\TrialComponentMetricSummary' {stdDev} -> stdDev) (\s@TrialComponentMetricSummary' {} a -> s {stdDev = a} :: TrialComponentMetricSummary)

-- | The maximum value of the metric.
trialComponentMetricSummary_max :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
trialComponentMetricSummary_max = Lens.lens (\TrialComponentMetricSummary' {max} -> max) (\s@TrialComponentMetricSummary' {} a -> s {max = a} :: TrialComponentMetricSummary)

-- | When the metric was last updated.
trialComponentMetricSummary_timeStamp :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.UTCTime)
trialComponentMetricSummary_timeStamp = Lens.lens (\TrialComponentMetricSummary' {timeStamp} -> timeStamp) (\s@TrialComponentMetricSummary' {} a -> s {timeStamp = a} :: TrialComponentMetricSummary) Core.. Lens.mapping Core._Time

-- | The number of samples used to generate the metric.
trialComponentMetricSummary_count :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Int)
trialComponentMetricSummary_count = Lens.lens (\TrialComponentMetricSummary' {count} -> count) (\s@TrialComponentMetricSummary' {} a -> s {count = a} :: TrialComponentMetricSummary)

-- | The Amazon Resource Name (ARN) of the source.
trialComponentMetricSummary_sourceArn :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Text)
trialComponentMetricSummary_sourceArn = Lens.lens (\TrialComponentMetricSummary' {sourceArn} -> sourceArn) (\s@TrialComponentMetricSummary' {} a -> s {sourceArn = a} :: TrialComponentMetricSummary)

-- | The average value of the metric.
trialComponentMetricSummary_avg :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
trialComponentMetricSummary_avg = Lens.lens (\TrialComponentMetricSummary' {avg} -> avg) (\s@TrialComponentMetricSummary' {} a -> s {avg = a} :: TrialComponentMetricSummary)

-- | The most recent value of the metric.
trialComponentMetricSummary_last :: Lens.Lens' TrialComponentMetricSummary (Core.Maybe Core.Double)
trialComponentMetricSummary_last = Lens.lens (\TrialComponentMetricSummary' {last} -> last) (\s@TrialComponentMetricSummary' {} a -> s {last = a} :: TrialComponentMetricSummary)

instance Core.FromJSON TrialComponentMetricSummary where
  parseJSON =
    Core.withObject
      "TrialComponentMetricSummary"
      ( \x ->
          TrialComponentMetricSummary'
            Core.<$> (x Core..:? "MetricName")
            Core.<*> (x Core..:? "Min")
            Core.<*> (x Core..:? "StdDev")
            Core.<*> (x Core..:? "Max")
            Core.<*> (x Core..:? "TimeStamp")
            Core.<*> (x Core..:? "Count")
            Core.<*> (x Core..:? "SourceArn")
            Core.<*> (x Core..:? "Avg")
            Core.<*> (x Core..:? "Last")
      )

instance Core.Hashable TrialComponentMetricSummary

instance Core.NFData TrialComponentMetricSummary
