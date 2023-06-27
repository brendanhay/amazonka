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
-- Module      : Amazonka.SageMaker.Types.TrialComponentMetricSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentMetricSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the metrics of a trial component.
--
-- /See:/ 'newTrialComponentMetricSummary' smart constructor.
data TrialComponentMetricSummary = TrialComponentMetricSummary'
  { -- | The average value of the metric.
    avg :: Prelude.Maybe Prelude.Double,
    -- | The number of samples used to generate the metric.
    count :: Prelude.Maybe Prelude.Int,
    -- | The most recent value of the metric.
    last :: Prelude.Maybe Prelude.Double,
    -- | The maximum value of the metric.
    max :: Prelude.Maybe Prelude.Double,
    -- | The name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The minimum value of the metric.
    min :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The standard deviation of the metric.
    stdDev :: Prelude.Maybe Prelude.Double,
    -- | When the metric was last updated.
    timeStamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentMetricSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'avg', 'trialComponentMetricSummary_avg' - The average value of the metric.
--
-- 'count', 'trialComponentMetricSummary_count' - The number of samples used to generate the metric.
--
-- 'last', 'trialComponentMetricSummary_last' - The most recent value of the metric.
--
-- 'max', 'trialComponentMetricSummary_max' - The maximum value of the metric.
--
-- 'metricName', 'trialComponentMetricSummary_metricName' - The name of the metric.
--
-- 'min', 'trialComponentMetricSummary_min' - The minimum value of the metric.
--
-- 'sourceArn', 'trialComponentMetricSummary_sourceArn' - The Amazon Resource Name (ARN) of the source.
--
-- 'stdDev', 'trialComponentMetricSummary_stdDev' - The standard deviation of the metric.
--
-- 'timeStamp', 'trialComponentMetricSummary_timeStamp' - When the metric was last updated.
newTrialComponentMetricSummary ::
  TrialComponentMetricSummary
newTrialComponentMetricSummary =
  TrialComponentMetricSummary'
    { avg = Prelude.Nothing,
      count = Prelude.Nothing,
      last = Prelude.Nothing,
      max = Prelude.Nothing,
      metricName = Prelude.Nothing,
      min = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      stdDev = Prelude.Nothing,
      timeStamp = Prelude.Nothing
    }

-- | The average value of the metric.
trialComponentMetricSummary_avg :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_avg = Lens.lens (\TrialComponentMetricSummary' {avg} -> avg) (\s@TrialComponentMetricSummary' {} a -> s {avg = a} :: TrialComponentMetricSummary)

-- | The number of samples used to generate the metric.
trialComponentMetricSummary_count :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Int)
trialComponentMetricSummary_count = Lens.lens (\TrialComponentMetricSummary' {count} -> count) (\s@TrialComponentMetricSummary' {} a -> s {count = a} :: TrialComponentMetricSummary)

-- | The most recent value of the metric.
trialComponentMetricSummary_last :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_last = Lens.lens (\TrialComponentMetricSummary' {last} -> last) (\s@TrialComponentMetricSummary' {} a -> s {last = a} :: TrialComponentMetricSummary)

-- | The maximum value of the metric.
trialComponentMetricSummary_max :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_max = Lens.lens (\TrialComponentMetricSummary' {max} -> max) (\s@TrialComponentMetricSummary' {} a -> s {max = a} :: TrialComponentMetricSummary)

-- | The name of the metric.
trialComponentMetricSummary_metricName :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Text)
trialComponentMetricSummary_metricName = Lens.lens (\TrialComponentMetricSummary' {metricName} -> metricName) (\s@TrialComponentMetricSummary' {} a -> s {metricName = a} :: TrialComponentMetricSummary)

-- | The minimum value of the metric.
trialComponentMetricSummary_min :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_min = Lens.lens (\TrialComponentMetricSummary' {min} -> min) (\s@TrialComponentMetricSummary' {} a -> s {min = a} :: TrialComponentMetricSummary)

-- | The Amazon Resource Name (ARN) of the source.
trialComponentMetricSummary_sourceArn :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Text)
trialComponentMetricSummary_sourceArn = Lens.lens (\TrialComponentMetricSummary' {sourceArn} -> sourceArn) (\s@TrialComponentMetricSummary' {} a -> s {sourceArn = a} :: TrialComponentMetricSummary)

-- | The standard deviation of the metric.
trialComponentMetricSummary_stdDev :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_stdDev = Lens.lens (\TrialComponentMetricSummary' {stdDev} -> stdDev) (\s@TrialComponentMetricSummary' {} a -> s {stdDev = a} :: TrialComponentMetricSummary)

-- | When the metric was last updated.
trialComponentMetricSummary_timeStamp :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentMetricSummary_timeStamp = Lens.lens (\TrialComponentMetricSummary' {timeStamp} -> timeStamp) (\s@TrialComponentMetricSummary' {} a -> s {timeStamp = a} :: TrialComponentMetricSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TrialComponentMetricSummary where
  parseJSON =
    Data.withObject
      "TrialComponentMetricSummary"
      ( \x ->
          TrialComponentMetricSummary'
            Prelude.<$> (x Data..:? "Avg")
            Prelude.<*> (x Data..:? "Count")
            Prelude.<*> (x Data..:? "Last")
            Prelude.<*> (x Data..:? "Max")
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "Min")
            Prelude.<*> (x Data..:? "SourceArn")
            Prelude.<*> (x Data..:? "StdDev")
            Prelude.<*> (x Data..:? "TimeStamp")
      )

instance Prelude.Hashable TrialComponentMetricSummary where
  hashWithSalt _salt TrialComponentMetricSummary' {..} =
    _salt
      `Prelude.hashWithSalt` avg
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` last
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` stdDev
      `Prelude.hashWithSalt` timeStamp

instance Prelude.NFData TrialComponentMetricSummary where
  rnf TrialComponentMetricSummary' {..} =
    Prelude.rnf avg
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf last
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf stdDev
      `Prelude.seq` Prelude.rnf timeStamp
