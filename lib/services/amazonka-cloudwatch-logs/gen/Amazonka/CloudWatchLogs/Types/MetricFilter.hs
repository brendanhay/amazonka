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
-- Module      : Amazonka.CloudWatchLogs.Types.MetricFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.MetricFilter where

import Amazonka.CloudWatchLogs.Types.MetricTransformation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Metric filters express how CloudWatch Logs would extract metric
-- observations from ingested log events and transform them into metric
-- data in a CloudWatch metric.
--
-- /See:/ 'newMetricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { -- | The name of the metric filter.
    filterName :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the metric filter, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | The metric transformations.
    metricTransformations :: Prelude.Maybe (Prelude.NonEmpty MetricTransformation),
    filterPattern :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterName', 'metricFilter_filterName' - The name of the metric filter.
--
-- 'creationTime', 'metricFilter_creationTime' - The creation time of the metric filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'metricTransformations', 'metricFilter_metricTransformations' - The metric transformations.
--
-- 'filterPattern', 'metricFilter_filterPattern' - Undocumented member.
--
-- 'logGroupName', 'metricFilter_logGroupName' - The name of the log group.
newMetricFilter ::
  MetricFilter
newMetricFilter =
  MetricFilter'
    { filterName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      metricTransformations = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      logGroupName = Prelude.Nothing
    }

-- | The name of the metric filter.
metricFilter_filterName :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Text)
metricFilter_filterName = Lens.lens (\MetricFilter' {filterName} -> filterName) (\s@MetricFilter' {} a -> s {filterName = a} :: MetricFilter)

-- | The creation time of the metric filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
metricFilter_creationTime :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Natural)
metricFilter_creationTime = Lens.lens (\MetricFilter' {creationTime} -> creationTime) (\s@MetricFilter' {} a -> s {creationTime = a} :: MetricFilter)

-- | The metric transformations.
metricFilter_metricTransformations :: Lens.Lens' MetricFilter (Prelude.Maybe (Prelude.NonEmpty MetricTransformation))
metricFilter_metricTransformations = Lens.lens (\MetricFilter' {metricTransformations} -> metricTransformations) (\s@MetricFilter' {} a -> s {metricTransformations = a} :: MetricFilter) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
metricFilter_filterPattern :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Text)
metricFilter_filterPattern = Lens.lens (\MetricFilter' {filterPattern} -> filterPattern) (\s@MetricFilter' {} a -> s {filterPattern = a} :: MetricFilter)

-- | The name of the log group.
metricFilter_logGroupName :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Text)
metricFilter_logGroupName = Lens.lens (\MetricFilter' {logGroupName} -> logGroupName) (\s@MetricFilter' {} a -> s {logGroupName = a} :: MetricFilter)

instance Core.FromJSON MetricFilter where
  parseJSON =
    Core.withObject
      "MetricFilter"
      ( \x ->
          MetricFilter'
            Prelude.<$> (x Core..:? "filterName")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "metricTransformations")
            Prelude.<*> (x Core..:? "filterPattern")
            Prelude.<*> (x Core..:? "logGroupName")
      )

instance Prelude.Hashable MetricFilter where
  hashWithSalt _salt MetricFilter' {..} =
    _salt `Prelude.hashWithSalt` filterName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` metricTransformations
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData MetricFilter where
  rnf MetricFilter' {..} =
    Prelude.rnf filterName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf metricTransformations
      `Prelude.seq` Prelude.rnf filterPattern
      `Prelude.seq` Prelude.rnf logGroupName
