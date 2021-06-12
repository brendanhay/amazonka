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
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.MetricFilter where

import Network.AWS.CloudWatchLogs.Types.MetricTransformation
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metric filters express how CloudWatch Logs would extract metric
-- observations from ingested log events and transform them into metric
-- data in a CloudWatch metric.
--
-- /See:/ 'newMetricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { -- | The name of the metric filter.
    filterName :: Core.Maybe Core.Text,
    -- | The creation time of the metric filter, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural,
    filterPattern :: Core.Maybe Core.Text,
    -- | The name of the log group.
    logGroupName :: Core.Maybe Core.Text,
    -- | The metric transformations.
    metricTransformations :: Core.Maybe (Core.NonEmpty MetricTransformation)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'filterPattern', 'metricFilter_filterPattern' - Undocumented member.
--
-- 'logGroupName', 'metricFilter_logGroupName' - The name of the log group.
--
-- 'metricTransformations', 'metricFilter_metricTransformations' - The metric transformations.
newMetricFilter ::
  MetricFilter
newMetricFilter =
  MetricFilter'
    { filterName = Core.Nothing,
      creationTime = Core.Nothing,
      filterPattern = Core.Nothing,
      logGroupName = Core.Nothing,
      metricTransformations = Core.Nothing
    }

-- | The name of the metric filter.
metricFilter_filterName :: Lens.Lens' MetricFilter (Core.Maybe Core.Text)
metricFilter_filterName = Lens.lens (\MetricFilter' {filterName} -> filterName) (\s@MetricFilter' {} a -> s {filterName = a} :: MetricFilter)

-- | The creation time of the metric filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
metricFilter_creationTime :: Lens.Lens' MetricFilter (Core.Maybe Core.Natural)
metricFilter_creationTime = Lens.lens (\MetricFilter' {creationTime} -> creationTime) (\s@MetricFilter' {} a -> s {creationTime = a} :: MetricFilter)

-- | Undocumented member.
metricFilter_filterPattern :: Lens.Lens' MetricFilter (Core.Maybe Core.Text)
metricFilter_filterPattern = Lens.lens (\MetricFilter' {filterPattern} -> filterPattern) (\s@MetricFilter' {} a -> s {filterPattern = a} :: MetricFilter)

-- | The name of the log group.
metricFilter_logGroupName :: Lens.Lens' MetricFilter (Core.Maybe Core.Text)
metricFilter_logGroupName = Lens.lens (\MetricFilter' {logGroupName} -> logGroupName) (\s@MetricFilter' {} a -> s {logGroupName = a} :: MetricFilter)

-- | The metric transformations.
metricFilter_metricTransformations :: Lens.Lens' MetricFilter (Core.Maybe (Core.NonEmpty MetricTransformation))
metricFilter_metricTransformations = Lens.lens (\MetricFilter' {metricTransformations} -> metricTransformations) (\s@MetricFilter' {} a -> s {metricTransformations = a} :: MetricFilter) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON MetricFilter where
  parseJSON =
    Core.withObject
      "MetricFilter"
      ( \x ->
          MetricFilter'
            Core.<$> (x Core..:? "filterName")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "filterPattern")
            Core.<*> (x Core..:? "logGroupName")
            Core.<*> (x Core..:? "metricTransformations")
      )

instance Core.Hashable MetricFilter

instance Core.NFData MetricFilter
