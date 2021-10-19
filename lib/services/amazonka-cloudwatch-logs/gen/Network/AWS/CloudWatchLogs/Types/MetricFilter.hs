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
import qualified Network.AWS.Prelude as Prelude

-- | Metric filters express how CloudWatch Logs would extract metric
-- observations from ingested log events and transform them into metric
-- data in a CloudWatch metric.
--
-- /See:/ 'newMetricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { -- | The creation time of the metric filter, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | The name of the metric filter.
    filterName :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    filterPattern :: Prelude.Maybe Prelude.Text,
    -- | The metric transformations.
    metricTransformations :: Prelude.Maybe (Prelude.NonEmpty MetricTransformation)
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
-- 'creationTime', 'metricFilter_creationTime' - The creation time of the metric filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'filterName', 'metricFilter_filterName' - The name of the metric filter.
--
-- 'logGroupName', 'metricFilter_logGroupName' - The name of the log group.
--
-- 'filterPattern', 'metricFilter_filterPattern' - Undocumented member.
--
-- 'metricTransformations', 'metricFilter_metricTransformations' - The metric transformations.
newMetricFilter ::
  MetricFilter
newMetricFilter =
  MetricFilter'
    { creationTime = Prelude.Nothing,
      filterName = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      metricTransformations = Prelude.Nothing
    }

-- | The creation time of the metric filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
metricFilter_creationTime :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Natural)
metricFilter_creationTime = Lens.lens (\MetricFilter' {creationTime} -> creationTime) (\s@MetricFilter' {} a -> s {creationTime = a} :: MetricFilter)

-- | The name of the metric filter.
metricFilter_filterName :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Text)
metricFilter_filterName = Lens.lens (\MetricFilter' {filterName} -> filterName) (\s@MetricFilter' {} a -> s {filterName = a} :: MetricFilter)

-- | The name of the log group.
metricFilter_logGroupName :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Text)
metricFilter_logGroupName = Lens.lens (\MetricFilter' {logGroupName} -> logGroupName) (\s@MetricFilter' {} a -> s {logGroupName = a} :: MetricFilter)

-- | Undocumented member.
metricFilter_filterPattern :: Lens.Lens' MetricFilter (Prelude.Maybe Prelude.Text)
metricFilter_filterPattern = Lens.lens (\MetricFilter' {filterPattern} -> filterPattern) (\s@MetricFilter' {} a -> s {filterPattern = a} :: MetricFilter)

-- | The metric transformations.
metricFilter_metricTransformations :: Lens.Lens' MetricFilter (Prelude.Maybe (Prelude.NonEmpty MetricTransformation))
metricFilter_metricTransformations = Lens.lens (\MetricFilter' {metricTransformations} -> metricTransformations) (\s@MetricFilter' {} a -> s {metricTransformations = a} :: MetricFilter) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MetricFilter where
  parseJSON =
    Core.withObject
      "MetricFilter"
      ( \x ->
          MetricFilter'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "filterName")
            Prelude.<*> (x Core..:? "logGroupName")
            Prelude.<*> (x Core..:? "filterPattern")
            Prelude.<*> (x Core..:? "metricTransformations")
      )

instance Prelude.Hashable MetricFilter

instance Prelude.NFData MetricFilter
