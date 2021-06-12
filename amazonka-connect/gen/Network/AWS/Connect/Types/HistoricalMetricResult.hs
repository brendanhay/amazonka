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
-- Module      : Network.AWS.Connect.Types.HistoricalMetricResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricResult where

import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.HistoricalMetricData
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the historical metrics retrieved.
--
-- /See:/ 'newHistoricalMetricResult' smart constructor.
data HistoricalMetricResult = HistoricalMetricResult'
  { -- | The set of metrics.
    collections :: Core.Maybe [HistoricalMetricData],
    -- | The dimension for the metrics.
    dimensions :: Core.Maybe Dimensions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HistoricalMetricResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collections', 'historicalMetricResult_collections' - The set of metrics.
--
-- 'dimensions', 'historicalMetricResult_dimensions' - The dimension for the metrics.
newHistoricalMetricResult ::
  HistoricalMetricResult
newHistoricalMetricResult =
  HistoricalMetricResult'
    { collections = Core.Nothing,
      dimensions = Core.Nothing
    }

-- | The set of metrics.
historicalMetricResult_collections :: Lens.Lens' HistoricalMetricResult (Core.Maybe [HistoricalMetricData])
historicalMetricResult_collections = Lens.lens (\HistoricalMetricResult' {collections} -> collections) (\s@HistoricalMetricResult' {} a -> s {collections = a} :: HistoricalMetricResult) Core.. Lens.mapping Lens._Coerce

-- | The dimension for the metrics.
historicalMetricResult_dimensions :: Lens.Lens' HistoricalMetricResult (Core.Maybe Dimensions)
historicalMetricResult_dimensions = Lens.lens (\HistoricalMetricResult' {dimensions} -> dimensions) (\s@HistoricalMetricResult' {} a -> s {dimensions = a} :: HistoricalMetricResult)

instance Core.FromJSON HistoricalMetricResult where
  parseJSON =
    Core.withObject
      "HistoricalMetricResult"
      ( \x ->
          HistoricalMetricResult'
            Core.<$> (x Core..:? "Collections" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Dimensions")
      )

instance Core.Hashable HistoricalMetricResult

instance Core.NFData HistoricalMetricResult
