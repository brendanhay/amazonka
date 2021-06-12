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
-- Module      : Network.AWS.Connect.Types.CurrentMetricResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricResult where

import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.Dimensions
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a set of real-time metrics.
--
-- /See:/ 'newCurrentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { -- | The set of metrics.
    collections :: Core.Maybe [CurrentMetricData],
    -- | The dimensions for the metrics.
    dimensions :: Core.Maybe Dimensions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CurrentMetricResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collections', 'currentMetricResult_collections' - The set of metrics.
--
-- 'dimensions', 'currentMetricResult_dimensions' - The dimensions for the metrics.
newCurrentMetricResult ::
  CurrentMetricResult
newCurrentMetricResult =
  CurrentMetricResult'
    { collections = Core.Nothing,
      dimensions = Core.Nothing
    }

-- | The set of metrics.
currentMetricResult_collections :: Lens.Lens' CurrentMetricResult (Core.Maybe [CurrentMetricData])
currentMetricResult_collections = Lens.lens (\CurrentMetricResult' {collections} -> collections) (\s@CurrentMetricResult' {} a -> s {collections = a} :: CurrentMetricResult) Core.. Lens.mapping Lens._Coerce

-- | The dimensions for the metrics.
currentMetricResult_dimensions :: Lens.Lens' CurrentMetricResult (Core.Maybe Dimensions)
currentMetricResult_dimensions = Lens.lens (\CurrentMetricResult' {dimensions} -> dimensions) (\s@CurrentMetricResult' {} a -> s {dimensions = a} :: CurrentMetricResult)

instance Core.FromJSON CurrentMetricResult where
  parseJSON =
    Core.withObject
      "CurrentMetricResult"
      ( \x ->
          CurrentMetricResult'
            Core.<$> (x Core..:? "Collections" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Dimensions")
      )

instance Core.Hashable CurrentMetricResult

instance Core.NFData CurrentMetricResult
