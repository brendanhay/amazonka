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
-- Module      : Network.AWS.CostExplorer.Types.AnomalyDateInterval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyDateInterval where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The time period for an anomaly.
--
-- /See:/ 'newAnomalyDateInterval' smart constructor.
data AnomalyDateInterval = AnomalyDateInterval'
  { -- | The last date an anomaly was observed.
    endDate :: Core.Maybe Core.Text,
    -- | The first date an anomaly was observed.
    startDate :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnomalyDateInterval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDate', 'anomalyDateInterval_endDate' - The last date an anomaly was observed.
--
-- 'startDate', 'anomalyDateInterval_startDate' - The first date an anomaly was observed.
newAnomalyDateInterval ::
  -- | 'startDate'
  Core.Text ->
  AnomalyDateInterval
newAnomalyDateInterval pStartDate_ =
  AnomalyDateInterval'
    { endDate = Core.Nothing,
      startDate = pStartDate_
    }

-- | The last date an anomaly was observed.
anomalyDateInterval_endDate :: Lens.Lens' AnomalyDateInterval (Core.Maybe Core.Text)
anomalyDateInterval_endDate = Lens.lens (\AnomalyDateInterval' {endDate} -> endDate) (\s@AnomalyDateInterval' {} a -> s {endDate = a} :: AnomalyDateInterval)

-- | The first date an anomaly was observed.
anomalyDateInterval_startDate :: Lens.Lens' AnomalyDateInterval Core.Text
anomalyDateInterval_startDate = Lens.lens (\AnomalyDateInterval' {startDate} -> startDate) (\s@AnomalyDateInterval' {} a -> s {startDate = a} :: AnomalyDateInterval)

instance Core.Hashable AnomalyDateInterval

instance Core.NFData AnomalyDateInterval

instance Core.ToJSON AnomalyDateInterval where
  toJSON AnomalyDateInterval' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EndDate" Core..=) Core.<$> endDate,
            Core.Just ("StartDate" Core..= startDate)
          ]
      )
