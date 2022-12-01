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
-- Module      : Amazonka.CostExplorer.Types.AnomalyDateInterval
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.AnomalyDateInterval where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time period for an anomaly.
--
-- /See:/ 'newAnomalyDateInterval' smart constructor.
data AnomalyDateInterval = AnomalyDateInterval'
  { -- | The last date an anomaly was observed.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The first date an anomaly was observed.
    startDate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AnomalyDateInterval
newAnomalyDateInterval pStartDate_ =
  AnomalyDateInterval'
    { endDate = Prelude.Nothing,
      startDate = pStartDate_
    }

-- | The last date an anomaly was observed.
anomalyDateInterval_endDate :: Lens.Lens' AnomalyDateInterval (Prelude.Maybe Prelude.Text)
anomalyDateInterval_endDate = Lens.lens (\AnomalyDateInterval' {endDate} -> endDate) (\s@AnomalyDateInterval' {} a -> s {endDate = a} :: AnomalyDateInterval)

-- | The first date an anomaly was observed.
anomalyDateInterval_startDate :: Lens.Lens' AnomalyDateInterval Prelude.Text
anomalyDateInterval_startDate = Lens.lens (\AnomalyDateInterval' {startDate} -> startDate) (\s@AnomalyDateInterval' {} a -> s {startDate = a} :: AnomalyDateInterval)

instance Prelude.Hashable AnomalyDateInterval where
  hashWithSalt _salt AnomalyDateInterval' {..} =
    _salt `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` startDate

instance Prelude.NFData AnomalyDateInterval where
  rnf AnomalyDateInterval' {..} =
    Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf startDate

instance Core.ToJSON AnomalyDateInterval where
  toJSON AnomalyDateInterval' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EndDate" Core..=) Prelude.<$> endDate,
            Prelude.Just ("StartDate" Core..= startDate)
          ]
      )
