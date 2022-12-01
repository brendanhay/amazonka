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
-- Module      : Amazonka.SecurityHub.Types.DateFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.DateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.DateRange

-- | A date filter for querying findings.
--
-- /See:/ 'newDateFilter' smart constructor.
data DateFilter = DateFilter'
  { -- | A start date for the date filter.
    start :: Prelude.Maybe Prelude.Text,
    -- | A date range for the date filter.
    dateRange :: Prelude.Maybe DateRange,
    -- | An end date for the date filter.
    end :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'dateFilter_start' - A start date for the date filter.
--
-- 'dateRange', 'dateFilter_dateRange' - A date range for the date filter.
--
-- 'end', 'dateFilter_end' - An end date for the date filter.
newDateFilter ::
  DateFilter
newDateFilter =
  DateFilter'
    { start = Prelude.Nothing,
      dateRange = Prelude.Nothing,
      end = Prelude.Nothing
    }

-- | A start date for the date filter.
dateFilter_start :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.Text)
dateFilter_start = Lens.lens (\DateFilter' {start} -> start) (\s@DateFilter' {} a -> s {start = a} :: DateFilter)

-- | A date range for the date filter.
dateFilter_dateRange :: Lens.Lens' DateFilter (Prelude.Maybe DateRange)
dateFilter_dateRange = Lens.lens (\DateFilter' {dateRange} -> dateRange) (\s@DateFilter' {} a -> s {dateRange = a} :: DateFilter)

-- | An end date for the date filter.
dateFilter_end :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.Text)
dateFilter_end = Lens.lens (\DateFilter' {end} -> end) (\s@DateFilter' {} a -> s {end = a} :: DateFilter)

instance Core.FromJSON DateFilter where
  parseJSON =
    Core.withObject
      "DateFilter"
      ( \x ->
          DateFilter'
            Prelude.<$> (x Core..:? "Start")
            Prelude.<*> (x Core..:? "DateRange")
            Prelude.<*> (x Core..:? "End")
      )

instance Prelude.Hashable DateFilter where
  hashWithSalt _salt DateFilter' {..} =
    _salt `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` dateRange
      `Prelude.hashWithSalt` end

instance Prelude.NFData DateFilter where
  rnf DateFilter' {..} =
    Prelude.rnf start
      `Prelude.seq` Prelude.rnf dateRange
      `Prelude.seq` Prelude.rnf end

instance Core.ToJSON DateFilter where
  toJSON DateFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Start" Core..=) Prelude.<$> start,
            ("DateRange" Core..=) Prelude.<$> dateRange,
            ("End" Core..=) Prelude.<$> end
          ]
      )
