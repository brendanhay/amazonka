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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.DateFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.DateRange

-- | A date filter for querying findings.
--
-- /See:/ 'newDateFilter' smart constructor.
data DateFilter = DateFilter'
  { -- | A date range for the date filter.
    dateRange :: Prelude.Maybe DateRange,
    -- | A timestamp that provides the end date for the date filter.
    --
    -- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
    -- cannot contain spaces, and date and time should be separated by @T@. For
    -- more information, see
    -- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    end :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that provides the start date for the date filter.
    --
    -- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
    -- cannot contain spaces, and date and time should be separated by @T@. For
    -- more information, see
    -- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    start :: Prelude.Maybe Prelude.Text
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
-- 'dateRange', 'dateFilter_dateRange' - A date range for the date filter.
--
-- 'end', 'dateFilter_end' - A timestamp that provides the end date for the date filter.
--
-- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
-- cannot contain spaces, and date and time should be separated by @T@. For
-- more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
--
-- 'start', 'dateFilter_start' - A timestamp that provides the start date for the date filter.
--
-- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
-- cannot contain spaces, and date and time should be separated by @T@. For
-- more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
newDateFilter ::
  DateFilter
newDateFilter =
  DateFilter'
    { dateRange = Prelude.Nothing,
      end = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | A date range for the date filter.
dateFilter_dateRange :: Lens.Lens' DateFilter (Prelude.Maybe DateRange)
dateFilter_dateRange = Lens.lens (\DateFilter' {dateRange} -> dateRange) (\s@DateFilter' {} a -> s {dateRange = a} :: DateFilter)

-- | A timestamp that provides the end date for the date filter.
--
-- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
-- cannot contain spaces, and date and time should be separated by @T@. For
-- more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
dateFilter_end :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.Text)
dateFilter_end = Lens.lens (\DateFilter' {end} -> end) (\s@DateFilter' {} a -> s {end = a} :: DateFilter)

-- | A timestamp that provides the start date for the date filter.
--
-- A correctly formatted example is @2020-05-21T20:16:34.724Z@. The value
-- cannot contain spaces, and date and time should be separated by @T@. For
-- more information, see
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
dateFilter_start :: Lens.Lens' DateFilter (Prelude.Maybe Prelude.Text)
dateFilter_start = Lens.lens (\DateFilter' {start} -> start) (\s@DateFilter' {} a -> s {start = a} :: DateFilter)

instance Data.FromJSON DateFilter where
  parseJSON =
    Data.withObject
      "DateFilter"
      ( \x ->
          DateFilter'
            Prelude.<$> (x Data..:? "DateRange")
            Prelude.<*> (x Data..:? "End")
            Prelude.<*> (x Data..:? "Start")
      )

instance Prelude.Hashable DateFilter where
  hashWithSalt _salt DateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` dateRange
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start

instance Prelude.NFData DateFilter where
  rnf DateFilter' {..} =
    Prelude.rnf dateRange
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf start

instance Data.ToJSON DateFilter where
  toJSON DateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateRange" Data..=) Prelude.<$> dateRange,
            ("End" Data..=) Prelude.<$> end,
            ("Start" Data..=) Prelude.<$> start
          ]
      )
