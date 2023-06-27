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
-- Module      : Amazonka.LexV2Models.Types.DateRangeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DateRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The object used for specifying the data range that the customer wants
-- Amazon Lex to read through in the input transcripts.
--
-- /See:/ 'newDateRangeFilter' smart constructor.
data DateRangeFilter = DateRangeFilter'
  { -- | A timestamp indicating the start date for the date range filter.
    startDateTime :: Data.POSIX,
    -- | A timestamp indicating the end date for the date range filter.
    endDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDateTime', 'dateRangeFilter_startDateTime' - A timestamp indicating the start date for the date range filter.
--
-- 'endDateTime', 'dateRangeFilter_endDateTime' - A timestamp indicating the end date for the date range filter.
newDateRangeFilter ::
  -- | 'startDateTime'
  Prelude.UTCTime ->
  -- | 'endDateTime'
  Prelude.UTCTime ->
  DateRangeFilter
newDateRangeFilter pStartDateTime_ pEndDateTime_ =
  DateRangeFilter'
    { startDateTime =
        Data._Time Lens.# pStartDateTime_,
      endDateTime = Data._Time Lens.# pEndDateTime_
    }

-- | A timestamp indicating the start date for the date range filter.
dateRangeFilter_startDateTime :: Lens.Lens' DateRangeFilter Prelude.UTCTime
dateRangeFilter_startDateTime = Lens.lens (\DateRangeFilter' {startDateTime} -> startDateTime) (\s@DateRangeFilter' {} a -> s {startDateTime = a} :: DateRangeFilter) Prelude.. Data._Time

-- | A timestamp indicating the end date for the date range filter.
dateRangeFilter_endDateTime :: Lens.Lens' DateRangeFilter Prelude.UTCTime
dateRangeFilter_endDateTime = Lens.lens (\DateRangeFilter' {endDateTime} -> endDateTime) (\s@DateRangeFilter' {} a -> s {endDateTime = a} :: DateRangeFilter) Prelude.. Data._Time

instance Data.FromJSON DateRangeFilter where
  parseJSON =
    Data.withObject
      "DateRangeFilter"
      ( \x ->
          DateRangeFilter'
            Prelude.<$> (x Data..: "startDateTime")
            Prelude.<*> (x Data..: "endDateTime")
      )

instance Prelude.Hashable DateRangeFilter where
  hashWithSalt _salt DateRangeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` endDateTime

instance Prelude.NFData DateRangeFilter where
  rnf DateRangeFilter' {..} =
    Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf endDateTime

instance Data.ToJSON DateRangeFilter where
  toJSON DateRangeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("startDateTime" Data..= startDateTime),
            Prelude.Just ("endDateTime" Data..= endDateTime)
          ]
      )
