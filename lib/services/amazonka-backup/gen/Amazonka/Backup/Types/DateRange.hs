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
-- Module      : Amazonka.Backup.Types.DateRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.DateRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is a resource filter containing FromDate: DateTime and ToDate:
-- DateTime. Both values are required. Future DateTime values are not
-- permitted.
--
-- The date and time are in Unix format and Coordinated Universal Time
-- (UTC), and it is accurate to milliseconds ((milliseconds are optional).
-- For example, the value 1516925490.087 represents Friday, January 26,
-- 2018 12:11:30.087 AM.
--
-- /See:/ 'newDateRange' smart constructor.
data DateRange = DateRange'
  { -- | This value is the beginning date, inclusive.
    --
    -- The date and time are in Unix format and Coordinated Universal Time
    -- (UTC), and it is accurate to milliseconds (milliseconds are optional).
    fromDate :: Data.POSIX,
    -- | This value is the end date, inclusive.
    --
    -- The date and time are in Unix format and Coordinated Universal Time
    -- (UTC), and it is accurate to milliseconds (milliseconds are optional).
    toDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromDate', 'dateRange_fromDate' - This value is the beginning date, inclusive.
--
-- The date and time are in Unix format and Coordinated Universal Time
-- (UTC), and it is accurate to milliseconds (milliseconds are optional).
--
-- 'toDate', 'dateRange_toDate' - This value is the end date, inclusive.
--
-- The date and time are in Unix format and Coordinated Universal Time
-- (UTC), and it is accurate to milliseconds (milliseconds are optional).
newDateRange ::
  -- | 'fromDate'
  Prelude.UTCTime ->
  -- | 'toDate'
  Prelude.UTCTime ->
  DateRange
newDateRange pFromDate_ pToDate_ =
  DateRange'
    { fromDate = Data._Time Lens.# pFromDate_,
      toDate = Data._Time Lens.# pToDate_
    }

-- | This value is the beginning date, inclusive.
--
-- The date and time are in Unix format and Coordinated Universal Time
-- (UTC), and it is accurate to milliseconds (milliseconds are optional).
dateRange_fromDate :: Lens.Lens' DateRange Prelude.UTCTime
dateRange_fromDate = Lens.lens (\DateRange' {fromDate} -> fromDate) (\s@DateRange' {} a -> s {fromDate = a} :: DateRange) Prelude.. Data._Time

-- | This value is the end date, inclusive.
--
-- The date and time are in Unix format and Coordinated Universal Time
-- (UTC), and it is accurate to milliseconds (milliseconds are optional).
dateRange_toDate :: Lens.Lens' DateRange Prelude.UTCTime
dateRange_toDate = Lens.lens (\DateRange' {toDate} -> toDate) (\s@DateRange' {} a -> s {toDate = a} :: DateRange) Prelude.. Data._Time

instance Data.FromJSON DateRange where
  parseJSON =
    Data.withObject
      "DateRange"
      ( \x ->
          DateRange'
            Prelude.<$> (x Data..: "FromDate")
            Prelude.<*> (x Data..: "ToDate")
      )

instance Prelude.Hashable DateRange where
  hashWithSalt _salt DateRange' {..} =
    _salt
      `Prelude.hashWithSalt` fromDate
      `Prelude.hashWithSalt` toDate

instance Prelude.NFData DateRange where
  rnf DateRange' {..} =
    Prelude.rnf fromDate
      `Prelude.seq` Prelude.rnf toDate

instance Data.ToJSON DateRange where
  toJSON DateRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FromDate" Data..= fromDate),
            Prelude.Just ("ToDate" Data..= toDate)
          ]
      )
