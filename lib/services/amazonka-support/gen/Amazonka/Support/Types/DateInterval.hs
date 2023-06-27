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
-- Module      : Amazonka.Support.Types.DateInterval
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.DateInterval where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Date and time (UTC) format in RFC 3339 :
-- \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
--
-- /See:/ 'newDateInterval' smart constructor.
data DateInterval = DateInterval'
  { -- | End Date Time (UTC). RFC 3339 format :
    -- \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
    endDateTime :: Prelude.Maybe Prelude.Text,
    -- | A JSON object containing start and date time (UTC). Date and time format
    -- is RFC 3339 : \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
    startDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateInterval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDateTime', 'dateInterval_endDateTime' - End Date Time (UTC). RFC 3339 format :
-- \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
--
-- 'startDateTime', 'dateInterval_startDateTime' - A JSON object containing start and date time (UTC). Date and time format
-- is RFC 3339 : \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
newDateInterval ::
  DateInterval
newDateInterval =
  DateInterval'
    { endDateTime = Prelude.Nothing,
      startDateTime = Prelude.Nothing
    }

-- | End Date Time (UTC). RFC 3339 format :
-- \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
dateInterval_endDateTime :: Lens.Lens' DateInterval (Prelude.Maybe Prelude.Text)
dateInterval_endDateTime = Lens.lens (\DateInterval' {endDateTime} -> endDateTime) (\s@DateInterval' {} a -> s {endDateTime = a} :: DateInterval)

-- | A JSON object containing start and date time (UTC). Date and time format
-- is RFC 3339 : \'yyyy-MM-dd\'T\'HH:mm:ss.SSSZZ\'.
dateInterval_startDateTime :: Lens.Lens' DateInterval (Prelude.Maybe Prelude.Text)
dateInterval_startDateTime = Lens.lens (\DateInterval' {startDateTime} -> startDateTime) (\s@DateInterval' {} a -> s {startDateTime = a} :: DateInterval)

instance Data.FromJSON DateInterval where
  parseJSON =
    Data.withObject
      "DateInterval"
      ( \x ->
          DateInterval'
            Prelude.<$> (x Data..:? "endDateTime")
            Prelude.<*> (x Data..:? "startDateTime")
      )

instance Prelude.Hashable DateInterval where
  hashWithSalt _salt DateInterval' {..} =
    _salt
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` startDateTime

instance Prelude.NFData DateInterval where
  rnf DateInterval' {..} =
    Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf startDateTime
