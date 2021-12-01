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
-- Module      : Amazonka.AWSHealth.Types.DateTimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.DateTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A range of dates and times that is used by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EventFilter.html EventFilter>
-- and
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter>
-- objects. If @from@ is set and @to@ is set: match items where the
-- timestamp (@startTime@, @endTime@, or @lastUpdatedTime@) is between
-- @from@ and @to@ inclusive. If @from@ is set and @to@ is not set: match
-- items where the timestamp value is equal to or after @from@. If @from@
-- is not set and @to@ is set: match items where the timestamp value is
-- equal to or before @to@.
--
-- /See:/ 'newDateTimeRange' smart constructor.
data DateTimeRange = DateTimeRange'
  { -- | The ending date and time of a time range.
    to :: Prelude.Maybe Core.POSIX,
    -- | The starting date and time of a time range.
    from :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'to', 'dateTimeRange_to' - The ending date and time of a time range.
--
-- 'from', 'dateTimeRange_from' - The starting date and time of a time range.
newDateTimeRange ::
  DateTimeRange
newDateTimeRange =
  DateTimeRange'
    { to = Prelude.Nothing,
      from = Prelude.Nothing
    }

-- | The ending date and time of a time range.
dateTimeRange_to :: Lens.Lens' DateTimeRange (Prelude.Maybe Prelude.UTCTime)
dateTimeRange_to = Lens.lens (\DateTimeRange' {to} -> to) (\s@DateTimeRange' {} a -> s {to = a} :: DateTimeRange) Prelude.. Lens.mapping Core._Time

-- | The starting date and time of a time range.
dateTimeRange_from :: Lens.Lens' DateTimeRange (Prelude.Maybe Prelude.UTCTime)
dateTimeRange_from = Lens.lens (\DateTimeRange' {from} -> from) (\s@DateTimeRange' {} a -> s {from = a} :: DateTimeRange) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable DateTimeRange where
  hashWithSalt salt' DateTimeRange' {..} =
    salt' `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData DateTimeRange where
  rnf DateTimeRange' {..} =
    Prelude.rnf to `Prelude.seq` Prelude.rnf from

instance Core.ToJSON DateTimeRange where
  toJSON DateTimeRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("to" Core..=) Prelude.<$> to,
            ("from" Core..=) Prelude.<$> from
          ]
      )
