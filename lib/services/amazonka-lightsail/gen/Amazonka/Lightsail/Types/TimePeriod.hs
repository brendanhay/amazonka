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
-- Module      : Amazonka.Lightsail.Types.TimePeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.TimePeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sets the start date and end date for retrieving a cost estimate. The
-- start date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
--
-- /See:/ 'newTimePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { -- | The end of the time period. The end date is exclusive. For example, if
    -- @end@ is @2017-05-01@, Lightsail for Research retrieves cost and usage
    -- data from the start date up to, but not including, @2017-05-01@.
    end :: Prelude.Maybe Data.POSIX,
    -- | The beginning of the time period. The start date is inclusive. For
    -- example, if @start@ is @2017-01-01@, Lightsail for Research retrieves
    -- cost and usage data starting at @2017-01-01@ up to the end date. The
    -- start date must be equal to or no later than the current date to avoid a
    -- validation error.
    start :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimePeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'timePeriod_end' - The end of the time period. The end date is exclusive. For example, if
-- @end@ is @2017-05-01@, Lightsail for Research retrieves cost and usage
-- data from the start date up to, but not including, @2017-05-01@.
--
-- 'start', 'timePeriod_start' - The beginning of the time period. The start date is inclusive. For
-- example, if @start@ is @2017-01-01@, Lightsail for Research retrieves
-- cost and usage data starting at @2017-01-01@ up to the end date. The
-- start date must be equal to or no later than the current date to avoid a
-- validation error.
newTimePeriod ::
  TimePeriod
newTimePeriod =
  TimePeriod'
    { end = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | The end of the time period. The end date is exclusive. For example, if
-- @end@ is @2017-05-01@, Lightsail for Research retrieves cost and usage
-- data from the start date up to, but not including, @2017-05-01@.
timePeriod_end :: Lens.Lens' TimePeriod (Prelude.Maybe Prelude.UTCTime)
timePeriod_end = Lens.lens (\TimePeriod' {end} -> end) (\s@TimePeriod' {} a -> s {end = a} :: TimePeriod) Prelude.. Lens.mapping Data._Time

-- | The beginning of the time period. The start date is inclusive. For
-- example, if @start@ is @2017-01-01@, Lightsail for Research retrieves
-- cost and usage data starting at @2017-01-01@ up to the end date. The
-- start date must be equal to or no later than the current date to avoid a
-- validation error.
timePeriod_start :: Lens.Lens' TimePeriod (Prelude.Maybe Prelude.UTCTime)
timePeriod_start = Lens.lens (\TimePeriod' {start} -> start) (\s@TimePeriod' {} a -> s {start = a} :: TimePeriod) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimePeriod where
  parseJSON =
    Data.withObject
      "TimePeriod"
      ( \x ->
          TimePeriod'
            Prelude.<$> (x Data..:? "end")
            Prelude.<*> (x Data..:? "start")
      )

instance Prelude.Hashable TimePeriod where
  hashWithSalt _salt TimePeriod' {..} =
    _salt
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start

instance Prelude.NFData TimePeriod where
  rnf TimePeriod' {..} =
    Prelude.rnf end `Prelude.seq` Prelude.rnf start
