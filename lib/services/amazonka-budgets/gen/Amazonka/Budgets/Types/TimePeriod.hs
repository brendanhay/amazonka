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
-- Module      : Amazonka.Budgets.Types.TimePeriod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.TimePeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The period of time that\'s covered by a budget. The period has a start
-- date and an end date. The start date must come before the end date.
-- There are no restrictions on the end date.
--
-- /See:/ 'newTimePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { -- | The start date for a budget. If you created your budget and didn\'t
    -- specify a start date, Amazon Web Services defaults to the start of your
    -- chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For
    -- example, if you created your budget on January 24, 2018, chose @DAILY@,
    -- and didn\'t set a start date, Amazon Web Services set your start date to
    -- @01\/24\/18 00:00 UTC@. If you chose @MONTHLY@, Amazon Web Services set
    -- your start date to @01\/01\/18 00:00 UTC@. The defaults are the same for
    -- the Billing and Cost Management console and the API.
    --
    -- You can change your start date with the @UpdateBudget@ operation.
    start :: Prelude.Maybe Data.POSIX,
    -- | The end date for a budget. If you didn\'t specify an end date, Amazon
    -- Web Services set your end date to @06\/15\/87 00:00 UTC@. The defaults
    -- are the same for the Billing and Cost Management console and the API.
    --
    -- After the end date, Amazon Web Services deletes the budget and all the
    -- associated notifications and subscribers. You can change your end date
    -- with the @UpdateBudget@ operation.
    end :: Prelude.Maybe Data.POSIX
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
-- 'start', 'timePeriod_start' - The start date for a budget. If you created your budget and didn\'t
-- specify a start date, Amazon Web Services defaults to the start of your
-- chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For
-- example, if you created your budget on January 24, 2018, chose @DAILY@,
-- and didn\'t set a start date, Amazon Web Services set your start date to
-- @01\/24\/18 00:00 UTC@. If you chose @MONTHLY@, Amazon Web Services set
-- your start date to @01\/01\/18 00:00 UTC@. The defaults are the same for
-- the Billing and Cost Management console and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
--
-- 'end', 'timePeriod_end' - The end date for a budget. If you didn\'t specify an end date, Amazon
-- Web Services set your end date to @06\/15\/87 00:00 UTC@. The defaults
-- are the same for the Billing and Cost Management console and the API.
--
-- After the end date, Amazon Web Services deletes the budget and all the
-- associated notifications and subscribers. You can change your end date
-- with the @UpdateBudget@ operation.
newTimePeriod ::
  TimePeriod
newTimePeriod =
  TimePeriod'
    { start = Prelude.Nothing,
      end = Prelude.Nothing
    }

-- | The start date for a budget. If you created your budget and didn\'t
-- specify a start date, Amazon Web Services defaults to the start of your
-- chosen time period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For
-- example, if you created your budget on January 24, 2018, chose @DAILY@,
-- and didn\'t set a start date, Amazon Web Services set your start date to
-- @01\/24\/18 00:00 UTC@. If you chose @MONTHLY@, Amazon Web Services set
-- your start date to @01\/01\/18 00:00 UTC@. The defaults are the same for
-- the Billing and Cost Management console and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
timePeriod_start :: Lens.Lens' TimePeriod (Prelude.Maybe Prelude.UTCTime)
timePeriod_start = Lens.lens (\TimePeriod' {start} -> start) (\s@TimePeriod' {} a -> s {start = a} :: TimePeriod) Prelude.. Lens.mapping Data._Time

-- | The end date for a budget. If you didn\'t specify an end date, Amazon
-- Web Services set your end date to @06\/15\/87 00:00 UTC@. The defaults
-- are the same for the Billing and Cost Management console and the API.
--
-- After the end date, Amazon Web Services deletes the budget and all the
-- associated notifications and subscribers. You can change your end date
-- with the @UpdateBudget@ operation.
timePeriod_end :: Lens.Lens' TimePeriod (Prelude.Maybe Prelude.UTCTime)
timePeriod_end = Lens.lens (\TimePeriod' {end} -> end) (\s@TimePeriod' {} a -> s {end = a} :: TimePeriod) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimePeriod where
  parseJSON =
    Data.withObject
      "TimePeriod"
      ( \x ->
          TimePeriod'
            Prelude.<$> (x Data..:? "Start") Prelude.<*> (x Data..:? "End")
      )

instance Prelude.Hashable TimePeriod where
  hashWithSalt _salt TimePeriod' {..} =
    _salt `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` end

instance Prelude.NFData TimePeriod where
  rnf TimePeriod' {..} =
    Prelude.rnf start `Prelude.seq` Prelude.rnf end

instance Data.ToJSON TimePeriod where
  toJSON TimePeriod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Start" Data..=) Prelude.<$> start,
            ("End" Data..=) Prelude.<$> end
          ]
      )
