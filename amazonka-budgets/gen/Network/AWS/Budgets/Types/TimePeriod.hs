{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Budgets.Types.TimePeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimePeriod where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The period of time that is covered by a budget. The period has a start
-- date and an end date. The start date must come before the end date.
-- There are no restrictions on the end date.
--
-- /See:/ 'newTimePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { -- | The end date for a budget. If you didn\'t specify an end date, AWS set
    -- your end date to @06\/15\/87 00:00 UTC@. The defaults are the same for
    -- the AWS Billing and Cost Management console and the API.
    --
    -- After the end date, AWS deletes the budget and all associated
    -- notifications and subscribers. You can change your end date with the
    -- @UpdateBudget@ operation.
    end :: Prelude.Maybe Prelude.POSIX,
    -- | The start date for a budget. If you created your budget and didn\'t
    -- specify a start date, AWS defaults to the start of your chosen time
    -- period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you
    -- created your budget on January 24, 2018, chose @DAILY@, and didn\'t set
    -- a start date, AWS set your start date to @01\/24\/18 00:00 UTC@. If you
    -- chose @MONTHLY@, AWS set your start date to @01\/01\/18 00:00 UTC@. The
    -- defaults are the same for the AWS Billing and Cost Management console
    -- and the API.
    --
    -- You can change your start date with the @UpdateBudget@ operation.
    start :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimePeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'timePeriod_end' - The end date for a budget. If you didn\'t specify an end date, AWS set
-- your end date to @06\/15\/87 00:00 UTC@. The defaults are the same for
-- the AWS Billing and Cost Management console and the API.
--
-- After the end date, AWS deletes the budget and all associated
-- notifications and subscribers. You can change your end date with the
-- @UpdateBudget@ operation.
--
-- 'start', 'timePeriod_start' - The start date for a budget. If you created your budget and didn\'t
-- specify a start date, AWS defaults to the start of your chosen time
-- period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you
-- created your budget on January 24, 2018, chose @DAILY@, and didn\'t set
-- a start date, AWS set your start date to @01\/24\/18 00:00 UTC@. If you
-- chose @MONTHLY@, AWS set your start date to @01\/01\/18 00:00 UTC@. The
-- defaults are the same for the AWS Billing and Cost Management console
-- and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
newTimePeriod ::
  TimePeriod
newTimePeriod =
  TimePeriod'
    { end = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | The end date for a budget. If you didn\'t specify an end date, AWS set
-- your end date to @06\/15\/87 00:00 UTC@. The defaults are the same for
-- the AWS Billing and Cost Management console and the API.
--
-- After the end date, AWS deletes the budget and all associated
-- notifications and subscribers. You can change your end date with the
-- @UpdateBudget@ operation.
timePeriod_end :: Lens.Lens' TimePeriod (Prelude.Maybe Prelude.UTCTime)
timePeriod_end = Lens.lens (\TimePeriod' {end} -> end) (\s@TimePeriod' {} a -> s {end = a} :: TimePeriod) Prelude.. Lens.mapping Prelude._Time

-- | The start date for a budget. If you created your budget and didn\'t
-- specify a start date, AWS defaults to the start of your chosen time
-- period (DAILY, MONTHLY, QUARTERLY, or ANNUALLY). For example, if you
-- created your budget on January 24, 2018, chose @DAILY@, and didn\'t set
-- a start date, AWS set your start date to @01\/24\/18 00:00 UTC@. If you
-- chose @MONTHLY@, AWS set your start date to @01\/01\/18 00:00 UTC@. The
-- defaults are the same for the AWS Billing and Cost Management console
-- and the API.
--
-- You can change your start date with the @UpdateBudget@ operation.
timePeriod_start :: Lens.Lens' TimePeriod (Prelude.Maybe Prelude.UTCTime)
timePeriod_start = Lens.lens (\TimePeriod' {start} -> start) (\s@TimePeriod' {} a -> s {start = a} :: TimePeriod) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TimePeriod where
  parseJSON =
    Prelude.withObject
      "TimePeriod"
      ( \x ->
          TimePeriod'
            Prelude.<$> (x Prelude..:? "End")
            Prelude.<*> (x Prelude..:? "Start")
      )

instance Prelude.Hashable TimePeriod

instance Prelude.NFData TimePeriod

instance Prelude.ToJSON TimePeriod where
  toJSON TimePeriod' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("End" Prelude..=) Prelude.<$> end,
            ("Start" Prelude..=) Prelude.<$> start
          ]
      )
