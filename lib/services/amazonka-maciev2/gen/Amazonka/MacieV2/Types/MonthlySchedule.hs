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
-- Module      : Amazonka.MacieV2.Types.MonthlySchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.MonthlySchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a monthly recurrence pattern for running a classification job.
--
-- /See:/ 'newMonthlySchedule' smart constructor.
data MonthlySchedule = MonthlySchedule'
  { -- | The numeric day of the month when Amazon Macie runs the job. This value
    -- can be an integer from 1 through 31.
    --
    -- If this value exceeds the number of days in a certain month, Macie
    -- doesn\'t run the job that month. Macie runs the job only during months
    -- that have the specified day. For example, if this value is 31 and a
    -- month has only 30 days, Macie doesn\'t run the job that month. To run
    -- the job every month, specify a value that\'s less than 29.
    dayOfMonth :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonthlySchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfMonth', 'monthlySchedule_dayOfMonth' - The numeric day of the month when Amazon Macie runs the job. This value
-- can be an integer from 1 through 31.
--
-- If this value exceeds the number of days in a certain month, Macie
-- doesn\'t run the job that month. Macie runs the job only during months
-- that have the specified day. For example, if this value is 31 and a
-- month has only 30 days, Macie doesn\'t run the job that month. To run
-- the job every month, specify a value that\'s less than 29.
newMonthlySchedule ::
  MonthlySchedule
newMonthlySchedule =
  MonthlySchedule' {dayOfMonth = Prelude.Nothing}

-- | The numeric day of the month when Amazon Macie runs the job. This value
-- can be an integer from 1 through 31.
--
-- If this value exceeds the number of days in a certain month, Macie
-- doesn\'t run the job that month. Macie runs the job only during months
-- that have the specified day. For example, if this value is 31 and a
-- month has only 30 days, Macie doesn\'t run the job that month. To run
-- the job every month, specify a value that\'s less than 29.
monthlySchedule_dayOfMonth :: Lens.Lens' MonthlySchedule (Prelude.Maybe Prelude.Int)
monthlySchedule_dayOfMonth = Lens.lens (\MonthlySchedule' {dayOfMonth} -> dayOfMonth) (\s@MonthlySchedule' {} a -> s {dayOfMonth = a} :: MonthlySchedule)

instance Data.FromJSON MonthlySchedule where
  parseJSON =
    Data.withObject
      "MonthlySchedule"
      ( \x ->
          MonthlySchedule'
            Prelude.<$> (x Data..:? "dayOfMonth")
      )

instance Prelude.Hashable MonthlySchedule where
  hashWithSalt _salt MonthlySchedule' {..} =
    _salt `Prelude.hashWithSalt` dayOfMonth

instance Prelude.NFData MonthlySchedule where
  rnf MonthlySchedule' {..} = Prelude.rnf dayOfMonth

instance Data.ToJSON MonthlySchedule where
  toJSON MonthlySchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [("dayOfMonth" Data..=) Prelude.<$> dayOfMonth]
      )
