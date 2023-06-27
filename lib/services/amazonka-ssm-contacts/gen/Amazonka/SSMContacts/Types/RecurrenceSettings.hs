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
-- Module      : Amazonka.SSMContacts.Types.RecurrenceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.RecurrenceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.CoverageTime
import Amazonka.SSMContacts.Types.DayOfWeek
import Amazonka.SSMContacts.Types.HandOffTime
import Amazonka.SSMContacts.Types.MonthlySetting
import Amazonka.SSMContacts.Types.WeeklySetting

-- | Information about when an on-call rotation is in effect and how long the
-- rotation period lasts.
--
-- /See:/ 'newRecurrenceSettings' smart constructor.
data RecurrenceSettings = RecurrenceSettings'
  { -- | Information about on-call rotations that recur daily.
    dailySettings :: Prelude.Maybe [HandOffTime],
    -- | Information about on-call rotations that recur monthly.
    monthlySettings :: Prelude.Maybe [MonthlySetting],
    -- | Information about the days of the week included in on-call rotation
    -- coverage.
    shiftCoverages :: Prelude.Maybe (Prelude.HashMap DayOfWeek [CoverageTime]),
    -- | Information about on-call rotations that recur weekly.
    weeklySettings :: Prelude.Maybe [WeeklySetting],
    -- | The number of contacts, or shift team members designated to be on call
    -- concurrently during a shift. For example, in an on-call schedule
    -- containing ten contacts, a value of @2@ designates that two of them are
    -- on call at any given time.
    numberOfOnCalls :: Prelude.Natural,
    -- | The number of days, weeks, or months a single rotation lasts.
    recurrenceMultiplier :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecurrenceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dailySettings', 'recurrenceSettings_dailySettings' - Information about on-call rotations that recur daily.
--
-- 'monthlySettings', 'recurrenceSettings_monthlySettings' - Information about on-call rotations that recur monthly.
--
-- 'shiftCoverages', 'recurrenceSettings_shiftCoverages' - Information about the days of the week included in on-call rotation
-- coverage.
--
-- 'weeklySettings', 'recurrenceSettings_weeklySettings' - Information about on-call rotations that recur weekly.
--
-- 'numberOfOnCalls', 'recurrenceSettings_numberOfOnCalls' - The number of contacts, or shift team members designated to be on call
-- concurrently during a shift. For example, in an on-call schedule
-- containing ten contacts, a value of @2@ designates that two of them are
-- on call at any given time.
--
-- 'recurrenceMultiplier', 'recurrenceSettings_recurrenceMultiplier' - The number of days, weeks, or months a single rotation lasts.
newRecurrenceSettings ::
  -- | 'numberOfOnCalls'
  Prelude.Natural ->
  -- | 'recurrenceMultiplier'
  Prelude.Natural ->
  RecurrenceSettings
newRecurrenceSettings
  pNumberOfOnCalls_
  pRecurrenceMultiplier_ =
    RecurrenceSettings'
      { dailySettings =
          Prelude.Nothing,
        monthlySettings = Prelude.Nothing,
        shiftCoverages = Prelude.Nothing,
        weeklySettings = Prelude.Nothing,
        numberOfOnCalls = pNumberOfOnCalls_,
        recurrenceMultiplier = pRecurrenceMultiplier_
      }

-- | Information about on-call rotations that recur daily.
recurrenceSettings_dailySettings :: Lens.Lens' RecurrenceSettings (Prelude.Maybe [HandOffTime])
recurrenceSettings_dailySettings = Lens.lens (\RecurrenceSettings' {dailySettings} -> dailySettings) (\s@RecurrenceSettings' {} a -> s {dailySettings = a} :: RecurrenceSettings) Prelude.. Lens.mapping Lens.coerced

-- | Information about on-call rotations that recur monthly.
recurrenceSettings_monthlySettings :: Lens.Lens' RecurrenceSettings (Prelude.Maybe [MonthlySetting])
recurrenceSettings_monthlySettings = Lens.lens (\RecurrenceSettings' {monthlySettings} -> monthlySettings) (\s@RecurrenceSettings' {} a -> s {monthlySettings = a} :: RecurrenceSettings) Prelude.. Lens.mapping Lens.coerced

-- | Information about the days of the week included in on-call rotation
-- coverage.
recurrenceSettings_shiftCoverages :: Lens.Lens' RecurrenceSettings (Prelude.Maybe (Prelude.HashMap DayOfWeek [CoverageTime]))
recurrenceSettings_shiftCoverages = Lens.lens (\RecurrenceSettings' {shiftCoverages} -> shiftCoverages) (\s@RecurrenceSettings' {} a -> s {shiftCoverages = a} :: RecurrenceSettings) Prelude.. Lens.mapping Lens.coerced

-- | Information about on-call rotations that recur weekly.
recurrenceSettings_weeklySettings :: Lens.Lens' RecurrenceSettings (Prelude.Maybe [WeeklySetting])
recurrenceSettings_weeklySettings = Lens.lens (\RecurrenceSettings' {weeklySettings} -> weeklySettings) (\s@RecurrenceSettings' {} a -> s {weeklySettings = a} :: RecurrenceSettings) Prelude.. Lens.mapping Lens.coerced

-- | The number of contacts, or shift team members designated to be on call
-- concurrently during a shift. For example, in an on-call schedule
-- containing ten contacts, a value of @2@ designates that two of them are
-- on call at any given time.
recurrenceSettings_numberOfOnCalls :: Lens.Lens' RecurrenceSettings Prelude.Natural
recurrenceSettings_numberOfOnCalls = Lens.lens (\RecurrenceSettings' {numberOfOnCalls} -> numberOfOnCalls) (\s@RecurrenceSettings' {} a -> s {numberOfOnCalls = a} :: RecurrenceSettings)

-- | The number of days, weeks, or months a single rotation lasts.
recurrenceSettings_recurrenceMultiplier :: Lens.Lens' RecurrenceSettings Prelude.Natural
recurrenceSettings_recurrenceMultiplier = Lens.lens (\RecurrenceSettings' {recurrenceMultiplier} -> recurrenceMultiplier) (\s@RecurrenceSettings' {} a -> s {recurrenceMultiplier = a} :: RecurrenceSettings)

instance Data.FromJSON RecurrenceSettings where
  parseJSON =
    Data.withObject
      "RecurrenceSettings"
      ( \x ->
          RecurrenceSettings'
            Prelude.<$> (x Data..:? "DailySettings" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "MonthlySettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ShiftCoverages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WeeklySettings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "NumberOfOnCalls")
            Prelude.<*> (x Data..: "RecurrenceMultiplier")
      )

instance Prelude.Hashable RecurrenceSettings where
  hashWithSalt _salt RecurrenceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` dailySettings
      `Prelude.hashWithSalt` monthlySettings
      `Prelude.hashWithSalt` shiftCoverages
      `Prelude.hashWithSalt` weeklySettings
      `Prelude.hashWithSalt` numberOfOnCalls
      `Prelude.hashWithSalt` recurrenceMultiplier

instance Prelude.NFData RecurrenceSettings where
  rnf RecurrenceSettings' {..} =
    Prelude.rnf dailySettings
      `Prelude.seq` Prelude.rnf monthlySettings
      `Prelude.seq` Prelude.rnf shiftCoverages
      `Prelude.seq` Prelude.rnf weeklySettings
      `Prelude.seq` Prelude.rnf numberOfOnCalls
      `Prelude.seq` Prelude.rnf recurrenceMultiplier

instance Data.ToJSON RecurrenceSettings where
  toJSON RecurrenceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DailySettings" Data..=) Prelude.<$> dailySettings,
            ("MonthlySettings" Data..=)
              Prelude.<$> monthlySettings,
            ("ShiftCoverages" Data..=)
              Prelude.<$> shiftCoverages,
            ("WeeklySettings" Data..=)
              Prelude.<$> weeklySettings,
            Prelude.Just
              ("NumberOfOnCalls" Data..= numberOfOnCalls),
            Prelude.Just
              ( "RecurrenceMultiplier"
                  Data..= recurrenceMultiplier
              )
          ]
      )
