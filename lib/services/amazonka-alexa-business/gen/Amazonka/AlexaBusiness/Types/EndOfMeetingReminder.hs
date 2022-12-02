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
-- Module      : Amazonka.AlexaBusiness.Types.EndOfMeetingReminder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.EndOfMeetingReminder where

import Amazonka.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- /See:/ 'newEndOfMeetingReminder' smart constructor.
data EndOfMeetingReminder = EndOfMeetingReminder'
  { -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Prelude.Maybe EndOfMeetingReminderType,
    -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A range of 3 to 15 minutes that determines when the reminder begins.
    reminderAtMinutes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Int)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndOfMeetingReminder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reminderType', 'endOfMeetingReminder_reminderType' - The type of sound that users hear during the end of meeting reminder.
--
-- 'enabled', 'endOfMeetingReminder_enabled' - Whether an end of meeting reminder is enabled or not.
--
-- 'reminderAtMinutes', 'endOfMeetingReminder_reminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
newEndOfMeetingReminder ::
  EndOfMeetingReminder
newEndOfMeetingReminder =
  EndOfMeetingReminder'
    { reminderType =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      reminderAtMinutes = Prelude.Nothing
    }

-- | The type of sound that users hear during the end of meeting reminder.
endOfMeetingReminder_reminderType :: Lens.Lens' EndOfMeetingReminder (Prelude.Maybe EndOfMeetingReminderType)
endOfMeetingReminder_reminderType = Lens.lens (\EndOfMeetingReminder' {reminderType} -> reminderType) (\s@EndOfMeetingReminder' {} a -> s {reminderType = a} :: EndOfMeetingReminder)

-- | Whether an end of meeting reminder is enabled or not.
endOfMeetingReminder_enabled :: Lens.Lens' EndOfMeetingReminder (Prelude.Maybe Prelude.Bool)
endOfMeetingReminder_enabled = Lens.lens (\EndOfMeetingReminder' {enabled} -> enabled) (\s@EndOfMeetingReminder' {} a -> s {enabled = a} :: EndOfMeetingReminder)

-- | A range of 3 to 15 minutes that determines when the reminder begins.
endOfMeetingReminder_reminderAtMinutes :: Lens.Lens' EndOfMeetingReminder (Prelude.Maybe (Prelude.NonEmpty Prelude.Int))
endOfMeetingReminder_reminderAtMinutes = Lens.lens (\EndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@EndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: EndOfMeetingReminder) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EndOfMeetingReminder where
  parseJSON =
    Data.withObject
      "EndOfMeetingReminder"
      ( \x ->
          EndOfMeetingReminder'
            Prelude.<$> (x Data..:? "ReminderType")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "ReminderAtMinutes")
      )

instance Prelude.Hashable EndOfMeetingReminder where
  hashWithSalt _salt EndOfMeetingReminder' {..} =
    _salt `Prelude.hashWithSalt` reminderType
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` reminderAtMinutes

instance Prelude.NFData EndOfMeetingReminder where
  rnf EndOfMeetingReminder' {..} =
    Prelude.rnf reminderType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf reminderAtMinutes
