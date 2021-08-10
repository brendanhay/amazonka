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
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- /See:/ 'newEndOfMeetingReminder' smart constructor.
data EndOfMeetingReminder = EndOfMeetingReminder'
  { -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Prelude.Maybe EndOfMeetingReminderType,
    -- | A range of 3 to 15 minutes that determines when the reminder begins.
    reminderAtMinutes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Int),
    -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool
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
-- 'reminderAtMinutes', 'endOfMeetingReminder_reminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
--
-- 'enabled', 'endOfMeetingReminder_enabled' - Whether an end of meeting reminder is enabled or not.
newEndOfMeetingReminder ::
  EndOfMeetingReminder
newEndOfMeetingReminder =
  EndOfMeetingReminder'
    { reminderType =
        Prelude.Nothing,
      reminderAtMinutes = Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The type of sound that users hear during the end of meeting reminder.
endOfMeetingReminder_reminderType :: Lens.Lens' EndOfMeetingReminder (Prelude.Maybe EndOfMeetingReminderType)
endOfMeetingReminder_reminderType = Lens.lens (\EndOfMeetingReminder' {reminderType} -> reminderType) (\s@EndOfMeetingReminder' {} a -> s {reminderType = a} :: EndOfMeetingReminder)

-- | A range of 3 to 15 minutes that determines when the reminder begins.
endOfMeetingReminder_reminderAtMinutes :: Lens.Lens' EndOfMeetingReminder (Prelude.Maybe (Prelude.NonEmpty Prelude.Int))
endOfMeetingReminder_reminderAtMinutes = Lens.lens (\EndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@EndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: EndOfMeetingReminder) Prelude.. Lens.mapping Lens._Coerce

-- | Whether an end of meeting reminder is enabled or not.
endOfMeetingReminder_enabled :: Lens.Lens' EndOfMeetingReminder (Prelude.Maybe Prelude.Bool)
endOfMeetingReminder_enabled = Lens.lens (\EndOfMeetingReminder' {enabled} -> enabled) (\s@EndOfMeetingReminder' {} a -> s {enabled = a} :: EndOfMeetingReminder)

instance Core.FromJSON EndOfMeetingReminder where
  parseJSON =
    Core.withObject
      "EndOfMeetingReminder"
      ( \x ->
          EndOfMeetingReminder'
            Prelude.<$> (x Core..:? "ReminderType")
            Prelude.<*> (x Core..:? "ReminderAtMinutes")
            Prelude.<*> (x Core..:? "Enabled")
      )

instance Prelude.Hashable EndOfMeetingReminder

instance Prelude.NFData EndOfMeetingReminder
