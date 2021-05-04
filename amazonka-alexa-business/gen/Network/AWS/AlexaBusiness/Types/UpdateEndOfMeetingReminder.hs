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
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- /See:/ 'newUpdateEndOfMeetingReminder' smart constructor.
data UpdateEndOfMeetingReminder = UpdateEndOfMeetingReminder'
  { -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Prelude.Maybe EndOfMeetingReminderType,
    -- | Updates settings for the end of meeting reminder feature that are
    -- applied to a room profile. The end of meeting reminder enables Alexa to
    -- remind users when a meeting is ending.
    reminderAtMinutes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Int),
    -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndOfMeetingReminder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reminderType', 'updateEndOfMeetingReminder_reminderType' - The type of sound that users hear during the end of meeting reminder.
--
-- 'reminderAtMinutes', 'updateEndOfMeetingReminder_reminderAtMinutes' - Updates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
--
-- 'enabled', 'updateEndOfMeetingReminder_enabled' - Whether an end of meeting reminder is enabled or not.
newUpdateEndOfMeetingReminder ::
  UpdateEndOfMeetingReminder
newUpdateEndOfMeetingReminder =
  UpdateEndOfMeetingReminder'
    { reminderType =
        Prelude.Nothing,
      reminderAtMinutes = Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The type of sound that users hear during the end of meeting reminder.
updateEndOfMeetingReminder_reminderType :: Lens.Lens' UpdateEndOfMeetingReminder (Prelude.Maybe EndOfMeetingReminderType)
updateEndOfMeetingReminder_reminderType = Lens.lens (\UpdateEndOfMeetingReminder' {reminderType} -> reminderType) (\s@UpdateEndOfMeetingReminder' {} a -> s {reminderType = a} :: UpdateEndOfMeetingReminder)

-- | Updates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
updateEndOfMeetingReminder_reminderAtMinutes :: Lens.Lens' UpdateEndOfMeetingReminder (Prelude.Maybe (Prelude.NonEmpty Prelude.Int))
updateEndOfMeetingReminder_reminderAtMinutes = Lens.lens (\UpdateEndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@UpdateEndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: UpdateEndOfMeetingReminder) Prelude.. Lens.mapping Prelude._Coerce

-- | Whether an end of meeting reminder is enabled or not.
updateEndOfMeetingReminder_enabled :: Lens.Lens' UpdateEndOfMeetingReminder (Prelude.Maybe Prelude.Bool)
updateEndOfMeetingReminder_enabled = Lens.lens (\UpdateEndOfMeetingReminder' {enabled} -> enabled) (\s@UpdateEndOfMeetingReminder' {} a -> s {enabled = a} :: UpdateEndOfMeetingReminder)

instance Prelude.Hashable UpdateEndOfMeetingReminder

instance Prelude.NFData UpdateEndOfMeetingReminder

instance Prelude.ToJSON UpdateEndOfMeetingReminder where
  toJSON UpdateEndOfMeetingReminder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReminderType" Prelude..=)
              Prelude.<$> reminderType,
            ("ReminderAtMinutes" Prelude..=)
              Prelude.<$> reminderAtMinutes,
            ("Enabled" Prelude..=) Prelude.<$> enabled
          ]
      )
