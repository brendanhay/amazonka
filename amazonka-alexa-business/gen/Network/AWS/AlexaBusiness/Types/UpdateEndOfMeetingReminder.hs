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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- /See:/ 'newUpdateEndOfMeetingReminder' smart constructor.
data UpdateEndOfMeetingReminder = UpdateEndOfMeetingReminder'
  { -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Core.Maybe EndOfMeetingReminderType,
    -- | Updates settings for the end of meeting reminder feature that are
    -- applied to a room profile. The end of meeting reminder enables Alexa to
    -- remind users when a meeting is ending.
    reminderAtMinutes :: Core.Maybe (Core.NonEmpty Core.Int),
    -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      reminderAtMinutes = Core.Nothing,
      enabled = Core.Nothing
    }

-- | The type of sound that users hear during the end of meeting reminder.
updateEndOfMeetingReminder_reminderType :: Lens.Lens' UpdateEndOfMeetingReminder (Core.Maybe EndOfMeetingReminderType)
updateEndOfMeetingReminder_reminderType = Lens.lens (\UpdateEndOfMeetingReminder' {reminderType} -> reminderType) (\s@UpdateEndOfMeetingReminder' {} a -> s {reminderType = a} :: UpdateEndOfMeetingReminder)

-- | Updates settings for the end of meeting reminder feature that are
-- applied to a room profile. The end of meeting reminder enables Alexa to
-- remind users when a meeting is ending.
updateEndOfMeetingReminder_reminderAtMinutes :: Lens.Lens' UpdateEndOfMeetingReminder (Core.Maybe (Core.NonEmpty Core.Int))
updateEndOfMeetingReminder_reminderAtMinutes = Lens.lens (\UpdateEndOfMeetingReminder' {reminderAtMinutes} -> reminderAtMinutes) (\s@UpdateEndOfMeetingReminder' {} a -> s {reminderAtMinutes = a} :: UpdateEndOfMeetingReminder) Core.. Lens.mapping Lens._Coerce

-- | Whether an end of meeting reminder is enabled or not.
updateEndOfMeetingReminder_enabled :: Lens.Lens' UpdateEndOfMeetingReminder (Core.Maybe Core.Bool)
updateEndOfMeetingReminder_enabled = Lens.lens (\UpdateEndOfMeetingReminder' {enabled} -> enabled) (\s@UpdateEndOfMeetingReminder' {} a -> s {enabled = a} :: UpdateEndOfMeetingReminder)

instance Core.Hashable UpdateEndOfMeetingReminder

instance Core.NFData UpdateEndOfMeetingReminder

instance Core.ToJSON UpdateEndOfMeetingReminder where
  toJSON UpdateEndOfMeetingReminder' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ReminderType" Core..=) Core.<$> reminderType,
            ("ReminderAtMinutes" Core..=)
              Core.<$> reminderAtMinutes,
            ("Enabled" Core..=) Core.<$> enabled
          ]
      )
