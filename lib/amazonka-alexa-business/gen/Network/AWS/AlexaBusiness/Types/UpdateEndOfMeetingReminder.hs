{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
  ( UpdateEndOfMeetingReminder (..),

    -- * Smart constructor
    mkUpdateEndOfMeetingReminder,

    -- * Lenses
    ueomrEnabled,
    ueomrReminderAtMinutes,
    ueomrReminderType,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /See:/ 'mkUpdateEndOfMeetingReminder' smart constructor.
data UpdateEndOfMeetingReminder = UpdateEndOfMeetingReminder'
  { -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Core.Maybe Core.Bool,
    -- | Updates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
    reminderAtMinutes :: Core.Maybe (Core.NonEmpty Core.Int),
    -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Core.Maybe Types.EndOfMeetingReminderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndOfMeetingReminder' value with any optional fields omitted.
mkUpdateEndOfMeetingReminder ::
  UpdateEndOfMeetingReminder
mkUpdateEndOfMeetingReminder =
  UpdateEndOfMeetingReminder'
    { enabled = Core.Nothing,
      reminderAtMinutes = Core.Nothing,
      reminderType = Core.Nothing
    }

-- | Whether an end of meeting reminder is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueomrEnabled :: Lens.Lens' UpdateEndOfMeetingReminder (Core.Maybe Core.Bool)
ueomrEnabled = Lens.field @"enabled"
{-# DEPRECATED ueomrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Updates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /Note:/ Consider using 'reminderAtMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueomrReminderAtMinutes :: Lens.Lens' UpdateEndOfMeetingReminder (Core.Maybe (Core.NonEmpty Core.Int))
ueomrReminderAtMinutes = Lens.field @"reminderAtMinutes"
{-# DEPRECATED ueomrReminderAtMinutes "Use generic-lens or generic-optics with 'reminderAtMinutes' instead." #-}

-- | The type of sound that users hear during the end of meeting reminder.
--
-- /Note:/ Consider using 'reminderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueomrReminderType :: Lens.Lens' UpdateEndOfMeetingReminder (Core.Maybe Types.EndOfMeetingReminderType)
ueomrReminderType = Lens.field @"reminderType"
{-# DEPRECATED ueomrReminderType "Use generic-lens or generic-optics with 'reminderType' instead." #-}

instance Core.FromJSON UpdateEndOfMeetingReminder where
  toJSON UpdateEndOfMeetingReminder {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("ReminderAtMinutes" Core..=) Core.<$> reminderAtMinutes,
            ("ReminderType" Core..=) Core.<$> reminderType
          ]
      )
