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

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /See:/ 'mkUpdateEndOfMeetingReminder' smart constructor.
data UpdateEndOfMeetingReminder = UpdateEndOfMeetingReminder'
  { enabled ::
      Lude.Maybe Lude.Bool,
    reminderAtMinutes ::
      Lude.Maybe (Lude.NonEmpty Lude.Int),
    reminderType ::
      Lude.Maybe EndOfMeetingReminderType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndOfMeetingReminder' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether an end of meeting reminder is enabled or not.
-- * 'reminderAtMinutes' - Updates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
-- * 'reminderType' - The type of sound that users hear during the end of meeting reminder.
mkUpdateEndOfMeetingReminder ::
  UpdateEndOfMeetingReminder
mkUpdateEndOfMeetingReminder =
  UpdateEndOfMeetingReminder'
    { enabled = Lude.Nothing,
      reminderAtMinutes = Lude.Nothing,
      reminderType = Lude.Nothing
    }

-- | Whether an end of meeting reminder is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueomrEnabled :: Lens.Lens' UpdateEndOfMeetingReminder (Lude.Maybe Lude.Bool)
ueomrEnabled = Lens.lens (enabled :: UpdateEndOfMeetingReminder -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateEndOfMeetingReminder)
{-# DEPRECATED ueomrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Updates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /Note:/ Consider using 'reminderAtMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueomrReminderAtMinutes :: Lens.Lens' UpdateEndOfMeetingReminder (Lude.Maybe (Lude.NonEmpty Lude.Int))
ueomrReminderAtMinutes = Lens.lens (reminderAtMinutes :: UpdateEndOfMeetingReminder -> Lude.Maybe (Lude.NonEmpty Lude.Int)) (\s a -> s {reminderAtMinutes = a} :: UpdateEndOfMeetingReminder)
{-# DEPRECATED ueomrReminderAtMinutes "Use generic-lens or generic-optics with 'reminderAtMinutes' instead." #-}

-- | The type of sound that users hear during the end of meeting reminder.
--
-- /Note:/ Consider using 'reminderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueomrReminderType :: Lens.Lens' UpdateEndOfMeetingReminder (Lude.Maybe EndOfMeetingReminderType)
ueomrReminderType = Lens.lens (reminderType :: UpdateEndOfMeetingReminder -> Lude.Maybe EndOfMeetingReminderType) (\s a -> s {reminderType = a} :: UpdateEndOfMeetingReminder)
{-# DEPRECATED ueomrReminderType "Use generic-lens or generic-optics with 'reminderType' instead." #-}

instance Lude.ToJSON UpdateEndOfMeetingReminder where
  toJSON UpdateEndOfMeetingReminder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("ReminderAtMinutes" Lude..=) Lude.<$> reminderAtMinutes,
            ("ReminderType" Lude..=) Lude.<$> reminderType
          ]
      )
