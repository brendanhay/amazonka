{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
  ( EndOfMeetingReminder (..),

    -- * Smart constructor
    mkEndOfMeetingReminder,

    -- * Lenses
    eomrEnabled,
    eomrReminderAtMinutes,
    eomrReminderType,
  )
where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /See:/ 'mkEndOfMeetingReminder' smart constructor.
data EndOfMeetingReminder = EndOfMeetingReminder'
  { -- | Whether an end of meeting reminder is enabled or not.
    enabled :: Lude.Maybe Lude.Bool,
    -- | A range of 3 to 15 minutes that determines when the reminder begins.
    reminderAtMinutes :: Lude.Maybe (Lude.NonEmpty Lude.Int),
    -- | The type of sound that users hear during the end of meeting reminder.
    reminderType :: Lude.Maybe EndOfMeetingReminderType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndOfMeetingReminder' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether an end of meeting reminder is enabled or not.
-- * 'reminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
-- * 'reminderType' - The type of sound that users hear during the end of meeting reminder.
mkEndOfMeetingReminder ::
  EndOfMeetingReminder
mkEndOfMeetingReminder =
  EndOfMeetingReminder'
    { enabled = Lude.Nothing,
      reminderAtMinutes = Lude.Nothing,
      reminderType = Lude.Nothing
    }

-- | Whether an end of meeting reminder is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eomrEnabled :: Lens.Lens' EndOfMeetingReminder (Lude.Maybe Lude.Bool)
eomrEnabled = Lens.lens (enabled :: EndOfMeetingReminder -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EndOfMeetingReminder)
{-# DEPRECATED eomrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A range of 3 to 15 minutes that determines when the reminder begins.
--
-- /Note:/ Consider using 'reminderAtMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eomrReminderAtMinutes :: Lens.Lens' EndOfMeetingReminder (Lude.Maybe (Lude.NonEmpty Lude.Int))
eomrReminderAtMinutes = Lens.lens (reminderAtMinutes :: EndOfMeetingReminder -> Lude.Maybe (Lude.NonEmpty Lude.Int)) (\s a -> s {reminderAtMinutes = a} :: EndOfMeetingReminder)
{-# DEPRECATED eomrReminderAtMinutes "Use generic-lens or generic-optics with 'reminderAtMinutes' instead." #-}

-- | The type of sound that users hear during the end of meeting reminder.
--
-- /Note:/ Consider using 'reminderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eomrReminderType :: Lens.Lens' EndOfMeetingReminder (Lude.Maybe EndOfMeetingReminderType)
eomrReminderType = Lens.lens (reminderType :: EndOfMeetingReminder -> Lude.Maybe EndOfMeetingReminderType) (\s a -> s {reminderType = a} :: EndOfMeetingReminder)
{-# DEPRECATED eomrReminderType "Use generic-lens or generic-optics with 'reminderType' instead." #-}

instance Lude.FromJSON EndOfMeetingReminder where
  parseJSON =
    Lude.withObject
      "EndOfMeetingReminder"
      ( \x ->
          EndOfMeetingReminder'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "ReminderAtMinutes")
            Lude.<*> (x Lude..:? "ReminderType")
      )
