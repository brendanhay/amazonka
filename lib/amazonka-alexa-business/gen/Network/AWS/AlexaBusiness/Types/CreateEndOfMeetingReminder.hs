{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
  ( CreateEndOfMeetingReminder (..),

    -- * Smart constructor
    mkCreateEndOfMeetingReminder,

    -- * Lenses
    ceomrReminderAtMinutes,
    ceomrReminderType,
    ceomrEnabled,
  )
where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Creates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /See:/ 'mkCreateEndOfMeetingReminder' smart constructor.
data CreateEndOfMeetingReminder = CreateEndOfMeetingReminder'
  { reminderAtMinutes ::
      Lude.NonEmpty Lude.Int,
    reminderType ::
      EndOfMeetingReminderType,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEndOfMeetingReminder' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether an end of meeting reminder is enabled or not.
-- * 'reminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
-- * 'reminderType' - The type of sound that users hear during the end of meeting reminder.
mkCreateEndOfMeetingReminder ::
  -- | 'reminderAtMinutes'
  Lude.NonEmpty Lude.Int ->
  -- | 'reminderType'
  EndOfMeetingReminderType ->
  -- | 'enabled'
  Lude.Bool ->
  CreateEndOfMeetingReminder
mkCreateEndOfMeetingReminder
  pReminderAtMinutes_
  pReminderType_
  pEnabled_ =
    CreateEndOfMeetingReminder'
      { reminderAtMinutes =
          pReminderAtMinutes_,
        reminderType = pReminderType_,
        enabled = pEnabled_
      }

-- | A range of 3 to 15 minutes that determines when the reminder begins.
--
-- /Note:/ Consider using 'reminderAtMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceomrReminderAtMinutes :: Lens.Lens' CreateEndOfMeetingReminder (Lude.NonEmpty Lude.Int)
ceomrReminderAtMinutes = Lens.lens (reminderAtMinutes :: CreateEndOfMeetingReminder -> Lude.NonEmpty Lude.Int) (\s a -> s {reminderAtMinutes = a} :: CreateEndOfMeetingReminder)
{-# DEPRECATED ceomrReminderAtMinutes "Use generic-lens or generic-optics with 'reminderAtMinutes' instead." #-}

-- | The type of sound that users hear during the end of meeting reminder.
--
-- /Note:/ Consider using 'reminderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceomrReminderType :: Lens.Lens' CreateEndOfMeetingReminder EndOfMeetingReminderType
ceomrReminderType = Lens.lens (reminderType :: CreateEndOfMeetingReminder -> EndOfMeetingReminderType) (\s a -> s {reminderType = a} :: CreateEndOfMeetingReminder)
{-# DEPRECATED ceomrReminderType "Use generic-lens or generic-optics with 'reminderType' instead." #-}

-- | Whether an end of meeting reminder is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceomrEnabled :: Lens.Lens' CreateEndOfMeetingReminder Lude.Bool
ceomrEnabled = Lens.lens (enabled :: CreateEndOfMeetingReminder -> Lude.Bool) (\s a -> s {enabled = a} :: CreateEndOfMeetingReminder)
{-# DEPRECATED ceomrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON CreateEndOfMeetingReminder where
  toJSON CreateEndOfMeetingReminder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReminderAtMinutes" Lude..= reminderAtMinutes),
            Lude.Just ("ReminderType" Lude..= reminderType),
            Lude.Just ("Enabled" Lude..= enabled)
          ]
      )
