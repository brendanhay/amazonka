{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
  ( EndOfMeetingReminder (..)
  -- * Smart constructor
  , mkEndOfMeetingReminder
  -- * Lenses
  , eomrEnabled
  , eomrReminderAtMinutes
  , eomrReminderType
  ) where

import qualified Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending. 
--
-- /See:/ 'mkEndOfMeetingReminder' smart constructor.
data EndOfMeetingReminder = EndOfMeetingReminder'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Whether an end of meeting reminder is enabled or not.
  , reminderAtMinutes :: Core.Maybe (Core.NonEmpty Core.Int)
    -- ^ A range of 3 to 15 minutes that determines when the reminder begins.
  , reminderType :: Core.Maybe Types.EndOfMeetingReminderType
    -- ^ The type of sound that users hear during the end of meeting reminder. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndOfMeetingReminder' value with any optional fields omitted.
mkEndOfMeetingReminder
    :: EndOfMeetingReminder
mkEndOfMeetingReminder
  = EndOfMeetingReminder'{enabled = Core.Nothing,
                          reminderAtMinutes = Core.Nothing, reminderType = Core.Nothing}

-- | Whether an end of meeting reminder is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eomrEnabled :: Lens.Lens' EndOfMeetingReminder (Core.Maybe Core.Bool)
eomrEnabled = Lens.field @"enabled"
{-# INLINEABLE eomrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | A range of 3 to 15 minutes that determines when the reminder begins.
--
-- /Note:/ Consider using 'reminderAtMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eomrReminderAtMinutes :: Lens.Lens' EndOfMeetingReminder (Core.Maybe (Core.NonEmpty Core.Int))
eomrReminderAtMinutes = Lens.field @"reminderAtMinutes"
{-# INLINEABLE eomrReminderAtMinutes #-}
{-# DEPRECATED reminderAtMinutes "Use generic-lens or generic-optics with 'reminderAtMinutes' instead"  #-}

-- | The type of sound that users hear during the end of meeting reminder. 
--
-- /Note:/ Consider using 'reminderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eomrReminderType :: Lens.Lens' EndOfMeetingReminder (Core.Maybe Types.EndOfMeetingReminderType)
eomrReminderType = Lens.field @"reminderType"
{-# INLINEABLE eomrReminderType #-}
{-# DEPRECATED reminderType "Use generic-lens or generic-optics with 'reminderType' instead"  #-}

instance Core.FromJSON EndOfMeetingReminder where
        parseJSON
          = Core.withObject "EndOfMeetingReminder" Core.$
              \ x ->
                EndOfMeetingReminder' Core.<$>
                  (x Core..:? "Enabled") Core.<*> x Core..:? "ReminderAtMinutes"
                    Core.<*> x Core..:? "ReminderType"
