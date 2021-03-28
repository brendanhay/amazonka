{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
  ( CreateEndOfMeetingReminder (..)
  -- * Smart constructor
  , mkCreateEndOfMeetingReminder
  -- * Lenses
  , ceomrReminderAtMinutes
  , ceomrReminderType
  , ceomrEnabled
  ) where

import qualified Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Creates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- /See:/ 'mkCreateEndOfMeetingReminder' smart constructor.
data CreateEndOfMeetingReminder = CreateEndOfMeetingReminder'
  { reminderAtMinutes :: Core.NonEmpty Core.Int
    -- ^ A range of 3 to 15 minutes that determines when the reminder begins.
  , reminderType :: Types.EndOfMeetingReminderType
    -- ^ The type of sound that users hear during the end of meeting reminder. 
  , enabled :: Core.Bool
    -- ^ Whether an end of meeting reminder is enabled or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEndOfMeetingReminder' value with any optional fields omitted.
mkCreateEndOfMeetingReminder
    :: Core.NonEmpty Core.Int -- ^ 'reminderAtMinutes'
    -> Types.EndOfMeetingReminderType -- ^ 'reminderType'
    -> Core.Bool -- ^ 'enabled'
    -> CreateEndOfMeetingReminder
mkCreateEndOfMeetingReminder reminderAtMinutes reminderType enabled
  = CreateEndOfMeetingReminder'{reminderAtMinutes, reminderType,
                                enabled}

-- | A range of 3 to 15 minutes that determines when the reminder begins.
--
-- /Note:/ Consider using 'reminderAtMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceomrReminderAtMinutes :: Lens.Lens' CreateEndOfMeetingReminder (Core.NonEmpty Core.Int)
ceomrReminderAtMinutes = Lens.field @"reminderAtMinutes"
{-# INLINEABLE ceomrReminderAtMinutes #-}
{-# DEPRECATED reminderAtMinutes "Use generic-lens or generic-optics with 'reminderAtMinutes' instead"  #-}

-- | The type of sound that users hear during the end of meeting reminder. 
--
-- /Note:/ Consider using 'reminderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceomrReminderType :: Lens.Lens' CreateEndOfMeetingReminder Types.EndOfMeetingReminderType
ceomrReminderType = Lens.field @"reminderType"
{-# INLINEABLE ceomrReminderType #-}
{-# DEPRECATED reminderType "Use generic-lens or generic-optics with 'reminderType' instead"  #-}

-- | Whether an end of meeting reminder is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceomrEnabled :: Lens.Lens' CreateEndOfMeetingReminder Core.Bool
ceomrEnabled = Lens.field @"enabled"
{-# INLINEABLE ceomrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON CreateEndOfMeetingReminder where
        toJSON CreateEndOfMeetingReminder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReminderAtMinutes" Core..= reminderAtMinutes),
                  Core.Just ("ReminderType" Core..= reminderType),
                  Core.Just ("Enabled" Core..= enabled)])
