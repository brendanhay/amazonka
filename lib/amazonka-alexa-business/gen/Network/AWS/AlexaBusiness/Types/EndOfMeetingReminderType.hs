{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
  ( EndOfMeetingReminderType
    ( EndOfMeetingReminderType'
    , EndOfMeetingReminderTypeAnnouncementTimeCheck
    , EndOfMeetingReminderTypeAnnouncementVariableTimeLeft
    , EndOfMeetingReminderTypeChime
    , EndOfMeetingReminderTypeKnock
    , fromEndOfMeetingReminderType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EndOfMeetingReminderType = EndOfMeetingReminderType'{fromEndOfMeetingReminderType
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern EndOfMeetingReminderTypeAnnouncementTimeCheck :: EndOfMeetingReminderType
pattern EndOfMeetingReminderTypeAnnouncementTimeCheck = EndOfMeetingReminderType' "ANNOUNCEMENT_TIME_CHECK"

pattern EndOfMeetingReminderTypeAnnouncementVariableTimeLeft :: EndOfMeetingReminderType
pattern EndOfMeetingReminderTypeAnnouncementVariableTimeLeft = EndOfMeetingReminderType' "ANNOUNCEMENT_VARIABLE_TIME_LEFT"

pattern EndOfMeetingReminderTypeChime :: EndOfMeetingReminderType
pattern EndOfMeetingReminderTypeChime = EndOfMeetingReminderType' "CHIME"

pattern EndOfMeetingReminderTypeKnock :: EndOfMeetingReminderType
pattern EndOfMeetingReminderTypeKnock = EndOfMeetingReminderType' "KNOCK"

{-# COMPLETE 
  EndOfMeetingReminderTypeAnnouncementTimeCheck,

  EndOfMeetingReminderTypeAnnouncementVariableTimeLeft,

  EndOfMeetingReminderTypeChime,

  EndOfMeetingReminderTypeKnock,
  EndOfMeetingReminderType'
  #-}
