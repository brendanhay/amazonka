{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
  ( EndOfMeetingReminderType
      ( EndOfMeetingReminderType',
        EOMRTAnnouncementTimeCheck,
        EOMRTAnnouncementVariableTimeLeft,
        EOMRTChime,
        EOMRTKnock
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EndOfMeetingReminderType = EndOfMeetingReminderType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern EOMRTAnnouncementTimeCheck :: EndOfMeetingReminderType
pattern EOMRTAnnouncementTimeCheck = EndOfMeetingReminderType' "ANNOUNCEMENT_TIME_CHECK"

pattern EOMRTAnnouncementVariableTimeLeft :: EndOfMeetingReminderType
pattern EOMRTAnnouncementVariableTimeLeft = EndOfMeetingReminderType' "ANNOUNCEMENT_VARIABLE_TIME_LEFT"

pattern EOMRTChime :: EndOfMeetingReminderType
pattern EOMRTChime = EndOfMeetingReminderType' "CHIME"

pattern EOMRTKnock :: EndOfMeetingReminderType
pattern EOMRTKnock = EndOfMeetingReminderType' "KNOCK"

{-# COMPLETE
  EOMRTAnnouncementTimeCheck,
  EOMRTAnnouncementVariableTimeLeft,
  EOMRTChime,
  EOMRTKnock,
  EndOfMeetingReminderType'
  #-}
