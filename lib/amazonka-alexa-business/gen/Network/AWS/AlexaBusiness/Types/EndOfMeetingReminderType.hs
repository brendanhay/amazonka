{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType where

import Network.AWS.Prelude

data EndOfMeetingReminderType
  = EOMRTAnnouncementTimeCheck
  | EOMRTAnnouncementVariableTimeLeft
  | EOMRTChime
  | EOMRTKnock
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText EndOfMeetingReminderType where
  parser =
    takeLowerText >>= \case
      "announcement_time_check" -> pure EOMRTAnnouncementTimeCheck
      "announcement_variable_time_left" -> pure EOMRTAnnouncementVariableTimeLeft
      "chime" -> pure EOMRTChime
      "knock" -> pure EOMRTKnock
      e ->
        fromTextError $
          "Failure parsing EndOfMeetingReminderType from value: '" <> e
            <> "'. Accepted values: announcement_time_check, announcement_variable_time_left, chime, knock"

instance ToText EndOfMeetingReminderType where
  toText = \case
    EOMRTAnnouncementTimeCheck -> "ANNOUNCEMENT_TIME_CHECK"
    EOMRTAnnouncementVariableTimeLeft -> "ANNOUNCEMENT_VARIABLE_TIME_LEFT"
    EOMRTChime -> "CHIME"
    EOMRTKnock -> "KNOCK"

instance Hashable EndOfMeetingReminderType

instance NFData EndOfMeetingReminderType

instance ToByteString EndOfMeetingReminderType

instance ToQuery EndOfMeetingReminderType

instance ToHeader EndOfMeetingReminderType

instance ToJSON EndOfMeetingReminderType where
  toJSON = toJSONText

instance FromJSON EndOfMeetingReminderType where
  parseJSON = parseJSONText "EndOfMeetingReminderType"
