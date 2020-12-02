{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
--
--
-- /See:/ 'endOfMeetingReminder' smart constructor.
data EndOfMeetingReminder = EndOfMeetingReminder'
  { _eomrEnabled ::
      !(Maybe Bool),
    _eomrReminderAtMinutes :: !(Maybe (List1 Int)),
    _eomrReminderType ::
      !(Maybe EndOfMeetingReminderType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndOfMeetingReminder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eomrEnabled' - Whether an end of meeting reminder is enabled or not.
--
-- * 'eomrReminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
--
-- * 'eomrReminderType' - The type of sound that users hear during the end of meeting reminder.
endOfMeetingReminder ::
  EndOfMeetingReminder
endOfMeetingReminder =
  EndOfMeetingReminder'
    { _eomrEnabled = Nothing,
      _eomrReminderAtMinutes = Nothing,
      _eomrReminderType = Nothing
    }

-- | Whether an end of meeting reminder is enabled or not.
eomrEnabled :: Lens' EndOfMeetingReminder (Maybe Bool)
eomrEnabled = lens _eomrEnabled (\s a -> s {_eomrEnabled = a})

-- | A range of 3 to 15 minutes that determines when the reminder begins.
eomrReminderAtMinutes :: Lens' EndOfMeetingReminder (Maybe (NonEmpty Int))
eomrReminderAtMinutes = lens _eomrReminderAtMinutes (\s a -> s {_eomrReminderAtMinutes = a}) . mapping _List1

-- | The type of sound that users hear during the end of meeting reminder.
eomrReminderType :: Lens' EndOfMeetingReminder (Maybe EndOfMeetingReminderType)
eomrReminderType = lens _eomrReminderType (\s a -> s {_eomrReminderType = a})

instance FromJSON EndOfMeetingReminder where
  parseJSON =
    withObject
      "EndOfMeetingReminder"
      ( \x ->
          EndOfMeetingReminder'
            <$> (x .:? "Enabled")
            <*> (x .:? "ReminderAtMinutes")
            <*> (x .:? "ReminderType")
      )

instance Hashable EndOfMeetingReminder

instance NFData EndOfMeetingReminder
