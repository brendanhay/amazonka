{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
--
--
-- /See:/ 'updateEndOfMeetingReminder' smart constructor.
data UpdateEndOfMeetingReminder = UpdateEndOfMeetingReminder'
  { _ueomrEnabled ::
      !(Maybe Bool),
    _ueomrReminderAtMinutes ::
      !(Maybe (List1 Int)),
    _ueomrReminderType ::
      !(Maybe EndOfMeetingReminderType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateEndOfMeetingReminder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueomrEnabled' - Whether an end of meeting reminder is enabled or not.
--
-- * 'ueomrReminderAtMinutes' - Updates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- * 'ueomrReminderType' - The type of sound that users hear during the end of meeting reminder.
updateEndOfMeetingReminder ::
  UpdateEndOfMeetingReminder
updateEndOfMeetingReminder =
  UpdateEndOfMeetingReminder'
    { _ueomrEnabled = Nothing,
      _ueomrReminderAtMinutes = Nothing,
      _ueomrReminderType = Nothing
    }

-- | Whether an end of meeting reminder is enabled or not.
ueomrEnabled :: Lens' UpdateEndOfMeetingReminder (Maybe Bool)
ueomrEnabled = lens _ueomrEnabled (\s a -> s {_ueomrEnabled = a})

-- | Updates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
ueomrReminderAtMinutes :: Lens' UpdateEndOfMeetingReminder (Maybe (NonEmpty Int))
ueomrReminderAtMinutes = lens _ueomrReminderAtMinutes (\s a -> s {_ueomrReminderAtMinutes = a}) . mapping _List1

-- | The type of sound that users hear during the end of meeting reminder.
ueomrReminderType :: Lens' UpdateEndOfMeetingReminder (Maybe EndOfMeetingReminderType)
ueomrReminderType = lens _ueomrReminderType (\s a -> s {_ueomrReminderType = a})

instance Hashable UpdateEndOfMeetingReminder

instance NFData UpdateEndOfMeetingReminder

instance ToJSON UpdateEndOfMeetingReminder where
  toJSON UpdateEndOfMeetingReminder' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _ueomrEnabled,
            ("ReminderAtMinutes" .=) <$> _ueomrReminderAtMinutes,
            ("ReminderType" .=) <$> _ueomrReminderType
          ]
      )
