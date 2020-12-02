{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Creates settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
--
--
-- /See:/ 'createEndOfMeetingReminder' smart constructor.
data CreateEndOfMeetingReminder = CreateEndOfMeetingReminder'
  { _ceomrReminderAtMinutes ::
      !(List1 Int),
    _ceomrReminderType ::
      !EndOfMeetingReminderType,
    _ceomrEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEndOfMeetingReminder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceomrReminderAtMinutes' - A range of 3 to 15 minutes that determines when the reminder begins.
--
-- * 'ceomrReminderType' - The type of sound that users hear during the end of meeting reminder.
--
-- * 'ceomrEnabled' - Whether an end of meeting reminder is enabled or not.
createEndOfMeetingReminder ::
  -- | 'ceomrReminderAtMinutes'
  NonEmpty Int ->
  -- | 'ceomrReminderType'
  EndOfMeetingReminderType ->
  -- | 'ceomrEnabled'
  Bool ->
  CreateEndOfMeetingReminder
createEndOfMeetingReminder
  pReminderAtMinutes_
  pReminderType_
  pEnabled_ =
    CreateEndOfMeetingReminder'
      { _ceomrReminderAtMinutes =
          _List1 # pReminderAtMinutes_,
        _ceomrReminderType = pReminderType_,
        _ceomrEnabled = pEnabled_
      }

-- | A range of 3 to 15 minutes that determines when the reminder begins.
ceomrReminderAtMinutes :: Lens' CreateEndOfMeetingReminder (NonEmpty Int)
ceomrReminderAtMinutes = lens _ceomrReminderAtMinutes (\s a -> s {_ceomrReminderAtMinutes = a}) . _List1

-- | The type of sound that users hear during the end of meeting reminder.
ceomrReminderType :: Lens' CreateEndOfMeetingReminder EndOfMeetingReminderType
ceomrReminderType = lens _ceomrReminderType (\s a -> s {_ceomrReminderType = a})

-- | Whether an end of meeting reminder is enabled or not.
ceomrEnabled :: Lens' CreateEndOfMeetingReminder Bool
ceomrEnabled = lens _ceomrEnabled (\s a -> s {_ceomrEnabled = a})

instance Hashable CreateEndOfMeetingReminder

instance NFData CreateEndOfMeetingReminder

instance ToJSON CreateEndOfMeetingReminder where
  toJSON CreateEndOfMeetingReminder' {..} =
    object
      ( catMaybes
          [ Just ("ReminderAtMinutes" .= _ceomrReminderAtMinutes),
            Just ("ReminderType" .= _ceomrReminderType),
            Just ("Enabled" .= _ceomrEnabled)
          ]
      )
