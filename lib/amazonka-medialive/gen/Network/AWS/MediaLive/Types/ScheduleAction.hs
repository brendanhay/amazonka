{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleAction where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ScheduleActionSettings
import Network.AWS.MediaLive.Types.ScheduleActionStartSettings
import Network.AWS.Prelude

-- | Contains information on a single schedule action.
--
-- /See:/ 'scheduleAction' smart constructor.
data ScheduleAction = ScheduleAction'
  { _saActionName :: !Text,
    _saScheduleActionStartSettings ::
      !ScheduleActionStartSettings,
    _saScheduleActionSettings :: !ScheduleActionSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saActionName' - The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
--
-- * 'saScheduleActionStartSettings' - The time for the action to start in the channel.
--
-- * 'saScheduleActionSettings' - Settings for this schedule action.
scheduleAction ::
  -- | 'saActionName'
  Text ->
  -- | 'saScheduleActionStartSettings'
  ScheduleActionStartSettings ->
  -- | 'saScheduleActionSettings'
  ScheduleActionSettings ->
  ScheduleAction
scheduleAction
  pActionName_
  pScheduleActionStartSettings_
  pScheduleActionSettings_ =
    ScheduleAction'
      { _saActionName = pActionName_,
        _saScheduleActionStartSettings = pScheduleActionStartSettings_,
        _saScheduleActionSettings = pScheduleActionSettings_
      }

-- | The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
saActionName :: Lens' ScheduleAction Text
saActionName = lens _saActionName (\s a -> s {_saActionName = a})

-- | The time for the action to start in the channel.
saScheduleActionStartSettings :: Lens' ScheduleAction ScheduleActionStartSettings
saScheduleActionStartSettings = lens _saScheduleActionStartSettings (\s a -> s {_saScheduleActionStartSettings = a})

-- | Settings for this schedule action.
saScheduleActionSettings :: Lens' ScheduleAction ScheduleActionSettings
saScheduleActionSettings = lens _saScheduleActionSettings (\s a -> s {_saScheduleActionSettings = a})

instance FromJSON ScheduleAction where
  parseJSON =
    withObject
      "ScheduleAction"
      ( \x ->
          ScheduleAction'
            <$> (x .: "actionName")
            <*> (x .: "scheduleActionStartSettings")
            <*> (x .: "scheduleActionSettings")
      )

instance Hashable ScheduleAction

instance NFData ScheduleAction

instance ToJSON ScheduleAction where
  toJSON ScheduleAction' {..} =
    object
      ( catMaybes
          [ Just ("actionName" .= _saActionName),
            Just
              ("scheduleActionStartSettings" .= _saScheduleActionStartSettings),
            Just ("scheduleActionSettings" .= _saScheduleActionSettings)
          ]
      )
