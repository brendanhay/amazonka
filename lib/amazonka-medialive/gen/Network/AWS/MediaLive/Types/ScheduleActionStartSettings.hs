{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleActionStartSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import Network.AWS.Prelude

-- | Settings to specify when an action should occur. Only one of the options must be selected.
--
-- /See:/ 'scheduleActionStartSettings' smart constructor.
data ScheduleActionStartSettings = ScheduleActionStartSettings'
  { _sassImmediateModeScheduleActionStartSettings ::
      !( Maybe
           ImmediateModeScheduleActionStartSettings
       ),
    _sassFollowModeScheduleActionStartSettings ::
      !( Maybe
           FollowModeScheduleActionStartSettings
       ),
    _sassFixedModeScheduleActionStartSettings ::
      !( Maybe
           FixedModeScheduleActionStartSettings
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sassImmediateModeScheduleActionStartSettings' - Option for specifying an action that should be applied immediately.
--
-- * 'sassFollowModeScheduleActionStartSettings' - Option for specifying an action as relative to another action.
--
-- * 'sassFixedModeScheduleActionStartSettings' - Option for specifying the start time for an action.
scheduleActionStartSettings ::
  ScheduleActionStartSettings
scheduleActionStartSettings =
  ScheduleActionStartSettings'
    { _sassImmediateModeScheduleActionStartSettings =
        Nothing,
      _sassFollowModeScheduleActionStartSettings = Nothing,
      _sassFixedModeScheduleActionStartSettings = Nothing
    }

-- | Option for specifying an action that should be applied immediately.
sassImmediateModeScheduleActionStartSettings :: Lens' ScheduleActionStartSettings (Maybe ImmediateModeScheduleActionStartSettings)
sassImmediateModeScheduleActionStartSettings = lens _sassImmediateModeScheduleActionStartSettings (\s a -> s {_sassImmediateModeScheduleActionStartSettings = a})

-- | Option for specifying an action as relative to another action.
sassFollowModeScheduleActionStartSettings :: Lens' ScheduleActionStartSettings (Maybe FollowModeScheduleActionStartSettings)
sassFollowModeScheduleActionStartSettings = lens _sassFollowModeScheduleActionStartSettings (\s a -> s {_sassFollowModeScheduleActionStartSettings = a})

-- | Option for specifying the start time for an action.
sassFixedModeScheduleActionStartSettings :: Lens' ScheduleActionStartSettings (Maybe FixedModeScheduleActionStartSettings)
sassFixedModeScheduleActionStartSettings = lens _sassFixedModeScheduleActionStartSettings (\s a -> s {_sassFixedModeScheduleActionStartSettings = a})

instance FromJSON ScheduleActionStartSettings where
  parseJSON =
    withObject
      "ScheduleActionStartSettings"
      ( \x ->
          ScheduleActionStartSettings'
            <$> (x .:? "immediateModeScheduleActionStartSettings")
            <*> (x .:? "followModeScheduleActionStartSettings")
            <*> (x .:? "fixedModeScheduleActionStartSettings")
      )

instance Hashable ScheduleActionStartSettings

instance NFData ScheduleActionStartSettings

instance ToJSON ScheduleActionStartSettings where
  toJSON ScheduleActionStartSettings' {..} =
    object
      ( catMaybes
          [ ("immediateModeScheduleActionStartSettings" .=)
              <$> _sassImmediateModeScheduleActionStartSettings,
            ("followModeScheduleActionStartSettings" .=)
              <$> _sassFollowModeScheduleActionStartSettings,
            ("fixedModeScheduleActionStartSettings" .=)
              <$> _sassFixedModeScheduleActionStartSettings
          ]
      )
