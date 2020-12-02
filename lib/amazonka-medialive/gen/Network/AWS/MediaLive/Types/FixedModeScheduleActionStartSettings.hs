{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Start time for the action.
--
-- /See:/ 'fixedModeScheduleActionStartSettings' smart constructor.
newtype FixedModeScheduleActionStartSettings = FixedModeScheduleActionStartSettings'
  { _fmsassTime ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FixedModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmsassTime' - Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
fixedModeScheduleActionStartSettings ::
  -- | 'fmsassTime'
  Text ->
  FixedModeScheduleActionStartSettings
fixedModeScheduleActionStartSettings pTime_ =
  FixedModeScheduleActionStartSettings' {_fmsassTime = pTime_}

-- | Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
fmsassTime :: Lens' FixedModeScheduleActionStartSettings Text
fmsassTime = lens _fmsassTime (\s a -> s {_fmsassTime = a})

instance FromJSON FixedModeScheduleActionStartSettings where
  parseJSON =
    withObject
      "FixedModeScheduleActionStartSettings"
      (\x -> FixedModeScheduleActionStartSettings' <$> (x .: "time"))

instance Hashable FixedModeScheduleActionStartSettings

instance NFData FixedModeScheduleActionStartSettings

instance ToJSON FixedModeScheduleActionStartSettings where
  toJSON FixedModeScheduleActionStartSettings' {..} =
    object (catMaybes [Just ("time" .= _fmsassTime)])
