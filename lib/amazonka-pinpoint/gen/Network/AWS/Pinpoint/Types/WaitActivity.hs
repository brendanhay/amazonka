{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WaitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WaitActivity where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.WaitTime
import Network.AWS.Prelude

-- | Specifies the settings for a wait activity in a journey. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
--
--
-- /See:/ 'waitActivity' smart constructor.
data WaitActivity = WaitActivity'
  { _waNextActivity :: !(Maybe Text),
    _waWaitTime :: !(Maybe WaitTime)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WaitActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waNextActivity' - The unique identifier for the next activity to perform, after performing the wait activity.
--
-- * 'waWaitTime' - The amount of time to wait or the date and time when the activity moves participants to the next activity in the journey.
waitActivity ::
  WaitActivity
waitActivity =
  WaitActivity' {_waNextActivity = Nothing, _waWaitTime = Nothing}

-- | The unique identifier for the next activity to perform, after performing the wait activity.
waNextActivity :: Lens' WaitActivity (Maybe Text)
waNextActivity = lens _waNextActivity (\s a -> s {_waNextActivity = a})

-- | The amount of time to wait or the date and time when the activity moves participants to the next activity in the journey.
waWaitTime :: Lens' WaitActivity (Maybe WaitTime)
waWaitTime = lens _waWaitTime (\s a -> s {_waWaitTime = a})

instance FromJSON WaitActivity where
  parseJSON =
    withObject
      "WaitActivity"
      ( \x ->
          WaitActivity' <$> (x .:? "NextActivity") <*> (x .:? "WaitTime")
      )

instance Hashable WaitActivity

instance NFData WaitActivity

instance ToJSON WaitActivity where
  toJSON WaitActivity' {..} =
    object
      ( catMaybes
          [ ("NextActivity" .=) <$> _waNextActivity,
            ("WaitTime" .=) <$> _waWaitTime
          ]
      )
