{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WaitTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WaitTime where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a duration or a date and time that indicates when Amazon Pinpoint determines whether an activity's conditions have been met or an activity moves participants to the next activity in a journey.
--
--
--
-- /See:/ 'waitTime' smart constructor.
data WaitTime = WaitTime'
  { _wtWaitFor :: !(Maybe Text),
    _wtWaitUntil :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WaitTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtWaitFor' - The amount of time to wait, as a duration in ISO 8601 format, before determining whether the activity's conditions have been met or moving participants to the next activity in the journey.
--
-- * 'wtWaitUntil' - The date and time, in ISO 8601 format, when Amazon Pinpoint determines whether the activity's conditions have been met or the activity moves participants to the next activity in the journey.
waitTime ::
  WaitTime
waitTime = WaitTime' {_wtWaitFor = Nothing, _wtWaitUntil = Nothing}

-- | The amount of time to wait, as a duration in ISO 8601 format, before determining whether the activity's conditions have been met or moving participants to the next activity in the journey.
wtWaitFor :: Lens' WaitTime (Maybe Text)
wtWaitFor = lens _wtWaitFor (\s a -> s {_wtWaitFor = a})

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines whether the activity's conditions have been met or the activity moves participants to the next activity in the journey.
wtWaitUntil :: Lens' WaitTime (Maybe Text)
wtWaitUntil = lens _wtWaitUntil (\s a -> s {_wtWaitUntil = a})

instance FromJSON WaitTime where
  parseJSON =
    withObject
      "WaitTime"
      (\x -> WaitTime' <$> (x .:? "WaitFor") <*> (x .:? "WaitUntil"))

instance Hashable WaitTime

instance NFData WaitTime

instance ToJSON WaitTime where
  toJSON WaitTime' {..} =
    object
      ( catMaybes
          [("WaitFor" .=) <$> _wtWaitFor, ("WaitUntil" .=) <$> _wtWaitUntil]
      )
