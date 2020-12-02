{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.QuietTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.QuietTime where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the start and end times that define a time range when messages aren't sent to endpoints.
--
--
--
-- /See:/ 'quietTime' smart constructor.
data QuietTime = QuietTime'
  { _qtStart :: !(Maybe Text),
    _qtEnd :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QuietTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qtStart' - The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- * 'qtEnd' - The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
quietTime ::
  QuietTime
quietTime = QuietTime' {_qtStart = Nothing, _qtEnd = Nothing}

-- | The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
qtStart :: Lens' QuietTime (Maybe Text)
qtStart = lens _qtStart (\s a -> s {_qtStart = a})

-- | The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
qtEnd :: Lens' QuietTime (Maybe Text)
qtEnd = lens _qtEnd (\s a -> s {_qtEnd = a})

instance FromJSON QuietTime where
  parseJSON =
    withObject
      "QuietTime"
      (\x -> QuietTime' <$> (x .:? "Start") <*> (x .:? "End"))

instance Hashable QuietTime

instance NFData QuietTime

instance ToJSON QuietTime where
  toJSON QuietTime' {..} =
    object
      (catMaybes [("Start" .=) <$> _qtStart, ("End" .=) <$> _qtEnd])
