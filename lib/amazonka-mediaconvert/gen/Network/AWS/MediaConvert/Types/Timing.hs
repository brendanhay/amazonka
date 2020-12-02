{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Timing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Timing where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- /See:/ 'timing' smart constructor.
data Timing = Timing'
  { _tStartTime :: !(Maybe POSIX),
    _tFinishTime :: !(Maybe POSIX),
    _tSubmitTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Timing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStartTime' - The time, in Unix epoch format, that transcoding for the job began.
--
-- * 'tFinishTime' - The time, in Unix epoch format, that the transcoding job finished
--
-- * 'tSubmitTime' - The time, in Unix epoch format, that you submitted the job.
timing ::
  Timing
timing =
  Timing'
    { _tStartTime = Nothing,
      _tFinishTime = Nothing,
      _tSubmitTime = Nothing
    }

-- | The time, in Unix epoch format, that transcoding for the job began.
tStartTime :: Lens' Timing (Maybe UTCTime)
tStartTime = lens _tStartTime (\s a -> s {_tStartTime = a}) . mapping _Time

-- | The time, in Unix epoch format, that the transcoding job finished
tFinishTime :: Lens' Timing (Maybe UTCTime)
tFinishTime = lens _tFinishTime (\s a -> s {_tFinishTime = a}) . mapping _Time

-- | The time, in Unix epoch format, that you submitted the job.
tSubmitTime :: Lens' Timing (Maybe UTCTime)
tSubmitTime = lens _tSubmitTime (\s a -> s {_tSubmitTime = a}) . mapping _Time

instance FromJSON Timing where
  parseJSON =
    withObject
      "Timing"
      ( \x ->
          Timing'
            <$> (x .:? "startTime")
            <*> (x .:? "finishTime")
            <*> (x .:? "submitTime")
      )

instance Hashable Timing

instance NFData Timing
