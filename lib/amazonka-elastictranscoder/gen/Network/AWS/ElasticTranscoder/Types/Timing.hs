{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Timing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Timing where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the timing of a job.
--
--
--
-- /See:/ 'timing' smart constructor.
data Timing = Timing'
  { _tSubmitTimeMillis :: !(Maybe Integer),
    _tFinishTimeMillis :: !(Maybe Integer),
    _tStartTimeMillis :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Timing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tSubmitTimeMillis' - The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
--
-- * 'tFinishTimeMillis' - The time the job finished transcoding, in epoch milliseconds.
--
-- * 'tStartTimeMillis' - The time the job began transcoding, in epoch milliseconds.
timing ::
  Timing
timing =
  Timing'
    { _tSubmitTimeMillis = Nothing,
      _tFinishTimeMillis = Nothing,
      _tStartTimeMillis = Nothing
    }

-- | The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
tSubmitTimeMillis :: Lens' Timing (Maybe Integer)
tSubmitTimeMillis = lens _tSubmitTimeMillis (\s a -> s {_tSubmitTimeMillis = a})

-- | The time the job finished transcoding, in epoch milliseconds.
tFinishTimeMillis :: Lens' Timing (Maybe Integer)
tFinishTimeMillis = lens _tFinishTimeMillis (\s a -> s {_tFinishTimeMillis = a})

-- | The time the job began transcoding, in epoch milliseconds.
tStartTimeMillis :: Lens' Timing (Maybe Integer)
tStartTimeMillis = lens _tStartTimeMillis (\s a -> s {_tStartTimeMillis = a})

instance FromJSON Timing where
  parseJSON =
    withObject
      "Timing"
      ( \x ->
          Timing'
            <$> (x .:? "SubmitTimeMillis")
            <*> (x .:? "FinishTimeMillis")
            <*> (x .:? "StartTimeMillis")
      )

instance Hashable Timing

instance NFData Timing
