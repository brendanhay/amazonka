{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobTimeout
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobTimeout where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a job timeout configuration.
--
--
--
-- /See:/ 'jobTimeout' smart constructor.
newtype JobTimeout = JobTimeout'
  { _jtAttemptDurationSeconds ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobTimeout' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtAttemptDurationSeconds' - The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
jobTimeout ::
  JobTimeout
jobTimeout = JobTimeout' {_jtAttemptDurationSeconds = Nothing}

-- | The time duration in seconds (measured from the job attempt's @startedAt@ timestamp) after which AWS Batch terminates your jobs if they have not finished.
jtAttemptDurationSeconds :: Lens' JobTimeout (Maybe Int)
jtAttemptDurationSeconds = lens _jtAttemptDurationSeconds (\s a -> s {_jtAttemptDurationSeconds = a})

instance FromJSON JobTimeout where
  parseJSON =
    withObject
      "JobTimeout"
      (\x -> JobTimeout' <$> (x .:? "attemptDurationSeconds"))

instance Hashable JobTimeout

instance NFData JobTimeout

instance ToJSON JobTimeout where
  toJSON JobTimeout' {..} =
    object
      ( catMaybes
          [("attemptDurationSeconds" .=) <$> _jtAttemptDurationSeconds]
      )
