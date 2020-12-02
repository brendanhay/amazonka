{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobMessages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobMessages where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides messages from the service about jobs that you have already successfully submitted.
--
-- /See:/ 'jobMessages' smart constructor.
data JobMessages = JobMessages'
  { _jmWarning :: !(Maybe [Text]),
    _jmInfo :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobMessages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jmWarning' - List of messages that warn about conditions that might cause your job not to run or to fail.
--
-- * 'jmInfo' - List of messages that are informational only and don't indicate a problem with your job.
jobMessages ::
  JobMessages
jobMessages = JobMessages' {_jmWarning = Nothing, _jmInfo = Nothing}

-- | List of messages that warn about conditions that might cause your job not to run or to fail.
jmWarning :: Lens' JobMessages [Text]
jmWarning = lens _jmWarning (\s a -> s {_jmWarning = a}) . _Default . _Coerce

-- | List of messages that are informational only and don't indicate a problem with your job.
jmInfo :: Lens' JobMessages [Text]
jmInfo = lens _jmInfo (\s a -> s {_jmInfo = a}) . _Default . _Coerce

instance FromJSON JobMessages where
  parseJSON =
    withObject
      "JobMessages"
      ( \x ->
          JobMessages'
            <$> (x .:? "warning" .!= mempty) <*> (x .:? "info" .!= mempty)
      )

instance Hashable JobMessages

instance NFData JobMessages
