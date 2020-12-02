{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. If you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
--
--
-- /See:/ 'batchRetryStrategy' smart constructor.
newtype BatchRetryStrategy = BatchRetryStrategy'
  { _brsAttempts ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchRetryStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsAttempts' - The number of times to attempt to retry, if the job fails. Valid values are 1–10.
batchRetryStrategy ::
  BatchRetryStrategy
batchRetryStrategy = BatchRetryStrategy' {_brsAttempts = Nothing}

-- | The number of times to attempt to retry, if the job fails. Valid values are 1–10.
brsAttempts :: Lens' BatchRetryStrategy (Maybe Int)
brsAttempts = lens _brsAttempts (\s a -> s {_brsAttempts = a})

instance FromJSON BatchRetryStrategy where
  parseJSON =
    withObject
      "BatchRetryStrategy"
      (\x -> BatchRetryStrategy' <$> (x .:? "Attempts"))

instance Hashable BatchRetryStrategy

instance NFData BatchRetryStrategy

instance ToJSON BatchRetryStrategy where
  toJSON BatchRetryStrategy' {..} =
    object (catMaybes [("Attempts" .=) <$> _brsAttempts])
