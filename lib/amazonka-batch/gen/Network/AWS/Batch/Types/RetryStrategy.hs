{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.RetryStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.RetryStrategy where

import Network.AWS.Batch.Types.EvaluateOnExit
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The retry strategy associated with a job.
--
--
--
-- /See:/ 'retryStrategy' smart constructor.
data RetryStrategy = RetryStrategy'
  { _rsEvaluateOnExit ::
      !(Maybe [EvaluateOnExit]),
    _rsAttempts :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetryStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsEvaluateOnExit' - Array of up to 5 objects that specify conditions under which the job should be retried or failed. If this parameter is specified, then the @attempts@ parameter must also be specified.
--
-- * 'rsAttempts' - The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If the value of @attempts@ is greater than one, the job is retried on failure the same number of attempts as the value.
retryStrategy ::
  RetryStrategy
retryStrategy =
  RetryStrategy'
    { _rsEvaluateOnExit = Nothing,
      _rsAttempts = Nothing
    }

-- | Array of up to 5 objects that specify conditions under which the job should be retried or failed. If this parameter is specified, then the @attempts@ parameter must also be specified.
rsEvaluateOnExit :: Lens' RetryStrategy [EvaluateOnExit]
rsEvaluateOnExit = lens _rsEvaluateOnExit (\s a -> s {_rsEvaluateOnExit = a}) . _Default . _Coerce

-- | The number of times to move a job to the @RUNNABLE@ status. You may specify between 1 and 10 attempts. If the value of @attempts@ is greater than one, the job is retried on failure the same number of attempts as the value.
rsAttempts :: Lens' RetryStrategy (Maybe Int)
rsAttempts = lens _rsAttempts (\s a -> s {_rsAttempts = a})

instance FromJSON RetryStrategy where
  parseJSON =
    withObject
      "RetryStrategy"
      ( \x ->
          RetryStrategy'
            <$> (x .:? "evaluateOnExit" .!= mempty) <*> (x .:? "attempts")
      )

instance Hashable RetryStrategy

instance NFData RetryStrategy

instance ToJSON RetryStrategy where
  toJSON RetryStrategy' {..} =
    object
      ( catMaybes
          [ ("evaluateOnExit" .=) <$> _rsEvaluateOnExit,
            ("attempts" .=) <$> _rsAttempts
          ]
      )
