{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStatusCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStatusCounters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The numbers of training jobs launched by a hyperparameter tuning job, categorized by status.
--
--
--
-- /See:/ 'trainingJobStatusCounters' smart constructor.
data TrainingJobStatusCounters = TrainingJobStatusCounters'
  { _tjscStopped ::
      !(Maybe Nat),
    _tjscRetryableError :: !(Maybe Nat),
    _tjscInProgress :: !(Maybe Nat),
    _tjscNonRetryableError :: !(Maybe Nat),
    _tjscCompleted :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrainingJobStatusCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjscStopped' - The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
--
-- * 'tjscRetryableError' - The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
--
-- * 'tjscInProgress' - The number of in-progress training jobs launched by a hyperparameter tuning job.
--
-- * 'tjscNonRetryableError' - The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
--
-- * 'tjscCompleted' - The number of completed training jobs launched by the hyperparameter tuning job.
trainingJobStatusCounters ::
  TrainingJobStatusCounters
trainingJobStatusCounters =
  TrainingJobStatusCounters'
    { _tjscStopped = Nothing,
      _tjscRetryableError = Nothing,
      _tjscInProgress = Nothing,
      _tjscNonRetryableError = Nothing,
      _tjscCompleted = Nothing
    }

-- | The number of training jobs launched by a hyperparameter tuning job that were manually stopped.
tjscStopped :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscStopped = lens _tjscStopped (\s a -> s {_tjscStopped = a}) . mapping _Nat

-- | The number of training jobs that failed, but can be retried. A failed training job can be retried only if it failed because an internal service error occurred.
tjscRetryableError :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscRetryableError = lens _tjscRetryableError (\s a -> s {_tjscRetryableError = a}) . mapping _Nat

-- | The number of in-progress training jobs launched by a hyperparameter tuning job.
tjscInProgress :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscInProgress = lens _tjscInProgress (\s a -> s {_tjscInProgress = a}) . mapping _Nat

-- | The number of training jobs that failed and can't be retried. A failed training job can't be retried if it failed because a client error occurred.
tjscNonRetryableError :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscNonRetryableError = lens _tjscNonRetryableError (\s a -> s {_tjscNonRetryableError = a}) . mapping _Nat

-- | The number of completed training jobs launched by the hyperparameter tuning job.
tjscCompleted :: Lens' TrainingJobStatusCounters (Maybe Natural)
tjscCompleted = lens _tjscCompleted (\s a -> s {_tjscCompleted = a}) . mapping _Nat

instance FromJSON TrainingJobStatusCounters where
  parseJSON =
    withObject
      "TrainingJobStatusCounters"
      ( \x ->
          TrainingJobStatusCounters'
            <$> (x .:? "Stopped")
            <*> (x .:? "RetryableError")
            <*> (x .:? "InProgress")
            <*> (x .:? "NonRetryableError")
            <*> (x .:? "Completed")
      )

instance Hashable TrainingJobStatusCounters

instance NFData TrainingJobStatusCounters
