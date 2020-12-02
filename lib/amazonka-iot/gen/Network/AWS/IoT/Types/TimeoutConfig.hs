{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimeoutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimeoutConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the amount of time each device has to finish its execution of the job. A timer is started when the job execution status is set to @IN_PROGRESS@ . If the job execution status is not set to another terminal state before the timer expires, it will be automatically set to @TIMED_OUT@ .
--
--
--
-- /See:/ 'timeoutConfig' smart constructor.
newtype TimeoutConfig = TimeoutConfig'
  { _tcInProgressTimeoutInMinutes ::
      Maybe Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeoutConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcInProgressTimeoutInMinutes' - Specifies the amount of time, in minutes, this device has to finish execution of this job. The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The in progress timer can't be updated and will apply to all job executions for the job. Whenever a job execution remains in the IN_PROGRESS status for longer than this interval, the job execution will fail and switch to the terminal @TIMED_OUT@ status.
timeoutConfig ::
  TimeoutConfig
timeoutConfig =
  TimeoutConfig' {_tcInProgressTimeoutInMinutes = Nothing}

-- | Specifies the amount of time, in minutes, this device has to finish execution of this job. The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The in progress timer can't be updated and will apply to all job executions for the job. Whenever a job execution remains in the IN_PROGRESS status for longer than this interval, the job execution will fail and switch to the terminal @TIMED_OUT@ status.
tcInProgressTimeoutInMinutes :: Lens' TimeoutConfig (Maybe Integer)
tcInProgressTimeoutInMinutes = lens _tcInProgressTimeoutInMinutes (\s a -> s {_tcInProgressTimeoutInMinutes = a})

instance FromJSON TimeoutConfig where
  parseJSON =
    withObject
      "TimeoutConfig"
      (\x -> TimeoutConfig' <$> (x .:? "inProgressTimeoutInMinutes"))

instance Hashable TimeoutConfig

instance NFData TimeoutConfig

instance ToJSON TimeoutConfig where
  toJSON TimeoutConfig' {..} =
    object
      ( catMaybes
          [ ("inProgressTimeoutInMinutes" .=)
              <$> _tcInProgressTimeoutInMinutes
          ]
      )
