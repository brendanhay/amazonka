{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionsRolloutConfig where

import Network.AWS.IoT.Types.ExponentialRolloutRate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Allows you to create a staged rollout of a job.
--
--
--
-- /See:/ 'jobExecutionsRolloutConfig' smart constructor.
data JobExecutionsRolloutConfig = JobExecutionsRolloutConfig'
  { _jercExponentialRate ::
      !(Maybe ExponentialRolloutRate),
    _jercMaximumPerMinute :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecutionsRolloutConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jercExponentialRate' - The rate of increase for a job rollout. This parameter allows you to define an exponential rate for a job rollout.
--
-- * 'jercMaximumPerMinute' - The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
jobExecutionsRolloutConfig ::
  JobExecutionsRolloutConfig
jobExecutionsRolloutConfig =
  JobExecutionsRolloutConfig'
    { _jercExponentialRate = Nothing,
      _jercMaximumPerMinute = Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate for a job rollout.
jercExponentialRate :: Lens' JobExecutionsRolloutConfig (Maybe ExponentialRolloutRate)
jercExponentialRate = lens _jercExponentialRate (\s a -> s {_jercExponentialRate = a})

-- | The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
jercMaximumPerMinute :: Lens' JobExecutionsRolloutConfig (Maybe Natural)
jercMaximumPerMinute = lens _jercMaximumPerMinute (\s a -> s {_jercMaximumPerMinute = a}) . mapping _Nat

instance FromJSON JobExecutionsRolloutConfig where
  parseJSON =
    withObject
      "JobExecutionsRolloutConfig"
      ( \x ->
          JobExecutionsRolloutConfig'
            <$> (x .:? "exponentialRate") <*> (x .:? "maximumPerMinute")
      )

instance Hashable JobExecutionsRolloutConfig

instance NFData JobExecutionsRolloutConfig

instance ToJSON JobExecutionsRolloutConfig where
  toJSON JobExecutionsRolloutConfig' {..} =
    object
      ( catMaybes
          [ ("exponentialRate" .=) <$> _jercExponentialRate,
            ("maximumPerMinute" .=) <$> _jercMaximumPerMinute
          ]
      )
