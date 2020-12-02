{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig where

import Network.AWS.IoT.Types.AWSJobExponentialRolloutRate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration for the rollout of OTA updates.
--
--
--
-- /See:/ 'awsJobExecutionsRolloutConfig' smart constructor.
data AWSJobExecutionsRolloutConfig = AWSJobExecutionsRolloutConfig'
  { _ajercExponentialRate ::
      !( Maybe
           AWSJobExponentialRolloutRate
       ),
    _ajercMaximumPerMinute ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSJobExecutionsRolloutConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajercExponentialRate' - The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
--
-- * 'ajercMaximumPerMinute' - The maximum number of OTA update job executions started per minute.
awsJobExecutionsRolloutConfig ::
  AWSJobExecutionsRolloutConfig
awsJobExecutionsRolloutConfig =
  AWSJobExecutionsRolloutConfig'
    { _ajercExponentialRate = Nothing,
      _ajercMaximumPerMinute = Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
ajercExponentialRate :: Lens' AWSJobExecutionsRolloutConfig (Maybe AWSJobExponentialRolloutRate)
ajercExponentialRate = lens _ajercExponentialRate (\s a -> s {_ajercExponentialRate = a})

-- | The maximum number of OTA update job executions started per minute.
ajercMaximumPerMinute :: Lens' AWSJobExecutionsRolloutConfig (Maybe Natural)
ajercMaximumPerMinute = lens _ajercMaximumPerMinute (\s a -> s {_ajercMaximumPerMinute = a}) . mapping _Nat

instance FromJSON AWSJobExecutionsRolloutConfig where
  parseJSON =
    withObject
      "AWSJobExecutionsRolloutConfig"
      ( \x ->
          AWSJobExecutionsRolloutConfig'
            <$> (x .:? "exponentialRate") <*> (x .:? "maximumPerMinute")
      )

instance Hashable AWSJobExecutionsRolloutConfig

instance NFData AWSJobExecutionsRolloutConfig

instance ToJSON AWSJobExecutionsRolloutConfig where
  toJSON AWSJobExecutionsRolloutConfig' {..} =
    object
      ( catMaybes
          [ ("exponentialRate" .=) <$> _ajercExponentialRate,
            ("maximumPerMinute" .=) <$> _ajercMaximumPerMinute
          ]
      )
