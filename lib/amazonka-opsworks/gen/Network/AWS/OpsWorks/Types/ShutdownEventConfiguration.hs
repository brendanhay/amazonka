{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ShutdownEventConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Shutdown event configuration.
--
--
--
-- /See:/ 'shutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { _secExecutionTimeout ::
      !(Maybe Int),
    _secDelayUntilElbConnectionsDrained ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShutdownEventConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'secExecutionTimeout' - The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
--
-- * 'secDelayUntilElbConnectionsDrained' - Whether to enable Elastic Load Balancing connection draining. For more information, see <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
shutdownEventConfiguration ::
  ShutdownEventConfiguration
shutdownEventConfiguration =
  ShutdownEventConfiguration'
    { _secExecutionTimeout = Nothing,
      _secDelayUntilElbConnectionsDrained = Nothing
    }

-- | The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
secExecutionTimeout :: Lens' ShutdownEventConfiguration (Maybe Int)
secExecutionTimeout = lens _secExecutionTimeout (\s a -> s {_secExecutionTimeout = a})

-- | Whether to enable Elastic Load Balancing connection draining. For more information, see <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
secDelayUntilElbConnectionsDrained :: Lens' ShutdownEventConfiguration (Maybe Bool)
secDelayUntilElbConnectionsDrained = lens _secDelayUntilElbConnectionsDrained (\s a -> s {_secDelayUntilElbConnectionsDrained = a})

instance FromJSON ShutdownEventConfiguration where
  parseJSON =
    withObject
      "ShutdownEventConfiguration"
      ( \x ->
          ShutdownEventConfiguration'
            <$> (x .:? "ExecutionTimeout")
            <*> (x .:? "DelayUntilElbConnectionsDrained")
      )

instance Hashable ShutdownEventConfiguration

instance NFData ShutdownEventConfiguration

instance ToJSON ShutdownEventConfiguration where
  toJSON ShutdownEventConfiguration' {..} =
    object
      ( catMaybes
          [ ("ExecutionTimeout" .=) <$> _secExecutionTimeout,
            ("DelayUntilElbConnectionsDrained" .=)
              <$> _secDelayUntilElbConnectionsDrained
          ]
      )
