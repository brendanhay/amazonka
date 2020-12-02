{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.RuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.RuntimeConfiguration where

import Network.AWS.GameLift.Types.ServerProcess
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of server process configurations that describe what processes to run on each instance in a fleet. Server processes run either a custom game build executable or a Realtime Servers script. Each instance in the fleet starts the specified server processes and continues to start new processes as existing processes end. Each instance regularly checks for an updated runtime configuration.
--
--
-- The runtime configuration enables the instances in a fleet to run multiple processes simultaneously. Learn more about <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running Multiple Processes on a Fleet > .
--
-- A Amazon GameLift instance is limited to 50 processes running simultaneously. To calculate the total number of processes in a runtime configuration, add the values of the @ConcurrentExecutions@ parameter for each 'ServerProcess' object.
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * 'DescribeFleetAttributes'
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- /See:/ 'runtimeConfiguration' smart constructor.
data RuntimeConfiguration = RuntimeConfiguration'
  { _rcGameSessionActivationTimeoutSeconds ::
      !(Maybe Nat),
    _rcServerProcesses ::
      !(Maybe (List1 ServerProcess)),
    _rcMaxConcurrentGameSessionActivations ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcGameSessionActivationTimeoutSeconds' - The maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
--
-- * 'rcServerProcesses' - A collection of server process configurations that describe which server processes to run on each instance in a fleet.
--
-- * 'rcMaxConcurrentGameSessionActivations' - The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
runtimeConfiguration ::
  RuntimeConfiguration
runtimeConfiguration =
  RuntimeConfiguration'
    { _rcGameSessionActivationTimeoutSeconds =
        Nothing,
      _rcServerProcesses = Nothing,
      _rcMaxConcurrentGameSessionActivations = Nothing
    }

-- | The maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
rcGameSessionActivationTimeoutSeconds :: Lens' RuntimeConfiguration (Maybe Natural)
rcGameSessionActivationTimeoutSeconds = lens _rcGameSessionActivationTimeoutSeconds (\s a -> s {_rcGameSessionActivationTimeoutSeconds = a}) . mapping _Nat

-- | A collection of server process configurations that describe which server processes to run on each instance in a fleet.
rcServerProcesses :: Lens' RuntimeConfiguration (Maybe (NonEmpty ServerProcess))
rcServerProcesses = lens _rcServerProcesses (\s a -> s {_rcServerProcesses = a}) . mapping _List1

-- | The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
rcMaxConcurrentGameSessionActivations :: Lens' RuntimeConfiguration (Maybe Natural)
rcMaxConcurrentGameSessionActivations = lens _rcMaxConcurrentGameSessionActivations (\s a -> s {_rcMaxConcurrentGameSessionActivations = a}) . mapping _Nat

instance FromJSON RuntimeConfiguration where
  parseJSON =
    withObject
      "RuntimeConfiguration"
      ( \x ->
          RuntimeConfiguration'
            <$> (x .:? "GameSessionActivationTimeoutSeconds")
            <*> (x .:? "ServerProcesses")
            <*> (x .:? "MaxConcurrentGameSessionActivations")
      )

instance Hashable RuntimeConfiguration

instance NFData RuntimeConfiguration

instance ToJSON RuntimeConfiguration where
  toJSON RuntimeConfiguration' {..} =
    object
      ( catMaybes
          [ ("GameSessionActivationTimeoutSeconds" .=)
              <$> _rcGameSessionActivationTimeoutSeconds,
            ("ServerProcesses" .=) <$> _rcServerProcesses,
            ("MaxConcurrentGameSessionActivations" .=)
              <$> _rcMaxConcurrentGameSessionActivations
          ]
      )
