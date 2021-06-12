{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.RuntimeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.RuntimeConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.ServerProcess
import qualified Network.AWS.Lens as Lens

-- | A collection of server process configurations that describe what
-- processes to run on each instance in a fleet. Server processes run
-- either a custom game build executable or a Realtime Servers script. Each
-- instance in the fleet starts the specified server processes and
-- continues to start new processes as existing processes end. Each
-- instance regularly checks for an updated runtime configuration.
--
-- The runtime configuration enables the instances in a fleet to run
-- multiple processes simultaneously. Learn more about
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running Multiple Processes on a Fleet>
-- .
--
-- A Amazon GameLift instance is limited to 50 processes running
-- simultaneously. To calculate the total number of processes in a runtime
-- configuration, add the values of the @ConcurrentExecutions@ parameter
-- for each ServerProcess object.
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- /See:/ 'newRuntimeConfiguration' smart constructor.
data RuntimeConfiguration = RuntimeConfiguration'
  { -- | The maximum amount of time (in seconds) that a game session can remain
    -- in status @ACTIVATING@. If the game session is not active before the
    -- timeout, activation is terminated and the game session status is changed
    -- to @TERMINATED@.
    gameSessionActivationTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | A collection of server process configurations that describe which server
    -- processes to run on each instance in a fleet.
    serverProcesses :: Core.Maybe (Core.NonEmpty ServerProcess),
    -- | The maximum number of game sessions with status @ACTIVATING@ to allow on
    -- an instance simultaneously. This setting limits the amount of instance
    -- resources that can be used for new game activations at any one time.
    maxConcurrentGameSessionActivations :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionActivationTimeoutSeconds', 'runtimeConfiguration_gameSessionActivationTimeoutSeconds' - The maximum amount of time (in seconds) that a game session can remain
-- in status @ACTIVATING@. If the game session is not active before the
-- timeout, activation is terminated and the game session status is changed
-- to @TERMINATED@.
--
-- 'serverProcesses', 'runtimeConfiguration_serverProcesses' - A collection of server process configurations that describe which server
-- processes to run on each instance in a fleet.
--
-- 'maxConcurrentGameSessionActivations', 'runtimeConfiguration_maxConcurrentGameSessionActivations' - The maximum number of game sessions with status @ACTIVATING@ to allow on
-- an instance simultaneously. This setting limits the amount of instance
-- resources that can be used for new game activations at any one time.
newRuntimeConfiguration ::
  RuntimeConfiguration
newRuntimeConfiguration =
  RuntimeConfiguration'
    { gameSessionActivationTimeoutSeconds =
        Core.Nothing,
      serverProcesses = Core.Nothing,
      maxConcurrentGameSessionActivations = Core.Nothing
    }

-- | The maximum amount of time (in seconds) that a game session can remain
-- in status @ACTIVATING@. If the game session is not active before the
-- timeout, activation is terminated and the game session status is changed
-- to @TERMINATED@.
runtimeConfiguration_gameSessionActivationTimeoutSeconds :: Lens.Lens' RuntimeConfiguration (Core.Maybe Core.Natural)
runtimeConfiguration_gameSessionActivationTimeoutSeconds = Lens.lens (\RuntimeConfiguration' {gameSessionActivationTimeoutSeconds} -> gameSessionActivationTimeoutSeconds) (\s@RuntimeConfiguration' {} a -> s {gameSessionActivationTimeoutSeconds = a} :: RuntimeConfiguration)

-- | A collection of server process configurations that describe which server
-- processes to run on each instance in a fleet.
runtimeConfiguration_serverProcesses :: Lens.Lens' RuntimeConfiguration (Core.Maybe (Core.NonEmpty ServerProcess))
runtimeConfiguration_serverProcesses = Lens.lens (\RuntimeConfiguration' {serverProcesses} -> serverProcesses) (\s@RuntimeConfiguration' {} a -> s {serverProcesses = a} :: RuntimeConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of game sessions with status @ACTIVATING@ to allow on
-- an instance simultaneously. This setting limits the amount of instance
-- resources that can be used for new game activations at any one time.
runtimeConfiguration_maxConcurrentGameSessionActivations :: Lens.Lens' RuntimeConfiguration (Core.Maybe Core.Natural)
runtimeConfiguration_maxConcurrentGameSessionActivations = Lens.lens (\RuntimeConfiguration' {maxConcurrentGameSessionActivations} -> maxConcurrentGameSessionActivations) (\s@RuntimeConfiguration' {} a -> s {maxConcurrentGameSessionActivations = a} :: RuntimeConfiguration)

instance Core.FromJSON RuntimeConfiguration where
  parseJSON =
    Core.withObject
      "RuntimeConfiguration"
      ( \x ->
          RuntimeConfiguration'
            Core.<$> (x Core..:? "GameSessionActivationTimeoutSeconds")
            Core.<*> (x Core..:? "ServerProcesses")
            Core.<*> (x Core..:? "MaxConcurrentGameSessionActivations")
      )

instance Core.Hashable RuntimeConfiguration

instance Core.NFData RuntimeConfiguration

instance Core.ToJSON RuntimeConfiguration where
  toJSON RuntimeConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GameSessionActivationTimeoutSeconds" Core..=)
              Core.<$> gameSessionActivationTimeoutSeconds,
            ("ServerProcesses" Core..=) Core.<$> serverProcesses,
            ("MaxConcurrentGameSessionActivations" Core..=)
              Core.<$> maxConcurrentGameSessionActivations
          ]
      )
