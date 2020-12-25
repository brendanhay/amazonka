{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.RuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.RuntimeConfiguration
  ( RuntimeConfiguration (..),

    -- * Smart constructor
    mkRuntimeConfiguration,

    -- * Lenses
    rcGameSessionActivationTimeoutSeconds,
    rcMaxConcurrentGameSessionActivations,
    rcServerProcesses,
  )
where

import qualified Network.AWS.GameLift.Types.ServerProcess as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of server process configurations that describe what processes to run on each instance in a fleet. Server processes run either a custom game build executable or a Realtime Servers script. Each instance in the fleet starts the specified server processes and continues to start new processes as existing processes end. Each instance regularly checks for an updated runtime configuration.
--
-- The runtime configuration enables the instances in a fleet to run multiple processes simultaneously. Learn more about <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-multiprocess.html Running Multiple Processes on a Fleet > .
-- A Amazon GameLift instance is limited to 50 processes running simultaneously. To calculate the total number of processes in a runtime configuration, add the values of the @ConcurrentExecutions@ parameter for each 'ServerProcess' object.
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- /See:/ 'mkRuntimeConfiguration' smart constructor.
data RuntimeConfiguration = RuntimeConfiguration'
  { -- | The maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
    gameSessionActivationTimeoutSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
    maxConcurrentGameSessionActivations :: Core.Maybe Core.Natural,
    -- | A collection of server process configurations that describe which server processes to run on each instance in a fleet.
    serverProcesses :: Core.Maybe (Core.NonEmpty Types.ServerProcess)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuntimeConfiguration' value with any optional fields omitted.
mkRuntimeConfiguration ::
  RuntimeConfiguration
mkRuntimeConfiguration =
  RuntimeConfiguration'
    { gameSessionActivationTimeoutSeconds =
        Core.Nothing,
      maxConcurrentGameSessionActivations = Core.Nothing,
      serverProcesses = Core.Nothing
    }

-- | The maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
--
-- /Note:/ Consider using 'gameSessionActivationTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcGameSessionActivationTimeoutSeconds :: Lens.Lens' RuntimeConfiguration (Core.Maybe Core.Natural)
rcGameSessionActivationTimeoutSeconds = Lens.field @"gameSessionActivationTimeoutSeconds"
{-# DEPRECATED rcGameSessionActivationTimeoutSeconds "Use generic-lens or generic-optics with 'gameSessionActivationTimeoutSeconds' instead." #-}

-- | The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
--
-- /Note:/ Consider using 'maxConcurrentGameSessionActivations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaxConcurrentGameSessionActivations :: Lens.Lens' RuntimeConfiguration (Core.Maybe Core.Natural)
rcMaxConcurrentGameSessionActivations = Lens.field @"maxConcurrentGameSessionActivations"
{-# DEPRECATED rcMaxConcurrentGameSessionActivations "Use generic-lens or generic-optics with 'maxConcurrentGameSessionActivations' instead." #-}

-- | A collection of server process configurations that describe which server processes to run on each instance in a fleet.
--
-- /Note:/ Consider using 'serverProcesses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcServerProcesses :: Lens.Lens' RuntimeConfiguration (Core.Maybe (Core.NonEmpty Types.ServerProcess))
rcServerProcesses = Lens.field @"serverProcesses"
{-# DEPRECATED rcServerProcesses "Use generic-lens or generic-optics with 'serverProcesses' instead." #-}

instance Core.FromJSON RuntimeConfiguration where
  toJSON RuntimeConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("GameSessionActivationTimeoutSeconds" Core..=)
              Core.<$> gameSessionActivationTimeoutSeconds,
            ("MaxConcurrentGameSessionActivations" Core..=)
              Core.<$> maxConcurrentGameSessionActivations,
            ("ServerProcesses" Core..=) Core.<$> serverProcesses
          ]
      )

instance Core.FromJSON RuntimeConfiguration where
  parseJSON =
    Core.withObject "RuntimeConfiguration" Core.$
      \x ->
        RuntimeConfiguration'
          Core.<$> (x Core..:? "GameSessionActivationTimeoutSeconds")
          Core.<*> (x Core..:? "MaxConcurrentGameSessionActivations")
          Core.<*> (x Core..:? "ServerProcesses")
