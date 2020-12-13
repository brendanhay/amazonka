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
    rcServerProcesses,
    rcMaxConcurrentGameSessionActivations,
  )
where

import Network.AWS.GameLift.Types.ServerProcess
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    gameSessionActivationTimeoutSeconds :: Lude.Maybe Lude.Natural,
    -- | A collection of server process configurations that describe which server processes to run on each instance in a fleet.
    serverProcesses :: Lude.Maybe (Lude.NonEmpty ServerProcess),
    -- | The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
    maxConcurrentGameSessionActivations :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuntimeConfiguration' with the minimum fields required to make a request.
--
-- * 'gameSessionActivationTimeoutSeconds' - The maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
-- * 'serverProcesses' - A collection of server process configurations that describe which server processes to run on each instance in a fleet.
-- * 'maxConcurrentGameSessionActivations' - The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
mkRuntimeConfiguration ::
  RuntimeConfiguration
mkRuntimeConfiguration =
  RuntimeConfiguration'
    { gameSessionActivationTimeoutSeconds =
        Lude.Nothing,
      serverProcesses = Lude.Nothing,
      maxConcurrentGameSessionActivations = Lude.Nothing
    }

-- | The maximum amount of time (in seconds) that a game session can remain in status @ACTIVATING@ . If the game session is not active before the timeout, activation is terminated and the game session status is changed to @TERMINATED@ .
--
-- /Note:/ Consider using 'gameSessionActivationTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcGameSessionActivationTimeoutSeconds :: Lens.Lens' RuntimeConfiguration (Lude.Maybe Lude.Natural)
rcGameSessionActivationTimeoutSeconds = Lens.lens (gameSessionActivationTimeoutSeconds :: RuntimeConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {gameSessionActivationTimeoutSeconds = a} :: RuntimeConfiguration)
{-# DEPRECATED rcGameSessionActivationTimeoutSeconds "Use generic-lens or generic-optics with 'gameSessionActivationTimeoutSeconds' instead." #-}

-- | A collection of server process configurations that describe which server processes to run on each instance in a fleet.
--
-- /Note:/ Consider using 'serverProcesses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcServerProcesses :: Lens.Lens' RuntimeConfiguration (Lude.Maybe (Lude.NonEmpty ServerProcess))
rcServerProcesses = Lens.lens (serverProcesses :: RuntimeConfiguration -> Lude.Maybe (Lude.NonEmpty ServerProcess)) (\s a -> s {serverProcesses = a} :: RuntimeConfiguration)
{-# DEPRECATED rcServerProcesses "Use generic-lens or generic-optics with 'serverProcesses' instead." #-}

-- | The maximum number of game sessions with status @ACTIVATING@ to allow on an instance simultaneously. This setting limits the amount of instance resources that can be used for new game activations at any one time.
--
-- /Note:/ Consider using 'maxConcurrentGameSessionActivations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMaxConcurrentGameSessionActivations :: Lens.Lens' RuntimeConfiguration (Lude.Maybe Lude.Natural)
rcMaxConcurrentGameSessionActivations = Lens.lens (maxConcurrentGameSessionActivations :: RuntimeConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentGameSessionActivations = a} :: RuntimeConfiguration)
{-# DEPRECATED rcMaxConcurrentGameSessionActivations "Use generic-lens or generic-optics with 'maxConcurrentGameSessionActivations' instead." #-}

instance Lude.FromJSON RuntimeConfiguration where
  parseJSON =
    Lude.withObject
      "RuntimeConfiguration"
      ( \x ->
          RuntimeConfiguration'
            Lude.<$> (x Lude..:? "GameSessionActivationTimeoutSeconds")
            Lude.<*> (x Lude..:? "ServerProcesses")
            Lude.<*> (x Lude..:? "MaxConcurrentGameSessionActivations")
      )

instance Lude.ToJSON RuntimeConfiguration where
  toJSON RuntimeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GameSessionActivationTimeoutSeconds" Lude..=)
              Lude.<$> gameSessionActivationTimeoutSeconds,
            ("ServerProcesses" Lude..=) Lude.<$> serverProcesses,
            ("MaxConcurrentGameSessionActivations" Lude..=)
              Lude.<$> maxConcurrentGameSessionActivations
          ]
      )
