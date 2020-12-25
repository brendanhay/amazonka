{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ServerProcess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ServerProcess
  ( ServerProcess (..),

    -- * Smart constructor
    mkServerProcess,

    -- * Lenses
    spLaunchPath,
    spConcurrentExecutions,
    spParameters,
  )
where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A set of instructions for launching server processes on each instance in a fleet. Server processes run either a custom game build executable or a Realtime Servers script. Each instruction set identifies the location of the custom game build executable or Realtime launch script, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's @'RuntimeConfiguration' @ .
--
-- /See:/ 'mkServerProcess' smart constructor.
data ServerProcess = ServerProcess'
  { -- | The location of the server executable in a custom game build or the name of the Realtime script file that contains the @Init()@ function. Game builds and Realtime scripts are installed on instances at the root:
    --
    --
    --     * Windows (for custom game builds only): @C:\game@ . Example: "@C:\game\MyGame\server.exe@ "
    --
    --
    --     * Linux: @/local/game@ . Examples: "@/local/game/MyGame/server.exe@ " or "@/local/game/MyRealtimeScript.js@ "
    launchPath :: Types.NonZeroAndMaxString,
    -- | The number of server processes that use this configuration to run concurrently on an instance.
    concurrentExecutions :: Core.Natural,
    -- | An optional list of parameters to pass to the server executable or Realtime script on launch.
    parameters :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerProcess' value with any optional fields omitted.
mkServerProcess ::
  -- | 'launchPath'
  Types.NonZeroAndMaxString ->
  -- | 'concurrentExecutions'
  Core.Natural ->
  ServerProcess
mkServerProcess launchPath concurrentExecutions =
  ServerProcess'
    { launchPath,
      concurrentExecutions,
      parameters = Core.Nothing
    }

-- | The location of the server executable in a custom game build or the name of the Realtime script file that contains the @Init()@ function. Game builds and Realtime scripts are installed on instances at the root:
--
--
--     * Windows (for custom game builds only): @C:\game@ . Example: "@C:\game\MyGame\server.exe@ "
--
--
--     * Linux: @/local/game@ . Examples: "@/local/game/MyGame/server.exe@ " or "@/local/game/MyRealtimeScript.js@ "
--
--
--
-- /Note:/ Consider using 'launchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spLaunchPath :: Lens.Lens' ServerProcess Types.NonZeroAndMaxString
spLaunchPath = Lens.field @"launchPath"
{-# DEPRECATED spLaunchPath "Use generic-lens or generic-optics with 'launchPath' instead." #-}

-- | The number of server processes that use this configuration to run concurrently on an instance.
--
-- /Note:/ Consider using 'concurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spConcurrentExecutions :: Lens.Lens' ServerProcess Core.Natural
spConcurrentExecutions = Lens.field @"concurrentExecutions"
{-# DEPRECATED spConcurrentExecutions "Use generic-lens or generic-optics with 'concurrentExecutions' instead." #-}

-- | An optional list of parameters to pass to the server executable or Realtime script on launch.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spParameters :: Lens.Lens' ServerProcess (Core.Maybe Types.NonZeroAndMaxString)
spParameters = Lens.field @"parameters"
{-# DEPRECATED spParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON ServerProcess where
  toJSON ServerProcess {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("LaunchPath" Core..= launchPath),
            Core.Just ("ConcurrentExecutions" Core..= concurrentExecutions),
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.FromJSON ServerProcess where
  parseJSON =
    Core.withObject "ServerProcess" Core.$
      \x ->
        ServerProcess'
          Core.<$> (x Core..: "LaunchPath")
          Core.<*> (x Core..: "ConcurrentExecutions")
          Core.<*> (x Core..:? "Parameters")
