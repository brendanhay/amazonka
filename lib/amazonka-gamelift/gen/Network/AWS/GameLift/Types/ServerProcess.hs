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
    spParameters,
    spLaunchPath,
    spConcurrentExecutions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of instructions for launching server processes on each instance in a fleet. Server processes run either a custom game build executable or a Realtime Servers script. Each instruction set identifies the location of the custom game build executable or Realtime launch script, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's @'RuntimeConfiguration' @ .
--
-- /See:/ 'mkServerProcess' smart constructor.
data ServerProcess = ServerProcess'
  { parameters ::
      Lude.Maybe Lude.Text,
    launchPath :: Lude.Text,
    concurrentExecutions :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerProcess' with the minimum fields required to make a request.
--
-- * 'concurrentExecutions' - The number of server processes that use this configuration to run concurrently on an instance.
-- * 'launchPath' - The location of the server executable in a custom game build or the name of the Realtime script file that contains the @Init()@ function. Game builds and Realtime scripts are installed on instances at the root:
--
--
--     * Windows (for custom game builds only): @C:\game@ . Example: "@C:\game\MyGame\server.exe@ "
--
--
--     * Linux: @/local/game@ . Examples: "@/local/game/MyGame/server.exe@ " or "@/local/game/MyRealtimeScript.js@ "
--
--
-- * 'parameters' - An optional list of parameters to pass to the server executable or Realtime script on launch.
mkServerProcess ::
  -- | 'launchPath'
  Lude.Text ->
  -- | 'concurrentExecutions'
  Lude.Natural ->
  ServerProcess
mkServerProcess pLaunchPath_ pConcurrentExecutions_ =
  ServerProcess'
    { parameters = Lude.Nothing,
      launchPath = pLaunchPath_,
      concurrentExecutions = pConcurrentExecutions_
    }

-- | An optional list of parameters to pass to the server executable or Realtime script on launch.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spParameters :: Lens.Lens' ServerProcess (Lude.Maybe Lude.Text)
spParameters = Lens.lens (parameters :: ServerProcess -> Lude.Maybe Lude.Text) (\s a -> s {parameters = a} :: ServerProcess)
{-# DEPRECATED spParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

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
spLaunchPath :: Lens.Lens' ServerProcess Lude.Text
spLaunchPath = Lens.lens (launchPath :: ServerProcess -> Lude.Text) (\s a -> s {launchPath = a} :: ServerProcess)
{-# DEPRECATED spLaunchPath "Use generic-lens or generic-optics with 'launchPath' instead." #-}

-- | The number of server processes that use this configuration to run concurrently on an instance.
--
-- /Note:/ Consider using 'concurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spConcurrentExecutions :: Lens.Lens' ServerProcess Lude.Natural
spConcurrentExecutions = Lens.lens (concurrentExecutions :: ServerProcess -> Lude.Natural) (\s a -> s {concurrentExecutions = a} :: ServerProcess)
{-# DEPRECATED spConcurrentExecutions "Use generic-lens or generic-optics with 'concurrentExecutions' instead." #-}

instance Lude.FromJSON ServerProcess where
  parseJSON =
    Lude.withObject
      "ServerProcess"
      ( \x ->
          ServerProcess'
            Lude.<$> (x Lude..:? "Parameters")
            Lude.<*> (x Lude..: "LaunchPath")
            Lude.<*> (x Lude..: "ConcurrentExecutions")
      )

instance Lude.ToJSON ServerProcess where
  toJSON ServerProcess' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("LaunchPath" Lude..= launchPath),
            Lude.Just ("ConcurrentExecutions" Lude..= concurrentExecutions)
          ]
      )
