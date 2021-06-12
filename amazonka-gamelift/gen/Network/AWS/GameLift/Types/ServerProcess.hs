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
-- Module      : Network.AWS.GameLift.Types.ServerProcess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ServerProcess where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A set of instructions for launching server processes on each instance in
-- a fleet. Server processes run either a custom game build executable or a
-- Realtime Servers script. Each instruction set identifies the location of
-- the custom game build executable or Realtime launch script, optional
-- launch parameters, and the number of server processes with this
-- configuration to maintain concurrently on the instance. Server process
-- configurations make up a fleet\'s @ RuntimeConfiguration @.
--
-- /See:/ 'newServerProcess' smart constructor.
data ServerProcess = ServerProcess'
  { -- | An optional list of parameters to pass to the server executable or
    -- Realtime script on launch.
    parameters :: Core.Maybe Core.Text,
    -- | The location of the server executable in a custom game build or the name
    -- of the Realtime script file that contains the @Init()@ function. Game
    -- builds and Realtime scripts are installed on instances at the root:
    --
    -- -   Windows (for custom game builds only): @C:\\game@. Example:
    --     \"@C:\\game\\MyGame\\server.exe@\"
    --
    -- -   Linux: @\/local\/game@. Examples:
    --     \"@\/local\/game\/MyGame\/server.exe@\" or
    --     \"@\/local\/game\/MyRealtimeScript.js@\"
    launchPath :: Core.Text,
    -- | The number of server processes that use this configuration to run
    -- concurrently on an instance.
    concurrentExecutions :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerProcess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'serverProcess_parameters' - An optional list of parameters to pass to the server executable or
-- Realtime script on launch.
--
-- 'launchPath', 'serverProcess_launchPath' - The location of the server executable in a custom game build or the name
-- of the Realtime script file that contains the @Init()@ function. Game
-- builds and Realtime scripts are installed on instances at the root:
--
-- -   Windows (for custom game builds only): @C:\\game@. Example:
--     \"@C:\\game\\MyGame\\server.exe@\"
--
-- -   Linux: @\/local\/game@. Examples:
--     \"@\/local\/game\/MyGame\/server.exe@\" or
--     \"@\/local\/game\/MyRealtimeScript.js@\"
--
-- 'concurrentExecutions', 'serverProcess_concurrentExecutions' - The number of server processes that use this configuration to run
-- concurrently on an instance.
newServerProcess ::
  -- | 'launchPath'
  Core.Text ->
  -- | 'concurrentExecutions'
  Core.Natural ->
  ServerProcess
newServerProcess pLaunchPath_ pConcurrentExecutions_ =
  ServerProcess'
    { parameters = Core.Nothing,
      launchPath = pLaunchPath_,
      concurrentExecutions = pConcurrentExecutions_
    }

-- | An optional list of parameters to pass to the server executable or
-- Realtime script on launch.
serverProcess_parameters :: Lens.Lens' ServerProcess (Core.Maybe Core.Text)
serverProcess_parameters = Lens.lens (\ServerProcess' {parameters} -> parameters) (\s@ServerProcess' {} a -> s {parameters = a} :: ServerProcess)

-- | The location of the server executable in a custom game build or the name
-- of the Realtime script file that contains the @Init()@ function. Game
-- builds and Realtime scripts are installed on instances at the root:
--
-- -   Windows (for custom game builds only): @C:\\game@. Example:
--     \"@C:\\game\\MyGame\\server.exe@\"
--
-- -   Linux: @\/local\/game@. Examples:
--     \"@\/local\/game\/MyGame\/server.exe@\" or
--     \"@\/local\/game\/MyRealtimeScript.js@\"
serverProcess_launchPath :: Lens.Lens' ServerProcess Core.Text
serverProcess_launchPath = Lens.lens (\ServerProcess' {launchPath} -> launchPath) (\s@ServerProcess' {} a -> s {launchPath = a} :: ServerProcess)

-- | The number of server processes that use this configuration to run
-- concurrently on an instance.
serverProcess_concurrentExecutions :: Lens.Lens' ServerProcess Core.Natural
serverProcess_concurrentExecutions = Lens.lens (\ServerProcess' {concurrentExecutions} -> concurrentExecutions) (\s@ServerProcess' {} a -> s {concurrentExecutions = a} :: ServerProcess)

instance Core.FromJSON ServerProcess where
  parseJSON =
    Core.withObject
      "ServerProcess"
      ( \x ->
          ServerProcess'
            Core.<$> (x Core..:? "Parameters")
            Core.<*> (x Core..: "LaunchPath")
            Core.<*> (x Core..: "ConcurrentExecutions")
      )

instance Core.Hashable ServerProcess

instance Core.NFData ServerProcess

instance Core.ToJSON ServerProcess where
  toJSON ServerProcess' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("LaunchPath" Core..= launchPath),
            Core.Just
              ( "ConcurrentExecutions"
                  Core..= concurrentExecutions
              )
          ]
      )
