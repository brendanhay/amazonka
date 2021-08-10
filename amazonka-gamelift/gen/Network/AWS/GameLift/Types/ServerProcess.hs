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
import qualified Network.AWS.Prelude as Prelude

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
    parameters :: Prelude.Maybe Prelude.Text,
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
    launchPath :: Prelude.Text,
    -- | The number of server processes that use this configuration to run
    -- concurrently on an instance.
    concurrentExecutions :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'concurrentExecutions'
  Prelude.Natural ->
  ServerProcess
newServerProcess pLaunchPath_ pConcurrentExecutions_ =
  ServerProcess'
    { parameters = Prelude.Nothing,
      launchPath = pLaunchPath_,
      concurrentExecutions = pConcurrentExecutions_
    }

-- | An optional list of parameters to pass to the server executable or
-- Realtime script on launch.
serverProcess_parameters :: Lens.Lens' ServerProcess (Prelude.Maybe Prelude.Text)
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
serverProcess_launchPath :: Lens.Lens' ServerProcess Prelude.Text
serverProcess_launchPath = Lens.lens (\ServerProcess' {launchPath} -> launchPath) (\s@ServerProcess' {} a -> s {launchPath = a} :: ServerProcess)

-- | The number of server processes that use this configuration to run
-- concurrently on an instance.
serverProcess_concurrentExecutions :: Lens.Lens' ServerProcess Prelude.Natural
serverProcess_concurrentExecutions = Lens.lens (\ServerProcess' {concurrentExecutions} -> concurrentExecutions) (\s@ServerProcess' {} a -> s {concurrentExecutions = a} :: ServerProcess)

instance Core.FromJSON ServerProcess where
  parseJSON =
    Core.withObject
      "ServerProcess"
      ( \x ->
          ServerProcess'
            Prelude.<$> (x Core..:? "Parameters")
            Prelude.<*> (x Core..: "LaunchPath")
            Prelude.<*> (x Core..: "ConcurrentExecutions")
      )

instance Prelude.Hashable ServerProcess

instance Prelude.NFData ServerProcess

instance Core.ToJSON ServerProcess where
  toJSON ServerProcess' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("LaunchPath" Core..= launchPath),
            Prelude.Just
              ( "ConcurrentExecutions"
                  Core..= concurrentExecutions
              )
          ]
      )
