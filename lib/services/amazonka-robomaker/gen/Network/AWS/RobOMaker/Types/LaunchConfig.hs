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
-- Module      : Network.AWS.RobOMaker.Types.LaunchConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.LaunchConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.PortForwardingConfig

-- | Information about a launch configuration.
--
-- /See:/ 'newLaunchConfig' smart constructor.
data LaunchConfig = LaunchConfig'
  { -- | If you\'ve specified @General@ as the value for your
    -- @RobotSoftwareSuite@, you can use this field to specify a list of
    -- commands for your container image.
    --
    -- If you\'ve specified @SimulationRuntime@ as the value for your
    -- @SimulationSoftwareSuite@, you can use this field to specify a list of
    -- commands for your container image.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The package name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The port forwarding configuration.
    portForwardingConfig :: Prelude.Maybe PortForwardingConfig,
    -- | The launch file name.
    launchFile :: Prelude.Maybe Prelude.Text,
    -- | The environment variables for the application launch.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Boolean indicating whether a streaming session will be configured for
    -- the application. If @True@, AWS RoboMaker will configure a connection so
    -- you can interact with your application as it is running in the
    -- simulation. You must configure and launch the component. It must have a
    -- graphical user interface.
    streamUI :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'launchConfig_command' - If you\'ve specified @General@ as the value for your
-- @RobotSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
--
-- If you\'ve specified @SimulationRuntime@ as the value for your
-- @SimulationSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
--
-- 'packageName', 'launchConfig_packageName' - The package name.
--
-- 'portForwardingConfig', 'launchConfig_portForwardingConfig' - The port forwarding configuration.
--
-- 'launchFile', 'launchConfig_launchFile' - The launch file name.
--
-- 'environmentVariables', 'launchConfig_environmentVariables' - The environment variables for the application launch.
--
-- 'streamUI', 'launchConfig_streamUI' - Boolean indicating whether a streaming session will be configured for
-- the application. If @True@, AWS RoboMaker will configure a connection so
-- you can interact with your application as it is running in the
-- simulation. You must configure and launch the component. It must have a
-- graphical user interface.
newLaunchConfig ::
  LaunchConfig
newLaunchConfig =
  LaunchConfig'
    { command = Prelude.Nothing,
      packageName = Prelude.Nothing,
      portForwardingConfig = Prelude.Nothing,
      launchFile = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      streamUI = Prelude.Nothing
    }

-- | If you\'ve specified @General@ as the value for your
-- @RobotSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
--
-- If you\'ve specified @SimulationRuntime@ as the value for your
-- @SimulationSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
launchConfig_command :: Lens.Lens' LaunchConfig (Prelude.Maybe [Prelude.Text])
launchConfig_command = Lens.lens (\LaunchConfig' {command} -> command) (\s@LaunchConfig' {} a -> s {command = a} :: LaunchConfig) Prelude.. Lens.mapping Lens.coerced

-- | The package name.
launchConfig_packageName :: Lens.Lens' LaunchConfig (Prelude.Maybe Prelude.Text)
launchConfig_packageName = Lens.lens (\LaunchConfig' {packageName} -> packageName) (\s@LaunchConfig' {} a -> s {packageName = a} :: LaunchConfig)

-- | The port forwarding configuration.
launchConfig_portForwardingConfig :: Lens.Lens' LaunchConfig (Prelude.Maybe PortForwardingConfig)
launchConfig_portForwardingConfig = Lens.lens (\LaunchConfig' {portForwardingConfig} -> portForwardingConfig) (\s@LaunchConfig' {} a -> s {portForwardingConfig = a} :: LaunchConfig)

-- | The launch file name.
launchConfig_launchFile :: Lens.Lens' LaunchConfig (Prelude.Maybe Prelude.Text)
launchConfig_launchFile = Lens.lens (\LaunchConfig' {launchFile} -> launchFile) (\s@LaunchConfig' {} a -> s {launchFile = a} :: LaunchConfig)

-- | The environment variables for the application launch.
launchConfig_environmentVariables :: Lens.Lens' LaunchConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchConfig_environmentVariables = Lens.lens (\LaunchConfig' {environmentVariables} -> environmentVariables) (\s@LaunchConfig' {} a -> s {environmentVariables = a} :: LaunchConfig) Prelude.. Lens.mapping Lens.coerced

-- | Boolean indicating whether a streaming session will be configured for
-- the application. If @True@, AWS RoboMaker will configure a connection so
-- you can interact with your application as it is running in the
-- simulation. You must configure and launch the component. It must have a
-- graphical user interface.
launchConfig_streamUI :: Lens.Lens' LaunchConfig (Prelude.Maybe Prelude.Bool)
launchConfig_streamUI = Lens.lens (\LaunchConfig' {streamUI} -> streamUI) (\s@LaunchConfig' {} a -> s {streamUI = a} :: LaunchConfig)

instance Core.FromJSON LaunchConfig where
  parseJSON =
    Core.withObject
      "LaunchConfig"
      ( \x ->
          LaunchConfig'
            Prelude.<$> (x Core..:? "command" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "packageName")
            Prelude.<*> (x Core..:? "portForwardingConfig")
            Prelude.<*> (x Core..:? "launchFile")
            Prelude.<*> ( x Core..:? "environmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "streamUI")
      )

instance Prelude.Hashable LaunchConfig

instance Prelude.NFData LaunchConfig

instance Core.ToJSON LaunchConfig where
  toJSON LaunchConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("command" Core..=) Prelude.<$> command,
            ("packageName" Core..=) Prelude.<$> packageName,
            ("portForwardingConfig" Core..=)
              Prelude.<$> portForwardingConfig,
            ("launchFile" Core..=) Prelude.<$> launchFile,
            ("environmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            ("streamUI" Core..=) Prelude.<$> streamUI
          ]
      )
