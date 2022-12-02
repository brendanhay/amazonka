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
-- Module      : Amazonka.RobOMaker.Types.LaunchConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.LaunchConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.PortForwardingConfig

-- | Information about a launch configuration.
--
-- /See:/ 'newLaunchConfig' smart constructor.
data LaunchConfig = LaunchConfig'
  { -- | The package name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The launch file name.
    launchFile :: Prelude.Maybe Prelude.Text,
    -- | If you\'ve specified @General@ as the value for your
    -- @RobotSoftwareSuite@, you can use this field to specify a list of
    -- commands for your container image.
    --
    -- If you\'ve specified @SimulationRuntime@ as the value for your
    -- @SimulationSoftwareSuite@, you can use this field to specify a list of
    -- commands for your container image.
    command :: Prelude.Maybe [Prelude.Text],
    -- | Boolean indicating whether a streaming session will be configured for
    -- the application. If @True@, AWS RoboMaker will configure a connection so
    -- you can interact with your application as it is running in the
    -- simulation. You must configure and launch the component. It must have a
    -- graphical user interface.
    streamUI :: Prelude.Maybe Prelude.Bool,
    -- | The environment variables for the application launch.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The port forwarding configuration.
    portForwardingConfig :: Prelude.Maybe PortForwardingConfig
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
-- 'packageName', 'launchConfig_packageName' - The package name.
--
-- 'launchFile', 'launchConfig_launchFile' - The launch file name.
--
-- 'command', 'launchConfig_command' - If you\'ve specified @General@ as the value for your
-- @RobotSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
--
-- If you\'ve specified @SimulationRuntime@ as the value for your
-- @SimulationSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
--
-- 'streamUI', 'launchConfig_streamUI' - Boolean indicating whether a streaming session will be configured for
-- the application. If @True@, AWS RoboMaker will configure a connection so
-- you can interact with your application as it is running in the
-- simulation. You must configure and launch the component. It must have a
-- graphical user interface.
--
-- 'environmentVariables', 'launchConfig_environmentVariables' - The environment variables for the application launch.
--
-- 'portForwardingConfig', 'launchConfig_portForwardingConfig' - The port forwarding configuration.
newLaunchConfig ::
  LaunchConfig
newLaunchConfig =
  LaunchConfig'
    { packageName = Prelude.Nothing,
      launchFile = Prelude.Nothing,
      command = Prelude.Nothing,
      streamUI = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      portForwardingConfig = Prelude.Nothing
    }

-- | The package name.
launchConfig_packageName :: Lens.Lens' LaunchConfig (Prelude.Maybe Prelude.Text)
launchConfig_packageName = Lens.lens (\LaunchConfig' {packageName} -> packageName) (\s@LaunchConfig' {} a -> s {packageName = a} :: LaunchConfig)

-- | The launch file name.
launchConfig_launchFile :: Lens.Lens' LaunchConfig (Prelude.Maybe Prelude.Text)
launchConfig_launchFile = Lens.lens (\LaunchConfig' {launchFile} -> launchFile) (\s@LaunchConfig' {} a -> s {launchFile = a} :: LaunchConfig)

-- | If you\'ve specified @General@ as the value for your
-- @RobotSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
--
-- If you\'ve specified @SimulationRuntime@ as the value for your
-- @SimulationSoftwareSuite@, you can use this field to specify a list of
-- commands for your container image.
launchConfig_command :: Lens.Lens' LaunchConfig (Prelude.Maybe [Prelude.Text])
launchConfig_command = Lens.lens (\LaunchConfig' {command} -> command) (\s@LaunchConfig' {} a -> s {command = a} :: LaunchConfig) Prelude.. Lens.mapping Lens.coerced

-- | Boolean indicating whether a streaming session will be configured for
-- the application. If @True@, AWS RoboMaker will configure a connection so
-- you can interact with your application as it is running in the
-- simulation. You must configure and launch the component. It must have a
-- graphical user interface.
launchConfig_streamUI :: Lens.Lens' LaunchConfig (Prelude.Maybe Prelude.Bool)
launchConfig_streamUI = Lens.lens (\LaunchConfig' {streamUI} -> streamUI) (\s@LaunchConfig' {} a -> s {streamUI = a} :: LaunchConfig)

-- | The environment variables for the application launch.
launchConfig_environmentVariables :: Lens.Lens' LaunchConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchConfig_environmentVariables = Lens.lens (\LaunchConfig' {environmentVariables} -> environmentVariables) (\s@LaunchConfig' {} a -> s {environmentVariables = a} :: LaunchConfig) Prelude.. Lens.mapping Lens.coerced

-- | The port forwarding configuration.
launchConfig_portForwardingConfig :: Lens.Lens' LaunchConfig (Prelude.Maybe PortForwardingConfig)
launchConfig_portForwardingConfig = Lens.lens (\LaunchConfig' {portForwardingConfig} -> portForwardingConfig) (\s@LaunchConfig' {} a -> s {portForwardingConfig = a} :: LaunchConfig)

instance Data.FromJSON LaunchConfig where
  parseJSON =
    Data.withObject
      "LaunchConfig"
      ( \x ->
          LaunchConfig'
            Prelude.<$> (x Data..:? "packageName")
            Prelude.<*> (x Data..:? "launchFile")
            Prelude.<*> (x Data..:? "command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "streamUI")
            Prelude.<*> ( x Data..:? "environmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "portForwardingConfig")
      )

instance Prelude.Hashable LaunchConfig where
  hashWithSalt _salt LaunchConfig' {..} =
    _salt `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` launchFile
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` streamUI
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` portForwardingConfig

instance Prelude.NFData LaunchConfig where
  rnf LaunchConfig' {..} =
    Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf launchFile
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf streamUI
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf portForwardingConfig

instance Data.ToJSON LaunchConfig where
  toJSON LaunchConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("packageName" Data..=) Prelude.<$> packageName,
            ("launchFile" Data..=) Prelude.<$> launchFile,
            ("command" Data..=) Prelude.<$> command,
            ("streamUI" Data..=) Prelude.<$> streamUI,
            ("environmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("portForwardingConfig" Data..=)
              Prelude.<$> portForwardingConfig
          ]
      )
