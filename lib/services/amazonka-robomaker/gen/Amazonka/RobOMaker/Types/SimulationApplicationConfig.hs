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
-- Module      : Amazonka.RobOMaker.Types.SimulationApplicationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationApplicationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.LaunchConfig
import Amazonka.RobOMaker.Types.Tool
import Amazonka.RobOMaker.Types.UploadConfiguration
import Amazonka.RobOMaker.Types.WorldConfig

-- | Information about a simulation application configuration.
--
-- /See:/ 'newSimulationApplicationConfig' smart constructor.
data SimulationApplicationConfig = SimulationApplicationConfig'
  { -- | The version of the simulation application.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about tools configured for the simulation application.
    tools :: Prelude.Maybe [Tool],
    -- | Information about upload configurations for the simulation application.
    uploadConfigurations :: Prelude.Maybe [UploadConfiguration],
    -- | A Boolean indicating whether to use default simulation application
    -- tools. The default tools are rviz, rqt, terminal and rosbag record. The
    -- default is @False@.
    --
    -- This API is no longer supported and will throw an error if used.
    useDefaultTools :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean indicating whether to use default upload configurations. By
    -- default, @.ros@ and @.gazebo@ files are uploaded when the application
    -- terminates and all ROS topics will be recorded.
    --
    -- If you set this value, you must specify an @outputLocation@.
    --
    -- This API is no longer supported and will throw an error if used.
    useDefaultUploadConfigurations :: Prelude.Maybe Prelude.Bool,
    -- | A list of world configurations.
    worldConfigs :: Prelude.Maybe [WorldConfig],
    -- | The application information for the simulation application.
    application :: Prelude.Text,
    -- | The launch configuration for the simulation application.
    launchConfig :: LaunchConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationApplicationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'simulationApplicationConfig_applicationVersion' - The version of the simulation application.
--
-- 'tools', 'simulationApplicationConfig_tools' - Information about tools configured for the simulation application.
--
-- 'uploadConfigurations', 'simulationApplicationConfig_uploadConfigurations' - Information about upload configurations for the simulation application.
--
-- 'useDefaultTools', 'simulationApplicationConfig_useDefaultTools' - A Boolean indicating whether to use default simulation application
-- tools. The default tools are rviz, rqt, terminal and rosbag record. The
-- default is @False@.
--
-- This API is no longer supported and will throw an error if used.
--
-- 'useDefaultUploadConfigurations', 'simulationApplicationConfig_useDefaultUploadConfigurations' - A Boolean indicating whether to use default upload configurations. By
-- default, @.ros@ and @.gazebo@ files are uploaded when the application
-- terminates and all ROS topics will be recorded.
--
-- If you set this value, you must specify an @outputLocation@.
--
-- This API is no longer supported and will throw an error if used.
--
-- 'worldConfigs', 'simulationApplicationConfig_worldConfigs' - A list of world configurations.
--
-- 'application', 'simulationApplicationConfig_application' - The application information for the simulation application.
--
-- 'launchConfig', 'simulationApplicationConfig_launchConfig' - The launch configuration for the simulation application.
newSimulationApplicationConfig ::
  -- | 'application'
  Prelude.Text ->
  -- | 'launchConfig'
  LaunchConfig ->
  SimulationApplicationConfig
newSimulationApplicationConfig
  pApplication_
  pLaunchConfig_ =
    SimulationApplicationConfig'
      { applicationVersion =
          Prelude.Nothing,
        tools = Prelude.Nothing,
        uploadConfigurations = Prelude.Nothing,
        useDefaultTools = Prelude.Nothing,
        useDefaultUploadConfigurations =
          Prelude.Nothing,
        worldConfigs = Prelude.Nothing,
        application = pApplication_,
        launchConfig = pLaunchConfig_
      }

-- | The version of the simulation application.
simulationApplicationConfig_applicationVersion :: Lens.Lens' SimulationApplicationConfig (Prelude.Maybe Prelude.Text)
simulationApplicationConfig_applicationVersion = Lens.lens (\SimulationApplicationConfig' {applicationVersion} -> applicationVersion) (\s@SimulationApplicationConfig' {} a -> s {applicationVersion = a} :: SimulationApplicationConfig)

-- | Information about tools configured for the simulation application.
simulationApplicationConfig_tools :: Lens.Lens' SimulationApplicationConfig (Prelude.Maybe [Tool])
simulationApplicationConfig_tools = Lens.lens (\SimulationApplicationConfig' {tools} -> tools) (\s@SimulationApplicationConfig' {} a -> s {tools = a} :: SimulationApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | Information about upload configurations for the simulation application.
simulationApplicationConfig_uploadConfigurations :: Lens.Lens' SimulationApplicationConfig (Prelude.Maybe [UploadConfiguration])
simulationApplicationConfig_uploadConfigurations = Lens.lens (\SimulationApplicationConfig' {uploadConfigurations} -> uploadConfigurations) (\s@SimulationApplicationConfig' {} a -> s {uploadConfigurations = a} :: SimulationApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean indicating whether to use default simulation application
-- tools. The default tools are rviz, rqt, terminal and rosbag record. The
-- default is @False@.
--
-- This API is no longer supported and will throw an error if used.
simulationApplicationConfig_useDefaultTools :: Lens.Lens' SimulationApplicationConfig (Prelude.Maybe Prelude.Bool)
simulationApplicationConfig_useDefaultTools = Lens.lens (\SimulationApplicationConfig' {useDefaultTools} -> useDefaultTools) (\s@SimulationApplicationConfig' {} a -> s {useDefaultTools = a} :: SimulationApplicationConfig)

-- | A Boolean indicating whether to use default upload configurations. By
-- default, @.ros@ and @.gazebo@ files are uploaded when the application
-- terminates and all ROS topics will be recorded.
--
-- If you set this value, you must specify an @outputLocation@.
--
-- This API is no longer supported and will throw an error if used.
simulationApplicationConfig_useDefaultUploadConfigurations :: Lens.Lens' SimulationApplicationConfig (Prelude.Maybe Prelude.Bool)
simulationApplicationConfig_useDefaultUploadConfigurations = Lens.lens (\SimulationApplicationConfig' {useDefaultUploadConfigurations} -> useDefaultUploadConfigurations) (\s@SimulationApplicationConfig' {} a -> s {useDefaultUploadConfigurations = a} :: SimulationApplicationConfig)

-- | A list of world configurations.
simulationApplicationConfig_worldConfigs :: Lens.Lens' SimulationApplicationConfig (Prelude.Maybe [WorldConfig])
simulationApplicationConfig_worldConfigs = Lens.lens (\SimulationApplicationConfig' {worldConfigs} -> worldConfigs) (\s@SimulationApplicationConfig' {} a -> s {worldConfigs = a} :: SimulationApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | The application information for the simulation application.
simulationApplicationConfig_application :: Lens.Lens' SimulationApplicationConfig Prelude.Text
simulationApplicationConfig_application = Lens.lens (\SimulationApplicationConfig' {application} -> application) (\s@SimulationApplicationConfig' {} a -> s {application = a} :: SimulationApplicationConfig)

-- | The launch configuration for the simulation application.
simulationApplicationConfig_launchConfig :: Lens.Lens' SimulationApplicationConfig LaunchConfig
simulationApplicationConfig_launchConfig = Lens.lens (\SimulationApplicationConfig' {launchConfig} -> launchConfig) (\s@SimulationApplicationConfig' {} a -> s {launchConfig = a} :: SimulationApplicationConfig)

instance Data.FromJSON SimulationApplicationConfig where
  parseJSON =
    Data.withObject
      "SimulationApplicationConfig"
      ( \x ->
          SimulationApplicationConfig'
            Prelude.<$> (x Data..:? "applicationVersion")
            Prelude.<*> (x Data..:? "tools" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "uploadConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "useDefaultTools")
            Prelude.<*> (x Data..:? "useDefaultUploadConfigurations")
            Prelude.<*> (x Data..:? "worldConfigs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "application")
            Prelude.<*> (x Data..: "launchConfig")
      )

instance Prelude.Hashable SimulationApplicationConfig where
  hashWithSalt _salt SimulationApplicationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` tools
      `Prelude.hashWithSalt` uploadConfigurations
      `Prelude.hashWithSalt` useDefaultTools
      `Prelude.hashWithSalt` useDefaultUploadConfigurations
      `Prelude.hashWithSalt` worldConfigs
      `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` launchConfig

instance Prelude.NFData SimulationApplicationConfig where
  rnf SimulationApplicationConfig' {..} =
    Prelude.rnf applicationVersion `Prelude.seq`
      Prelude.rnf tools `Prelude.seq`
        Prelude.rnf uploadConfigurations `Prelude.seq`
          Prelude.rnf useDefaultTools `Prelude.seq`
            Prelude.rnf useDefaultUploadConfigurations `Prelude.seq`
              Prelude.rnf worldConfigs `Prelude.seq`
                Prelude.rnf application `Prelude.seq`
                  Prelude.rnf launchConfig

instance Data.ToJSON SimulationApplicationConfig where
  toJSON SimulationApplicationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationVersion" Data..=)
              Prelude.<$> applicationVersion,
            ("tools" Data..=) Prelude.<$> tools,
            ("uploadConfigurations" Data..=)
              Prelude.<$> uploadConfigurations,
            ("useDefaultTools" Data..=)
              Prelude.<$> useDefaultTools,
            ("useDefaultUploadConfigurations" Data..=)
              Prelude.<$> useDefaultUploadConfigurations,
            ("worldConfigs" Data..=) Prelude.<$> worldConfigs,
            Prelude.Just ("application" Data..= application),
            Prelude.Just ("launchConfig" Data..= launchConfig)
          ]
      )
