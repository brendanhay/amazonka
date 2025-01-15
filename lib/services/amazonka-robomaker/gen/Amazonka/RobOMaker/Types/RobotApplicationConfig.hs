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
-- Module      : Amazonka.RobOMaker.Types.RobotApplicationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RobotApplicationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.LaunchConfig
import Amazonka.RobOMaker.Types.Tool
import Amazonka.RobOMaker.Types.UploadConfiguration

-- | Application configuration information for a robot.
--
-- /See:/ 'newRobotApplicationConfig' smart constructor.
data RobotApplicationConfig = RobotApplicationConfig'
  { -- | The version of the robot application.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about tools configured for the robot application.
    tools :: Prelude.Maybe [Tool],
    -- | The upload configurations for the robot application.
    uploadConfigurations :: Prelude.Maybe [UploadConfiguration],
    -- | A Boolean indicating whether to use default robot application tools. The
    -- default tools are rviz, rqt, terminal and rosbag record. The default is
    -- @False@.
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
    -- | The application information for the robot application.
    application :: Prelude.Text,
    -- | The launch configuration for the robot application.
    launchConfig :: LaunchConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RobotApplicationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'robotApplicationConfig_applicationVersion' - The version of the robot application.
--
-- 'tools', 'robotApplicationConfig_tools' - Information about tools configured for the robot application.
--
-- 'uploadConfigurations', 'robotApplicationConfig_uploadConfigurations' - The upload configurations for the robot application.
--
-- 'useDefaultTools', 'robotApplicationConfig_useDefaultTools' - A Boolean indicating whether to use default robot application tools. The
-- default tools are rviz, rqt, terminal and rosbag record. The default is
-- @False@.
--
-- This API is no longer supported and will throw an error if used.
--
-- 'useDefaultUploadConfigurations', 'robotApplicationConfig_useDefaultUploadConfigurations' - A Boolean indicating whether to use default upload configurations. By
-- default, @.ros@ and @.gazebo@ files are uploaded when the application
-- terminates and all ROS topics will be recorded.
--
-- If you set this value, you must specify an @outputLocation@.
--
-- This API is no longer supported and will throw an error if used.
--
-- 'application', 'robotApplicationConfig_application' - The application information for the robot application.
--
-- 'launchConfig', 'robotApplicationConfig_launchConfig' - The launch configuration for the robot application.
newRobotApplicationConfig ::
  -- | 'application'
  Prelude.Text ->
  -- | 'launchConfig'
  LaunchConfig ->
  RobotApplicationConfig
newRobotApplicationConfig
  pApplication_
  pLaunchConfig_ =
    RobotApplicationConfig'
      { applicationVersion =
          Prelude.Nothing,
        tools = Prelude.Nothing,
        uploadConfigurations = Prelude.Nothing,
        useDefaultTools = Prelude.Nothing,
        useDefaultUploadConfigurations = Prelude.Nothing,
        application = pApplication_,
        launchConfig = pLaunchConfig_
      }

-- | The version of the robot application.
robotApplicationConfig_applicationVersion :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe Prelude.Text)
robotApplicationConfig_applicationVersion = Lens.lens (\RobotApplicationConfig' {applicationVersion} -> applicationVersion) (\s@RobotApplicationConfig' {} a -> s {applicationVersion = a} :: RobotApplicationConfig)

-- | Information about tools configured for the robot application.
robotApplicationConfig_tools :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe [Tool])
robotApplicationConfig_tools = Lens.lens (\RobotApplicationConfig' {tools} -> tools) (\s@RobotApplicationConfig' {} a -> s {tools = a} :: RobotApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | The upload configurations for the robot application.
robotApplicationConfig_uploadConfigurations :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe [UploadConfiguration])
robotApplicationConfig_uploadConfigurations = Lens.lens (\RobotApplicationConfig' {uploadConfigurations} -> uploadConfigurations) (\s@RobotApplicationConfig' {} a -> s {uploadConfigurations = a} :: RobotApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean indicating whether to use default robot application tools. The
-- default tools are rviz, rqt, terminal and rosbag record. The default is
-- @False@.
--
-- This API is no longer supported and will throw an error if used.
robotApplicationConfig_useDefaultTools :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe Prelude.Bool)
robotApplicationConfig_useDefaultTools = Lens.lens (\RobotApplicationConfig' {useDefaultTools} -> useDefaultTools) (\s@RobotApplicationConfig' {} a -> s {useDefaultTools = a} :: RobotApplicationConfig)

-- | A Boolean indicating whether to use default upload configurations. By
-- default, @.ros@ and @.gazebo@ files are uploaded when the application
-- terminates and all ROS topics will be recorded.
--
-- If you set this value, you must specify an @outputLocation@.
--
-- This API is no longer supported and will throw an error if used.
robotApplicationConfig_useDefaultUploadConfigurations :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe Prelude.Bool)
robotApplicationConfig_useDefaultUploadConfigurations = Lens.lens (\RobotApplicationConfig' {useDefaultUploadConfigurations} -> useDefaultUploadConfigurations) (\s@RobotApplicationConfig' {} a -> s {useDefaultUploadConfigurations = a} :: RobotApplicationConfig)

-- | The application information for the robot application.
robotApplicationConfig_application :: Lens.Lens' RobotApplicationConfig Prelude.Text
robotApplicationConfig_application = Lens.lens (\RobotApplicationConfig' {application} -> application) (\s@RobotApplicationConfig' {} a -> s {application = a} :: RobotApplicationConfig)

-- | The launch configuration for the robot application.
robotApplicationConfig_launchConfig :: Lens.Lens' RobotApplicationConfig LaunchConfig
robotApplicationConfig_launchConfig = Lens.lens (\RobotApplicationConfig' {launchConfig} -> launchConfig) (\s@RobotApplicationConfig' {} a -> s {launchConfig = a} :: RobotApplicationConfig)

instance Data.FromJSON RobotApplicationConfig where
  parseJSON =
    Data.withObject
      "RobotApplicationConfig"
      ( \x ->
          RobotApplicationConfig'
            Prelude.<$> (x Data..:? "applicationVersion")
            Prelude.<*> (x Data..:? "tools" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "uploadConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "useDefaultTools")
            Prelude.<*> (x Data..:? "useDefaultUploadConfigurations")
            Prelude.<*> (x Data..: "application")
            Prelude.<*> (x Data..: "launchConfig")
      )

instance Prelude.Hashable RobotApplicationConfig where
  hashWithSalt _salt RobotApplicationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` tools
      `Prelude.hashWithSalt` uploadConfigurations
      `Prelude.hashWithSalt` useDefaultTools
      `Prelude.hashWithSalt` useDefaultUploadConfigurations
      `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` launchConfig

instance Prelude.NFData RobotApplicationConfig where
  rnf RobotApplicationConfig' {..} =
    Prelude.rnf applicationVersion `Prelude.seq`
      Prelude.rnf tools `Prelude.seq`
        Prelude.rnf uploadConfigurations `Prelude.seq`
          Prelude.rnf useDefaultTools `Prelude.seq`
            Prelude.rnf useDefaultUploadConfigurations `Prelude.seq`
              Prelude.rnf application `Prelude.seq`
                Prelude.rnf launchConfig

instance Data.ToJSON RobotApplicationConfig where
  toJSON RobotApplicationConfig' {..} =
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
            Prelude.Just ("application" Data..= application),
            Prelude.Just ("launchConfig" Data..= launchConfig)
          ]
      )
