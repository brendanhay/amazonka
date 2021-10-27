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
-- Module      : Network.AWS.RobOMaker.Types.RobotApplicationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.RobotApplicationConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.LaunchConfig
import Network.AWS.RobOMaker.Types.Tool
import Network.AWS.RobOMaker.Types.UploadConfiguration

-- | Application configuration information for a robot.
--
-- /See:/ 'newRobotApplicationConfig' smart constructor.
data RobotApplicationConfig = RobotApplicationConfig'
  { -- | A Boolean indicating whether to use default upload configurations. By
    -- default, @.ros@ and @.gazebo@ files are uploaded when the application
    -- terminates and all ROS topics will be recorded.
    --
    -- If you set this value, you must specify an @outputLocation@.
    useDefaultUploadConfigurations :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean indicating whether to use default robot application tools. The
    -- default tools are rviz, rqt, terminal and rosbag record. The default is
    -- @False@.
    useDefaultTools :: Prelude.Maybe Prelude.Bool,
    -- | The version of the robot application.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | The upload configurations for the robot application.
    uploadConfigurations :: Prelude.Maybe [UploadConfiguration],
    -- | Information about tools configured for the robot application.
    tools :: Prelude.Maybe [Tool],
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
-- 'useDefaultUploadConfigurations', 'robotApplicationConfig_useDefaultUploadConfigurations' - A Boolean indicating whether to use default upload configurations. By
-- default, @.ros@ and @.gazebo@ files are uploaded when the application
-- terminates and all ROS topics will be recorded.
--
-- If you set this value, you must specify an @outputLocation@.
--
-- 'useDefaultTools', 'robotApplicationConfig_useDefaultTools' - A Boolean indicating whether to use default robot application tools. The
-- default tools are rviz, rqt, terminal and rosbag record. The default is
-- @False@.
--
-- 'applicationVersion', 'robotApplicationConfig_applicationVersion' - The version of the robot application.
--
-- 'uploadConfigurations', 'robotApplicationConfig_uploadConfigurations' - The upload configurations for the robot application.
--
-- 'tools', 'robotApplicationConfig_tools' - Information about tools configured for the robot application.
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
      { useDefaultUploadConfigurations =
          Prelude.Nothing,
        useDefaultTools = Prelude.Nothing,
        applicationVersion = Prelude.Nothing,
        uploadConfigurations = Prelude.Nothing,
        tools = Prelude.Nothing,
        application = pApplication_,
        launchConfig = pLaunchConfig_
      }

-- | A Boolean indicating whether to use default upload configurations. By
-- default, @.ros@ and @.gazebo@ files are uploaded when the application
-- terminates and all ROS topics will be recorded.
--
-- If you set this value, you must specify an @outputLocation@.
robotApplicationConfig_useDefaultUploadConfigurations :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe Prelude.Bool)
robotApplicationConfig_useDefaultUploadConfigurations = Lens.lens (\RobotApplicationConfig' {useDefaultUploadConfigurations} -> useDefaultUploadConfigurations) (\s@RobotApplicationConfig' {} a -> s {useDefaultUploadConfigurations = a} :: RobotApplicationConfig)

-- | A Boolean indicating whether to use default robot application tools. The
-- default tools are rviz, rqt, terminal and rosbag record. The default is
-- @False@.
robotApplicationConfig_useDefaultTools :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe Prelude.Bool)
robotApplicationConfig_useDefaultTools = Lens.lens (\RobotApplicationConfig' {useDefaultTools} -> useDefaultTools) (\s@RobotApplicationConfig' {} a -> s {useDefaultTools = a} :: RobotApplicationConfig)

-- | The version of the robot application.
robotApplicationConfig_applicationVersion :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe Prelude.Text)
robotApplicationConfig_applicationVersion = Lens.lens (\RobotApplicationConfig' {applicationVersion} -> applicationVersion) (\s@RobotApplicationConfig' {} a -> s {applicationVersion = a} :: RobotApplicationConfig)

-- | The upload configurations for the robot application.
robotApplicationConfig_uploadConfigurations :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe [UploadConfiguration])
robotApplicationConfig_uploadConfigurations = Lens.lens (\RobotApplicationConfig' {uploadConfigurations} -> uploadConfigurations) (\s@RobotApplicationConfig' {} a -> s {uploadConfigurations = a} :: RobotApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | Information about tools configured for the robot application.
robotApplicationConfig_tools :: Lens.Lens' RobotApplicationConfig (Prelude.Maybe [Tool])
robotApplicationConfig_tools = Lens.lens (\RobotApplicationConfig' {tools} -> tools) (\s@RobotApplicationConfig' {} a -> s {tools = a} :: RobotApplicationConfig) Prelude.. Lens.mapping Lens.coerced

-- | The application information for the robot application.
robotApplicationConfig_application :: Lens.Lens' RobotApplicationConfig Prelude.Text
robotApplicationConfig_application = Lens.lens (\RobotApplicationConfig' {application} -> application) (\s@RobotApplicationConfig' {} a -> s {application = a} :: RobotApplicationConfig)

-- | The launch configuration for the robot application.
robotApplicationConfig_launchConfig :: Lens.Lens' RobotApplicationConfig LaunchConfig
robotApplicationConfig_launchConfig = Lens.lens (\RobotApplicationConfig' {launchConfig} -> launchConfig) (\s@RobotApplicationConfig' {} a -> s {launchConfig = a} :: RobotApplicationConfig)

instance Core.FromJSON RobotApplicationConfig where
  parseJSON =
    Core.withObject
      "RobotApplicationConfig"
      ( \x ->
          RobotApplicationConfig'
            Prelude.<$> (x Core..:? "useDefaultUploadConfigurations")
            Prelude.<*> (x Core..:? "useDefaultTools")
            Prelude.<*> (x Core..:? "applicationVersion")
            Prelude.<*> ( x Core..:? "uploadConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "tools" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "application")
            Prelude.<*> (x Core..: "launchConfig")
      )

instance Prelude.Hashable RobotApplicationConfig

instance Prelude.NFData RobotApplicationConfig

instance Core.ToJSON RobotApplicationConfig where
  toJSON RobotApplicationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("useDefaultUploadConfigurations" Core..=)
              Prelude.<$> useDefaultUploadConfigurations,
            ("useDefaultTools" Core..=)
              Prelude.<$> useDefaultTools,
            ("applicationVersion" Core..=)
              Prelude.<$> applicationVersion,
            ("uploadConfigurations" Core..=)
              Prelude.<$> uploadConfigurations,
            ("tools" Core..=) Prelude.<$> tools,
            Prelude.Just ("application" Core..= application),
            Prelude.Just ("launchConfig" Core..= launchConfig)
          ]
      )
