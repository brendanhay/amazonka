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
-- Module      : Amazonka.RobOMaker.Types.DeploymentApplicationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DeploymentApplicationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.DeploymentLaunchConfig

-- | Information about a deployment application configuration.
--
-- /See:/ 'newDeploymentApplicationConfig' smart constructor.
data DeploymentApplicationConfig = DeploymentApplicationConfig'
  { -- | The Amazon Resource Name (ARN) of the robot application.
    application :: Prelude.Text,
    -- | The version of the application.
    applicationVersion :: Prelude.Text,
    -- | The launch configuration.
    launchConfig :: DeploymentLaunchConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentApplicationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'deploymentApplicationConfig_application' - The Amazon Resource Name (ARN) of the robot application.
--
-- 'applicationVersion', 'deploymentApplicationConfig_applicationVersion' - The version of the application.
--
-- 'launchConfig', 'deploymentApplicationConfig_launchConfig' - The launch configuration.
newDeploymentApplicationConfig ::
  -- | 'application'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Text ->
  -- | 'launchConfig'
  DeploymentLaunchConfig ->
  DeploymentApplicationConfig
newDeploymentApplicationConfig
  pApplication_
  pApplicationVersion_
  pLaunchConfig_ =
    DeploymentApplicationConfig'
      { application =
          pApplication_,
        applicationVersion = pApplicationVersion_,
        launchConfig = pLaunchConfig_
      }

-- | The Amazon Resource Name (ARN) of the robot application.
deploymentApplicationConfig_application :: Lens.Lens' DeploymentApplicationConfig Prelude.Text
deploymentApplicationConfig_application = Lens.lens (\DeploymentApplicationConfig' {application} -> application) (\s@DeploymentApplicationConfig' {} a -> s {application = a} :: DeploymentApplicationConfig)

-- | The version of the application.
deploymentApplicationConfig_applicationVersion :: Lens.Lens' DeploymentApplicationConfig Prelude.Text
deploymentApplicationConfig_applicationVersion = Lens.lens (\DeploymentApplicationConfig' {applicationVersion} -> applicationVersion) (\s@DeploymentApplicationConfig' {} a -> s {applicationVersion = a} :: DeploymentApplicationConfig)

-- | The launch configuration.
deploymentApplicationConfig_launchConfig :: Lens.Lens' DeploymentApplicationConfig DeploymentLaunchConfig
deploymentApplicationConfig_launchConfig = Lens.lens (\DeploymentApplicationConfig' {launchConfig} -> launchConfig) (\s@DeploymentApplicationConfig' {} a -> s {launchConfig = a} :: DeploymentApplicationConfig)

instance Core.FromJSON DeploymentApplicationConfig where
  parseJSON =
    Core.withObject
      "DeploymentApplicationConfig"
      ( \x ->
          DeploymentApplicationConfig'
            Prelude.<$> (x Core..: "application")
            Prelude.<*> (x Core..: "applicationVersion")
            Prelude.<*> (x Core..: "launchConfig")
      )

instance Prelude.Hashable DeploymentApplicationConfig where
  hashWithSalt salt' DeploymentApplicationConfig' {..} =
    salt' `Prelude.hashWithSalt` launchConfig
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` application

instance Prelude.NFData DeploymentApplicationConfig where
  rnf DeploymentApplicationConfig' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf launchConfig
      `Prelude.seq` Prelude.rnf applicationVersion

instance Core.ToJSON DeploymentApplicationConfig where
  toJSON DeploymentApplicationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("application" Core..= application),
            Prelude.Just
              ("applicationVersion" Core..= applicationVersion),
            Prelude.Just ("launchConfig" Core..= launchConfig)
          ]
      )
