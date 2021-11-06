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
-- Module      : Amazonka.RobOMaker.Types.DeploymentLaunchConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DeploymentLaunchConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for a deployment launch.
--
-- /See:/ 'newDeploymentLaunchConfig' smart constructor.
data DeploymentLaunchConfig = DeploymentLaunchConfig'
  { -- | The deployment pre-launch file. This file will be executed prior to the
    -- launch file.
    preLaunchFile :: Prelude.Maybe Prelude.Text,
    -- | The deployment post-launch file. This file will be executed after the
    -- launch file.
    postLaunchFile :: Prelude.Maybe Prelude.Text,
    -- | An array of key\/value pairs specifying environment variables for the
    -- robot application
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The package name.
    packageName :: Prelude.Text,
    -- | The launch file name.
    launchFile :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentLaunchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preLaunchFile', 'deploymentLaunchConfig_preLaunchFile' - The deployment pre-launch file. This file will be executed prior to the
-- launch file.
--
-- 'postLaunchFile', 'deploymentLaunchConfig_postLaunchFile' - The deployment post-launch file. This file will be executed after the
-- launch file.
--
-- 'environmentVariables', 'deploymentLaunchConfig_environmentVariables' - An array of key\/value pairs specifying environment variables for the
-- robot application
--
-- 'packageName', 'deploymentLaunchConfig_packageName' - The package name.
--
-- 'launchFile', 'deploymentLaunchConfig_launchFile' - The launch file name.
newDeploymentLaunchConfig ::
  -- | 'packageName'
  Prelude.Text ->
  -- | 'launchFile'
  Prelude.Text ->
  DeploymentLaunchConfig
newDeploymentLaunchConfig pPackageName_ pLaunchFile_ =
  DeploymentLaunchConfig'
    { preLaunchFile =
        Prelude.Nothing,
      postLaunchFile = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      packageName = pPackageName_,
      launchFile = pLaunchFile_
    }

-- | The deployment pre-launch file. This file will be executed prior to the
-- launch file.
deploymentLaunchConfig_preLaunchFile :: Lens.Lens' DeploymentLaunchConfig (Prelude.Maybe Prelude.Text)
deploymentLaunchConfig_preLaunchFile = Lens.lens (\DeploymentLaunchConfig' {preLaunchFile} -> preLaunchFile) (\s@DeploymentLaunchConfig' {} a -> s {preLaunchFile = a} :: DeploymentLaunchConfig)

-- | The deployment post-launch file. This file will be executed after the
-- launch file.
deploymentLaunchConfig_postLaunchFile :: Lens.Lens' DeploymentLaunchConfig (Prelude.Maybe Prelude.Text)
deploymentLaunchConfig_postLaunchFile = Lens.lens (\DeploymentLaunchConfig' {postLaunchFile} -> postLaunchFile) (\s@DeploymentLaunchConfig' {} a -> s {postLaunchFile = a} :: DeploymentLaunchConfig)

-- | An array of key\/value pairs specifying environment variables for the
-- robot application
deploymentLaunchConfig_environmentVariables :: Lens.Lens' DeploymentLaunchConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deploymentLaunchConfig_environmentVariables = Lens.lens (\DeploymentLaunchConfig' {environmentVariables} -> environmentVariables) (\s@DeploymentLaunchConfig' {} a -> s {environmentVariables = a} :: DeploymentLaunchConfig) Prelude.. Lens.mapping Lens.coerced

-- | The package name.
deploymentLaunchConfig_packageName :: Lens.Lens' DeploymentLaunchConfig Prelude.Text
deploymentLaunchConfig_packageName = Lens.lens (\DeploymentLaunchConfig' {packageName} -> packageName) (\s@DeploymentLaunchConfig' {} a -> s {packageName = a} :: DeploymentLaunchConfig)

-- | The launch file name.
deploymentLaunchConfig_launchFile :: Lens.Lens' DeploymentLaunchConfig Prelude.Text
deploymentLaunchConfig_launchFile = Lens.lens (\DeploymentLaunchConfig' {launchFile} -> launchFile) (\s@DeploymentLaunchConfig' {} a -> s {launchFile = a} :: DeploymentLaunchConfig)

instance Core.FromJSON DeploymentLaunchConfig where
  parseJSON =
    Core.withObject
      "DeploymentLaunchConfig"
      ( \x ->
          DeploymentLaunchConfig'
            Prelude.<$> (x Core..:? "preLaunchFile")
            Prelude.<*> (x Core..:? "postLaunchFile")
            Prelude.<*> ( x Core..:? "environmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "packageName")
            Prelude.<*> (x Core..: "launchFile")
      )

instance Prelude.Hashable DeploymentLaunchConfig

instance Prelude.NFData DeploymentLaunchConfig

instance Core.ToJSON DeploymentLaunchConfig where
  toJSON DeploymentLaunchConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("preLaunchFile" Core..=) Prelude.<$> preLaunchFile,
            ("postLaunchFile" Core..=)
              Prelude.<$> postLaunchFile,
            ("environmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            Prelude.Just ("packageName" Core..= packageName),
            Prelude.Just ("launchFile" Core..= launchFile)
          ]
      )
