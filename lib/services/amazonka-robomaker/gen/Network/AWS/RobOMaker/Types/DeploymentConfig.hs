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
-- Module      : Network.AWS.RobOMaker.Types.DeploymentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.DeploymentConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.S3Object

-- | Information about a deployment configuration.
--
-- /See:/ 'newDeploymentConfig' smart constructor.
data DeploymentConfig = DeploymentConfig'
  { -- | The percentage of robots receiving the deployment at the same time.
    concurrentDeploymentPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The download condition file.
    downloadConditionFile :: Prelude.Maybe S3Object,
    -- | The percentage of deployments that need to fail before stopping
    -- deployment.
    failureThresholdPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time, in seconds, to wait for deployment to a single robot
    -- to complete. Choose a time between 1 minute and 7 days. The default is 5
    -- hours.
    robotDeploymentTimeoutInSeconds :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'concurrentDeploymentPercentage', 'deploymentConfig_concurrentDeploymentPercentage' - The percentage of robots receiving the deployment at the same time.
--
-- 'downloadConditionFile', 'deploymentConfig_downloadConditionFile' - The download condition file.
--
-- 'failureThresholdPercentage', 'deploymentConfig_failureThresholdPercentage' - The percentage of deployments that need to fail before stopping
-- deployment.
--
-- 'robotDeploymentTimeoutInSeconds', 'deploymentConfig_robotDeploymentTimeoutInSeconds' - The amount of time, in seconds, to wait for deployment to a single robot
-- to complete. Choose a time between 1 minute and 7 days. The default is 5
-- hours.
newDeploymentConfig ::
  DeploymentConfig
newDeploymentConfig =
  DeploymentConfig'
    { concurrentDeploymentPercentage =
        Prelude.Nothing,
      downloadConditionFile = Prelude.Nothing,
      failureThresholdPercentage = Prelude.Nothing,
      robotDeploymentTimeoutInSeconds = Prelude.Nothing
    }

-- | The percentage of robots receiving the deployment at the same time.
deploymentConfig_concurrentDeploymentPercentage :: Lens.Lens' DeploymentConfig (Prelude.Maybe Prelude.Natural)
deploymentConfig_concurrentDeploymentPercentage = Lens.lens (\DeploymentConfig' {concurrentDeploymentPercentage} -> concurrentDeploymentPercentage) (\s@DeploymentConfig' {} a -> s {concurrentDeploymentPercentage = a} :: DeploymentConfig)

-- | The download condition file.
deploymentConfig_downloadConditionFile :: Lens.Lens' DeploymentConfig (Prelude.Maybe S3Object)
deploymentConfig_downloadConditionFile = Lens.lens (\DeploymentConfig' {downloadConditionFile} -> downloadConditionFile) (\s@DeploymentConfig' {} a -> s {downloadConditionFile = a} :: DeploymentConfig)

-- | The percentage of deployments that need to fail before stopping
-- deployment.
deploymentConfig_failureThresholdPercentage :: Lens.Lens' DeploymentConfig (Prelude.Maybe Prelude.Natural)
deploymentConfig_failureThresholdPercentage = Lens.lens (\DeploymentConfig' {failureThresholdPercentage} -> failureThresholdPercentage) (\s@DeploymentConfig' {} a -> s {failureThresholdPercentage = a} :: DeploymentConfig)

-- | The amount of time, in seconds, to wait for deployment to a single robot
-- to complete. Choose a time between 1 minute and 7 days. The default is 5
-- hours.
deploymentConfig_robotDeploymentTimeoutInSeconds :: Lens.Lens' DeploymentConfig (Prelude.Maybe Prelude.Integer)
deploymentConfig_robotDeploymentTimeoutInSeconds = Lens.lens (\DeploymentConfig' {robotDeploymentTimeoutInSeconds} -> robotDeploymentTimeoutInSeconds) (\s@DeploymentConfig' {} a -> s {robotDeploymentTimeoutInSeconds = a} :: DeploymentConfig)

instance Core.FromJSON DeploymentConfig where
  parseJSON =
    Core.withObject
      "DeploymentConfig"
      ( \x ->
          DeploymentConfig'
            Prelude.<$> (x Core..:? "concurrentDeploymentPercentage")
            Prelude.<*> (x Core..:? "downloadConditionFile")
            Prelude.<*> (x Core..:? "failureThresholdPercentage")
            Prelude.<*> (x Core..:? "robotDeploymentTimeoutInSeconds")
      )

instance Prelude.Hashable DeploymentConfig

instance Prelude.NFData DeploymentConfig

instance Core.ToJSON DeploymentConfig where
  toJSON DeploymentConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("concurrentDeploymentPercentage" Core..=)
              Prelude.<$> concurrentDeploymentPercentage,
            ("downloadConditionFile" Core..=)
              Prelude.<$> downloadConditionFile,
            ("failureThresholdPercentage" Core..=)
              Prelude.<$> failureThresholdPercentage,
            ("robotDeploymentTimeoutInSeconds" Core..=)
              Prelude.<$> robotDeploymentTimeoutInSeconds
          ]
      )
