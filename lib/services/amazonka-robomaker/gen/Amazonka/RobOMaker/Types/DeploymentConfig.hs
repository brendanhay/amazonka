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
-- Module      : Amazonka.RobOMaker.Types.DeploymentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DeploymentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.S3Object

-- | Information about a deployment configuration.
--
-- /See:/ 'newDeploymentConfig' smart constructor.
data DeploymentConfig = DeploymentConfig'
  { -- | The amount of time, in seconds, to wait for deployment to a single robot
    -- to complete. Choose a time between 1 minute and 7 days. The default is 5
    -- hours.
    robotDeploymentTimeoutInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The download condition file.
    downloadConditionFile :: Prelude.Maybe S3Object,
    -- | The percentage of robots receiving the deployment at the same time.
    concurrentDeploymentPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of deployments that need to fail before stopping
    -- deployment.
    failureThresholdPercentage :: Prelude.Maybe Prelude.Natural
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
-- 'robotDeploymentTimeoutInSeconds', 'deploymentConfig_robotDeploymentTimeoutInSeconds' - The amount of time, in seconds, to wait for deployment to a single robot
-- to complete. Choose a time between 1 minute and 7 days. The default is 5
-- hours.
--
-- 'downloadConditionFile', 'deploymentConfig_downloadConditionFile' - The download condition file.
--
-- 'concurrentDeploymentPercentage', 'deploymentConfig_concurrentDeploymentPercentage' - The percentage of robots receiving the deployment at the same time.
--
-- 'failureThresholdPercentage', 'deploymentConfig_failureThresholdPercentage' - The percentage of deployments that need to fail before stopping
-- deployment.
newDeploymentConfig ::
  DeploymentConfig
newDeploymentConfig =
  DeploymentConfig'
    { robotDeploymentTimeoutInSeconds =
        Prelude.Nothing,
      downloadConditionFile = Prelude.Nothing,
      concurrentDeploymentPercentage = Prelude.Nothing,
      failureThresholdPercentage = Prelude.Nothing
    }

-- | The amount of time, in seconds, to wait for deployment to a single robot
-- to complete. Choose a time between 1 minute and 7 days. The default is 5
-- hours.
deploymentConfig_robotDeploymentTimeoutInSeconds :: Lens.Lens' DeploymentConfig (Prelude.Maybe Prelude.Integer)
deploymentConfig_robotDeploymentTimeoutInSeconds = Lens.lens (\DeploymentConfig' {robotDeploymentTimeoutInSeconds} -> robotDeploymentTimeoutInSeconds) (\s@DeploymentConfig' {} a -> s {robotDeploymentTimeoutInSeconds = a} :: DeploymentConfig)

-- | The download condition file.
deploymentConfig_downloadConditionFile :: Lens.Lens' DeploymentConfig (Prelude.Maybe S3Object)
deploymentConfig_downloadConditionFile = Lens.lens (\DeploymentConfig' {downloadConditionFile} -> downloadConditionFile) (\s@DeploymentConfig' {} a -> s {downloadConditionFile = a} :: DeploymentConfig)

-- | The percentage of robots receiving the deployment at the same time.
deploymentConfig_concurrentDeploymentPercentage :: Lens.Lens' DeploymentConfig (Prelude.Maybe Prelude.Natural)
deploymentConfig_concurrentDeploymentPercentage = Lens.lens (\DeploymentConfig' {concurrentDeploymentPercentage} -> concurrentDeploymentPercentage) (\s@DeploymentConfig' {} a -> s {concurrentDeploymentPercentage = a} :: DeploymentConfig)

-- | The percentage of deployments that need to fail before stopping
-- deployment.
deploymentConfig_failureThresholdPercentage :: Lens.Lens' DeploymentConfig (Prelude.Maybe Prelude.Natural)
deploymentConfig_failureThresholdPercentage = Lens.lens (\DeploymentConfig' {failureThresholdPercentage} -> failureThresholdPercentage) (\s@DeploymentConfig' {} a -> s {failureThresholdPercentage = a} :: DeploymentConfig)

instance Core.FromJSON DeploymentConfig where
  parseJSON =
    Core.withObject
      "DeploymentConfig"
      ( \x ->
          DeploymentConfig'
            Prelude.<$> (x Core..:? "robotDeploymentTimeoutInSeconds")
            Prelude.<*> (x Core..:? "downloadConditionFile")
            Prelude.<*> (x Core..:? "concurrentDeploymentPercentage")
            Prelude.<*> (x Core..:? "failureThresholdPercentage")
      )

instance Prelude.Hashable DeploymentConfig where
  hashWithSalt _salt DeploymentConfig' {..} =
    _salt
      `Prelude.hashWithSalt` robotDeploymentTimeoutInSeconds
      `Prelude.hashWithSalt` downloadConditionFile
      `Prelude.hashWithSalt` concurrentDeploymentPercentage
      `Prelude.hashWithSalt` failureThresholdPercentage

instance Prelude.NFData DeploymentConfig where
  rnf DeploymentConfig' {..} =
    Prelude.rnf robotDeploymentTimeoutInSeconds
      `Prelude.seq` Prelude.rnf downloadConditionFile
      `Prelude.seq` Prelude.rnf concurrentDeploymentPercentage
      `Prelude.seq` Prelude.rnf failureThresholdPercentage

instance Core.ToJSON DeploymentConfig where
  toJSON DeploymentConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("robotDeploymentTimeoutInSeconds" Core..=)
              Prelude.<$> robotDeploymentTimeoutInSeconds,
            ("downloadConditionFile" Core..=)
              Prelude.<$> downloadConditionFile,
            ("concurrentDeploymentPercentage" Core..=)
              Prelude.<$> concurrentDeploymentPercentage,
            ("failureThresholdPercentage" Core..=)
              Prelude.<$> failureThresholdPercentage
          ]
      )
