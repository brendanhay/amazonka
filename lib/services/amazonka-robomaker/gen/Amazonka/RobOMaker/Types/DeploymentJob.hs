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
-- Module      : Amazonka.RobOMaker.Types.DeploymentJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DeploymentJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.DeploymentApplicationConfig
import Amazonka.RobOMaker.Types.DeploymentConfig
import Amazonka.RobOMaker.Types.DeploymentJobErrorCode
import Amazonka.RobOMaker.Types.DeploymentStatus

-- | Information about a deployment job.
--
-- /See:/ 'newDeploymentJob' smart constructor.
data DeploymentJob = DeploymentJob'
  { -- | The deployment application configuration.
    deploymentApplicationConfigs :: Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig),
    -- | The deployment job failure code.
    failureCode :: Prelude.Maybe DeploymentJobErrorCode,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleet :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deployment job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment job.
    status :: Prelude.Maybe DeploymentStatus,
    -- | The deployment configuration.
    deploymentConfig :: Prelude.Maybe DeploymentConfig,
    -- | The time, in milliseconds since the epoch, when the deployment job was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A short description of the reason why the deployment job failed.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentApplicationConfigs', 'deploymentJob_deploymentApplicationConfigs' - The deployment application configuration.
--
-- 'failureCode', 'deploymentJob_failureCode' - The deployment job failure code.
--
-- 'fleet', 'deploymentJob_fleet' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'arn', 'deploymentJob_arn' - The Amazon Resource Name (ARN) of the deployment job.
--
-- 'status', 'deploymentJob_status' - The status of the deployment job.
--
-- 'deploymentConfig', 'deploymentJob_deploymentConfig' - The deployment configuration.
--
-- 'createdAt', 'deploymentJob_createdAt' - The time, in milliseconds since the epoch, when the deployment job was
-- created.
--
-- 'failureReason', 'deploymentJob_failureReason' - A short description of the reason why the deployment job failed.
newDeploymentJob ::
  DeploymentJob
newDeploymentJob =
  DeploymentJob'
    { deploymentApplicationConfigs =
        Prelude.Nothing,
      failureCode = Prelude.Nothing,
      fleet = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      deploymentConfig = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The deployment application configuration.
deploymentJob_deploymentApplicationConfigs :: Lens.Lens' DeploymentJob (Prelude.Maybe (Prelude.NonEmpty DeploymentApplicationConfig))
deploymentJob_deploymentApplicationConfigs = Lens.lens (\DeploymentJob' {deploymentApplicationConfigs} -> deploymentApplicationConfigs) (\s@DeploymentJob' {} a -> s {deploymentApplicationConfigs = a} :: DeploymentJob) Prelude.. Lens.mapping Lens.coerced

-- | The deployment job failure code.
deploymentJob_failureCode :: Lens.Lens' DeploymentJob (Prelude.Maybe DeploymentJobErrorCode)
deploymentJob_failureCode = Lens.lens (\DeploymentJob' {failureCode} -> failureCode) (\s@DeploymentJob' {} a -> s {failureCode = a} :: DeploymentJob)

-- | The Amazon Resource Name (ARN) of the fleet.
deploymentJob_fleet :: Lens.Lens' DeploymentJob (Prelude.Maybe Prelude.Text)
deploymentJob_fleet = Lens.lens (\DeploymentJob' {fleet} -> fleet) (\s@DeploymentJob' {} a -> s {fleet = a} :: DeploymentJob)

-- | The Amazon Resource Name (ARN) of the deployment job.
deploymentJob_arn :: Lens.Lens' DeploymentJob (Prelude.Maybe Prelude.Text)
deploymentJob_arn = Lens.lens (\DeploymentJob' {arn} -> arn) (\s@DeploymentJob' {} a -> s {arn = a} :: DeploymentJob)

-- | The status of the deployment job.
deploymentJob_status :: Lens.Lens' DeploymentJob (Prelude.Maybe DeploymentStatus)
deploymentJob_status = Lens.lens (\DeploymentJob' {status} -> status) (\s@DeploymentJob' {} a -> s {status = a} :: DeploymentJob)

-- | The deployment configuration.
deploymentJob_deploymentConfig :: Lens.Lens' DeploymentJob (Prelude.Maybe DeploymentConfig)
deploymentJob_deploymentConfig = Lens.lens (\DeploymentJob' {deploymentConfig} -> deploymentConfig) (\s@DeploymentJob' {} a -> s {deploymentConfig = a} :: DeploymentJob)

-- | The time, in milliseconds since the epoch, when the deployment job was
-- created.
deploymentJob_createdAt :: Lens.Lens' DeploymentJob (Prelude.Maybe Prelude.UTCTime)
deploymentJob_createdAt = Lens.lens (\DeploymentJob' {createdAt} -> createdAt) (\s@DeploymentJob' {} a -> s {createdAt = a} :: DeploymentJob) Prelude.. Lens.mapping Core._Time

-- | A short description of the reason why the deployment job failed.
deploymentJob_failureReason :: Lens.Lens' DeploymentJob (Prelude.Maybe Prelude.Text)
deploymentJob_failureReason = Lens.lens (\DeploymentJob' {failureReason} -> failureReason) (\s@DeploymentJob' {} a -> s {failureReason = a} :: DeploymentJob)

instance Core.FromJSON DeploymentJob where
  parseJSON =
    Core.withObject
      "DeploymentJob"
      ( \x ->
          DeploymentJob'
            Prelude.<$> (x Core..:? "deploymentApplicationConfigs")
            Prelude.<*> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "fleet")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "deploymentConfig")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable DeploymentJob where
  hashWithSalt _salt DeploymentJob' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentApplicationConfigs
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` fleet
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` deploymentConfig
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData DeploymentJob where
  rnf DeploymentJob' {..} =
    Prelude.rnf deploymentApplicationConfigs
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf deploymentConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureReason
