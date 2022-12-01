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
-- Module      : Amazonka.M2.Types.DeploymentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DeploymentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types.DeploymentLifecycle
import qualified Amazonka.Prelude as Prelude

-- | A subset of information about a specific deployment.
--
-- /See:/ 'newDeploymentSummary' smart constructor.
data DeploymentSummary = DeploymentSummary'
  { -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the application.
    applicationId :: Prelude.Text,
    -- | The version of the application.
    applicationVersion :: Prelude.Natural,
    -- | The timestamp when the deployment was created.
    creationTime :: Core.POSIX,
    -- | The unique identifier of the deployment.
    deploymentId :: Prelude.Text,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Text,
    -- | The current status of the deployment.
    status :: DeploymentLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'deploymentSummary_statusReason' - The reason for the reported status.
--
-- 'applicationId', 'deploymentSummary_applicationId' - The unique identifier of the application.
--
-- 'applicationVersion', 'deploymentSummary_applicationVersion' - The version of the application.
--
-- 'creationTime', 'deploymentSummary_creationTime' - The timestamp when the deployment was created.
--
-- 'deploymentId', 'deploymentSummary_deploymentId' - The unique identifier of the deployment.
--
-- 'environmentId', 'deploymentSummary_environmentId' - The unique identifier of the environment.
--
-- 'status', 'deploymentSummary_status' - The current status of the deployment.
newDeploymentSummary ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'deploymentId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'status'
  DeploymentLifecycle ->
  DeploymentSummary
newDeploymentSummary
  pApplicationId_
  pApplicationVersion_
  pCreationTime_
  pDeploymentId_
  pEnvironmentId_
  pStatus_ =
    DeploymentSummary'
      { statusReason = Prelude.Nothing,
        applicationId = pApplicationId_,
        applicationVersion = pApplicationVersion_,
        creationTime = Core._Time Lens.# pCreationTime_,
        deploymentId = pDeploymentId_,
        environmentId = pEnvironmentId_,
        status = pStatus_
      }

-- | The reason for the reported status.
deploymentSummary_statusReason :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_statusReason = Lens.lens (\DeploymentSummary' {statusReason} -> statusReason) (\s@DeploymentSummary' {} a -> s {statusReason = a} :: DeploymentSummary)

-- | The unique identifier of the application.
deploymentSummary_applicationId :: Lens.Lens' DeploymentSummary Prelude.Text
deploymentSummary_applicationId = Lens.lens (\DeploymentSummary' {applicationId} -> applicationId) (\s@DeploymentSummary' {} a -> s {applicationId = a} :: DeploymentSummary)

-- | The version of the application.
deploymentSummary_applicationVersion :: Lens.Lens' DeploymentSummary Prelude.Natural
deploymentSummary_applicationVersion = Lens.lens (\DeploymentSummary' {applicationVersion} -> applicationVersion) (\s@DeploymentSummary' {} a -> s {applicationVersion = a} :: DeploymentSummary)

-- | The timestamp when the deployment was created.
deploymentSummary_creationTime :: Lens.Lens' DeploymentSummary Prelude.UTCTime
deploymentSummary_creationTime = Lens.lens (\DeploymentSummary' {creationTime} -> creationTime) (\s@DeploymentSummary' {} a -> s {creationTime = a} :: DeploymentSummary) Prelude.. Core._Time

-- | The unique identifier of the deployment.
deploymentSummary_deploymentId :: Lens.Lens' DeploymentSummary Prelude.Text
deploymentSummary_deploymentId = Lens.lens (\DeploymentSummary' {deploymentId} -> deploymentId) (\s@DeploymentSummary' {} a -> s {deploymentId = a} :: DeploymentSummary)

-- | The unique identifier of the environment.
deploymentSummary_environmentId :: Lens.Lens' DeploymentSummary Prelude.Text
deploymentSummary_environmentId = Lens.lens (\DeploymentSummary' {environmentId} -> environmentId) (\s@DeploymentSummary' {} a -> s {environmentId = a} :: DeploymentSummary)

-- | The current status of the deployment.
deploymentSummary_status :: Lens.Lens' DeploymentSummary DeploymentLifecycle
deploymentSummary_status = Lens.lens (\DeploymentSummary' {status} -> status) (\s@DeploymentSummary' {} a -> s {status = a} :: DeploymentSummary)

instance Core.FromJSON DeploymentSummary where
  parseJSON =
    Core.withObject
      "DeploymentSummary"
      ( \x ->
          DeploymentSummary'
            Prelude.<$> (x Core..:? "statusReason")
            Prelude.<*> (x Core..: "applicationId")
            Prelude.<*> (x Core..: "applicationVersion")
            Prelude.<*> (x Core..: "creationTime")
            Prelude.<*> (x Core..: "deploymentId")
            Prelude.<*> (x Core..: "environmentId")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable DeploymentSummary where
  hashWithSalt _salt DeploymentSummary' {..} =
    _salt `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` status

instance Prelude.NFData DeploymentSummary where
  rnf DeploymentSummary' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf status
