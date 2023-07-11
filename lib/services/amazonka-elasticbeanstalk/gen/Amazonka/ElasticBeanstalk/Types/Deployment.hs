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
-- Module      : Amazonka.ElasticBeanstalk.Types.Deployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.Deployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an application version deployment.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The ID of the deployment. This number increases by one each time that
    -- you deploy source code or change instance configuration settings.
    deploymentId :: Prelude.Maybe Prelude.Integer,
    -- | For in-progress deployments, the time that the deployment started.
    --
    -- For completed deployments, the time that the deployment ended.
    deploymentTime :: Prelude.Maybe Data.ISO8601,
    -- | The status of the deployment:
    --
    -- -   @In Progress@ : The deployment is in progress.
    --
    -- -   @Deployed@ : The deployment succeeded.
    --
    -- -   @Failed@ : The deployment failed.
    status :: Prelude.Maybe Prelude.Text,
    -- | The version label of the application version in the deployment.
    versionLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'deployment_deploymentId' - The ID of the deployment. This number increases by one each time that
-- you deploy source code or change instance configuration settings.
--
-- 'deploymentTime', 'deployment_deploymentTime' - For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
--
-- 'status', 'deployment_status' - The status of the deployment:
--
-- -   @In Progress@ : The deployment is in progress.
--
-- -   @Deployed@ : The deployment succeeded.
--
-- -   @Failed@ : The deployment failed.
--
-- 'versionLabel', 'deployment_versionLabel' - The version label of the application version in the deployment.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { deploymentId = Prelude.Nothing,
      deploymentTime = Prelude.Nothing,
      status = Prelude.Nothing,
      versionLabel = Prelude.Nothing
    }

-- | The ID of the deployment. This number increases by one each time that
-- you deploy source code or change instance configuration settings.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Integer)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
deployment_deploymentTime :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_deploymentTime = Lens.lens (\Deployment' {deploymentTime} -> deploymentTime) (\s@Deployment' {} a -> s {deploymentTime = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The status of the deployment:
--
-- -   @In Progress@ : The deployment is in progress.
--
-- -   @Deployed@ : The deployment succeeded.
--
-- -   @Failed@ : The deployment failed.
deployment_status :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The version label of the application version in the deployment.
deployment_versionLabel :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_versionLabel = Lens.lens (\Deployment' {versionLabel} -> versionLabel) (\s@Deployment' {} a -> s {versionLabel = a} :: Deployment)

instance Data.FromXML Deployment where
  parseXML x =
    Deployment'
      Prelude.<$> (x Data..@? "DeploymentId")
      Prelude.<*> (x Data..@? "DeploymentTime")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "VersionLabel")

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionLabel
