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
-- Module      : Network.AWS.ElasticBeanstalk.Types.Deployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Deployment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an application version deployment.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The status of the deployment:
    --
    -- -   @In Progress@ : The deployment is in progress.
    --
    -- -   @Deployed@ : The deployment succeeded.
    --
    -- -   @Failed@ : The deployment failed.
    status :: Core.Maybe Core.Text,
    -- | The ID of the deployment. This number increases by one each time that
    -- you deploy source code or change instance configuration settings.
    deploymentId :: Core.Maybe Core.Integer,
    -- | The version label of the application version in the deployment.
    versionLabel :: Core.Maybe Core.Text,
    -- | For in-progress deployments, the time that the deployment started.
    --
    -- For completed deployments, the time that the deployment ended.
    deploymentTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deployment_status' - The status of the deployment:
--
-- -   @In Progress@ : The deployment is in progress.
--
-- -   @Deployed@ : The deployment succeeded.
--
-- -   @Failed@ : The deployment failed.
--
-- 'deploymentId', 'deployment_deploymentId' - The ID of the deployment. This number increases by one each time that
-- you deploy source code or change instance configuration settings.
--
-- 'versionLabel', 'deployment_versionLabel' - The version label of the application version in the deployment.
--
-- 'deploymentTime', 'deployment_deploymentTime' - For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { status = Core.Nothing,
      deploymentId = Core.Nothing,
      versionLabel = Core.Nothing,
      deploymentTime = Core.Nothing
    }

-- | The status of the deployment:
--
-- -   @In Progress@ : The deployment is in progress.
--
-- -   @Deployed@ : The deployment succeeded.
--
-- -   @Failed@ : The deployment failed.
deployment_status :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The ID of the deployment. This number increases by one each time that
-- you deploy source code or change instance configuration settings.
deployment_deploymentId :: Lens.Lens' Deployment (Core.Maybe Core.Integer)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The version label of the application version in the deployment.
deployment_versionLabel :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_versionLabel = Lens.lens (\Deployment' {versionLabel} -> versionLabel) (\s@Deployment' {} a -> s {versionLabel = a} :: Deployment)

-- | For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
deployment_deploymentTime :: Lens.Lens' Deployment (Core.Maybe Core.UTCTime)
deployment_deploymentTime = Lens.lens (\Deployment' {deploymentTime} -> deploymentTime) (\s@Deployment' {} a -> s {deploymentTime = a} :: Deployment) Core.. Lens.mapping Core._Time

instance Core.FromXML Deployment where
  parseXML x =
    Deployment'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "DeploymentId")
      Core.<*> (x Core..@? "VersionLabel")
      Core.<*> (x Core..@? "DeploymentTime")

instance Core.Hashable Deployment

instance Core.NFData Deployment
