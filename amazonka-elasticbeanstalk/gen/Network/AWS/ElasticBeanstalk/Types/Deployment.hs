{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deployment. This number increases by one each time that
    -- you deploy source code or change instance configuration settings.
    deploymentId :: Prelude.Maybe Prelude.Integer,
    -- | The version label of the application version in the deployment.
    versionLabel :: Prelude.Maybe Prelude.Text,
    -- | For in-progress deployments, the time that the deployment started.
    --
    -- For completed deployments, the time that the deployment ended.
    deploymentTime :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      versionLabel = Prelude.Nothing,
      deploymentTime = Prelude.Nothing
    }

-- | The status of the deployment:
--
-- -   @In Progress@ : The deployment is in progress.
--
-- -   @Deployed@ : The deployment succeeded.
--
-- -   @Failed@ : The deployment failed.
deployment_status :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The ID of the deployment. This number increases by one each time that
-- you deploy source code or change instance configuration settings.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Integer)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The version label of the application version in the deployment.
deployment_versionLabel :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_versionLabel = Lens.lens (\Deployment' {versionLabel} -> versionLabel) (\s@Deployment' {} a -> s {versionLabel = a} :: Deployment)

-- | For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
deployment_deploymentTime :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_deploymentTime = Lens.lens (\Deployment' {deploymentTime} -> deploymentTime) (\s@Deployment' {} a -> s {deploymentTime = a} :: Deployment) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML Deployment where
  parseXML x =
    Deployment'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "DeploymentId")
      Prelude.<*> (x Prelude..@? "VersionLabel")
      Prelude.<*> (x Prelude..@? "DeploymentTime")

instance Prelude.Hashable Deployment

instance Prelude.NFData Deployment
