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
-- Module      : Network.AWS.Greengrass.Types.Deployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Deployment where

import Network.AWS.Greengrass.Types.DeploymentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a deployment.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The type of the deployment.
    deploymentType :: Prelude.Maybe DeploymentType,
    -- | The time, in milliseconds since the epoch, when the deployment was
    -- created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the deployment.
    deploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group for this deployment.
    groupArn :: Prelude.Maybe Prelude.Text
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
-- 'deploymentId', 'deployment_deploymentId' - The ID of the deployment.
--
-- 'deploymentType', 'deployment_deploymentType' - The type of the deployment.
--
-- 'createdAt', 'deployment_createdAt' - The time, in milliseconds since the epoch, when the deployment was
-- created.
--
-- 'deploymentArn', 'deployment_deploymentArn' - The ARN of the deployment.
--
-- 'groupArn', 'deployment_groupArn' - The ARN of the group for this deployment.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { deploymentId = Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      deploymentArn = Prelude.Nothing,
      groupArn = Prelude.Nothing
    }

-- | The ID of the deployment.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The type of the deployment.
deployment_deploymentType :: Lens.Lens' Deployment (Prelude.Maybe DeploymentType)
deployment_deploymentType = Lens.lens (\Deployment' {deploymentType} -> deploymentType) (\s@Deployment' {} a -> s {deploymentType = a} :: Deployment)

-- | The time, in milliseconds since the epoch, when the deployment was
-- created.
deployment_createdAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_createdAt = Lens.lens (\Deployment' {createdAt} -> createdAt) (\s@Deployment' {} a -> s {createdAt = a} :: Deployment)

-- | The ARN of the deployment.
deployment_deploymentArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentArn = Lens.lens (\Deployment' {deploymentArn} -> deploymentArn) (\s@Deployment' {} a -> s {deploymentArn = a} :: Deployment)

-- | The ARN of the group for this deployment.
deployment_groupArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_groupArn = Lens.lens (\Deployment' {groupArn} -> groupArn) (\s@Deployment' {} a -> s {groupArn = a} :: Deployment)

instance Prelude.FromJSON Deployment where
  parseJSON =
    Prelude.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Prelude..:? "DeploymentId")
            Prelude.<*> (x Prelude..:? "DeploymentType")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "DeploymentArn")
            Prelude.<*> (x Prelude..:? "GroupArn")
      )

instance Prelude.Hashable Deployment

instance Prelude.NFData Deployment
