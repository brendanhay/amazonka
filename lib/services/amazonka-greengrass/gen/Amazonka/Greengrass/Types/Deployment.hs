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
-- Module      : Amazonka.Greengrass.Types.Deployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.Deployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.DeploymentType
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The time, in milliseconds since the epoch, when the deployment was
    -- created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the deployment.
    deploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The type of the deployment.
    deploymentType :: Prelude.Maybe DeploymentType,
    -- | The ARN of the group for this deployment.
    groupArn :: Prelude.Maybe Prelude.Text
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
-- 'createdAt', 'deployment_createdAt' - The time, in milliseconds since the epoch, when the deployment was
-- created.
--
-- 'deploymentArn', 'deployment_deploymentArn' - The ARN of the deployment.
--
-- 'deploymentId', 'deployment_deploymentId' - The ID of the deployment.
--
-- 'deploymentType', 'deployment_deploymentType' - The type of the deployment.
--
-- 'groupArn', 'deployment_groupArn' - The ARN of the group for this deployment.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { createdAt = Prelude.Nothing,
      deploymentArn = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      groupArn = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the deployment was
-- created.
deployment_createdAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_createdAt = Lens.lens (\Deployment' {createdAt} -> createdAt) (\s@Deployment' {} a -> s {createdAt = a} :: Deployment)

-- | The ARN of the deployment.
deployment_deploymentArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentArn = Lens.lens (\Deployment' {deploymentArn} -> deploymentArn) (\s@Deployment' {} a -> s {deploymentArn = a} :: Deployment)

-- | The ID of the deployment.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The type of the deployment.
deployment_deploymentType :: Lens.Lens' Deployment (Prelude.Maybe DeploymentType)
deployment_deploymentType = Lens.lens (\Deployment' {deploymentType} -> deploymentType) (\s@Deployment' {} a -> s {deploymentType = a} :: Deployment)

-- | The ARN of the group for this deployment.
deployment_groupArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_groupArn = Lens.lens (\Deployment' {groupArn} -> groupArn) (\s@Deployment' {} a -> s {groupArn = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DeploymentArn")
            Prelude.<*> (x Data..:? "DeploymentId")
            Prelude.<*> (x Data..:? "DeploymentType")
            Prelude.<*> (x Data..:? "GroupArn")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deploymentArn
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` groupArn

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deploymentArn
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf groupArn
