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
-- Module      : Amazonka.ApiGatewayV2.Types.Deployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Deployment where

import Amazonka.ApiGatewayV2.Types.DeploymentStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An immutable representation of an API that can be called by users. A
-- Deployment must be associated with a Stage for it to be callable over
-- the internet.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | Specifies whether a deployment was automatically released.
    autoDeployed :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the Deployment resource was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The identifier for the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
    deploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | May contain additional feedback on the status of an API deployment.
    deploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The description for the deployment.
    description :: Prelude.Maybe Prelude.Text
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
-- 'autoDeployed', 'deployment_autoDeployed' - Specifies whether a deployment was automatically released.
--
-- 'createdDate', 'deployment_createdDate' - The date and time when the Deployment resource was created.
--
-- 'deploymentId', 'deployment_deploymentId' - The identifier for the deployment.
--
-- 'deploymentStatus', 'deployment_deploymentStatus' - The status of the deployment: PENDING, FAILED, or SUCCEEDED.
--
-- 'deploymentStatusMessage', 'deployment_deploymentStatusMessage' - May contain additional feedback on the status of an API deployment.
--
-- 'description', 'deployment_description' - The description for the deployment.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { autoDeployed = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      deploymentStatusMessage = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Specifies whether a deployment was automatically released.
deployment_autoDeployed :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Bool)
deployment_autoDeployed = Lens.lens (\Deployment' {autoDeployed} -> autoDeployed) (\s@Deployment' {} a -> s {autoDeployed = a} :: Deployment)

-- | The date and time when the Deployment resource was created.
deployment_createdDate :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_createdDate = Lens.lens (\Deployment' {createdDate} -> createdDate) (\s@Deployment' {} a -> s {createdDate = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The identifier for the deployment.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The status of the deployment: PENDING, FAILED, or SUCCEEDED.
deployment_deploymentStatus :: Lens.Lens' Deployment (Prelude.Maybe DeploymentStatus)
deployment_deploymentStatus = Lens.lens (\Deployment' {deploymentStatus} -> deploymentStatus) (\s@Deployment' {} a -> s {deploymentStatus = a} :: Deployment)

-- | May contain additional feedback on the status of an API deployment.
deployment_deploymentStatusMessage :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentStatusMessage = Lens.lens (\Deployment' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@Deployment' {} a -> s {deploymentStatusMessage = a} :: Deployment)

-- | The description for the deployment.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "autoDeployed")
            Prelude.<*> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "deploymentStatus")
            Prelude.<*> (x Data..:? "deploymentStatusMessage")
            Prelude.<*> (x Data..:? "description")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt
      `Prelude.hashWithSalt` autoDeployed
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` description

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf autoDeployed
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf description
