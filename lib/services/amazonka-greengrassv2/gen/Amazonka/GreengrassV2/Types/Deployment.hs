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
-- Module      : Amazonka.GreengrassV2.Types.Deployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.Deployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.DeploymentStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a deployment.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The time at which the deployment was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the deployment.
    deploymentName :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment.
    deploymentStatus :: Prelude.Maybe DeploymentStatus,
    -- | Whether or not the deployment is the latest revision for its target.
    isLatestForTarget :: Prelude.Maybe Prelude.Bool,
    -- | The parent deployment\'s target
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- within a subdeployment.
    parentTargetArn :: Prelude.Maybe Prelude.Text,
    -- | The revision number of the deployment.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group. When creating a subdeployment,
    -- the targetARN can only be a thing group.
    targetArn :: Prelude.Maybe Prelude.Text
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
-- 'creationTimestamp', 'deployment_creationTimestamp' - The time at which the deployment was created, expressed in ISO 8601
-- format.
--
-- 'deploymentId', 'deployment_deploymentId' - The ID of the deployment.
--
-- 'deploymentName', 'deployment_deploymentName' - The name of the deployment.
--
-- 'deploymentStatus', 'deployment_deploymentStatus' - The status of the deployment.
--
-- 'isLatestForTarget', 'deployment_isLatestForTarget' - Whether or not the deployment is the latest revision for its target.
--
-- 'parentTargetArn', 'deployment_parentTargetArn' - The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
--
-- 'revisionId', 'deployment_revisionId' - The revision number of the deployment.
--
-- 'targetArn', 'deployment_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group. When creating a subdeployment,
-- the targetARN can only be a thing group.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { creationTimestamp = Prelude.Nothing,
      deploymentId = Prelude.Nothing,
      deploymentName = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      isLatestForTarget = Prelude.Nothing,
      parentTargetArn = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      targetArn = Prelude.Nothing
    }

-- | The time at which the deployment was created, expressed in ISO 8601
-- format.
deployment_creationTimestamp :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_creationTimestamp = Lens.lens (\Deployment' {creationTimestamp} -> creationTimestamp) (\s@Deployment' {} a -> s {creationTimestamp = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The ID of the deployment.
deployment_deploymentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentId = Lens.lens (\Deployment' {deploymentId} -> deploymentId) (\s@Deployment' {} a -> s {deploymentId = a} :: Deployment)

-- | The name of the deployment.
deployment_deploymentName :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentName = Lens.lens (\Deployment' {deploymentName} -> deploymentName) (\s@Deployment' {} a -> s {deploymentName = a} :: Deployment)

-- | The status of the deployment.
deployment_deploymentStatus :: Lens.Lens' Deployment (Prelude.Maybe DeploymentStatus)
deployment_deploymentStatus = Lens.lens (\Deployment' {deploymentStatus} -> deploymentStatus) (\s@Deployment' {} a -> s {deploymentStatus = a} :: Deployment)

-- | Whether or not the deployment is the latest revision for its target.
deployment_isLatestForTarget :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Bool)
deployment_isLatestForTarget = Lens.lens (\Deployment' {isLatestForTarget} -> isLatestForTarget) (\s@Deployment' {} a -> s {isLatestForTarget = a} :: Deployment)

-- | The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
deployment_parentTargetArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_parentTargetArn = Lens.lens (\Deployment' {parentTargetArn} -> parentTargetArn) (\s@Deployment' {} a -> s {parentTargetArn = a} :: Deployment)

-- | The revision number of the deployment.
deployment_revisionId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_revisionId = Lens.lens (\Deployment' {revisionId} -> revisionId) (\s@Deployment' {} a -> s {revisionId = a} :: Deployment)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group. When creating a subdeployment,
-- the targetARN can only be a thing group.
deployment_targetArn :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_targetArn = Lens.lens (\Deployment' {targetArn} -> targetArn) (\s@Deployment' {} a -> s {targetArn = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "creationTimestamp")
            Prelude.<*> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "deploymentName")
            Prelude.<*> (x Data..:? "deploymentStatus")
            Prelude.<*> (x Data..:? "isLatestForTarget")
            Prelude.<*> (x Data..:? "parentTargetArn")
            Prelude.<*> (x Data..:? "revisionId")
            Prelude.<*> (x Data..:? "targetArn")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentName
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` isLatestForTarget
      `Prelude.hashWithSalt` parentTargetArn
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentName
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf isLatestForTarget
      `Prelude.seq` Prelude.rnf parentTargetArn
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf targetArn
