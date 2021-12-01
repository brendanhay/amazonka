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
-- Module      : Amazonka.Proton.Types.EnvironmentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus
import Amazonka.Proton.Types.Provisioning

-- | A summary of the environment detail data.
--
-- /See:/ 'newEnvironmentSummary' smart constructor.
data EnvironmentSummary = EnvironmentSummary'
  { -- | An environment deployment status message.
    deploymentStatusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the environment account that the environment infrastructure
    -- resources are provisioned in.
    environmentAccountId :: Prelude.Maybe Prelude.Text,
    -- | When included, indicates that the environment template is for customer
    -- provisioned and managed infrastructure.
    provisioning :: Prelude.Maybe Provisioning,
    -- | The Amazon Resource Name (ARN) of the AWS Proton service role that
    -- allows AWS Proton to make calls to other services on your behalf.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account connection that the environment is
    -- associated with.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The description of the environment.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Text,
    -- | The time when the environment was created.
    createdAt :: Core.POSIX,
    -- | The environment deployment status.
    deploymentStatus :: DeploymentStatus,
    -- | The time when a deployment of the environment was last attempted.
    lastDeploymentAttemptedAt :: Core.POSIX,
    -- | The time when the environment was last deployed successfully.
    lastDeploymentSucceededAt :: Core.POSIX,
    -- | The name of the environment.
    name :: Prelude.Text,
    -- | The ID of the major version of the environment template.
    templateMajorVersion :: Prelude.Text,
    -- | The ID of the minor version of the environment template.
    templateMinorVersion :: Prelude.Text,
    -- | The name of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatusMessage', 'environmentSummary_deploymentStatusMessage' - An environment deployment status message.
--
-- 'environmentAccountId', 'environmentSummary_environmentAccountId' - The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
--
-- 'provisioning', 'environmentSummary_provisioning' - When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
--
-- 'protonServiceRoleArn', 'environmentSummary_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make calls to other services on your behalf.
--
-- 'environmentAccountConnectionId', 'environmentSummary_environmentAccountConnectionId' - The ID of the environment account connection that the environment is
-- associated with.
--
-- 'description', 'environmentSummary_description' - The description of the environment.
--
-- 'arn', 'environmentSummary_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'createdAt', 'environmentSummary_createdAt' - The time when the environment was created.
--
-- 'deploymentStatus', 'environmentSummary_deploymentStatus' - The environment deployment status.
--
-- 'lastDeploymentAttemptedAt', 'environmentSummary_lastDeploymentAttemptedAt' - The time when a deployment of the environment was last attempted.
--
-- 'lastDeploymentSucceededAt', 'environmentSummary_lastDeploymentSucceededAt' - The time when the environment was last deployed successfully.
--
-- 'name', 'environmentSummary_name' - The name of the environment.
--
-- 'templateMajorVersion', 'environmentSummary_templateMajorVersion' - The ID of the major version of the environment template.
--
-- 'templateMinorVersion', 'environmentSummary_templateMinorVersion' - The ID of the minor version of the environment template.
--
-- 'templateName', 'environmentSummary_templateName' - The name of the environment template.
newEnvironmentSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'deploymentStatus'
  DeploymentStatus ->
  -- | 'lastDeploymentAttemptedAt'
  Prelude.UTCTime ->
  -- | 'lastDeploymentSucceededAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'templateMajorVersion'
  Prelude.Text ->
  -- | 'templateMinorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  EnvironmentSummary
newEnvironmentSummary
  pArn_
  pCreatedAt_
  pDeploymentStatus_
  pLastDeploymentAttemptedAt_
  pLastDeploymentSucceededAt_
  pName_
  pTemplateMajorVersion_
  pTemplateMinorVersion_
  pTemplateName_ =
    EnvironmentSummary'
      { deploymentStatusMessage =
          Prelude.Nothing,
        environmentAccountId = Prelude.Nothing,
        provisioning = Prelude.Nothing,
        protonServiceRoleArn = Prelude.Nothing,
        environmentAccountConnectionId = Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        lastDeploymentAttemptedAt =
          Core._Time Lens.# pLastDeploymentAttemptedAt_,
        lastDeploymentSucceededAt =
          Core._Time Lens.# pLastDeploymentSucceededAt_,
        name = pName_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateMinorVersion = pTemplateMinorVersion_,
        templateName = pTemplateName_
      }

-- | An environment deployment status message.
environmentSummary_deploymentStatusMessage :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_deploymentStatusMessage = Lens.lens (\EnvironmentSummary' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@EnvironmentSummary' {} a -> s {deploymentStatusMessage = a} :: EnvironmentSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
environmentSummary_environmentAccountId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_environmentAccountId = Lens.lens (\EnvironmentSummary' {environmentAccountId} -> environmentAccountId) (\s@EnvironmentSummary' {} a -> s {environmentAccountId = a} :: EnvironmentSummary)

-- | When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
environmentSummary_provisioning :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Provisioning)
environmentSummary_provisioning = Lens.lens (\EnvironmentSummary' {provisioning} -> provisioning) (\s@EnvironmentSummary' {} a -> s {provisioning = a} :: EnvironmentSummary)

-- | The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make calls to other services on your behalf.
environmentSummary_protonServiceRoleArn :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_protonServiceRoleArn = Lens.lens (\EnvironmentSummary' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@EnvironmentSummary' {} a -> s {protonServiceRoleArn = a} :: EnvironmentSummary)

-- | The ID of the environment account connection that the environment is
-- associated with.
environmentSummary_environmentAccountConnectionId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_environmentAccountConnectionId = Lens.lens (\EnvironmentSummary' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@EnvironmentSummary' {} a -> s {environmentAccountConnectionId = a} :: EnvironmentSummary)

-- | The description of the environment.
environmentSummary_description :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_description = Lens.lens (\EnvironmentSummary' {description} -> description) (\s@EnvironmentSummary' {} a -> s {description = a} :: EnvironmentSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the environment.
environmentSummary_arn :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_arn = Lens.lens (\EnvironmentSummary' {arn} -> arn) (\s@EnvironmentSummary' {} a -> s {arn = a} :: EnvironmentSummary)

-- | The time when the environment was created.
environmentSummary_createdAt :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_createdAt = Lens.lens (\EnvironmentSummary' {createdAt} -> createdAt) (\s@EnvironmentSummary' {} a -> s {createdAt = a} :: EnvironmentSummary) Prelude.. Core._Time

-- | The environment deployment status.
environmentSummary_deploymentStatus :: Lens.Lens' EnvironmentSummary DeploymentStatus
environmentSummary_deploymentStatus = Lens.lens (\EnvironmentSummary' {deploymentStatus} -> deploymentStatus) (\s@EnvironmentSummary' {} a -> s {deploymentStatus = a} :: EnvironmentSummary)

-- | The time when a deployment of the environment was last attempted.
environmentSummary_lastDeploymentAttemptedAt :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_lastDeploymentAttemptedAt = Lens.lens (\EnvironmentSummary' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@EnvironmentSummary' {} a -> s {lastDeploymentAttemptedAt = a} :: EnvironmentSummary) Prelude.. Core._Time

-- | The time when the environment was last deployed successfully.
environmentSummary_lastDeploymentSucceededAt :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_lastDeploymentSucceededAt = Lens.lens (\EnvironmentSummary' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@EnvironmentSummary' {} a -> s {lastDeploymentSucceededAt = a} :: EnvironmentSummary) Prelude.. Core._Time

-- | The name of the environment.
environmentSummary_name :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_name = Lens.lens (\EnvironmentSummary' {name} -> name) (\s@EnvironmentSummary' {} a -> s {name = a} :: EnvironmentSummary)

-- | The ID of the major version of the environment template.
environmentSummary_templateMajorVersion :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_templateMajorVersion = Lens.lens (\EnvironmentSummary' {templateMajorVersion} -> templateMajorVersion) (\s@EnvironmentSummary' {} a -> s {templateMajorVersion = a} :: EnvironmentSummary)

-- | The ID of the minor version of the environment template.
environmentSummary_templateMinorVersion :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_templateMinorVersion = Lens.lens (\EnvironmentSummary' {templateMinorVersion} -> templateMinorVersion) (\s@EnvironmentSummary' {} a -> s {templateMinorVersion = a} :: EnvironmentSummary)

-- | The name of the environment template.
environmentSummary_templateName :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_templateName = Lens.lens (\EnvironmentSummary' {templateName} -> templateName) (\s@EnvironmentSummary' {} a -> s {templateName = a} :: EnvironmentSummary)

instance Core.FromJSON EnvironmentSummary where
  parseJSON =
    Core.withObject
      "EnvironmentSummary"
      ( \x ->
          EnvironmentSummary'
            Prelude.<$> (x Core..:? "deploymentStatusMessage")
            Prelude.<*> (x Core..:? "environmentAccountId")
            Prelude.<*> (x Core..:? "provisioning")
            Prelude.<*> (x Core..:? "protonServiceRoleArn")
            Prelude.<*> (x Core..:? "environmentAccountConnectionId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "deploymentStatus")
            Prelude.<*> (x Core..: "lastDeploymentAttemptedAt")
            Prelude.<*> (x Core..: "lastDeploymentSucceededAt")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "templateMajorVersion")
            Prelude.<*> (x Core..: "templateMinorVersion")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable EnvironmentSummary where
  hashWithSalt salt' EnvironmentSummary' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` provisioning
      `Prelude.hashWithSalt` environmentAccountId
      `Prelude.hashWithSalt` deploymentStatusMessage

instance Prelude.NFData EnvironmentSummary where
  rnf EnvironmentSummary' {..} =
    Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId
      `Prelude.seq` Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf provisioning
      `Prelude.seq` Prelude.rnf environmentAccountId
