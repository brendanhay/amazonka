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
-- Module      : Amazonka.Proton.Types.ServicePipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServicePipeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus

-- | Detailed data of an Proton service instance pipeline resource.
--
-- /See:/ 'newServicePipeline' smart constructor.
data ServicePipeline = ServicePipeline'
  { -- | A service pipeline deployment status message.
    deploymentStatusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The service spec that was used to create the service pipeline.
    spec :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the service pipeline.
    arn :: Prelude.Text,
    -- | The time when the service pipeline was created.
    createdAt :: Core.POSIX,
    -- | The deployment status of the service pipeline.
    deploymentStatus :: DeploymentStatus,
    -- | The time when a deployment of the service pipeline was last attempted.
    lastDeploymentAttemptedAt :: Core.POSIX,
    -- | The time when the service pipeline was last deployed successfully.
    lastDeploymentSucceededAt :: Core.POSIX,
    -- | The major version of the service template that was used to create the
    -- service pipeline.
    templateMajorVersion :: Prelude.Text,
    -- | The minor version of the service template that was used to create the
    -- service pipeline.
    templateMinorVersion :: Prelude.Text,
    -- | The name of the service template that was used to create the service
    -- pipeline.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServicePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatusMessage', 'servicePipeline_deploymentStatusMessage' - A service pipeline deployment status message.
--
-- 'spec', 'servicePipeline_spec' - The service spec that was used to create the service pipeline.
--
-- 'arn', 'servicePipeline_arn' - The Amazon Resource Name (ARN) of the service pipeline.
--
-- 'createdAt', 'servicePipeline_createdAt' - The time when the service pipeline was created.
--
-- 'deploymentStatus', 'servicePipeline_deploymentStatus' - The deployment status of the service pipeline.
--
-- 'lastDeploymentAttemptedAt', 'servicePipeline_lastDeploymentAttemptedAt' - The time when a deployment of the service pipeline was last attempted.
--
-- 'lastDeploymentSucceededAt', 'servicePipeline_lastDeploymentSucceededAt' - The time when the service pipeline was last deployed successfully.
--
-- 'templateMajorVersion', 'servicePipeline_templateMajorVersion' - The major version of the service template that was used to create the
-- service pipeline.
--
-- 'templateMinorVersion', 'servicePipeline_templateMinorVersion' - The minor version of the service template that was used to create the
-- service pipeline.
--
-- 'templateName', 'servicePipeline_templateName' - The name of the service template that was used to create the service
-- pipeline.
newServicePipeline ::
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
  -- | 'templateMajorVersion'
  Prelude.Text ->
  -- | 'templateMinorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  ServicePipeline
newServicePipeline
  pArn_
  pCreatedAt_
  pDeploymentStatus_
  pLastDeploymentAttemptedAt_
  pLastDeploymentSucceededAt_
  pTemplateMajorVersion_
  pTemplateMinorVersion_
  pTemplateName_ =
    ServicePipeline'
      { deploymentStatusMessage =
          Prelude.Nothing,
        spec = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        lastDeploymentAttemptedAt =
          Core._Time Lens.# pLastDeploymentAttemptedAt_,
        lastDeploymentSucceededAt =
          Core._Time Lens.# pLastDeploymentSucceededAt_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateMinorVersion = pTemplateMinorVersion_,
        templateName = pTemplateName_
      }

-- | A service pipeline deployment status message.
servicePipeline_deploymentStatusMessage :: Lens.Lens' ServicePipeline (Prelude.Maybe Prelude.Text)
servicePipeline_deploymentStatusMessage = Lens.lens (\ServicePipeline' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@ServicePipeline' {} a -> s {deploymentStatusMessage = a} :: ServicePipeline) Prelude.. Lens.mapping Core._Sensitive

-- | The service spec that was used to create the service pipeline.
servicePipeline_spec :: Lens.Lens' ServicePipeline (Prelude.Maybe Prelude.Text)
servicePipeline_spec = Lens.lens (\ServicePipeline' {spec} -> spec) (\s@ServicePipeline' {} a -> s {spec = a} :: ServicePipeline) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the service pipeline.
servicePipeline_arn :: Lens.Lens' ServicePipeline Prelude.Text
servicePipeline_arn = Lens.lens (\ServicePipeline' {arn} -> arn) (\s@ServicePipeline' {} a -> s {arn = a} :: ServicePipeline)

-- | The time when the service pipeline was created.
servicePipeline_createdAt :: Lens.Lens' ServicePipeline Prelude.UTCTime
servicePipeline_createdAt = Lens.lens (\ServicePipeline' {createdAt} -> createdAt) (\s@ServicePipeline' {} a -> s {createdAt = a} :: ServicePipeline) Prelude.. Core._Time

-- | The deployment status of the service pipeline.
servicePipeline_deploymentStatus :: Lens.Lens' ServicePipeline DeploymentStatus
servicePipeline_deploymentStatus = Lens.lens (\ServicePipeline' {deploymentStatus} -> deploymentStatus) (\s@ServicePipeline' {} a -> s {deploymentStatus = a} :: ServicePipeline)

-- | The time when a deployment of the service pipeline was last attempted.
servicePipeline_lastDeploymentAttemptedAt :: Lens.Lens' ServicePipeline Prelude.UTCTime
servicePipeline_lastDeploymentAttemptedAt = Lens.lens (\ServicePipeline' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@ServicePipeline' {} a -> s {lastDeploymentAttemptedAt = a} :: ServicePipeline) Prelude.. Core._Time

-- | The time when the service pipeline was last deployed successfully.
servicePipeline_lastDeploymentSucceededAt :: Lens.Lens' ServicePipeline Prelude.UTCTime
servicePipeline_lastDeploymentSucceededAt = Lens.lens (\ServicePipeline' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@ServicePipeline' {} a -> s {lastDeploymentSucceededAt = a} :: ServicePipeline) Prelude.. Core._Time

-- | The major version of the service template that was used to create the
-- service pipeline.
servicePipeline_templateMajorVersion :: Lens.Lens' ServicePipeline Prelude.Text
servicePipeline_templateMajorVersion = Lens.lens (\ServicePipeline' {templateMajorVersion} -> templateMajorVersion) (\s@ServicePipeline' {} a -> s {templateMajorVersion = a} :: ServicePipeline)

-- | The minor version of the service template that was used to create the
-- service pipeline.
servicePipeline_templateMinorVersion :: Lens.Lens' ServicePipeline Prelude.Text
servicePipeline_templateMinorVersion = Lens.lens (\ServicePipeline' {templateMinorVersion} -> templateMinorVersion) (\s@ServicePipeline' {} a -> s {templateMinorVersion = a} :: ServicePipeline)

-- | The name of the service template that was used to create the service
-- pipeline.
servicePipeline_templateName :: Lens.Lens' ServicePipeline Prelude.Text
servicePipeline_templateName = Lens.lens (\ServicePipeline' {templateName} -> templateName) (\s@ServicePipeline' {} a -> s {templateName = a} :: ServicePipeline)

instance Core.FromJSON ServicePipeline where
  parseJSON =
    Core.withObject
      "ServicePipeline"
      ( \x ->
          ServicePipeline'
            Prelude.<$> (x Core..:? "deploymentStatusMessage")
            Prelude.<*> (x Core..:? "spec")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "deploymentStatus")
            Prelude.<*> (x Core..: "lastDeploymentAttemptedAt")
            Prelude.<*> (x Core..: "lastDeploymentSucceededAt")
            Prelude.<*> (x Core..: "templateMajorVersion")
            Prelude.<*> (x Core..: "templateMinorVersion")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable ServicePipeline where
  hashWithSalt _salt ServicePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ServicePipeline where
  rnf ServicePipeline' {..} =
    Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf templateName
