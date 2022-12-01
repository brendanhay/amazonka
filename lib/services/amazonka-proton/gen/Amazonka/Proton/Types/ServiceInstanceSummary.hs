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
-- Module      : Amazonka.Proton.Types.ServiceInstanceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceInstanceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus

-- | Summary data of an Proton service instance resource.
--
-- /See:/ 'newServiceInstanceSummary' smart constructor.
data ServiceInstanceSummary = ServiceInstanceSummary'
  { -- | A service instance deployment status message.
    deploymentStatusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the service instance.
    arn :: Prelude.Text,
    -- | The time when the service instance was created.
    createdAt :: Core.POSIX,
    -- | The service instance deployment status.
    deploymentStatus :: DeploymentStatus,
    -- | The name of the environment that the service instance was deployed into.
    environmentName :: Prelude.Text,
    -- | The time when a deployment of the service was last attempted.
    lastDeploymentAttemptedAt :: Core.POSIX,
    -- | The time when the service was last deployed successfully.
    lastDeploymentSucceededAt :: Core.POSIX,
    -- | The name of the service instance.
    name :: Prelude.Text,
    -- | The name of the service that the service instance belongs to.
    serviceName :: Prelude.Text,
    -- | The service instance template major version.
    templateMajorVersion :: Prelude.Text,
    -- | The service instance template minor version.
    templateMinorVersion :: Prelude.Text,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatusMessage', 'serviceInstanceSummary_deploymentStatusMessage' - A service instance deployment status message.
--
-- 'arn', 'serviceInstanceSummary_arn' - The Amazon Resource Name (ARN) of the service instance.
--
-- 'createdAt', 'serviceInstanceSummary_createdAt' - The time when the service instance was created.
--
-- 'deploymentStatus', 'serviceInstanceSummary_deploymentStatus' - The service instance deployment status.
--
-- 'environmentName', 'serviceInstanceSummary_environmentName' - The name of the environment that the service instance was deployed into.
--
-- 'lastDeploymentAttemptedAt', 'serviceInstanceSummary_lastDeploymentAttemptedAt' - The time when a deployment of the service was last attempted.
--
-- 'lastDeploymentSucceededAt', 'serviceInstanceSummary_lastDeploymentSucceededAt' - The time when the service was last deployed successfully.
--
-- 'name', 'serviceInstanceSummary_name' - The name of the service instance.
--
-- 'serviceName', 'serviceInstanceSummary_serviceName' - The name of the service that the service instance belongs to.
--
-- 'templateMajorVersion', 'serviceInstanceSummary_templateMajorVersion' - The service instance template major version.
--
-- 'templateMinorVersion', 'serviceInstanceSummary_templateMinorVersion' - The service instance template minor version.
--
-- 'templateName', 'serviceInstanceSummary_templateName' - The name of the service template.
newServiceInstanceSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'deploymentStatus'
  DeploymentStatus ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'lastDeploymentAttemptedAt'
  Prelude.UTCTime ->
  -- | 'lastDeploymentSucceededAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'templateMajorVersion'
  Prelude.Text ->
  -- | 'templateMinorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  ServiceInstanceSummary
newServiceInstanceSummary
  pArn_
  pCreatedAt_
  pDeploymentStatus_
  pEnvironmentName_
  pLastDeploymentAttemptedAt_
  pLastDeploymentSucceededAt_
  pName_
  pServiceName_
  pTemplateMajorVersion_
  pTemplateMinorVersion_
  pTemplateName_ =
    ServiceInstanceSummary'
      { deploymentStatusMessage =
          Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        environmentName = pEnvironmentName_,
        lastDeploymentAttemptedAt =
          Core._Time Lens.# pLastDeploymentAttemptedAt_,
        lastDeploymentSucceededAt =
          Core._Time Lens.# pLastDeploymentSucceededAt_,
        name = pName_,
        serviceName = pServiceName_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateMinorVersion = pTemplateMinorVersion_,
        templateName = pTemplateName_
      }

-- | A service instance deployment status message.
serviceInstanceSummary_deploymentStatusMessage :: Lens.Lens' ServiceInstanceSummary (Prelude.Maybe Prelude.Text)
serviceInstanceSummary_deploymentStatusMessage = Lens.lens (\ServiceInstanceSummary' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@ServiceInstanceSummary' {} a -> s {deploymentStatusMessage = a} :: ServiceInstanceSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the service instance.
serviceInstanceSummary_arn :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_arn = Lens.lens (\ServiceInstanceSummary' {arn} -> arn) (\s@ServiceInstanceSummary' {} a -> s {arn = a} :: ServiceInstanceSummary)

-- | The time when the service instance was created.
serviceInstanceSummary_createdAt :: Lens.Lens' ServiceInstanceSummary Prelude.UTCTime
serviceInstanceSummary_createdAt = Lens.lens (\ServiceInstanceSummary' {createdAt} -> createdAt) (\s@ServiceInstanceSummary' {} a -> s {createdAt = a} :: ServiceInstanceSummary) Prelude.. Core._Time

-- | The service instance deployment status.
serviceInstanceSummary_deploymentStatus :: Lens.Lens' ServiceInstanceSummary DeploymentStatus
serviceInstanceSummary_deploymentStatus = Lens.lens (\ServiceInstanceSummary' {deploymentStatus} -> deploymentStatus) (\s@ServiceInstanceSummary' {} a -> s {deploymentStatus = a} :: ServiceInstanceSummary)

-- | The name of the environment that the service instance was deployed into.
serviceInstanceSummary_environmentName :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_environmentName = Lens.lens (\ServiceInstanceSummary' {environmentName} -> environmentName) (\s@ServiceInstanceSummary' {} a -> s {environmentName = a} :: ServiceInstanceSummary)

-- | The time when a deployment of the service was last attempted.
serviceInstanceSummary_lastDeploymentAttemptedAt :: Lens.Lens' ServiceInstanceSummary Prelude.UTCTime
serviceInstanceSummary_lastDeploymentAttemptedAt = Lens.lens (\ServiceInstanceSummary' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@ServiceInstanceSummary' {} a -> s {lastDeploymentAttemptedAt = a} :: ServiceInstanceSummary) Prelude.. Core._Time

-- | The time when the service was last deployed successfully.
serviceInstanceSummary_lastDeploymentSucceededAt :: Lens.Lens' ServiceInstanceSummary Prelude.UTCTime
serviceInstanceSummary_lastDeploymentSucceededAt = Lens.lens (\ServiceInstanceSummary' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@ServiceInstanceSummary' {} a -> s {lastDeploymentSucceededAt = a} :: ServiceInstanceSummary) Prelude.. Core._Time

-- | The name of the service instance.
serviceInstanceSummary_name :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_name = Lens.lens (\ServiceInstanceSummary' {name} -> name) (\s@ServiceInstanceSummary' {} a -> s {name = a} :: ServiceInstanceSummary)

-- | The name of the service that the service instance belongs to.
serviceInstanceSummary_serviceName :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_serviceName = Lens.lens (\ServiceInstanceSummary' {serviceName} -> serviceName) (\s@ServiceInstanceSummary' {} a -> s {serviceName = a} :: ServiceInstanceSummary)

-- | The service instance template major version.
serviceInstanceSummary_templateMajorVersion :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_templateMajorVersion = Lens.lens (\ServiceInstanceSummary' {templateMajorVersion} -> templateMajorVersion) (\s@ServiceInstanceSummary' {} a -> s {templateMajorVersion = a} :: ServiceInstanceSummary)

-- | The service instance template minor version.
serviceInstanceSummary_templateMinorVersion :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_templateMinorVersion = Lens.lens (\ServiceInstanceSummary' {templateMinorVersion} -> templateMinorVersion) (\s@ServiceInstanceSummary' {} a -> s {templateMinorVersion = a} :: ServiceInstanceSummary)

-- | The name of the service template.
serviceInstanceSummary_templateName :: Lens.Lens' ServiceInstanceSummary Prelude.Text
serviceInstanceSummary_templateName = Lens.lens (\ServiceInstanceSummary' {templateName} -> templateName) (\s@ServiceInstanceSummary' {} a -> s {templateName = a} :: ServiceInstanceSummary)

instance Core.FromJSON ServiceInstanceSummary where
  parseJSON =
    Core.withObject
      "ServiceInstanceSummary"
      ( \x ->
          ServiceInstanceSummary'
            Prelude.<$> (x Core..:? "deploymentStatusMessage")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "deploymentStatus")
            Prelude.<*> (x Core..: "environmentName")
            Prelude.<*> (x Core..: "lastDeploymentAttemptedAt")
            Prelude.<*> (x Core..: "lastDeploymentSucceededAt")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "serviceName")
            Prelude.<*> (x Core..: "templateMajorVersion")
            Prelude.<*> (x Core..: "templateMinorVersion")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable ServiceInstanceSummary where
  hashWithSalt _salt ServiceInstanceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData ServiceInstanceSummary where
  rnf ServiceInstanceSummary' {..} =
    Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf templateName
