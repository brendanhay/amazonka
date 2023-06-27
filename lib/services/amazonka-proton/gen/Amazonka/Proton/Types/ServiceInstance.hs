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
-- Module      : Amazonka.Proton.Types.ServiceInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ServiceInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus

-- | Detailed data of an Proton service instance resource.
--
-- /See:/ 'newServiceInstance' smart constructor.
data ServiceInstance = ServiceInstance'
  { -- | The message associated with the service instance deployment status.
    deploymentStatusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The last client request token received.
    lastClientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The service spec that was used to create the service instance.
    spec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the service instance.
    arn :: Prelude.Text,
    -- | The time when the service instance was created.
    createdAt :: Data.POSIX,
    -- | The service instance deployment status.
    deploymentStatus :: DeploymentStatus,
    -- | The name of the environment that the service instance was deployed into.
    environmentName :: Prelude.Text,
    -- | The time when a deployment of the service instance was last attempted.
    lastDeploymentAttemptedAt :: Data.POSIX,
    -- | The time when the service instance was last deployed successfully.
    lastDeploymentSucceededAt :: Data.POSIX,
    -- | The name of the service instance.
    name :: Prelude.Text,
    -- | The name of the service that the service instance belongs to.
    serviceName :: Prelude.Text,
    -- | The major version of the service template that was used to create the
    -- service instance.
    templateMajorVersion :: Prelude.Text,
    -- | The minor version of the service template that was used to create the
    -- service instance.
    templateMinorVersion :: Prelude.Text,
    -- | The name of the service template that was used to create the service
    -- instance.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatusMessage', 'serviceInstance_deploymentStatusMessage' - The message associated with the service instance deployment status.
--
-- 'lastClientRequestToken', 'serviceInstance_lastClientRequestToken' - The last client request token received.
--
-- 'spec', 'serviceInstance_spec' - The service spec that was used to create the service instance.
--
-- 'arn', 'serviceInstance_arn' - The Amazon Resource Name (ARN) of the service instance.
--
-- 'createdAt', 'serviceInstance_createdAt' - The time when the service instance was created.
--
-- 'deploymentStatus', 'serviceInstance_deploymentStatus' - The service instance deployment status.
--
-- 'environmentName', 'serviceInstance_environmentName' - The name of the environment that the service instance was deployed into.
--
-- 'lastDeploymentAttemptedAt', 'serviceInstance_lastDeploymentAttemptedAt' - The time when a deployment of the service instance was last attempted.
--
-- 'lastDeploymentSucceededAt', 'serviceInstance_lastDeploymentSucceededAt' - The time when the service instance was last deployed successfully.
--
-- 'name', 'serviceInstance_name' - The name of the service instance.
--
-- 'serviceName', 'serviceInstance_serviceName' - The name of the service that the service instance belongs to.
--
-- 'templateMajorVersion', 'serviceInstance_templateMajorVersion' - The major version of the service template that was used to create the
-- service instance.
--
-- 'templateMinorVersion', 'serviceInstance_templateMinorVersion' - The minor version of the service template that was used to create the
-- service instance.
--
-- 'templateName', 'serviceInstance_templateName' - The name of the service template that was used to create the service
-- instance.
newServiceInstance ::
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
  ServiceInstance
newServiceInstance
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
    ServiceInstance'
      { deploymentStatusMessage =
          Prelude.Nothing,
        lastClientRequestToken = Prelude.Nothing,
        spec = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        environmentName = pEnvironmentName_,
        lastDeploymentAttemptedAt =
          Data._Time Lens.# pLastDeploymentAttemptedAt_,
        lastDeploymentSucceededAt =
          Data._Time Lens.# pLastDeploymentSucceededAt_,
        name = pName_,
        serviceName = pServiceName_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateMinorVersion = pTemplateMinorVersion_,
        templateName = pTemplateName_
      }

-- | The message associated with the service instance deployment status.
serviceInstance_deploymentStatusMessage :: Lens.Lens' ServiceInstance (Prelude.Maybe Prelude.Text)
serviceInstance_deploymentStatusMessage = Lens.lens (\ServiceInstance' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@ServiceInstance' {} a -> s {deploymentStatusMessage = a} :: ServiceInstance) Prelude.. Lens.mapping Data._Sensitive

-- | The last client request token received.
serviceInstance_lastClientRequestToken :: Lens.Lens' ServiceInstance (Prelude.Maybe Prelude.Text)
serviceInstance_lastClientRequestToken = Lens.lens (\ServiceInstance' {lastClientRequestToken} -> lastClientRequestToken) (\s@ServiceInstance' {} a -> s {lastClientRequestToken = a} :: ServiceInstance)

-- | The service spec that was used to create the service instance.
serviceInstance_spec :: Lens.Lens' ServiceInstance (Prelude.Maybe Prelude.Text)
serviceInstance_spec = Lens.lens (\ServiceInstance' {spec} -> spec) (\s@ServiceInstance' {} a -> s {spec = a} :: ServiceInstance) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the service instance.
serviceInstance_arn :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_arn = Lens.lens (\ServiceInstance' {arn} -> arn) (\s@ServiceInstance' {} a -> s {arn = a} :: ServiceInstance)

-- | The time when the service instance was created.
serviceInstance_createdAt :: Lens.Lens' ServiceInstance Prelude.UTCTime
serviceInstance_createdAt = Lens.lens (\ServiceInstance' {createdAt} -> createdAt) (\s@ServiceInstance' {} a -> s {createdAt = a} :: ServiceInstance) Prelude.. Data._Time

-- | The service instance deployment status.
serviceInstance_deploymentStatus :: Lens.Lens' ServiceInstance DeploymentStatus
serviceInstance_deploymentStatus = Lens.lens (\ServiceInstance' {deploymentStatus} -> deploymentStatus) (\s@ServiceInstance' {} a -> s {deploymentStatus = a} :: ServiceInstance)

-- | The name of the environment that the service instance was deployed into.
serviceInstance_environmentName :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_environmentName = Lens.lens (\ServiceInstance' {environmentName} -> environmentName) (\s@ServiceInstance' {} a -> s {environmentName = a} :: ServiceInstance)

-- | The time when a deployment of the service instance was last attempted.
serviceInstance_lastDeploymentAttemptedAt :: Lens.Lens' ServiceInstance Prelude.UTCTime
serviceInstance_lastDeploymentAttemptedAt = Lens.lens (\ServiceInstance' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@ServiceInstance' {} a -> s {lastDeploymentAttemptedAt = a} :: ServiceInstance) Prelude.. Data._Time

-- | The time when the service instance was last deployed successfully.
serviceInstance_lastDeploymentSucceededAt :: Lens.Lens' ServiceInstance Prelude.UTCTime
serviceInstance_lastDeploymentSucceededAt = Lens.lens (\ServiceInstance' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@ServiceInstance' {} a -> s {lastDeploymentSucceededAt = a} :: ServiceInstance) Prelude.. Data._Time

-- | The name of the service instance.
serviceInstance_name :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_name = Lens.lens (\ServiceInstance' {name} -> name) (\s@ServiceInstance' {} a -> s {name = a} :: ServiceInstance)

-- | The name of the service that the service instance belongs to.
serviceInstance_serviceName :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_serviceName = Lens.lens (\ServiceInstance' {serviceName} -> serviceName) (\s@ServiceInstance' {} a -> s {serviceName = a} :: ServiceInstance)

-- | The major version of the service template that was used to create the
-- service instance.
serviceInstance_templateMajorVersion :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_templateMajorVersion = Lens.lens (\ServiceInstance' {templateMajorVersion} -> templateMajorVersion) (\s@ServiceInstance' {} a -> s {templateMajorVersion = a} :: ServiceInstance)

-- | The minor version of the service template that was used to create the
-- service instance.
serviceInstance_templateMinorVersion :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_templateMinorVersion = Lens.lens (\ServiceInstance' {templateMinorVersion} -> templateMinorVersion) (\s@ServiceInstance' {} a -> s {templateMinorVersion = a} :: ServiceInstance)

-- | The name of the service template that was used to create the service
-- instance.
serviceInstance_templateName :: Lens.Lens' ServiceInstance Prelude.Text
serviceInstance_templateName = Lens.lens (\ServiceInstance' {templateName} -> templateName) (\s@ServiceInstance' {} a -> s {templateName = a} :: ServiceInstance)

instance Data.FromJSON ServiceInstance where
  parseJSON =
    Data.withObject
      "ServiceInstance"
      ( \x ->
          ServiceInstance'
            Prelude.<$> (x Data..:? "deploymentStatusMessage")
            Prelude.<*> (x Data..:? "lastClientRequestToken")
            Prelude.<*> (x Data..:? "spec")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "deploymentStatus")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "lastDeploymentAttemptedAt")
            Prelude.<*> (x Data..: "lastDeploymentSucceededAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "serviceName")
            Prelude.<*> (x Data..: "templateMajorVersion")
            Prelude.<*> (x Data..: "templateMinorVersion")
            Prelude.<*> (x Data..: "templateName")
      )

instance Prelude.Hashable ServiceInstance where
  hashWithSalt _salt ServiceInstance' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` lastClientRequestToken
      `Prelude.hashWithSalt` spec
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

instance Prelude.NFData ServiceInstance where
  rnf ServiceInstance' {..} =
    Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf lastClientRequestToken
      `Prelude.seq` Prelude.rnf spec
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
