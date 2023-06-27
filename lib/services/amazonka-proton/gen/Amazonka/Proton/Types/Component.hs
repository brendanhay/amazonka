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
-- Module      : Amazonka.Proton.Types.Component
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Component where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus

-- | Detailed data of an Proton component resource.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- /See:/ 'newComponent' smart constructor.
data Component = Component'
  { -- | The message associated with the component deployment status.
    deploymentStatusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A description of the component.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The last token the client requested.
    lastClientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The time when a deployment of the component was last attempted.
    lastDeploymentAttemptedAt :: Prelude.Maybe Data.POSIX,
    -- | The time when the component was last deployed successfully.
    lastDeploymentSucceededAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the service instance that this component is attached to.
    -- Provided when a component is attached to a service instance.
    serviceInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the service that @serviceInstanceName@ is associated with.
    -- Provided when a component is attached to a service instance.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The service spec that the component uses to access service inputs.
    -- Provided when a component is attached to a service instance.
    serviceSpec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the component.
    arn :: Prelude.Text,
    -- | The time when the component was created.
    createdAt :: Data.POSIX,
    -- | The component deployment status.
    deploymentStatus :: DeploymentStatus,
    -- | The name of the Proton environment that this component is associated
    -- with.
    environmentName :: Prelude.Text,
    -- | The time when the component was last modified.
    lastModifiedAt :: Data.POSIX,
    -- | The name of the component.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Component' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatusMessage', 'component_deploymentStatusMessage' - The message associated with the component deployment status.
--
-- 'description', 'component_description' - A description of the component.
--
-- 'lastClientRequestToken', 'component_lastClientRequestToken' - The last token the client requested.
--
-- 'lastDeploymentAttemptedAt', 'component_lastDeploymentAttemptedAt' - The time when a deployment of the component was last attempted.
--
-- 'lastDeploymentSucceededAt', 'component_lastDeploymentSucceededAt' - The time when the component was last deployed successfully.
--
-- 'serviceInstanceName', 'component_serviceInstanceName' - The name of the service instance that this component is attached to.
-- Provided when a component is attached to a service instance.
--
-- 'serviceName', 'component_serviceName' - The name of the service that @serviceInstanceName@ is associated with.
-- Provided when a component is attached to a service instance.
--
-- 'serviceSpec', 'component_serviceSpec' - The service spec that the component uses to access service inputs.
-- Provided when a component is attached to a service instance.
--
-- 'arn', 'component_arn' - The Amazon Resource Name (ARN) of the component.
--
-- 'createdAt', 'component_createdAt' - The time when the component was created.
--
-- 'deploymentStatus', 'component_deploymentStatus' - The component deployment status.
--
-- 'environmentName', 'component_environmentName' - The name of the Proton environment that this component is associated
-- with.
--
-- 'lastModifiedAt', 'component_lastModifiedAt' - The time when the component was last modified.
--
-- 'name', 'component_name' - The name of the component.
newComponent ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'deploymentStatus'
  DeploymentStatus ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  Component
newComponent
  pArn_
  pCreatedAt_
  pDeploymentStatus_
  pEnvironmentName_
  pLastModifiedAt_
  pName_ =
    Component'
      { deploymentStatusMessage =
          Prelude.Nothing,
        description = Prelude.Nothing,
        lastClientRequestToken = Prelude.Nothing,
        lastDeploymentAttemptedAt = Prelude.Nothing,
        lastDeploymentSucceededAt = Prelude.Nothing,
        serviceInstanceName = Prelude.Nothing,
        serviceName = Prelude.Nothing,
        serviceSpec = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        environmentName = pEnvironmentName_,
        lastModifiedAt = Data._Time Lens.# pLastModifiedAt_,
        name = pName_
      }

-- | The message associated with the component deployment status.
component_deploymentStatusMessage :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_deploymentStatusMessage = Lens.lens (\Component' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@Component' {} a -> s {deploymentStatusMessage = a} :: Component) Prelude.. Lens.mapping Data._Sensitive

-- | A description of the component.
component_description :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_description = Lens.lens (\Component' {description} -> description) (\s@Component' {} a -> s {description = a} :: Component) Prelude.. Lens.mapping Data._Sensitive

-- | The last token the client requested.
component_lastClientRequestToken :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_lastClientRequestToken = Lens.lens (\Component' {lastClientRequestToken} -> lastClientRequestToken) (\s@Component' {} a -> s {lastClientRequestToken = a} :: Component)

-- | The time when a deployment of the component was last attempted.
component_lastDeploymentAttemptedAt :: Lens.Lens' Component (Prelude.Maybe Prelude.UTCTime)
component_lastDeploymentAttemptedAt = Lens.lens (\Component' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@Component' {} a -> s {lastDeploymentAttemptedAt = a} :: Component) Prelude.. Lens.mapping Data._Time

-- | The time when the component was last deployed successfully.
component_lastDeploymentSucceededAt :: Lens.Lens' Component (Prelude.Maybe Prelude.UTCTime)
component_lastDeploymentSucceededAt = Lens.lens (\Component' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@Component' {} a -> s {lastDeploymentSucceededAt = a} :: Component) Prelude.. Lens.mapping Data._Time

-- | The name of the service instance that this component is attached to.
-- Provided when a component is attached to a service instance.
component_serviceInstanceName :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_serviceInstanceName = Lens.lens (\Component' {serviceInstanceName} -> serviceInstanceName) (\s@Component' {} a -> s {serviceInstanceName = a} :: Component)

-- | The name of the service that @serviceInstanceName@ is associated with.
-- Provided when a component is attached to a service instance.
component_serviceName :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_serviceName = Lens.lens (\Component' {serviceName} -> serviceName) (\s@Component' {} a -> s {serviceName = a} :: Component)

-- | The service spec that the component uses to access service inputs.
-- Provided when a component is attached to a service instance.
component_serviceSpec :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_serviceSpec = Lens.lens (\Component' {serviceSpec} -> serviceSpec) (\s@Component' {} a -> s {serviceSpec = a} :: Component) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the component.
component_arn :: Lens.Lens' Component Prelude.Text
component_arn = Lens.lens (\Component' {arn} -> arn) (\s@Component' {} a -> s {arn = a} :: Component)

-- | The time when the component was created.
component_createdAt :: Lens.Lens' Component Prelude.UTCTime
component_createdAt = Lens.lens (\Component' {createdAt} -> createdAt) (\s@Component' {} a -> s {createdAt = a} :: Component) Prelude.. Data._Time

-- | The component deployment status.
component_deploymentStatus :: Lens.Lens' Component DeploymentStatus
component_deploymentStatus = Lens.lens (\Component' {deploymentStatus} -> deploymentStatus) (\s@Component' {} a -> s {deploymentStatus = a} :: Component)

-- | The name of the Proton environment that this component is associated
-- with.
component_environmentName :: Lens.Lens' Component Prelude.Text
component_environmentName = Lens.lens (\Component' {environmentName} -> environmentName) (\s@Component' {} a -> s {environmentName = a} :: Component)

-- | The time when the component was last modified.
component_lastModifiedAt :: Lens.Lens' Component Prelude.UTCTime
component_lastModifiedAt = Lens.lens (\Component' {lastModifiedAt} -> lastModifiedAt) (\s@Component' {} a -> s {lastModifiedAt = a} :: Component) Prelude.. Data._Time

-- | The name of the component.
component_name :: Lens.Lens' Component Prelude.Text
component_name = Lens.lens (\Component' {name} -> name) (\s@Component' {} a -> s {name = a} :: Component)

instance Data.FromJSON Component where
  parseJSON =
    Data.withObject
      "Component"
      ( \x ->
          Component'
            Prelude.<$> (x Data..:? "deploymentStatusMessage")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastClientRequestToken")
            Prelude.<*> (x Data..:? "lastDeploymentAttemptedAt")
            Prelude.<*> (x Data..:? "lastDeploymentSucceededAt")
            Prelude.<*> (x Data..:? "serviceInstanceName")
            Prelude.<*> (x Data..:? "serviceName")
            Prelude.<*> (x Data..:? "serviceSpec")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "deploymentStatus")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "lastModifiedAt")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Component where
  hashWithSalt _salt Component' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastClientRequestToken
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceSpec
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` name

instance Prelude.NFData Component where
  rnf Component' {..} =
    Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastClientRequestToken
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf serviceInstanceName
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceSpec
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf name
