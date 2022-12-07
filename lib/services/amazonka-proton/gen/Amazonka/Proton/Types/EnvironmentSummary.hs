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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus
import Amazonka.Proton.Types.Provisioning

-- | Summary data of an Proton environment resource. An Proton environment is
-- a set of resources shared across Proton services.
--
-- /See:/ 'newEnvironmentSummary' smart constructor.
data EnvironmentSummary = EnvironmentSummary'
  { -- | When included, indicates that the environment template is for customer
    -- provisioned and managed infrastructure.
    provisioning :: Prelude.Maybe Provisioning,
    -- | The description of the environment.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An environment deployment status message.
    deploymentStatusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the Proton service role that allows
    -- Proton to make calls to other services on your behalf.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account that the environment infrastructure
    -- resources are provisioned in.
    environmentAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
    -- when provisioning directly defined components in this environment. It
    -- determines the scope of infrastructure that a component can provision.
    --
    -- The environment must have a @componentRoleArn@ to allow directly defined
    -- components to be associated with the environment.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    componentRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account connection that the environment is
    -- associated with.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Text,
    -- | The time when the environment was created.
    createdAt :: Data.POSIX,
    -- | The environment deployment status.
    deploymentStatus :: DeploymentStatus,
    -- | The time when a deployment of the environment was last attempted.
    lastDeploymentAttemptedAt :: Data.POSIX,
    -- | The time when the environment was last deployed successfully.
    lastDeploymentSucceededAt :: Data.POSIX,
    -- | The name of the environment.
    name :: Prelude.Text,
    -- | The major version of the environment template.
    templateMajorVersion :: Prelude.Text,
    -- | The minor version of the environment template.
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
-- 'provisioning', 'environmentSummary_provisioning' - When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
--
-- 'description', 'environmentSummary_description' - The description of the environment.
--
-- 'deploymentStatusMessage', 'environmentSummary_deploymentStatusMessage' - An environment deployment status message.
--
-- 'protonServiceRoleArn', 'environmentSummary_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make calls to other services on your behalf.
--
-- 'environmentAccountId', 'environmentSummary_environmentAccountId' - The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
--
-- 'componentRoleArn', 'environmentSummary_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in this environment. It
-- determines the scope of infrastructure that a component can provision.
--
-- The environment must have a @componentRoleArn@ to allow directly defined
-- components to be associated with the environment.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'environmentAccountConnectionId', 'environmentSummary_environmentAccountConnectionId' - The ID of the environment account connection that the environment is
-- associated with.
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
-- 'templateMajorVersion', 'environmentSummary_templateMajorVersion' - The major version of the environment template.
--
-- 'templateMinorVersion', 'environmentSummary_templateMinorVersion' - The minor version of the environment template.
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
      { provisioning = Prelude.Nothing,
        description = Prelude.Nothing,
        deploymentStatusMessage = Prelude.Nothing,
        protonServiceRoleArn = Prelude.Nothing,
        environmentAccountId = Prelude.Nothing,
        componentRoleArn = Prelude.Nothing,
        environmentAccountConnectionId = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        lastDeploymentAttemptedAt =
          Data._Time Lens.# pLastDeploymentAttemptedAt_,
        lastDeploymentSucceededAt =
          Data._Time Lens.# pLastDeploymentSucceededAt_,
        name = pName_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateMinorVersion = pTemplateMinorVersion_,
        templateName = pTemplateName_
      }

-- | When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
environmentSummary_provisioning :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Provisioning)
environmentSummary_provisioning = Lens.lens (\EnvironmentSummary' {provisioning} -> provisioning) (\s@EnvironmentSummary' {} a -> s {provisioning = a} :: EnvironmentSummary)

-- | The description of the environment.
environmentSummary_description :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_description = Lens.lens (\EnvironmentSummary' {description} -> description) (\s@EnvironmentSummary' {} a -> s {description = a} :: EnvironmentSummary) Prelude.. Lens.mapping Data._Sensitive

-- | An environment deployment status message.
environmentSummary_deploymentStatusMessage :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_deploymentStatusMessage = Lens.lens (\EnvironmentSummary' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@EnvironmentSummary' {} a -> s {deploymentStatusMessage = a} :: EnvironmentSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make calls to other services on your behalf.
environmentSummary_protonServiceRoleArn :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_protonServiceRoleArn = Lens.lens (\EnvironmentSummary' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@EnvironmentSummary' {} a -> s {protonServiceRoleArn = a} :: EnvironmentSummary)

-- | The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
environmentSummary_environmentAccountId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_environmentAccountId = Lens.lens (\EnvironmentSummary' {environmentAccountId} -> environmentAccountId) (\s@EnvironmentSummary' {} a -> s {environmentAccountId = a} :: EnvironmentSummary)

-- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in this environment. It
-- determines the scope of infrastructure that a component can provision.
--
-- The environment must have a @componentRoleArn@ to allow directly defined
-- components to be associated with the environment.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
environmentSummary_componentRoleArn :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_componentRoleArn = Lens.lens (\EnvironmentSummary' {componentRoleArn} -> componentRoleArn) (\s@EnvironmentSummary' {} a -> s {componentRoleArn = a} :: EnvironmentSummary)

-- | The ID of the environment account connection that the environment is
-- associated with.
environmentSummary_environmentAccountConnectionId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_environmentAccountConnectionId = Lens.lens (\EnvironmentSummary' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@EnvironmentSummary' {} a -> s {environmentAccountConnectionId = a} :: EnvironmentSummary)

-- | The Amazon Resource Name (ARN) of the environment.
environmentSummary_arn :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_arn = Lens.lens (\EnvironmentSummary' {arn} -> arn) (\s@EnvironmentSummary' {} a -> s {arn = a} :: EnvironmentSummary)

-- | The time when the environment was created.
environmentSummary_createdAt :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_createdAt = Lens.lens (\EnvironmentSummary' {createdAt} -> createdAt) (\s@EnvironmentSummary' {} a -> s {createdAt = a} :: EnvironmentSummary) Prelude.. Data._Time

-- | The environment deployment status.
environmentSummary_deploymentStatus :: Lens.Lens' EnvironmentSummary DeploymentStatus
environmentSummary_deploymentStatus = Lens.lens (\EnvironmentSummary' {deploymentStatus} -> deploymentStatus) (\s@EnvironmentSummary' {} a -> s {deploymentStatus = a} :: EnvironmentSummary)

-- | The time when a deployment of the environment was last attempted.
environmentSummary_lastDeploymentAttemptedAt :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_lastDeploymentAttemptedAt = Lens.lens (\EnvironmentSummary' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@EnvironmentSummary' {} a -> s {lastDeploymentAttemptedAt = a} :: EnvironmentSummary) Prelude.. Data._Time

-- | The time when the environment was last deployed successfully.
environmentSummary_lastDeploymentSucceededAt :: Lens.Lens' EnvironmentSummary Prelude.UTCTime
environmentSummary_lastDeploymentSucceededAt = Lens.lens (\EnvironmentSummary' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@EnvironmentSummary' {} a -> s {lastDeploymentSucceededAt = a} :: EnvironmentSummary) Prelude.. Data._Time

-- | The name of the environment.
environmentSummary_name :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_name = Lens.lens (\EnvironmentSummary' {name} -> name) (\s@EnvironmentSummary' {} a -> s {name = a} :: EnvironmentSummary)

-- | The major version of the environment template.
environmentSummary_templateMajorVersion :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_templateMajorVersion = Lens.lens (\EnvironmentSummary' {templateMajorVersion} -> templateMajorVersion) (\s@EnvironmentSummary' {} a -> s {templateMajorVersion = a} :: EnvironmentSummary)

-- | The minor version of the environment template.
environmentSummary_templateMinorVersion :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_templateMinorVersion = Lens.lens (\EnvironmentSummary' {templateMinorVersion} -> templateMinorVersion) (\s@EnvironmentSummary' {} a -> s {templateMinorVersion = a} :: EnvironmentSummary)

-- | The name of the environment template.
environmentSummary_templateName :: Lens.Lens' EnvironmentSummary Prelude.Text
environmentSummary_templateName = Lens.lens (\EnvironmentSummary' {templateName} -> templateName) (\s@EnvironmentSummary' {} a -> s {templateName = a} :: EnvironmentSummary)

instance Data.FromJSON EnvironmentSummary where
  parseJSON =
    Data.withObject
      "EnvironmentSummary"
      ( \x ->
          EnvironmentSummary'
            Prelude.<$> (x Data..:? "provisioning")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "deploymentStatusMessage")
            Prelude.<*> (x Data..:? "protonServiceRoleArn")
            Prelude.<*> (x Data..:? "environmentAccountId")
            Prelude.<*> (x Data..:? "componentRoleArn")
            Prelude.<*> (x Data..:? "environmentAccountConnectionId")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "deploymentStatus")
            Prelude.<*> (x Data..: "lastDeploymentAttemptedAt")
            Prelude.<*> (x Data..: "lastDeploymentSucceededAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "templateMajorVersion")
            Prelude.<*> (x Data..: "templateMinorVersion")
            Prelude.<*> (x Data..: "templateName")
      )

instance Prelude.Hashable EnvironmentSummary where
  hashWithSalt _salt EnvironmentSummary' {..} =
    _salt `Prelude.hashWithSalt` provisioning
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` environmentAccountId
      `Prelude.hashWithSalt` componentRoleArn
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData EnvironmentSummary where
  rnf EnvironmentSummary' {..} =
    Prelude.rnf provisioning
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf environmentAccountId
      `Prelude.seq` Prelude.rnf componentRoleArn
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf templateName
