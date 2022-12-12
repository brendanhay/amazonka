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
-- Module      : Amazonka.Proton.Types.Environment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Environment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus
import Amazonka.Proton.Types.Provisioning
import Amazonka.Proton.Types.RepositoryBranch

-- | Detailed data of an Proton environment resource. An Proton environment
-- is a set of resources shared across Proton services.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The Amazon Resource Name (ARN) of the IAM service role that allows
    -- Proton to provision infrastructure using CodeBuild-based provisioning on
    -- your behalf.
    codebuildRoleArn :: Prelude.Maybe Prelude.Text,
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
    -- | An environment deployment status message.
    deploymentStatusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The description of the environment.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the environment account connection that\'s used to provision
    -- infrastructure resources in an environment account.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account that the environment infrastructure
    -- resources are provisioned in.
    environmentAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Proton service role that allows
    -- Proton to make calls to other services on your behalf.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | When included, indicates that the environment template is for customer
    -- provisioned and managed infrastructure.
    provisioning :: Prelude.Maybe Provisioning,
    -- | The linked repository that you use to host your rendered infrastructure
    -- templates for self-managed provisioning. A linked repository is a
    -- repository that has been registered with Proton. For more information,
    -- see CreateRepository.
    provisioningRepository :: Prelude.Maybe RepositoryBranch,
    -- | The environment spec.
    spec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
    -- | The Amazon Resource Name (ARN) of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codebuildRoleArn', 'environment_codebuildRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that allows
-- Proton to provision infrastructure using CodeBuild-based provisioning on
-- your behalf.
--
-- 'componentRoleArn', 'environment_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
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
-- 'deploymentStatusMessage', 'environment_deploymentStatusMessage' - An environment deployment status message.
--
-- 'description', 'environment_description' - The description of the environment.
--
-- 'environmentAccountConnectionId', 'environment_environmentAccountConnectionId' - The ID of the environment account connection that\'s used to provision
-- infrastructure resources in an environment account.
--
-- 'environmentAccountId', 'environment_environmentAccountId' - The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
--
-- 'protonServiceRoleArn', 'environment_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make calls to other services on your behalf.
--
-- 'provisioning', 'environment_provisioning' - When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
--
-- 'provisioningRepository', 'environment_provisioningRepository' - The linked repository that you use to host your rendered infrastructure
-- templates for self-managed provisioning. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
--
-- 'spec', 'environment_spec' - The environment spec.
--
-- 'arn', 'environment_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'createdAt', 'environment_createdAt' - The time when the environment was created.
--
-- 'deploymentStatus', 'environment_deploymentStatus' - The environment deployment status.
--
-- 'lastDeploymentAttemptedAt', 'environment_lastDeploymentAttemptedAt' - The time when a deployment of the environment was last attempted.
--
-- 'lastDeploymentSucceededAt', 'environment_lastDeploymentSucceededAt' - The time when the environment was last deployed successfully.
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'templateMajorVersion', 'environment_templateMajorVersion' - The major version of the environment template.
--
-- 'templateMinorVersion', 'environment_templateMinorVersion' - The minor version of the environment template.
--
-- 'templateName', 'environment_templateName' - The Amazon Resource Name (ARN) of the environment template.
newEnvironment ::
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
  Environment
newEnvironment
  pArn_
  pCreatedAt_
  pDeploymentStatus_
  pLastDeploymentAttemptedAt_
  pLastDeploymentSucceededAt_
  pName_
  pTemplateMajorVersion_
  pTemplateMinorVersion_
  pTemplateName_ =
    Environment'
      { codebuildRoleArn = Prelude.Nothing,
        componentRoleArn = Prelude.Nothing,
        deploymentStatusMessage = Prelude.Nothing,
        description = Prelude.Nothing,
        environmentAccountConnectionId = Prelude.Nothing,
        environmentAccountId = Prelude.Nothing,
        protonServiceRoleArn = Prelude.Nothing,
        provisioning = Prelude.Nothing,
        provisioningRepository = Prelude.Nothing,
        spec = Prelude.Nothing,
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

-- | The Amazon Resource Name (ARN) of the IAM service role that allows
-- Proton to provision infrastructure using CodeBuild-based provisioning on
-- your behalf.
environment_codebuildRoleArn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_codebuildRoleArn = Lens.lens (\Environment' {codebuildRoleArn} -> codebuildRoleArn) (\s@Environment' {} a -> s {codebuildRoleArn = a} :: Environment)

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
environment_componentRoleArn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_componentRoleArn = Lens.lens (\Environment' {componentRoleArn} -> componentRoleArn) (\s@Environment' {} a -> s {componentRoleArn = a} :: Environment)

-- | An environment deployment status message.
environment_deploymentStatusMessage :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_deploymentStatusMessage = Lens.lens (\Environment' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@Environment' {} a -> s {deploymentStatusMessage = a} :: Environment) Prelude.. Lens.mapping Data._Sensitive

-- | The description of the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the environment account connection that\'s used to provision
-- infrastructure resources in an environment account.
environment_environmentAccountConnectionId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentAccountConnectionId = Lens.lens (\Environment' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@Environment' {} a -> s {environmentAccountConnectionId = a} :: Environment)

-- | The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
environment_environmentAccountId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentAccountId = Lens.lens (\Environment' {environmentAccountId} -> environmentAccountId) (\s@Environment' {} a -> s {environmentAccountId = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make calls to other services on your behalf.
environment_protonServiceRoleArn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_protonServiceRoleArn = Lens.lens (\Environment' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@Environment' {} a -> s {protonServiceRoleArn = a} :: Environment)

-- | When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
environment_provisioning :: Lens.Lens' Environment (Prelude.Maybe Provisioning)
environment_provisioning = Lens.lens (\Environment' {provisioning} -> provisioning) (\s@Environment' {} a -> s {provisioning = a} :: Environment)

-- | The linked repository that you use to host your rendered infrastructure
-- templates for self-managed provisioning. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
environment_provisioningRepository :: Lens.Lens' Environment (Prelude.Maybe RepositoryBranch)
environment_provisioningRepository = Lens.lens (\Environment' {provisioningRepository} -> provisioningRepository) (\s@Environment' {} a -> s {provisioningRepository = a} :: Environment)

-- | The environment spec.
environment_spec :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_spec = Lens.lens (\Environment' {spec} -> spec) (\s@Environment' {} a -> s {spec = a} :: Environment) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the environment.
environment_arn :: Lens.Lens' Environment Prelude.Text
environment_arn = Lens.lens (\Environment' {arn} -> arn) (\s@Environment' {} a -> s {arn = a} :: Environment)

-- | The time when the environment was created.
environment_createdAt :: Lens.Lens' Environment Prelude.UTCTime
environment_createdAt = Lens.lens (\Environment' {createdAt} -> createdAt) (\s@Environment' {} a -> s {createdAt = a} :: Environment) Prelude.. Data._Time

-- | The environment deployment status.
environment_deploymentStatus :: Lens.Lens' Environment DeploymentStatus
environment_deploymentStatus = Lens.lens (\Environment' {deploymentStatus} -> deploymentStatus) (\s@Environment' {} a -> s {deploymentStatus = a} :: Environment)

-- | The time when a deployment of the environment was last attempted.
environment_lastDeploymentAttemptedAt :: Lens.Lens' Environment Prelude.UTCTime
environment_lastDeploymentAttemptedAt = Lens.lens (\Environment' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@Environment' {} a -> s {lastDeploymentAttemptedAt = a} :: Environment) Prelude.. Data._Time

-- | The time when the environment was last deployed successfully.
environment_lastDeploymentSucceededAt :: Lens.Lens' Environment Prelude.UTCTime
environment_lastDeploymentSucceededAt = Lens.lens (\Environment' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@Environment' {} a -> s {lastDeploymentSucceededAt = a} :: Environment) Prelude.. Data._Time

-- | The name of the environment.
environment_name :: Lens.Lens' Environment Prelude.Text
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The major version of the environment template.
environment_templateMajorVersion :: Lens.Lens' Environment Prelude.Text
environment_templateMajorVersion = Lens.lens (\Environment' {templateMajorVersion} -> templateMajorVersion) (\s@Environment' {} a -> s {templateMajorVersion = a} :: Environment)

-- | The minor version of the environment template.
environment_templateMinorVersion :: Lens.Lens' Environment Prelude.Text
environment_templateMinorVersion = Lens.lens (\Environment' {templateMinorVersion} -> templateMinorVersion) (\s@Environment' {} a -> s {templateMinorVersion = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment template.
environment_templateName :: Lens.Lens' Environment Prelude.Text
environment_templateName = Lens.lens (\Environment' {templateName} -> templateName) (\s@Environment' {} a -> s {templateName = a} :: Environment)

instance Data.FromJSON Environment where
  parseJSON =
    Data.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Data..:? "codebuildRoleArn")
            Prelude.<*> (x Data..:? "componentRoleArn")
            Prelude.<*> (x Data..:? "deploymentStatusMessage")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "environmentAccountConnectionId")
            Prelude.<*> (x Data..:? "environmentAccountId")
            Prelude.<*> (x Data..:? "protonServiceRoleArn")
            Prelude.<*> (x Data..:? "provisioning")
            Prelude.<*> (x Data..:? "provisioningRepository")
            Prelude.<*> (x Data..:? "spec")
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

instance Prelude.Hashable Environment where
  hashWithSalt _salt Environment' {..} =
    _salt `Prelude.hashWithSalt` codebuildRoleArn
      `Prelude.hashWithSalt` componentRoleArn
      `Prelude.hashWithSalt` deploymentStatusMessage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` environmentAccountId
      `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` provisioning
      `Prelude.hashWithSalt` provisioningRepository
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf codebuildRoleArn
      `Prelude.seq` Prelude.rnf componentRoleArn
      `Prelude.seq` Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId
      `Prelude.seq` Prelude.rnf environmentAccountId
      `Prelude.seq` Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf provisioning
      `Prelude.seq` Prelude.rnf provisioningRepository
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf templateName
