{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Proton.UpdateEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an environment.
--
-- If the environment is associated with an environment account connection,
-- /don\'t/ update or include the @protonServiceRoleArn@ and
-- @provisioningRepository@ parameter to update or connect to an
-- environment account connection.
--
-- You can only update to a new environment account connection if that
-- connection was created in the same environment account that the current
-- environment account connection was created in. The account connection
-- must also be associated with the current environment.
--
-- If the environment /isn\'t/ associated with an environment account
-- connection, /don\'t/ update or include the
-- @environmentAccountConnectionId@ parameter. You /can\'t/ update or
-- connect the environment to an environment account connection if it
-- /isn\'t/ already associated with an environment connection.
--
-- You can update either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and value. You can’t update both.
--
-- If the environment was configured for Amazon Web Services-managed
-- provisioning, omit the @provisioningRepository@ parameter.
--
-- If the environment was configured for self-managed provisioning, specify
-- the @provisioningRepository@ parameter and omit the
-- @protonServiceRoleArn@ and @environmentAccountConnectionId@ parameters.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-environments.html Environments>
-- and
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-works-prov-methods.html Provisioning methods>
-- in the /Proton User Guide/.
--
-- There are four modes for updating an environment. The @deploymentType@
-- field defines the mode.
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the environment is deployed and updated with the new
--     spec that you provide. Only requested parameters are updated.
--     /Don’t/ include minor or major version parameters when you use this
--     @deployment-type@.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the environment is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can also specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the environment is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can also specify a different major
--     version that\'s higher than the major version in use and a minor
--     version.
module Amazonka.Proton.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_templateMajorVersion,
    updateEnvironment_codebuildRoleArn,
    updateEnvironment_provisioningRepository,
    updateEnvironment_description,
    updateEnvironment_templateMinorVersion,
    updateEnvironment_spec,
    updateEnvironment_protonServiceRoleArn,
    updateEnvironment_componentRoleArn,
    updateEnvironment_environmentAccountConnectionId,
    updateEnvironment_deploymentType,
    updateEnvironment_name,

    -- * Destructuring the Response
    UpdateEnvironmentResponse (..),
    newUpdateEnvironmentResponse,

    -- * Response Lenses
    updateEnvironmentResponse_httpStatus,
    updateEnvironmentResponse_environment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | The major version of the environment to update.
    templateMajorVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM service role that allows
    -- Proton to provision infrastructure using CodeBuild-based provisioning on
    -- your behalf.
    codebuildRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The linked repository that you use to host your rendered infrastructure
    -- templates for self-managed provisioning. A linked repository is a
    -- repository that has been registered with Proton. For more information,
    -- see CreateRepository.
    provisioningRepository :: Prelude.Maybe RepositoryBranchInput,
    -- | A description of the environment update.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The minor version of the environment to update.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The formatted specification that defines the update.
    spec :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the Proton service role that allows
    -- Proton to make API calls to other services your behalf.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
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
    -- | The ID of the environment account connection.
    --
    -- You can only update to a new environment account connection if it was
    -- created in the same environment account that the current environment
    -- account connection was created in and is associated with the current
    -- environment.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | There are four modes for updating an environment. The @deploymentType@
    -- field defines the mode.
    --
    -- []
    --     @NONE@
    --
    --     In this mode, a deployment /doesn\'t/ occur. Only the requested
    --     metadata parameters are updated.
    --
    -- []
    --     @CURRENT_VERSION@
    --
    --     In this mode, the environment is deployed and updated with the new
    --     spec that you provide. Only requested parameters are updated.
    --     /Don’t/ include major or minor version parameters when you use this
    --     @deployment-type@.
    --
    -- []
    --     @MINOR_VERSION@
    --
    --     In this mode, the environment is deployed and updated with the
    --     published, recommended (latest) minor version of the current major
    --     version in use, by default. You can also specify a different minor
    --     version of the current major version in use.
    --
    -- []
    --     @MAJOR_VERSION@
    --
    --     In this mode, the environment is deployed and updated with the
    --     published, recommended (latest) major and minor version of the
    --     current template, by default. You can also specify a different major
    --     version that is higher than the major version in use and a minor
    --     version (optional).
    deploymentType :: DeploymentUpdateType,
    -- | The name of the environment to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateMajorVersion', 'updateEnvironment_templateMajorVersion' - The major version of the environment to update.
--
-- 'codebuildRoleArn', 'updateEnvironment_codebuildRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that allows
-- Proton to provision infrastructure using CodeBuild-based provisioning on
-- your behalf.
--
-- 'provisioningRepository', 'updateEnvironment_provisioningRepository' - The linked repository that you use to host your rendered infrastructure
-- templates for self-managed provisioning. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
--
-- 'description', 'updateEnvironment_description' - A description of the environment update.
--
-- 'templateMinorVersion', 'updateEnvironment_templateMinorVersion' - The minor version of the environment to update.
--
-- 'spec', 'updateEnvironment_spec' - The formatted specification that defines the update.
--
-- 'protonServiceRoleArn', 'updateEnvironment_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make API calls to other services your behalf.
--
-- 'componentRoleArn', 'updateEnvironment_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
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
-- 'environmentAccountConnectionId', 'updateEnvironment_environmentAccountConnectionId' - The ID of the environment account connection.
--
-- You can only update to a new environment account connection if it was
-- created in the same environment account that the current environment
-- account connection was created in and is associated with the current
-- environment.
--
-- 'deploymentType', 'updateEnvironment_deploymentType' - There are four modes for updating an environment. The @deploymentType@
-- field defines the mode.
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the environment is deployed and updated with the new
--     spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     @deployment-type@.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the environment is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can also specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the environment is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can also specify a different major
--     version that is higher than the major version in use and a minor
--     version (optional).
--
-- 'name', 'updateEnvironment_name' - The name of the environment to update.
newUpdateEnvironment ::
  -- | 'deploymentType'
  DeploymentUpdateType ->
  -- | 'name'
  Prelude.Text ->
  UpdateEnvironment
newUpdateEnvironment pDeploymentType_ pName_ =
  UpdateEnvironment'
    { templateMajorVersion =
        Prelude.Nothing,
      codebuildRoleArn = Prelude.Nothing,
      provisioningRepository = Prelude.Nothing,
      description = Prelude.Nothing,
      templateMinorVersion = Prelude.Nothing,
      spec = Prelude.Nothing,
      protonServiceRoleArn = Prelude.Nothing,
      componentRoleArn = Prelude.Nothing,
      environmentAccountConnectionId = Prelude.Nothing,
      deploymentType = pDeploymentType_,
      name = pName_
    }

-- | The major version of the environment to update.
updateEnvironment_templateMajorVersion :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_templateMajorVersion = Lens.lens (\UpdateEnvironment' {templateMajorVersion} -> templateMajorVersion) (\s@UpdateEnvironment' {} a -> s {templateMajorVersion = a} :: UpdateEnvironment)

-- | The Amazon Resource Name (ARN) of the IAM service role that allows
-- Proton to provision infrastructure using CodeBuild-based provisioning on
-- your behalf.
updateEnvironment_codebuildRoleArn :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_codebuildRoleArn = Lens.lens (\UpdateEnvironment' {codebuildRoleArn} -> codebuildRoleArn) (\s@UpdateEnvironment' {} a -> s {codebuildRoleArn = a} :: UpdateEnvironment)

-- | The linked repository that you use to host your rendered infrastructure
-- templates for self-managed provisioning. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
updateEnvironment_provisioningRepository :: Lens.Lens' UpdateEnvironment (Prelude.Maybe RepositoryBranchInput)
updateEnvironment_provisioningRepository = Lens.lens (\UpdateEnvironment' {provisioningRepository} -> provisioningRepository) (\s@UpdateEnvironment' {} a -> s {provisioningRepository = a} :: UpdateEnvironment)

-- | A description of the environment update.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment) Prelude.. Lens.mapping Core._Sensitive

-- | The minor version of the environment to update.
updateEnvironment_templateMinorVersion :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_templateMinorVersion = Lens.lens (\UpdateEnvironment' {templateMinorVersion} -> templateMinorVersion) (\s@UpdateEnvironment' {} a -> s {templateMinorVersion = a} :: UpdateEnvironment)

-- | The formatted specification that defines the update.
updateEnvironment_spec :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_spec = Lens.lens (\UpdateEnvironment' {spec} -> spec) (\s@UpdateEnvironment' {} a -> s {spec = a} :: UpdateEnvironment) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make API calls to other services your behalf.
updateEnvironment_protonServiceRoleArn :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_protonServiceRoleArn = Lens.lens (\UpdateEnvironment' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@UpdateEnvironment' {} a -> s {protonServiceRoleArn = a} :: UpdateEnvironment)

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
updateEnvironment_componentRoleArn :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_componentRoleArn = Lens.lens (\UpdateEnvironment' {componentRoleArn} -> componentRoleArn) (\s@UpdateEnvironment' {} a -> s {componentRoleArn = a} :: UpdateEnvironment)

-- | The ID of the environment account connection.
--
-- You can only update to a new environment account connection if it was
-- created in the same environment account that the current environment
-- account connection was created in and is associated with the current
-- environment.
updateEnvironment_environmentAccountConnectionId :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_environmentAccountConnectionId = Lens.lens (\UpdateEnvironment' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@UpdateEnvironment' {} a -> s {environmentAccountConnectionId = a} :: UpdateEnvironment)

-- | There are four modes for updating an environment. The @deploymentType@
-- field defines the mode.
--
-- []
--     @NONE@
--
--     In this mode, a deployment /doesn\'t/ occur. Only the requested
--     metadata parameters are updated.
--
-- []
--     @CURRENT_VERSION@
--
--     In this mode, the environment is deployed and updated with the new
--     spec that you provide. Only requested parameters are updated.
--     /Don’t/ include major or minor version parameters when you use this
--     @deployment-type@.
--
-- []
--     @MINOR_VERSION@
--
--     In this mode, the environment is deployed and updated with the
--     published, recommended (latest) minor version of the current major
--     version in use, by default. You can also specify a different minor
--     version of the current major version in use.
--
-- []
--     @MAJOR_VERSION@
--
--     In this mode, the environment is deployed and updated with the
--     published, recommended (latest) major and minor version of the
--     current template, by default. You can also specify a different major
--     version that is higher than the major version in use and a minor
--     version (optional).
updateEnvironment_deploymentType :: Lens.Lens' UpdateEnvironment DeploymentUpdateType
updateEnvironment_deploymentType = Lens.lens (\UpdateEnvironment' {deploymentType} -> deploymentType) (\s@UpdateEnvironment' {} a -> s {deploymentType = a} :: UpdateEnvironment)

-- | The name of the environment to update.
updateEnvironment_name :: Lens.Lens' UpdateEnvironment Prelude.Text
updateEnvironment_name = Lens.lens (\UpdateEnvironment' {name} -> name) (\s@UpdateEnvironment' {} a -> s {name = a} :: UpdateEnvironment)

instance Core.AWSRequest UpdateEnvironment where
  type
    AWSResponse UpdateEnvironment =
      UpdateEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "environment")
      )

instance Prelude.Hashable UpdateEnvironment where
  hashWithSalt _salt UpdateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` codebuildRoleArn
      `Prelude.hashWithSalt` provisioningRepository
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` componentRoleArn
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateEnvironment where
  rnf UpdateEnvironment' {..} =
    Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf codebuildRoleArn
      `Prelude.seq` Prelude.rnf provisioningRepository
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf componentRoleArn
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId
      `Prelude.seq` Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.UpdateEnvironment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("templateMajorVersion" Core..=)
              Prelude.<$> templateMajorVersion,
            ("codebuildRoleArn" Core..=)
              Prelude.<$> codebuildRoleArn,
            ("provisioningRepository" Core..=)
              Prelude.<$> provisioningRepository,
            ("description" Core..=) Prelude.<$> description,
            ("templateMinorVersion" Core..=)
              Prelude.<$> templateMinorVersion,
            ("spec" Core..=) Prelude.<$> spec,
            ("protonServiceRoleArn" Core..=)
              Prelude.<$> protonServiceRoleArn,
            ("componentRoleArn" Core..=)
              Prelude.<$> componentRoleArn,
            ("environmentAccountConnectionId" Core..=)
              Prelude.<$> environmentAccountConnectionId,
            Prelude.Just
              ("deploymentType" Core..= deploymentType),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath UpdateEnvironment where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentResponse' smart constructor.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment detail data that\'s returned by Proton.
    environment :: Environment
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'environment', 'updateEnvironmentResponse_environment' - The environment detail data that\'s returned by Proton.
newUpdateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environment'
  Environment ->
  UpdateEnvironmentResponse
newUpdateEnvironmentResponse
  pHttpStatus_
  pEnvironment_ =
    UpdateEnvironmentResponse'
      { httpStatus =
          pHttpStatus_,
        environment = pEnvironment_
      }

-- | The response's http status code.
updateEnvironmentResponse_httpStatus :: Lens.Lens' UpdateEnvironmentResponse Prelude.Int
updateEnvironmentResponse_httpStatus = Lens.lens (\UpdateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentResponse)

-- | The environment detail data that\'s returned by Proton.
updateEnvironmentResponse_environment :: Lens.Lens' UpdateEnvironmentResponse Environment
updateEnvironmentResponse_environment = Lens.lens (\UpdateEnvironmentResponse' {environment} -> environment) (\s@UpdateEnvironmentResponse' {} a -> s {environment = a} :: UpdateEnvironmentResponse)

instance Prelude.NFData UpdateEnvironmentResponse where
  rnf UpdateEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environment
