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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an environment.
--
-- If the environment is associated with an environment account connection,
-- /don\'t/ update or include the @protonServiceRoleArn@ parameter to
-- update or connect to an environment account connection.
--
-- You can only update to a new environment account connection if it was
-- created in the same environment account that the current environment
-- account connection was created in and is associated with the current
-- environment.
--
-- If the environment /isn\'t/ associated with an environment account
-- connection, /don\'t/ update or include the
-- @environmentAccountConnectionId@ parameter to update or connect to an
-- environment account connection.
--
-- You can update either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and value. You can’t update both.
--
-- There are four modes for updating an environment as described in the
-- following. The @deploymentType@ field defines the mode.
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
--     version (optional).
module Amazonka.Proton.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_protonServiceRoleArn,
    updateEnvironment_environmentAccountConnectionId,
    updateEnvironment_spec,
    updateEnvironment_templateMinorVersion,
    updateEnvironment_description,
    updateEnvironment_templateMajorVersion,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | The Amazon Resource Name (ARN) of the AWS Proton service role that
    -- allows AWS Proton to make API calls to other services your behalf.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account connection.
    --
    -- You can only update to a new environment account connection if it was
    -- created in the same environment account that the current environment
    -- account connection was created in and is associated with the current
    -- environment.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The formatted specification that defines the update.
    spec :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the minor version of the environment to update.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the environment update.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the major version of the environment to update.
    templateMajorVersion :: Prelude.Maybe Prelude.Text,
    -- | There are four modes for updating an environment as described in the
    -- following. The @deploymentType@ field defines the mode.
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
-- 'protonServiceRoleArn', 'updateEnvironment_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make API calls to other services your behalf.
--
-- 'environmentAccountConnectionId', 'updateEnvironment_environmentAccountConnectionId' - The ID of the environment account connection.
--
-- You can only update to a new environment account connection if it was
-- created in the same environment account that the current environment
-- account connection was created in and is associated with the current
-- environment.
--
-- 'spec', 'updateEnvironment_spec' - The formatted specification that defines the update.
--
-- 'templateMinorVersion', 'updateEnvironment_templateMinorVersion' - The ID of the minor version of the environment to update.
--
-- 'description', 'updateEnvironment_description' - A description of the environment update.
--
-- 'templateMajorVersion', 'updateEnvironment_templateMajorVersion' - The ID of the major version of the environment to update.
--
-- 'deploymentType', 'updateEnvironment_deploymentType' - There are four modes for updating an environment as described in the
-- following. The @deploymentType@ field defines the mode.
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
    { protonServiceRoleArn =
        Prelude.Nothing,
      environmentAccountConnectionId = Prelude.Nothing,
      spec = Prelude.Nothing,
      templateMinorVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      templateMajorVersion = Prelude.Nothing,
      deploymentType = pDeploymentType_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make API calls to other services your behalf.
updateEnvironment_protonServiceRoleArn :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_protonServiceRoleArn = Lens.lens (\UpdateEnvironment' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@UpdateEnvironment' {} a -> s {protonServiceRoleArn = a} :: UpdateEnvironment)

-- | The ID of the environment account connection.
--
-- You can only update to a new environment account connection if it was
-- created in the same environment account that the current environment
-- account connection was created in and is associated with the current
-- environment.
updateEnvironment_environmentAccountConnectionId :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_environmentAccountConnectionId = Lens.lens (\UpdateEnvironment' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@UpdateEnvironment' {} a -> s {environmentAccountConnectionId = a} :: UpdateEnvironment)

-- | The formatted specification that defines the update.
updateEnvironment_spec :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_spec = Lens.lens (\UpdateEnvironment' {spec} -> spec) (\s@UpdateEnvironment' {} a -> s {spec = a} :: UpdateEnvironment) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the minor version of the environment to update.
updateEnvironment_templateMinorVersion :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_templateMinorVersion = Lens.lens (\UpdateEnvironment' {templateMinorVersion} -> templateMinorVersion) (\s@UpdateEnvironment' {} a -> s {templateMinorVersion = a} :: UpdateEnvironment)

-- | A description of the environment update.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the major version of the environment to update.
updateEnvironment_templateMajorVersion :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_templateMajorVersion = Lens.lens (\UpdateEnvironment' {templateMajorVersion} -> templateMajorVersion) (\s@UpdateEnvironment' {} a -> s {templateMajorVersion = a} :: UpdateEnvironment)

-- | There are four modes for updating an environment as described in the
-- following. The @deploymentType@ field defines the mode.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "environment")
      )

instance Prelude.Hashable UpdateEnvironment where
  hashWithSalt _salt UpdateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateEnvironment where
  rnf UpdateEnvironment' {..} =
    Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf templateMajorVersion
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
          [ ("protonServiceRoleArn" Core..=)
              Prelude.<$> protonServiceRoleArn,
            ("environmentAccountConnectionId" Core..=)
              Prelude.<$> environmentAccountConnectionId,
            ("spec" Core..=) Prelude.<$> spec,
            ("templateMinorVersion" Core..=)
              Prelude.<$> templateMinorVersion,
            ("description" Core..=) Prelude.<$> description,
            ("templateMajorVersion" Core..=)
              Prelude.<$> templateMajorVersion,
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
    -- | The environment detail data that\'s returned by AWS Proton.
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
-- 'environment', 'updateEnvironmentResponse_environment' - The environment detail data that\'s returned by AWS Proton.
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

-- | The environment detail data that\'s returned by AWS Proton.
updateEnvironmentResponse_environment :: Lens.Lens' UpdateEnvironmentResponse Environment
updateEnvironmentResponse_environment = Lens.lens (\UpdateEnvironmentResponse' {environment} -> environment) (\s@UpdateEnvironmentResponse' {} a -> s {environment = a} :: UpdateEnvironmentResponse)

instance Prelude.NFData UpdateEnvironmentResponse where
  rnf UpdateEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environment
