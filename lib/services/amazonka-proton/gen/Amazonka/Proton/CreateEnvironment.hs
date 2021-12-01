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
-- Module      : Amazonka.Proton.CreateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploy a new environment. An AWS Proton environment is created from an
-- environment template that defines infrastructure and resources that can
-- be shared across services. For more information, see the
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-environments.html Environments>
-- in the /AWS Proton Administrator Guide./
module Amazonka.Proton.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_protonServiceRoleArn,
    createEnvironment_environmentAccountConnectionId,
    createEnvironment_templateMinorVersion,
    createEnvironment_description,
    createEnvironment_tags,
    createEnvironment_name,
    createEnvironment_spec,
    createEnvironment_templateMajorVersion,
    createEnvironment_templateName,

    -- * Destructuring the Response
    CreateEnvironmentResponse (..),
    newCreateEnvironmentResponse,

    -- * Response Lenses
    createEnvironmentResponse_httpStatus,
    createEnvironmentResponse_environment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | The Amazon Resource Name (ARN) of the AWS Proton service role that
    -- allows AWS Proton to make calls to other services on your behalf. You
    -- must include either the @environmentAccountConnectionId@ or
    -- @protonServiceRoleArn@ parameter and value.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account connection that you provide if you\'re
    -- provisioning your environment infrastructure resources to an environment
    -- account. You must include either the @environmentAccountConnectionId@ or
    -- @protonServiceRoleArn@ parameter and value. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-env-account-connections.html Environment account connections>
    -- in the /AWS Proton Administrator guide/.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the minor version of the environment template.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the environment that\'s being created and deployed.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Create tags for your environment. For more information, see /AWS Proton
    -- resources and tagging/ in the
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/resources.html AWS Proton Administrator Guide>
    -- or
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html AWS Proton User Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the environment.
    name :: Prelude.Text,
    -- | A link to a YAML formatted spec file that provides inputs as defined in
    -- the environment template bundle schema file. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-environments.html Environments>
    -- in the /AWS Proton Administrator Guide/.
    spec :: Core.Sensitive Prelude.Text,
    -- | The ID of the major version of the environment template.
    templateMajorVersion :: Prelude.Text,
    -- | The name of the environment template. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-templates.html Environment Templates>
    -- in the /AWS Proton Administrator Guide/.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protonServiceRoleArn', 'createEnvironment_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make calls to other services on your behalf. You
-- must include either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and value.
--
-- 'environmentAccountConnectionId', 'createEnvironment_environmentAccountConnectionId' - The ID of the environment account connection that you provide if you\'re
-- provisioning your environment infrastructure resources to an environment
-- account. You must include either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and value. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-env-account-connections.html Environment account connections>
-- in the /AWS Proton Administrator guide/.
--
-- 'templateMinorVersion', 'createEnvironment_templateMinorVersion' - The ID of the minor version of the environment template.
--
-- 'description', 'createEnvironment_description' - A description of the environment that\'s being created and deployed.
--
-- 'tags', 'createEnvironment_tags' - Create tags for your environment. For more information, see /AWS Proton
-- resources and tagging/ in the
-- <https://docs.aws.amazon.com/proton/latest/adminguide/resources.html AWS Proton Administrator Guide>
-- or
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html AWS Proton User Guide>.
--
-- 'name', 'createEnvironment_name' - The name of the environment.
--
-- 'spec', 'createEnvironment_spec' - A link to a YAML formatted spec file that provides inputs as defined in
-- the environment template bundle schema file. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-environments.html Environments>
-- in the /AWS Proton Administrator Guide/.
--
-- 'templateMajorVersion', 'createEnvironment_templateMajorVersion' - The ID of the major version of the environment template.
--
-- 'templateName', 'createEnvironment_templateName' - The name of the environment template. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-templates.html Environment Templates>
-- in the /AWS Proton Administrator Guide/.
newCreateEnvironment ::
  -- | 'name'
  Prelude.Text ->
  -- | 'spec'
  Prelude.Text ->
  -- | 'templateMajorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment
  pName_
  pSpec_
  pTemplateMajorVersion_
  pTemplateName_ =
    CreateEnvironment'
      { protonServiceRoleArn =
          Prelude.Nothing,
        environmentAccountConnectionId = Prelude.Nothing,
        templateMinorVersion = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        spec = Core._Sensitive Lens.# pSpec_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateName = pTemplateName_
      }

-- | The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make calls to other services on your behalf. You
-- must include either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and value.
createEnvironment_protonServiceRoleArn :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_protonServiceRoleArn = Lens.lens (\CreateEnvironment' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@CreateEnvironment' {} a -> s {protonServiceRoleArn = a} :: CreateEnvironment)

-- | The ID of the environment account connection that you provide if you\'re
-- provisioning your environment infrastructure resources to an environment
-- account. You must include either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and value. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-env-account-connections.html Environment account connections>
-- in the /AWS Proton Administrator guide/.
createEnvironment_environmentAccountConnectionId :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_environmentAccountConnectionId = Lens.lens (\CreateEnvironment' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@CreateEnvironment' {} a -> s {environmentAccountConnectionId = a} :: CreateEnvironment)

-- | The ID of the minor version of the environment template.
createEnvironment_templateMinorVersion :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_templateMinorVersion = Lens.lens (\CreateEnvironment' {templateMinorVersion} -> templateMinorVersion) (\s@CreateEnvironment' {} a -> s {templateMinorVersion = a} :: CreateEnvironment)

-- | A description of the environment that\'s being created and deployed.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment) Prelude.. Lens.mapping Core._Sensitive

-- | Create tags for your environment. For more information, see /AWS Proton
-- resources and tagging/ in the
-- <https://docs.aws.amazon.com/proton/latest/adminguide/resources.html AWS Proton Administrator Guide>
-- or
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html AWS Proton User Guide>.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Tag])
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the environment.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

-- | A link to a YAML formatted spec file that provides inputs as defined in
-- the environment template bundle schema file. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-environments.html Environments>
-- in the /AWS Proton Administrator Guide/.
createEnvironment_spec :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_spec = Lens.lens (\CreateEnvironment' {spec} -> spec) (\s@CreateEnvironment' {} a -> s {spec = a} :: CreateEnvironment) Prelude.. Core._Sensitive

-- | The ID of the major version of the environment template.
createEnvironment_templateMajorVersion :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_templateMajorVersion = Lens.lens (\CreateEnvironment' {templateMajorVersion} -> templateMajorVersion) (\s@CreateEnvironment' {} a -> s {templateMajorVersion = a} :: CreateEnvironment)

-- | The name of the environment template. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-templates.html Environment Templates>
-- in the /AWS Proton Administrator Guide/.
createEnvironment_templateName :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_templateName = Lens.lens (\CreateEnvironment' {templateName} -> templateName) (\s@CreateEnvironment' {} a -> s {templateName = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "environment")
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt salt' CreateEnvironment' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` protonServiceRoleArn

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId

instance Core.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.CreateEnvironment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("protonServiceRoleArn" Core..=)
              Prelude.<$> protonServiceRoleArn,
            ("environmentAccountConnectionId" Core..=)
              Prelude.<$> environmentAccountConnectionId,
            ("templateMinorVersion" Core..=)
              Prelude.<$> templateMinorVersion,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("spec" Core..= spec),
            Prelude.Just
              ( "templateMajorVersion"
                  Core..= templateMajorVersion
              ),
            Prelude.Just ("templateName" Core..= templateName)
          ]
      )

instance Core.ToPath CreateEnvironment where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment detail data that\'s returned by AWS Proton.
    environment :: Environment
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'environment', 'createEnvironmentResponse_environment' - The environment detail data that\'s returned by AWS Proton.
newCreateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environment'
  Environment ->
  CreateEnvironmentResponse
newCreateEnvironmentResponse
  pHttpStatus_
  pEnvironment_ =
    CreateEnvironmentResponse'
      { httpStatus =
          pHttpStatus_,
        environment = pEnvironment_
      }

-- | The response's http status code.
createEnvironmentResponse_httpStatus :: Lens.Lens' CreateEnvironmentResponse Prelude.Int
createEnvironmentResponse_httpStatus = Lens.lens (\CreateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentResponse)

-- | The environment detail data that\'s returned by AWS Proton.
createEnvironmentResponse_environment :: Lens.Lens' CreateEnvironmentResponse Environment
createEnvironmentResponse_environment = Lens.lens (\CreateEnvironmentResponse' {environment} -> environment) (\s@CreateEnvironmentResponse' {} a -> s {environment = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environment
