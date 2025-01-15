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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploy a new environment. An Proton environment is created from an
-- environment template that defines infrastructure and resources that can
-- be shared across services.
--
-- __You can provision environments using the following methods:__
--
-- -   Amazon Web Services-managed provisioning: Proton makes direct calls
--     to provision your resources.
--
-- -   Self-managed provisioning: Proton makes pull requests on your
--     repository to provide compiled infrastructure as code (IaC) files
--     that your IaC engine uses to provision resources.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-environments.html Environments>
-- and
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-works-prov-methods.html Provisioning methods>
-- in the /Proton User Guide/.
module Amazonka.Proton.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_codebuildRoleArn,
    createEnvironment_componentRoleArn,
    createEnvironment_description,
    createEnvironment_environmentAccountConnectionId,
    createEnvironment_protonServiceRoleArn,
    createEnvironment_provisioningRepository,
    createEnvironment_tags,
    createEnvironment_templateMinorVersion,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | The Amazon Resource Name (ARN) of the IAM service role that allows
    -- Proton to provision infrastructure using CodeBuild-based provisioning on
    -- your behalf.
    --
    -- To use CodeBuild-based provisioning for the environment or for any
    -- service instance running in the environment, specify either the
    -- @environmentAccountConnectionId@ or @codebuildRoleArn@ parameter.
    codebuildRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
    -- when provisioning directly defined components in this environment. It
    -- determines the scope of infrastructure that a component can provision.
    --
    -- You must specify @componentRoleArn@ to allow directly defined components
    -- to be associated with this environment.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    componentRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the environment that\'s being created and deployed.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the environment account connection that you provide if you\'re
    -- provisioning your environment infrastructure resources to an environment
    -- account. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
    -- in the /Proton User guide/.
    --
    -- To use Amazon Web Services-managed provisioning for the environment,
    -- specify either the @environmentAccountConnectionId@ or
    -- @protonServiceRoleArn@ parameter and omit the @provisioningRepository@
    -- parameter.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Proton service role that allows
    -- Proton to make calls to other services on your behalf.
    --
    -- To use Amazon Web Services-managed provisioning for the environment,
    -- specify either the @environmentAccountConnectionId@ or
    -- @protonServiceRoleArn@ parameter and omit the @provisioningRepository@
    -- parameter.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The linked repository that you use to host your rendered infrastructure
    -- templates for self-managed provisioning. A linked repository is a
    -- repository that has been registered with Proton. For more information,
    -- see CreateRepository.
    --
    -- To use self-managed provisioning for the environment, specify this
    -- parameter and omit the @environmentAccountConnectionId@ and
    -- @protonServiceRoleArn@ parameters.
    provisioningRepository :: Prelude.Maybe RepositoryBranchInput,
    -- | An optional list of metadata items that you can associate with the
    -- Proton environment. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The minor version of the environment template.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment.
    name :: Prelude.Text,
    -- | A YAML formatted string that provides inputs as defined in the
    -- environment template bundle schema file. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-environments.html Environments>
    -- in the /Proton User Guide/.
    spec :: Data.Sensitive Prelude.Text,
    -- | The major version of the environment template.
    templateMajorVersion :: Prelude.Text,
    -- | The name of the environment template. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-templates.html Environment Templates>
    -- in the /Proton User Guide/.
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
-- 'codebuildRoleArn', 'createEnvironment_codebuildRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that allows
-- Proton to provision infrastructure using CodeBuild-based provisioning on
-- your behalf.
--
-- To use CodeBuild-based provisioning for the environment or for any
-- service instance running in the environment, specify either the
-- @environmentAccountConnectionId@ or @codebuildRoleArn@ parameter.
--
-- 'componentRoleArn', 'createEnvironment_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in this environment. It
-- determines the scope of infrastructure that a component can provision.
--
-- You must specify @componentRoleArn@ to allow directly defined components
-- to be associated with this environment.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'description', 'createEnvironment_description' - A description of the environment that\'s being created and deployed.
--
-- 'environmentAccountConnectionId', 'createEnvironment_environmentAccountConnectionId' - The ID of the environment account connection that you provide if you\'re
-- provisioning your environment infrastructure resources to an environment
-- account. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
--
-- To use Amazon Web Services-managed provisioning for the environment,
-- specify either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and omit the @provisioningRepository@
-- parameter.
--
-- 'protonServiceRoleArn', 'createEnvironment_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make calls to other services on your behalf.
--
-- To use Amazon Web Services-managed provisioning for the environment,
-- specify either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and omit the @provisioningRepository@
-- parameter.
--
-- 'provisioningRepository', 'createEnvironment_provisioningRepository' - The linked repository that you use to host your rendered infrastructure
-- templates for self-managed provisioning. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
--
-- To use self-managed provisioning for the environment, specify this
-- parameter and omit the @environmentAccountConnectionId@ and
-- @protonServiceRoleArn@ parameters.
--
-- 'tags', 'createEnvironment_tags' - An optional list of metadata items that you can associate with the
-- Proton environment. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'templateMinorVersion', 'createEnvironment_templateMinorVersion' - The minor version of the environment template.
--
-- 'name', 'createEnvironment_name' - The name of the environment.
--
-- 'spec', 'createEnvironment_spec' - A YAML formatted string that provides inputs as defined in the
-- environment template bundle schema file. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-environments.html Environments>
-- in the /Proton User Guide/.
--
-- 'templateMajorVersion', 'createEnvironment_templateMajorVersion' - The major version of the environment template.
--
-- 'templateName', 'createEnvironment_templateName' - The name of the environment template. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-templates.html Environment Templates>
-- in the /Proton User Guide/.
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
      { codebuildRoleArn =
          Prelude.Nothing,
        componentRoleArn = Prelude.Nothing,
        description = Prelude.Nothing,
        environmentAccountConnectionId = Prelude.Nothing,
        protonServiceRoleArn = Prelude.Nothing,
        provisioningRepository = Prelude.Nothing,
        tags = Prelude.Nothing,
        templateMinorVersion = Prelude.Nothing,
        name = pName_,
        spec = Data._Sensitive Lens.# pSpec_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateName = pTemplateName_
      }

-- | The Amazon Resource Name (ARN) of the IAM service role that allows
-- Proton to provision infrastructure using CodeBuild-based provisioning on
-- your behalf.
--
-- To use CodeBuild-based provisioning for the environment or for any
-- service instance running in the environment, specify either the
-- @environmentAccountConnectionId@ or @codebuildRoleArn@ parameter.
createEnvironment_codebuildRoleArn :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_codebuildRoleArn = Lens.lens (\CreateEnvironment' {codebuildRoleArn} -> codebuildRoleArn) (\s@CreateEnvironment' {} a -> s {codebuildRoleArn = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in this environment. It
-- determines the scope of infrastructure that a component can provision.
--
-- You must specify @componentRoleArn@ to allow directly defined components
-- to be associated with this environment.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
createEnvironment_componentRoleArn :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_componentRoleArn = Lens.lens (\CreateEnvironment' {componentRoleArn} -> componentRoleArn) (\s@CreateEnvironment' {} a -> s {componentRoleArn = a} :: CreateEnvironment)

-- | A description of the environment that\'s being created and deployed.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the environment account connection that you provide if you\'re
-- provisioning your environment infrastructure resources to an environment
-- account. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
--
-- To use Amazon Web Services-managed provisioning for the environment,
-- specify either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and omit the @provisioningRepository@
-- parameter.
createEnvironment_environmentAccountConnectionId :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_environmentAccountConnectionId = Lens.lens (\CreateEnvironment' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@CreateEnvironment' {} a -> s {environmentAccountConnectionId = a} :: CreateEnvironment)

-- | The Amazon Resource Name (ARN) of the Proton service role that allows
-- Proton to make calls to other services on your behalf.
--
-- To use Amazon Web Services-managed provisioning for the environment,
-- specify either the @environmentAccountConnectionId@ or
-- @protonServiceRoleArn@ parameter and omit the @provisioningRepository@
-- parameter.
createEnvironment_protonServiceRoleArn :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_protonServiceRoleArn = Lens.lens (\CreateEnvironment' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@CreateEnvironment' {} a -> s {protonServiceRoleArn = a} :: CreateEnvironment)

-- | The linked repository that you use to host your rendered infrastructure
-- templates for self-managed provisioning. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
--
-- To use self-managed provisioning for the environment, specify this
-- parameter and omit the @environmentAccountConnectionId@ and
-- @protonServiceRoleArn@ parameters.
createEnvironment_provisioningRepository :: Lens.Lens' CreateEnvironment (Prelude.Maybe RepositoryBranchInput)
createEnvironment_provisioningRepository = Lens.lens (\CreateEnvironment' {provisioningRepository} -> provisioningRepository) (\s@CreateEnvironment' {} a -> s {provisioningRepository = a} :: CreateEnvironment)

-- | An optional list of metadata items that you can associate with the
-- Proton environment. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Tag])
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The minor version of the environment template.
createEnvironment_templateMinorVersion :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_templateMinorVersion = Lens.lens (\CreateEnvironment' {templateMinorVersion} -> templateMinorVersion) (\s@CreateEnvironment' {} a -> s {templateMinorVersion = a} :: CreateEnvironment)

-- | The name of the environment.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

-- | A YAML formatted string that provides inputs as defined in the
-- environment template bundle schema file. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-environments.html Environments>
-- in the /Proton User Guide/.
createEnvironment_spec :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_spec = Lens.lens (\CreateEnvironment' {spec} -> spec) (\s@CreateEnvironment' {} a -> s {spec = a} :: CreateEnvironment) Prelude.. Data._Sensitive

-- | The major version of the environment template.
createEnvironment_templateMajorVersion :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_templateMajorVersion = Lens.lens (\CreateEnvironment' {templateMajorVersion} -> templateMajorVersion) (\s@CreateEnvironment' {} a -> s {templateMajorVersion = a} :: CreateEnvironment)

-- | The name of the environment template. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-templates.html Environment Templates>
-- in the /Proton User Guide/.
createEnvironment_templateName :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_templateName = Lens.lens (\CreateEnvironment' {templateName} -> templateName) (\s@CreateEnvironment' {} a -> s {templateName = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environment")
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` codebuildRoleArn
      `Prelude.hashWithSalt` componentRoleArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` provisioningRepository
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf codebuildRoleArn `Prelude.seq`
      Prelude.rnf componentRoleArn `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf environmentAccountConnectionId `Prelude.seq`
            Prelude.rnf protonServiceRoleArn `Prelude.seq`
              Prelude.rnf provisioningRepository `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf templateMinorVersion `Prelude.seq`
                    Prelude.rnf name `Prelude.seq`
                      Prelude.rnf spec `Prelude.seq`
                        Prelude.rnf templateMajorVersion `Prelude.seq`
                          Prelude.rnf templateName

instance Data.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateEnvironment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("codebuildRoleArn" Data..=)
              Prelude.<$> codebuildRoleArn,
            ("componentRoleArn" Data..=)
              Prelude.<$> componentRoleArn,
            ("description" Data..=) Prelude.<$> description,
            ("environmentAccountConnectionId" Data..=)
              Prelude.<$> environmentAccountConnectionId,
            ("protonServiceRoleArn" Data..=)
              Prelude.<$> protonServiceRoleArn,
            ("provisioningRepository" Data..=)
              Prelude.<$> provisioningRepository,
            ("tags" Data..=) Prelude.<$> tags,
            ("templateMinorVersion" Data..=)
              Prelude.<$> templateMinorVersion,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("spec" Data..= spec),
            Prelude.Just
              ( "templateMajorVersion"
                  Data..= templateMajorVersion
              ),
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )

instance Data.ToPath CreateEnvironment where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment detail data that\'s returned by Proton.
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
-- 'environment', 'createEnvironmentResponse_environment' - The environment detail data that\'s returned by Proton.
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

-- | The environment detail data that\'s returned by Proton.
createEnvironmentResponse_environment :: Lens.Lens' CreateEnvironmentResponse Environment
createEnvironmentResponse_environment = Lens.lens (\CreateEnvironmentResponse' {environment} -> environment) (\s@CreateEnvironmentResponse' {} a -> s {environment = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf environment
