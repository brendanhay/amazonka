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
-- Module      : Amazonka.Proton.CreateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an Proton service. An Proton service is an instantiation of a
-- service template and often includes several service instances and
-- pipeline. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-services.html Services>
-- in the /Proton User Guide/.
module Amazonka.Proton.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_branchName,
    createService_description,
    createService_repositoryConnectionArn,
    createService_repositoryId,
    createService_tags,
    createService_templateMinorVersion,
    createService_name,
    createService_spec,
    createService_templateMajorVersion,
    createService_templateName,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_httpStatus,
    createServiceResponse_service,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | The name of the code repository branch that holds the code that\'s
    -- deployed in Proton. /Don\'t/ include this parameter if your service
    -- template /doesn\'t/ include a service pipeline.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | A description of the Proton service.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the repository connection. For more
    -- information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html#setting-up-vcontrol Setting up an AWS CodeStar connection>
    -- in the /Proton User Guide/. /Don\'t/ include this parameter if your
    -- service template /doesn\'t/ include a service pipeline.
    repositoryConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the code repository. /Don\'t/ include this parameter if your
    -- service template /doesn\'t/ include a service pipeline.
    repositoryId :: Prelude.Maybe Prelude.Text,
    -- | An optional list of metadata items that you can associate with the
    -- Proton service. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The minor version of the service template that was used to create the
    -- service.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The service name.
    name :: Prelude.Text,
    -- | A link to a spec file that provides inputs as defined in the service
    -- template bundle schema file. The spec file is in YAML format. /Don’t/
    -- include pipeline inputs in the spec if your service template /doesn’t/
    -- include a service pipeline. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-create-svc.html Create a service>
    -- in the /Proton User Guide/.
    spec :: Data.Sensitive Prelude.Text,
    -- | The major version of the service template that was used to create the
    -- service.
    templateMajorVersion :: Prelude.Text,
    -- | The name of the service template that\'s used to create the service.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'createService_branchName' - The name of the code repository branch that holds the code that\'s
-- deployed in Proton. /Don\'t/ include this parameter if your service
-- template /doesn\'t/ include a service pipeline.
--
-- 'description', 'createService_description' - A description of the Proton service.
--
-- 'repositoryConnectionArn', 'createService_repositoryConnectionArn' - The Amazon Resource Name (ARN) of the repository connection. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html#setting-up-vcontrol Setting up an AWS CodeStar connection>
-- in the /Proton User Guide/. /Don\'t/ include this parameter if your
-- service template /doesn\'t/ include a service pipeline.
--
-- 'repositoryId', 'createService_repositoryId' - The ID of the code repository. /Don\'t/ include this parameter if your
-- service template /doesn\'t/ include a service pipeline.
--
-- 'tags', 'createService_tags' - An optional list of metadata items that you can associate with the
-- Proton service. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'templateMinorVersion', 'createService_templateMinorVersion' - The minor version of the service template that was used to create the
-- service.
--
-- 'name', 'createService_name' - The service name.
--
-- 'spec', 'createService_spec' - A link to a spec file that provides inputs as defined in the service
-- template bundle schema file. The spec file is in YAML format. /Don’t/
-- include pipeline inputs in the spec if your service template /doesn’t/
-- include a service pipeline. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-create-svc.html Create a service>
-- in the /Proton User Guide/.
--
-- 'templateMajorVersion', 'createService_templateMajorVersion' - The major version of the service template that was used to create the
-- service.
--
-- 'templateName', 'createService_templateName' - The name of the service template that\'s used to create the service.
newCreateService ::
  -- | 'name'
  Prelude.Text ->
  -- | 'spec'
  Prelude.Text ->
  -- | 'templateMajorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  CreateService
newCreateService
  pName_
  pSpec_
  pTemplateMajorVersion_
  pTemplateName_ =
    CreateService'
      { branchName = Prelude.Nothing,
        description = Prelude.Nothing,
        repositoryConnectionArn = Prelude.Nothing,
        repositoryId = Prelude.Nothing,
        tags = Prelude.Nothing,
        templateMinorVersion = Prelude.Nothing,
        name = pName_,
        spec = Data._Sensitive Lens.# pSpec_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateName = pTemplateName_
      }

-- | The name of the code repository branch that holds the code that\'s
-- deployed in Proton. /Don\'t/ include this parameter if your service
-- template /doesn\'t/ include a service pipeline.
createService_branchName :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_branchName = Lens.lens (\CreateService' {branchName} -> branchName) (\s@CreateService' {} a -> s {branchName = a} :: CreateService)

-- | A description of the Proton service.
createService_description :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_description = Lens.lens (\CreateService' {description} -> description) (\s@CreateService' {} a -> s {description = a} :: CreateService) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the repository connection. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html#setting-up-vcontrol Setting up an AWS CodeStar connection>
-- in the /Proton User Guide/. /Don\'t/ include this parameter if your
-- service template /doesn\'t/ include a service pipeline.
createService_repositoryConnectionArn :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_repositoryConnectionArn = Lens.lens (\CreateService' {repositoryConnectionArn} -> repositoryConnectionArn) (\s@CreateService' {} a -> s {repositoryConnectionArn = a} :: CreateService)

-- | The ID of the code repository. /Don\'t/ include this parameter if your
-- service template /doesn\'t/ include a service pipeline.
createService_repositoryId :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_repositoryId = Lens.lens (\CreateService' {repositoryId} -> repositoryId) (\s@CreateService' {} a -> s {repositoryId = a} :: CreateService)

-- | An optional list of metadata items that you can associate with the
-- Proton service. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe [Tag])
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The minor version of the service template that was used to create the
-- service.
createService_templateMinorVersion :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_templateMinorVersion = Lens.lens (\CreateService' {templateMinorVersion} -> templateMinorVersion) (\s@CreateService' {} a -> s {templateMinorVersion = a} :: CreateService)

-- | The service name.
createService_name :: Lens.Lens' CreateService Prelude.Text
createService_name = Lens.lens (\CreateService' {name} -> name) (\s@CreateService' {} a -> s {name = a} :: CreateService)

-- | A link to a spec file that provides inputs as defined in the service
-- template bundle schema file. The spec file is in YAML format. /Don’t/
-- include pipeline inputs in the spec if your service template /doesn’t/
-- include a service pipeline. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-create-svc.html Create a service>
-- in the /Proton User Guide/.
createService_spec :: Lens.Lens' CreateService Prelude.Text
createService_spec = Lens.lens (\CreateService' {spec} -> spec) (\s@CreateService' {} a -> s {spec = a} :: CreateService) Prelude.. Data._Sensitive

-- | The major version of the service template that was used to create the
-- service.
createService_templateMajorVersion :: Lens.Lens' CreateService Prelude.Text
createService_templateMajorVersion = Lens.lens (\CreateService' {templateMajorVersion} -> templateMajorVersion) (\s@CreateService' {} a -> s {templateMajorVersion = a} :: CreateService)

-- | The name of the service template that\'s used to create the service.
createService_templateName :: Lens.Lens' CreateService Prelude.Text
createService_templateName = Lens.lens (\CreateService' {templateName} -> templateName) (\s@CreateService' {} a -> s {templateName = a} :: CreateService)

instance Core.AWSRequest CreateService where
  type
    AWSResponse CreateService =
      CreateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "service")
      )

instance Prelude.Hashable CreateService where
  hashWithSalt _salt CreateService' {..} =
    _salt `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` repositoryConnectionArn
      `Prelude.hashWithSalt` repositoryId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData CreateService where
  rnf CreateService' {..} =
    Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf repositoryConnectionArn
      `Prelude.seq` Prelude.rnf repositoryId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateService where
  toJSON CreateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("branchName" Data..=) Prelude.<$> branchName,
            ("description" Data..=) Prelude.<$> description,
            ("repositoryConnectionArn" Data..=)
              Prelude.<$> repositoryConnectionArn,
            ("repositoryId" Data..=) Prelude.<$> repositoryId,
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

instance Data.ToPath CreateService where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service detail data that\'s returned by Proton.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createServiceResponse_httpStatus' - The response's http status code.
--
-- 'service', 'createServiceResponse_service' - The service detail data that\'s returned by Proton.
newCreateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'service'
  Service ->
  CreateServiceResponse
newCreateServiceResponse pHttpStatus_ pService_ =
  CreateServiceResponse'
    { httpStatus = pHttpStatus_,
      service = pService_
    }

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

-- | The service detail data that\'s returned by Proton.
createServiceResponse_service :: Lens.Lens' CreateServiceResponse Service
createServiceResponse_service = Lens.lens (\CreateServiceResponse' {service} -> service) (\s@CreateServiceResponse' {} a -> s {service = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse where
  rnf CreateServiceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf service
