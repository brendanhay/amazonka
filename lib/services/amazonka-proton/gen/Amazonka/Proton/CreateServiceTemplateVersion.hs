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
-- Module      : Amazonka.Proton.CreateServiceTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new major or minor version of a service template. A major
-- version of a service template is a version that /isn\'t/ backwards
-- compatible. A minor version of a service template is a version that\'s
-- backwards compatible within its major version.
module Amazonka.Proton.CreateServiceTemplateVersion
  ( -- * Creating a Request
    CreateServiceTemplateVersion (..),
    newCreateServiceTemplateVersion,

    -- * Request Lenses
    createServiceTemplateVersion_clientToken,
    createServiceTemplateVersion_majorVersion,
    createServiceTemplateVersion_description,
    createServiceTemplateVersion_tags,
    createServiceTemplateVersion_compatibleEnvironmentTemplates,
    createServiceTemplateVersion_source,
    createServiceTemplateVersion_templateName,

    -- * Destructuring the Response
    CreateServiceTemplateVersionResponse (..),
    newCreateServiceTemplateVersionResponse,

    -- * Response Lenses
    createServiceTemplateVersionResponse_httpStatus,
    createServiceTemplateVersionResponse_serviceTemplateVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceTemplateVersion' smart constructor.
data CreateServiceTemplateVersion = CreateServiceTemplateVersion'
  { -- | When included, if two identicial requests are made with the same client
    -- token, AWS Proton returns the service template version that the first
    -- request created.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | To create a new minor version of the service template, include a
    -- @majorVersion@.
    --
    -- To create a new major and minor version of the service template,
    -- /exclude/ @majorVersion@.
    majorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the new version of a service template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Create tags for a new version of a service template.
    tags :: Prelude.Maybe [Tag],
    -- | An array of compatible environment template objects for the new version
    -- of a service template.
    compatibleEnvironmentTemplates :: Prelude.NonEmpty CompatibleEnvironmentTemplateInput,
    -- | An object that includes the template bundle S3 bucket path and name for
    -- the new version of a service template.
    source :: TemplateVersionSourceInput,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createServiceTemplateVersion_clientToken' - When included, if two identicial requests are made with the same client
-- token, AWS Proton returns the service template version that the first
-- request created.
--
-- 'majorVersion', 'createServiceTemplateVersion_majorVersion' - To create a new minor version of the service template, include a
-- @majorVersion@.
--
-- To create a new major and minor version of the service template,
-- /exclude/ @majorVersion@.
--
-- 'description', 'createServiceTemplateVersion_description' - A description of the new version of a service template.
--
-- 'tags', 'createServiceTemplateVersion_tags' - Create tags for a new version of a service template.
--
-- 'compatibleEnvironmentTemplates', 'createServiceTemplateVersion_compatibleEnvironmentTemplates' - An array of compatible environment template objects for the new version
-- of a service template.
--
-- 'source', 'createServiceTemplateVersion_source' - An object that includes the template bundle S3 bucket path and name for
-- the new version of a service template.
--
-- 'templateName', 'createServiceTemplateVersion_templateName' - The name of the service template.
newCreateServiceTemplateVersion ::
  -- | 'compatibleEnvironmentTemplates'
  Prelude.NonEmpty CompatibleEnvironmentTemplateInput ->
  -- | 'source'
  TemplateVersionSourceInput ->
  -- | 'templateName'
  Prelude.Text ->
  CreateServiceTemplateVersion
newCreateServiceTemplateVersion
  pCompatibleEnvironmentTemplates_
  pSource_
  pTemplateName_ =
    CreateServiceTemplateVersion'
      { clientToken =
          Prelude.Nothing,
        majorVersion = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        compatibleEnvironmentTemplates =
          Lens.coerced
            Lens.# pCompatibleEnvironmentTemplates_,
        source = pSource_,
        templateName = pTemplateName_
      }

-- | When included, if two identicial requests are made with the same client
-- token, AWS Proton returns the service template version that the first
-- request created.
createServiceTemplateVersion_clientToken :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
createServiceTemplateVersion_clientToken = Lens.lens (\CreateServiceTemplateVersion' {clientToken} -> clientToken) (\s@CreateServiceTemplateVersion' {} a -> s {clientToken = a} :: CreateServiceTemplateVersion)

-- | To create a new minor version of the service template, include a
-- @majorVersion@.
--
-- To create a new major and minor version of the service template,
-- /exclude/ @majorVersion@.
createServiceTemplateVersion_majorVersion :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
createServiceTemplateVersion_majorVersion = Lens.lens (\CreateServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@CreateServiceTemplateVersion' {} a -> s {majorVersion = a} :: CreateServiceTemplateVersion)

-- | A description of the new version of a service template.
createServiceTemplateVersion_description :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
createServiceTemplateVersion_description = Lens.lens (\CreateServiceTemplateVersion' {description} -> description) (\s@CreateServiceTemplateVersion' {} a -> s {description = a} :: CreateServiceTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | Create tags for a new version of a service template.
createServiceTemplateVersion_tags :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe [Tag])
createServiceTemplateVersion_tags = Lens.lens (\CreateServiceTemplateVersion' {tags} -> tags) (\s@CreateServiceTemplateVersion' {} a -> s {tags = a} :: CreateServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | An array of compatible environment template objects for the new version
-- of a service template.
createServiceTemplateVersion_compatibleEnvironmentTemplates :: Lens.Lens' CreateServiceTemplateVersion (Prelude.NonEmpty CompatibleEnvironmentTemplateInput)
createServiceTemplateVersion_compatibleEnvironmentTemplates = Lens.lens (\CreateServiceTemplateVersion' {compatibleEnvironmentTemplates} -> compatibleEnvironmentTemplates) (\s@CreateServiceTemplateVersion' {} a -> s {compatibleEnvironmentTemplates = a} :: CreateServiceTemplateVersion) Prelude.. Lens.coerced

-- | An object that includes the template bundle S3 bucket path and name for
-- the new version of a service template.
createServiceTemplateVersion_source :: Lens.Lens' CreateServiceTemplateVersion TemplateVersionSourceInput
createServiceTemplateVersion_source = Lens.lens (\CreateServiceTemplateVersion' {source} -> source) (\s@CreateServiceTemplateVersion' {} a -> s {source = a} :: CreateServiceTemplateVersion)

-- | The name of the service template.
createServiceTemplateVersion_templateName :: Lens.Lens' CreateServiceTemplateVersion Prelude.Text
createServiceTemplateVersion_templateName = Lens.lens (\CreateServiceTemplateVersion' {templateName} -> templateName) (\s@CreateServiceTemplateVersion' {} a -> s {templateName = a} :: CreateServiceTemplateVersion)

instance Core.AWSRequest CreateServiceTemplateVersion where
  type
    AWSResponse CreateServiceTemplateVersion =
      CreateServiceTemplateVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "serviceTemplateVersion")
      )

instance
  Prelude.Hashable
    CreateServiceTemplateVersion
  where
  hashWithSalt salt' CreateServiceTemplateVersion' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` compatibleEnvironmentTemplates
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateServiceTemplateVersion where
  rnf CreateServiceTemplateVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf compatibleEnvironmentTemplates
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf majorVersion

instance Core.ToHeaders CreateServiceTemplateVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.CreateServiceTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateServiceTemplateVersion where
  toJSON CreateServiceTemplateVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("majorVersion" Core..=) Prelude.<$> majorVersion,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "compatibleEnvironmentTemplates"
                  Core..= compatibleEnvironmentTemplates
              ),
            Prelude.Just ("source" Core..= source),
            Prelude.Just ("templateName" Core..= templateName)
          ]
      )

instance Core.ToPath CreateServiceTemplateVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateServiceTemplateVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceTemplateVersionResponse' smart constructor.
data CreateServiceTemplateVersionResponse = CreateServiceTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service template version summary of detail data that\'s returned by
    -- AWS Proton.
    serviceTemplateVersion :: ServiceTemplateVersion
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createServiceTemplateVersionResponse_httpStatus' - The response's http status code.
--
-- 'serviceTemplateVersion', 'createServiceTemplateVersionResponse_serviceTemplateVersion' - The service template version summary of detail data that\'s returned by
-- AWS Proton.
newCreateServiceTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceTemplateVersion'
  ServiceTemplateVersion ->
  CreateServiceTemplateVersionResponse
newCreateServiceTemplateVersionResponse
  pHttpStatus_
  pServiceTemplateVersion_ =
    CreateServiceTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_,
        serviceTemplateVersion =
          pServiceTemplateVersion_
      }

-- | The response's http status code.
createServiceTemplateVersionResponse_httpStatus :: Lens.Lens' CreateServiceTemplateVersionResponse Prelude.Int
createServiceTemplateVersionResponse_httpStatus = Lens.lens (\CreateServiceTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@CreateServiceTemplateVersionResponse' {} a -> s {httpStatus = a} :: CreateServiceTemplateVersionResponse)

-- | The service template version summary of detail data that\'s returned by
-- AWS Proton.
createServiceTemplateVersionResponse_serviceTemplateVersion :: Lens.Lens' CreateServiceTemplateVersionResponse ServiceTemplateVersion
createServiceTemplateVersionResponse_serviceTemplateVersion = Lens.lens (\CreateServiceTemplateVersionResponse' {serviceTemplateVersion} -> serviceTemplateVersion) (\s@CreateServiceTemplateVersionResponse' {} a -> s {serviceTemplateVersion = a} :: CreateServiceTemplateVersionResponse)

instance
  Prelude.NFData
    CreateServiceTemplateVersionResponse
  where
  rnf CreateServiceTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceTemplateVersion
