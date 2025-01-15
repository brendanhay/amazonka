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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new major or minor version of a service template. A major
-- version of a service template is a version that /isn\'t/ backward
-- compatible. A minor version of a service template is a version that\'s
-- backward compatible within its major version.
module Amazonka.Proton.CreateServiceTemplateVersion
  ( -- * Creating a Request
    CreateServiceTemplateVersion (..),
    newCreateServiceTemplateVersion,

    -- * Request Lenses
    createServiceTemplateVersion_clientToken,
    createServiceTemplateVersion_description,
    createServiceTemplateVersion_majorVersion,
    createServiceTemplateVersion_supportedComponentSources,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceTemplateVersion' smart constructor.
data CreateServiceTemplateVersion = CreateServiceTemplateVersion'
  { -- | When included, if two identical requests are made with the same client
    -- token, Proton returns the service template version that the first
    -- request created.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the new version of a service template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | To create a new minor version of the service template, include a
    -- @major Version@.
    --
    -- To create a new major and minor version of the service template,
    -- /exclude/ @major Version@.
    majorVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of supported component sources. Components with supported
    -- sources can be attached to service instances based on this service
    -- template version.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    supportedComponentSources :: Prelude.Maybe [ServiceTemplateSupportedComponentSourceType],
    -- | An optional list of metadata items that you can associate with the
    -- Proton service template version. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | An array of environment template objects that are compatible with the
    -- new service template version. A service instance based on this service
    -- template version can run in environments based on compatible templates.
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
-- 'clientToken', 'createServiceTemplateVersion_clientToken' - When included, if two identical requests are made with the same client
-- token, Proton returns the service template version that the first
-- request created.
--
-- 'description', 'createServiceTemplateVersion_description' - A description of the new version of a service template.
--
-- 'majorVersion', 'createServiceTemplateVersion_majorVersion' - To create a new minor version of the service template, include a
-- @major Version@.
--
-- To create a new major and minor version of the service template,
-- /exclude/ @major Version@.
--
-- 'supportedComponentSources', 'createServiceTemplateVersion_supportedComponentSources' - An array of supported component sources. Components with supported
-- sources can be attached to service instances based on this service
-- template version.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'tags', 'createServiceTemplateVersion_tags' - An optional list of metadata items that you can associate with the
-- Proton service template version. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'compatibleEnvironmentTemplates', 'createServiceTemplateVersion_compatibleEnvironmentTemplates' - An array of environment template objects that are compatible with the
-- new service template version. A service instance based on this service
-- template version can run in environments based on compatible templates.
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
        description = Prelude.Nothing,
        majorVersion = Prelude.Nothing,
        supportedComponentSources = Prelude.Nothing,
        tags = Prelude.Nothing,
        compatibleEnvironmentTemplates =
          Lens.coerced
            Lens.# pCompatibleEnvironmentTemplates_,
        source = pSource_,
        templateName = pTemplateName_
      }

-- | When included, if two identical requests are made with the same client
-- token, Proton returns the service template version that the first
-- request created.
createServiceTemplateVersion_clientToken :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
createServiceTemplateVersion_clientToken = Lens.lens (\CreateServiceTemplateVersion' {clientToken} -> clientToken) (\s@CreateServiceTemplateVersion' {} a -> s {clientToken = a} :: CreateServiceTemplateVersion)

-- | A description of the new version of a service template.
createServiceTemplateVersion_description :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
createServiceTemplateVersion_description = Lens.lens (\CreateServiceTemplateVersion' {description} -> description) (\s@CreateServiceTemplateVersion' {} a -> s {description = a} :: CreateServiceTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | To create a new minor version of the service template, include a
-- @major Version@.
--
-- To create a new major and minor version of the service template,
-- /exclude/ @major Version@.
createServiceTemplateVersion_majorVersion :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
createServiceTemplateVersion_majorVersion = Lens.lens (\CreateServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@CreateServiceTemplateVersion' {} a -> s {majorVersion = a} :: CreateServiceTemplateVersion)

-- | An array of supported component sources. Components with supported
-- sources can be attached to service instances based on this service
-- template version.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
createServiceTemplateVersion_supportedComponentSources :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe [ServiceTemplateSupportedComponentSourceType])
createServiceTemplateVersion_supportedComponentSources = Lens.lens (\CreateServiceTemplateVersion' {supportedComponentSources} -> supportedComponentSources) (\s@CreateServiceTemplateVersion' {} a -> s {supportedComponentSources = a} :: CreateServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of metadata items that you can associate with the
-- Proton service template version. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createServiceTemplateVersion_tags :: Lens.Lens' CreateServiceTemplateVersion (Prelude.Maybe [Tag])
createServiceTemplateVersion_tags = Lens.lens (\CreateServiceTemplateVersion' {tags} -> tags) (\s@CreateServiceTemplateVersion' {} a -> s {tags = a} :: CreateServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | An array of environment template objects that are compatible with the
-- new service template version. A service instance based on this service
-- template version can run in environments based on compatible templates.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceTemplateVersion")
      )

instance
  Prelude.Hashable
    CreateServiceTemplateVersion
  where
  hashWithSalt _salt CreateServiceTemplateVersion' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` supportedComponentSources
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` compatibleEnvironmentTemplates
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData CreateServiceTemplateVersion where
  rnf CreateServiceTemplateVersion' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf majorVersion `Prelude.seq`
          Prelude.rnf supportedComponentSources `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf compatibleEnvironmentTemplates `Prelude.seq`
                Prelude.rnf source `Prelude.seq`
                  Prelude.rnf templateName

instance Data.ToHeaders CreateServiceTemplateVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateServiceTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServiceTemplateVersion where
  toJSON CreateServiceTemplateVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("majorVersion" Data..=) Prelude.<$> majorVersion,
            ("supportedComponentSources" Data..=)
              Prelude.<$> supportedComponentSources,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "compatibleEnvironmentTemplates"
                  Data..= compatibleEnvironmentTemplates
              ),
            Prelude.Just ("source" Data..= source),
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )

instance Data.ToPath CreateServiceTemplateVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServiceTemplateVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceTemplateVersionResponse' smart constructor.
data CreateServiceTemplateVersionResponse = CreateServiceTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service template version summary of detail data that\'s returned by
    -- Proton.
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
-- Proton.
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
-- Proton.
createServiceTemplateVersionResponse_serviceTemplateVersion :: Lens.Lens' CreateServiceTemplateVersionResponse ServiceTemplateVersion
createServiceTemplateVersionResponse_serviceTemplateVersion = Lens.lens (\CreateServiceTemplateVersionResponse' {serviceTemplateVersion} -> serviceTemplateVersion) (\s@CreateServiceTemplateVersionResponse' {} a -> s {serviceTemplateVersion = a} :: CreateServiceTemplateVersionResponse)

instance
  Prelude.NFData
    CreateServiceTemplateVersionResponse
  where
  rnf CreateServiceTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf serviceTemplateVersion
