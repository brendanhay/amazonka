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
-- Module      : Amazonka.Proton.CreateServiceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a service template. The administrator creates a service template
-- to define standardized infrastructure and an optional CI\/CD service
-- pipeline. Developers, in turn, select the service template from Proton.
-- If the selected service template includes a service pipeline definition,
-- they provide a link to their source code repository. Proton then deploys
-- and manages the infrastructure defined by the selected service template.
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-templates.html Proton templates>
-- in the /Proton User Guide/.
module Amazonka.Proton.CreateServiceTemplate
  ( -- * Creating a Request
    CreateServiceTemplate (..),
    newCreateServiceTemplate,

    -- * Request Lenses
    createServiceTemplate_description,
    createServiceTemplate_displayName,
    createServiceTemplate_encryptionKey,
    createServiceTemplate_pipelineProvisioning,
    createServiceTemplate_tags,
    createServiceTemplate_name,

    -- * Destructuring the Response
    CreateServiceTemplateResponse (..),
    newCreateServiceTemplateResponse,

    -- * Response Lenses
    createServiceTemplateResponse_httpStatus,
    createServiceTemplateResponse_serviceTemplate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceTemplate' smart constructor.
data CreateServiceTemplate = CreateServiceTemplate'
  { -- | A description of the service template.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the service template as displayed in the developer
    -- interface.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A customer provided encryption key that\'s used to encrypt data.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | By default, Proton provides a service pipeline for your service. When
    -- this parameter is included, it indicates that an Proton service pipeline
    -- /isn\'t/ provided for your service. After it\'s included, it /can\'t/ be
    -- changed. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-template-authoring.html#ag-template-bundles Template bundles>
    -- in the /Proton User Guide/.
    pipelineProvisioning :: Prelude.Maybe Provisioning,
    -- | An optional list of metadata items that you can associate with the
    -- Proton service template. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the service template.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createServiceTemplate_description' - A description of the service template.
--
-- 'displayName', 'createServiceTemplate_displayName' - The name of the service template as displayed in the developer
-- interface.
--
-- 'encryptionKey', 'createServiceTemplate_encryptionKey' - A customer provided encryption key that\'s used to encrypt data.
--
-- 'pipelineProvisioning', 'createServiceTemplate_pipelineProvisioning' - By default, Proton provides a service pipeline for your service. When
-- this parameter is included, it indicates that an Proton service pipeline
-- /isn\'t/ provided for your service. After it\'s included, it /can\'t/ be
-- changed. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-template-authoring.html#ag-template-bundles Template bundles>
-- in the /Proton User Guide/.
--
-- 'tags', 'createServiceTemplate_tags' - An optional list of metadata items that you can associate with the
-- Proton service template. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'name', 'createServiceTemplate_name' - The name of the service template.
newCreateServiceTemplate ::
  -- | 'name'
  Prelude.Text ->
  CreateServiceTemplate
newCreateServiceTemplate pName_ =
  CreateServiceTemplate'
    { description =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      pipelineProvisioning = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A description of the service template.
createServiceTemplate_description :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Prelude.Text)
createServiceTemplate_description = Lens.lens (\CreateServiceTemplate' {description} -> description) (\s@CreateServiceTemplate' {} a -> s {description = a} :: CreateServiceTemplate) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the service template as displayed in the developer
-- interface.
createServiceTemplate_displayName :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Prelude.Text)
createServiceTemplate_displayName = Lens.lens (\CreateServiceTemplate' {displayName} -> displayName) (\s@CreateServiceTemplate' {} a -> s {displayName = a} :: CreateServiceTemplate) Prelude.. Lens.mapping Data._Sensitive

-- | A customer provided encryption key that\'s used to encrypt data.
createServiceTemplate_encryptionKey :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Prelude.Text)
createServiceTemplate_encryptionKey = Lens.lens (\CreateServiceTemplate' {encryptionKey} -> encryptionKey) (\s@CreateServiceTemplate' {} a -> s {encryptionKey = a} :: CreateServiceTemplate)

-- | By default, Proton provides a service pipeline for your service. When
-- this parameter is included, it indicates that an Proton service pipeline
-- /isn\'t/ provided for your service. After it\'s included, it /can\'t/ be
-- changed. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-template-authoring.html#ag-template-bundles Template bundles>
-- in the /Proton User Guide/.
createServiceTemplate_pipelineProvisioning :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Provisioning)
createServiceTemplate_pipelineProvisioning = Lens.lens (\CreateServiceTemplate' {pipelineProvisioning} -> pipelineProvisioning) (\s@CreateServiceTemplate' {} a -> s {pipelineProvisioning = a} :: CreateServiceTemplate)

-- | An optional list of metadata items that you can associate with the
-- Proton service template. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createServiceTemplate_tags :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe [Tag])
createServiceTemplate_tags = Lens.lens (\CreateServiceTemplate' {tags} -> tags) (\s@CreateServiceTemplate' {} a -> s {tags = a} :: CreateServiceTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service template.
createServiceTemplate_name :: Lens.Lens' CreateServiceTemplate Prelude.Text
createServiceTemplate_name = Lens.lens (\CreateServiceTemplate' {name} -> name) (\s@CreateServiceTemplate' {} a -> s {name = a} :: CreateServiceTemplate)

instance Core.AWSRequest CreateServiceTemplate where
  type
    AWSResponse CreateServiceTemplate =
      CreateServiceTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceTemplate")
      )

instance Prelude.Hashable CreateServiceTemplate where
  hashWithSalt _salt CreateServiceTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` pipelineProvisioning
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateServiceTemplate where
  rnf CreateServiceTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf pipelineProvisioning
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateServiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateServiceTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServiceTemplate where
  toJSON CreateServiceTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("displayName" Data..=) Prelude.<$> displayName,
            ("encryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("pipelineProvisioning" Data..=)
              Prelude.<$> pipelineProvisioning,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateServiceTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServiceTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceTemplateResponse' smart constructor.
data CreateServiceTemplateResponse = CreateServiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service template detail data that\'s returned by Proton.
    serviceTemplate :: ServiceTemplate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createServiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'serviceTemplate', 'createServiceTemplateResponse_serviceTemplate' - The service template detail data that\'s returned by Proton.
newCreateServiceTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceTemplate'
  ServiceTemplate ->
  CreateServiceTemplateResponse
newCreateServiceTemplateResponse
  pHttpStatus_
  pServiceTemplate_ =
    CreateServiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        serviceTemplate = pServiceTemplate_
      }

-- | The response's http status code.
createServiceTemplateResponse_httpStatus :: Lens.Lens' CreateServiceTemplateResponse Prelude.Int
createServiceTemplateResponse_httpStatus = Lens.lens (\CreateServiceTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateServiceTemplateResponse' {} a -> s {httpStatus = a} :: CreateServiceTemplateResponse)

-- | The service template detail data that\'s returned by Proton.
createServiceTemplateResponse_serviceTemplate :: Lens.Lens' CreateServiceTemplateResponse ServiceTemplate
createServiceTemplateResponse_serviceTemplate = Lens.lens (\CreateServiceTemplateResponse' {serviceTemplate} -> serviceTemplate) (\s@CreateServiceTemplateResponse' {} a -> s {serviceTemplate = a} :: CreateServiceTemplateResponse)

instance Prelude.NFData CreateServiceTemplateResponse where
  rnf CreateServiceTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceTemplate
