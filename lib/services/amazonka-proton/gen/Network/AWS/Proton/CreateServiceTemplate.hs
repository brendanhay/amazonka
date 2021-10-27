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
-- Module      : Network.AWS.Proton.CreateServiceTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a service template. The administrator creates a service template
-- to define standardized infrastructure and an optional CICD service
-- pipeline. Developers, in turn, select the service template from AWS
-- Proton. If the selected service template includes a service pipeline
-- definition, they provide a link to their source code repository. AWS
-- Proton then deploys and manages the infrastructure defined by the
-- selected service template. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/managing-svc-templates.html Service Templates>
-- in the /AWS Proton Administrator Guide/.
module Network.AWS.Proton.CreateServiceTemplate
  ( -- * Creating a Request
    CreateServiceTemplate (..),
    newCreateServiceTemplate,

    -- * Request Lenses
    createServiceTemplate_displayName,
    createServiceTemplate_encryptionKey,
    createServiceTemplate_pipelineProvisioning,
    createServiceTemplate_description,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Proton.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateServiceTemplate' smart constructor.
data CreateServiceTemplate = CreateServiceTemplate'
  { -- | The name of the service template as displayed in the developer
    -- interface.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A customer provided encryption key that\'s used to encrypt data.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | AWS Proton includes a service pipeline for your service by default. When
    -- included, this parameter indicates that an AWS Proton service pipeline
    -- /won\'t/ be included for your service. Once specified, this parameter
    -- /can\'t/ be changed. For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-template-bundles.html Service template bundles>
    -- in the /AWS Proton Administrator Guide/.
    pipelineProvisioning :: Prelude.Maybe Provisioning,
    -- | A description of the service template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Create tags for your service template. For more information, see /AWS
    -- Proton resources and tagging/ in the
    -- <https://docs.aws.amazon.com/proton/latest/adminguide/resources.html AWS Proton Administrator Guide>
    -- or
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html AWS Proton User Guide>.
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
-- 'displayName', 'createServiceTemplate_displayName' - The name of the service template as displayed in the developer
-- interface.
--
-- 'encryptionKey', 'createServiceTemplate_encryptionKey' - A customer provided encryption key that\'s used to encrypt data.
--
-- 'pipelineProvisioning', 'createServiceTemplate_pipelineProvisioning' - AWS Proton includes a service pipeline for your service by default. When
-- included, this parameter indicates that an AWS Proton service pipeline
-- /won\'t/ be included for your service. Once specified, this parameter
-- /can\'t/ be changed. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-template-bundles.html Service template bundles>
-- in the /AWS Proton Administrator Guide/.
--
-- 'description', 'createServiceTemplate_description' - A description of the service template.
--
-- 'tags', 'createServiceTemplate_tags' - Create tags for your service template. For more information, see /AWS
-- Proton resources and tagging/ in the
-- <https://docs.aws.amazon.com/proton/latest/adminguide/resources.html AWS Proton Administrator Guide>
-- or
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html AWS Proton User Guide>.
--
-- 'name', 'createServiceTemplate_name' - The name of the service template.
newCreateServiceTemplate ::
  -- | 'name'
  Prelude.Text ->
  CreateServiceTemplate
newCreateServiceTemplate pName_ =
  CreateServiceTemplate'
    { displayName =
        Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      pipelineProvisioning = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The name of the service template as displayed in the developer
-- interface.
createServiceTemplate_displayName :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Prelude.Text)
createServiceTemplate_displayName = Lens.lens (\CreateServiceTemplate' {displayName} -> displayName) (\s@CreateServiceTemplate' {} a -> s {displayName = a} :: CreateServiceTemplate) Prelude.. Lens.mapping Core._Sensitive

-- | A customer provided encryption key that\'s used to encrypt data.
createServiceTemplate_encryptionKey :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Prelude.Text)
createServiceTemplate_encryptionKey = Lens.lens (\CreateServiceTemplate' {encryptionKey} -> encryptionKey) (\s@CreateServiceTemplate' {} a -> s {encryptionKey = a} :: CreateServiceTemplate)

-- | AWS Proton includes a service pipeline for your service by default. When
-- included, this parameter indicates that an AWS Proton service pipeline
-- /won\'t/ be included for your service. Once specified, this parameter
-- /can\'t/ be changed. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-template-bundles.html Service template bundles>
-- in the /AWS Proton Administrator Guide/.
createServiceTemplate_pipelineProvisioning :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Provisioning)
createServiceTemplate_pipelineProvisioning = Lens.lens (\CreateServiceTemplate' {pipelineProvisioning} -> pipelineProvisioning) (\s@CreateServiceTemplate' {} a -> s {pipelineProvisioning = a} :: CreateServiceTemplate)

-- | A description of the service template.
createServiceTemplate_description :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe Prelude.Text)
createServiceTemplate_description = Lens.lens (\CreateServiceTemplate' {description} -> description) (\s@CreateServiceTemplate' {} a -> s {description = a} :: CreateServiceTemplate) Prelude.. Lens.mapping Core._Sensitive

-- | Create tags for your service template. For more information, see /AWS
-- Proton resources and tagging/ in the
-- <https://docs.aws.amazon.com/proton/latest/adminguide/resources.html AWS Proton Administrator Guide>
-- or
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html AWS Proton User Guide>.
createServiceTemplate_tags :: Lens.Lens' CreateServiceTemplate (Prelude.Maybe [Tag])
createServiceTemplate_tags = Lens.lens (\CreateServiceTemplate' {tags} -> tags) (\s@CreateServiceTemplate' {} a -> s {tags = a} :: CreateServiceTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service template.
createServiceTemplate_name :: Lens.Lens' CreateServiceTemplate Prelude.Text
createServiceTemplate_name = Lens.lens (\CreateServiceTemplate' {name} -> name) (\s@CreateServiceTemplate' {} a -> s {name = a} :: CreateServiceTemplate)

instance Core.AWSRequest CreateServiceTemplate where
  type
    AWSResponse CreateServiceTemplate =
      CreateServiceTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "serviceTemplate")
      )

instance Prelude.Hashable CreateServiceTemplate

instance Prelude.NFData CreateServiceTemplate

instance Core.ToHeaders CreateServiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.CreateServiceTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateServiceTemplate where
  toJSON CreateServiceTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("displayName" Core..=) Prelude.<$> displayName,
            ("encryptionKey" Core..=) Prelude.<$> encryptionKey,
            ("pipelineProvisioning" Core..=)
              Prelude.<$> pipelineProvisioning,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateServiceTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateServiceTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceTemplateResponse' smart constructor.
data CreateServiceTemplateResponse = CreateServiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service template detail data that\'s returned by AWS Proton.
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
-- 'serviceTemplate', 'createServiceTemplateResponse_serviceTemplate' - The service template detail data that\'s returned by AWS Proton.
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

-- | The service template detail data that\'s returned by AWS Proton.
createServiceTemplateResponse_serviceTemplate :: Lens.Lens' CreateServiceTemplateResponse ServiceTemplate
createServiceTemplateResponse_serviceTemplate = Lens.lens (\CreateServiceTemplateResponse' {serviceTemplate} -> serviceTemplate) (\s@CreateServiceTemplateResponse' {} a -> s {serviceTemplate = a} :: CreateServiceTemplateResponse)

instance Prelude.NFData CreateServiceTemplateResponse
