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
-- Module      : Amazonka.ConnectCases.CreateTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a template in the Cases domain. This template is used to define
-- the case object model (that is, define what data can be captured on
-- cases) in a Cases domain. A template must have a unique name within a
-- domain, and it must reference existing field IDs and layout IDs.
-- Additionally, multiple fields with same IDs are not allowed within the
-- same Template.
module Amazonka.ConnectCases.CreateTemplate
  ( -- * Creating a Request
    CreateTemplate (..),
    newCreateTemplate,

    -- * Request Lenses
    createTemplate_layoutConfiguration,
    createTemplate_description,
    createTemplate_requiredFields,
    createTemplate_domainId,
    createTemplate_name,

    -- * Destructuring the Response
    CreateTemplateResponse (..),
    newCreateTemplateResponse,

    -- * Response Lenses
    createTemplateResponse_httpStatus,
    createTemplateResponse_templateArn,
    createTemplateResponse_templateId,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTemplate' smart constructor.
data CreateTemplate = CreateTemplate'
  { -- | Configuration of layouts associated to the template.
    layoutConfiguration :: Prelude.Maybe LayoutConfiguration,
    -- | A brief description of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of fields that must contain a value for a case to be successfully
    -- created with this template.
    requiredFields :: Prelude.Maybe [RequiredField],
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | A name for the template. It must be unique per domain.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layoutConfiguration', 'createTemplate_layoutConfiguration' - Configuration of layouts associated to the template.
--
-- 'description', 'createTemplate_description' - A brief description of the template.
--
-- 'requiredFields', 'createTemplate_requiredFields' - A list of fields that must contain a value for a case to be successfully
-- created with this template.
--
-- 'domainId', 'createTemplate_domainId' - The unique identifier of the Cases domain.
--
-- 'name', 'createTemplate_name' - A name for the template. It must be unique per domain.
newCreateTemplate ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateTemplate
newCreateTemplate pDomainId_ pName_ =
  CreateTemplate'
    { layoutConfiguration =
        Prelude.Nothing,
      description = Prelude.Nothing,
      requiredFields = Prelude.Nothing,
      domainId = pDomainId_,
      name = pName_
    }

-- | Configuration of layouts associated to the template.
createTemplate_layoutConfiguration :: Lens.Lens' CreateTemplate (Prelude.Maybe LayoutConfiguration)
createTemplate_layoutConfiguration = Lens.lens (\CreateTemplate' {layoutConfiguration} -> layoutConfiguration) (\s@CreateTemplate' {} a -> s {layoutConfiguration = a} :: CreateTemplate)

-- | A brief description of the template.
createTemplate_description :: Lens.Lens' CreateTemplate (Prelude.Maybe Prelude.Text)
createTemplate_description = Lens.lens (\CreateTemplate' {description} -> description) (\s@CreateTemplate' {} a -> s {description = a} :: CreateTemplate)

-- | A list of fields that must contain a value for a case to be successfully
-- created with this template.
createTemplate_requiredFields :: Lens.Lens' CreateTemplate (Prelude.Maybe [RequiredField])
createTemplate_requiredFields = Lens.lens (\CreateTemplate' {requiredFields} -> requiredFields) (\s@CreateTemplate' {} a -> s {requiredFields = a} :: CreateTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the Cases domain.
createTemplate_domainId :: Lens.Lens' CreateTemplate Prelude.Text
createTemplate_domainId = Lens.lens (\CreateTemplate' {domainId} -> domainId) (\s@CreateTemplate' {} a -> s {domainId = a} :: CreateTemplate)

-- | A name for the template. It must be unique per domain.
createTemplate_name :: Lens.Lens' CreateTemplate Prelude.Text
createTemplate_name = Lens.lens (\CreateTemplate' {name} -> name) (\s@CreateTemplate' {} a -> s {name = a} :: CreateTemplate)

instance Core.AWSRequest CreateTemplate where
  type
    AWSResponse CreateTemplate =
      CreateTemplateResponse
  service _ = defaultService
  request srv = Request.postJSON srv
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "templateArn")
            Prelude.<*> (x Core..:> "templateId")
      )

instance Prelude.Hashable CreateTemplate where
  hashWithSalt _salt CreateTemplate' {..} =
    _salt `Prelude.hashWithSalt` layoutConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` requiredFields
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateTemplate where
  rnf CreateTemplate' {..} =
    Prelude.rnf layoutConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf requiredFields
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTemplate where
  toJSON CreateTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("layoutConfiguration" Core..=)
              Prelude.<$> layoutConfiguration,
            ("description" Core..=) Prelude.<$> description,
            ("requiredFields" Core..=)
              Prelude.<$> requiredFields,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateTemplate where
  toPath CreateTemplate' {..} =
    Prelude.mconcat
      ["/domains/", Core.toBS domainId, "/templates"]

instance Core.ToQuery CreateTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTemplateResponse' smart constructor.
data CreateTemplateResponse = CreateTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the newly created template.
    templateArn :: Prelude.Text,
    -- | A unique identifier of a template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTemplateResponse_httpStatus' - The response's http status code.
--
-- 'templateArn', 'createTemplateResponse_templateArn' - The Amazon Resource Name (ARN) of the newly created template.
--
-- 'templateId', 'createTemplateResponse_templateId' - A unique identifier of a template.
newCreateTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'templateArn'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  CreateTemplateResponse
newCreateTemplateResponse
  pHttpStatus_
  pTemplateArn_
  pTemplateId_ =
    CreateTemplateResponse'
      { httpStatus = pHttpStatus_,
        templateArn = pTemplateArn_,
        templateId = pTemplateId_
      }

-- | The response's http status code.
createTemplateResponse_httpStatus :: Lens.Lens' CreateTemplateResponse Prelude.Int
createTemplateResponse_httpStatus = Lens.lens (\CreateTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateTemplateResponse' {} a -> s {httpStatus = a} :: CreateTemplateResponse)

-- | The Amazon Resource Name (ARN) of the newly created template.
createTemplateResponse_templateArn :: Lens.Lens' CreateTemplateResponse Prelude.Text
createTemplateResponse_templateArn = Lens.lens (\CreateTemplateResponse' {templateArn} -> templateArn) (\s@CreateTemplateResponse' {} a -> s {templateArn = a} :: CreateTemplateResponse)

-- | A unique identifier of a template.
createTemplateResponse_templateId :: Lens.Lens' CreateTemplateResponse Prelude.Text
createTemplateResponse_templateId = Lens.lens (\CreateTemplateResponse' {templateId} -> templateId) (\s@CreateTemplateResponse' {} a -> s {templateId = a} :: CreateTemplateResponse)

instance Prelude.NFData CreateTemplateResponse where
  rnf CreateTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateId
