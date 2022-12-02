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
-- Module      : Amazonka.ConnectCases.GetTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the requested template.
module Amazonka.ConnectCases.GetTemplate
  ( -- * Creating a Request
    GetTemplate (..),
    newGetTemplate,

    -- * Request Lenses
    getTemplate_domainId,
    getTemplate_templateId,

    -- * Destructuring the Response
    GetTemplateResponse (..),
    newGetTemplateResponse,

    -- * Response Lenses
    getTemplateResponse_tags,
    getTemplateResponse_layoutConfiguration,
    getTemplateResponse_description,
    getTemplateResponse_requiredFields,
    getTemplateResponse_httpStatus,
    getTemplateResponse_name,
    getTemplateResponse_status,
    getTemplateResponse_templateArn,
    getTemplateResponse_templateId,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | A unique identifier of a template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'getTemplate_domainId' - The unique identifier of the Cases domain.
--
-- 'templateId', 'getTemplate_templateId' - A unique identifier of a template.
newGetTemplate ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  GetTemplate
newGetTemplate pDomainId_ pTemplateId_ =
  GetTemplate'
    { domainId = pDomainId_,
      templateId = pTemplateId_
    }

-- | The unique identifier of the Cases domain.
getTemplate_domainId :: Lens.Lens' GetTemplate Prelude.Text
getTemplate_domainId = Lens.lens (\GetTemplate' {domainId} -> domainId) (\s@GetTemplate' {} a -> s {domainId = a} :: GetTemplate)

-- | A unique identifier of a template.
getTemplate_templateId :: Lens.Lens' GetTemplate Prelude.Text
getTemplate_templateId = Lens.lens (\GetTemplate' {templateId} -> templateId) (\s@GetTemplate' {} a -> s {templateId = a} :: GetTemplate)

instance Core.AWSRequest GetTemplate where
  type AWSResponse GetTemplate = GetTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemplateResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "layoutConfiguration")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "requiredFields" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "templateArn")
            Prelude.<*> (x Data..:> "templateId")
      )

instance Prelude.Hashable GetTemplate where
  hashWithSalt _salt GetTemplate' {..} =
    _salt `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData GetTemplate where
  rnf GetTemplate' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf templateId

instance Data.ToHeaders GetTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTemplate where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetTemplate where
  toPath GetTemplate' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/templates/",
        Data.toBS templateId
      ]

instance Data.ToQuery GetTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration of layouts associated to the template.
    layoutConfiguration :: Prelude.Maybe LayoutConfiguration,
    -- | A brief description of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of fields that must contain a value for a case to be successfully
    -- created with this template.
    requiredFields :: Prelude.Maybe [RequiredField],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the template.
    name :: Prelude.Text,
    -- | The status of the template.
    status :: TemplateStatus,
    -- | The Amazon Resource Name (ARN) of the template.
    templateArn :: Prelude.Text,
    -- | A unique identifier of a template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getTemplateResponse_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'layoutConfiguration', 'getTemplateResponse_layoutConfiguration' - Configuration of layouts associated to the template.
--
-- 'description', 'getTemplateResponse_description' - A brief description of the template.
--
-- 'requiredFields', 'getTemplateResponse_requiredFields' - A list of fields that must contain a value for a case to be successfully
-- created with this template.
--
-- 'httpStatus', 'getTemplateResponse_httpStatus' - The response's http status code.
--
-- 'name', 'getTemplateResponse_name' - The name of the template.
--
-- 'status', 'getTemplateResponse_status' - The status of the template.
--
-- 'templateArn', 'getTemplateResponse_templateArn' - The Amazon Resource Name (ARN) of the template.
--
-- 'templateId', 'getTemplateResponse_templateId' - A unique identifier of a template.
newGetTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  TemplateStatus ->
  -- | 'templateArn'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  GetTemplateResponse
newGetTemplateResponse
  pHttpStatus_
  pName_
  pStatus_
  pTemplateArn_
  pTemplateId_ =
    GetTemplateResponse'
      { tags = Prelude.Nothing,
        layoutConfiguration = Prelude.Nothing,
        description = Prelude.Nothing,
        requiredFields = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        status = pStatus_,
        templateArn = pTemplateArn_,
        templateId = pTemplateId_
      }

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
getTemplateResponse_tags :: Lens.Lens' GetTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getTemplateResponse_tags = Lens.lens (\GetTemplateResponse' {tags} -> tags) (\s@GetTemplateResponse' {} a -> s {tags = a} :: GetTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration of layouts associated to the template.
getTemplateResponse_layoutConfiguration :: Lens.Lens' GetTemplateResponse (Prelude.Maybe LayoutConfiguration)
getTemplateResponse_layoutConfiguration = Lens.lens (\GetTemplateResponse' {layoutConfiguration} -> layoutConfiguration) (\s@GetTemplateResponse' {} a -> s {layoutConfiguration = a} :: GetTemplateResponse)

-- | A brief description of the template.
getTemplateResponse_description :: Lens.Lens' GetTemplateResponse (Prelude.Maybe Prelude.Text)
getTemplateResponse_description = Lens.lens (\GetTemplateResponse' {description} -> description) (\s@GetTemplateResponse' {} a -> s {description = a} :: GetTemplateResponse)

-- | A list of fields that must contain a value for a case to be successfully
-- created with this template.
getTemplateResponse_requiredFields :: Lens.Lens' GetTemplateResponse (Prelude.Maybe [RequiredField])
getTemplateResponse_requiredFields = Lens.lens (\GetTemplateResponse' {requiredFields} -> requiredFields) (\s@GetTemplateResponse' {} a -> s {requiredFields = a} :: GetTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTemplateResponse_httpStatus :: Lens.Lens' GetTemplateResponse Prelude.Int
getTemplateResponse_httpStatus = Lens.lens (\GetTemplateResponse' {httpStatus} -> httpStatus) (\s@GetTemplateResponse' {} a -> s {httpStatus = a} :: GetTemplateResponse)

-- | The name of the template.
getTemplateResponse_name :: Lens.Lens' GetTemplateResponse Prelude.Text
getTemplateResponse_name = Lens.lens (\GetTemplateResponse' {name} -> name) (\s@GetTemplateResponse' {} a -> s {name = a} :: GetTemplateResponse)

-- | The status of the template.
getTemplateResponse_status :: Lens.Lens' GetTemplateResponse TemplateStatus
getTemplateResponse_status = Lens.lens (\GetTemplateResponse' {status} -> status) (\s@GetTemplateResponse' {} a -> s {status = a} :: GetTemplateResponse)

-- | The Amazon Resource Name (ARN) of the template.
getTemplateResponse_templateArn :: Lens.Lens' GetTemplateResponse Prelude.Text
getTemplateResponse_templateArn = Lens.lens (\GetTemplateResponse' {templateArn} -> templateArn) (\s@GetTemplateResponse' {} a -> s {templateArn = a} :: GetTemplateResponse)

-- | A unique identifier of a template.
getTemplateResponse_templateId :: Lens.Lens' GetTemplateResponse Prelude.Text
getTemplateResponse_templateId = Lens.lens (\GetTemplateResponse' {templateId} -> templateId) (\s@GetTemplateResponse' {} a -> s {templateId = a} :: GetTemplateResponse)

instance Prelude.NFData GetTemplateResponse where
  rnf GetTemplateResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf layoutConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf requiredFields
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateId
