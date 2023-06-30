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
-- Module      : Amazonka.IoT.DescribeManagedJobTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View details of a managed job template.
module Amazonka.IoT.DescribeManagedJobTemplate
  ( -- * Creating a Request
    DescribeManagedJobTemplate (..),
    newDescribeManagedJobTemplate,

    -- * Request Lenses
    describeManagedJobTemplate_templateVersion,
    describeManagedJobTemplate_templateName,

    -- * Destructuring the Response
    DescribeManagedJobTemplateResponse (..),
    newDescribeManagedJobTemplateResponse,

    -- * Response Lenses
    describeManagedJobTemplateResponse_description,
    describeManagedJobTemplateResponse_document,
    describeManagedJobTemplateResponse_documentParameters,
    describeManagedJobTemplateResponse_environments,
    describeManagedJobTemplateResponse_templateArn,
    describeManagedJobTemplateResponse_templateName,
    describeManagedJobTemplateResponse_templateVersion,
    describeManagedJobTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeManagedJobTemplate' smart constructor.
data DescribeManagedJobTemplate = DescribeManagedJobTemplate'
  { -- | An optional parameter to specify version of a managed template. If not
    -- specified, the pre-defined default version is returned.
    templateVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a managed job template, which is required.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateVersion', 'describeManagedJobTemplate_templateVersion' - An optional parameter to specify version of a managed template. If not
-- specified, the pre-defined default version is returned.
--
-- 'templateName', 'describeManagedJobTemplate_templateName' - The unique name of a managed job template, which is required.
newDescribeManagedJobTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DescribeManagedJobTemplate
newDescribeManagedJobTemplate pTemplateName_ =
  DescribeManagedJobTemplate'
    { templateVersion =
        Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | An optional parameter to specify version of a managed template. If not
-- specified, the pre-defined default version is returned.
describeManagedJobTemplate_templateVersion :: Lens.Lens' DescribeManagedJobTemplate (Prelude.Maybe Prelude.Text)
describeManagedJobTemplate_templateVersion = Lens.lens (\DescribeManagedJobTemplate' {templateVersion} -> templateVersion) (\s@DescribeManagedJobTemplate' {} a -> s {templateVersion = a} :: DescribeManagedJobTemplate)

-- | The unique name of a managed job template, which is required.
describeManagedJobTemplate_templateName :: Lens.Lens' DescribeManagedJobTemplate Prelude.Text
describeManagedJobTemplate_templateName = Lens.lens (\DescribeManagedJobTemplate' {templateName} -> templateName) (\s@DescribeManagedJobTemplate' {} a -> s {templateName = a} :: DescribeManagedJobTemplate)

instance Core.AWSRequest DescribeManagedJobTemplate where
  type
    AWSResponse DescribeManagedJobTemplate =
      DescribeManagedJobTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeManagedJobTemplateResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "document")
            Prelude.<*> ( x
                            Data..?> "documentParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "environments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "templateArn")
            Prelude.<*> (x Data..?> "templateName")
            Prelude.<*> (x Data..?> "templateVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeManagedJobTemplate where
  hashWithSalt _salt DescribeManagedJobTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` templateVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DescribeManagedJobTemplate where
  rnf DescribeManagedJobTemplate' {..} =
    Prelude.rnf templateVersion
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders DescribeManagedJobTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeManagedJobTemplate where
  toPath DescribeManagedJobTemplate' {..} =
    Prelude.mconcat
      ["/managed-job-templates/", Data.toBS templateName]

instance Data.ToQuery DescribeManagedJobTemplate where
  toQuery DescribeManagedJobTemplate' {..} =
    Prelude.mconcat
      ["templateVersion" Data.=: templateVersion]

-- | /See:/ 'newDescribeManagedJobTemplateResponse' smart constructor.
data DescribeManagedJobTemplateResponse = DescribeManagedJobTemplateResponse'
  { -- | The unique description of a managed template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The document schema for a managed job template.
    document :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs that you can use as guidance to specify the
    -- inputs for creating a job from a managed template.
    --
    -- @documentParameters@ can only be used when creating jobs from Amazon Web
    -- Services managed templates. This parameter can\'t be used with custom
    -- job templates or to create jobs from them.
    documentParameters :: Prelude.Maybe [DocumentParameter],
    -- | A list of environments that are supported with the managed job template.
    environments :: Prelude.Maybe [Prelude.Text],
    -- | The unique Amazon Resource Name (ARN) of the managed template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a managed template, such as @AWS-Reboot@.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The version for a managed template.
    templateVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeManagedJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeManagedJobTemplateResponse_description' - The unique description of a managed template.
--
-- 'document', 'describeManagedJobTemplateResponse_document' - The document schema for a managed job template.
--
-- 'documentParameters', 'describeManagedJobTemplateResponse_documentParameters' - A map of key-value pairs that you can use as guidance to specify the
-- inputs for creating a job from a managed template.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
--
-- 'environments', 'describeManagedJobTemplateResponse_environments' - A list of environments that are supported with the managed job template.
--
-- 'templateArn', 'describeManagedJobTemplateResponse_templateArn' - The unique Amazon Resource Name (ARN) of the managed template.
--
-- 'templateName', 'describeManagedJobTemplateResponse_templateName' - The unique name of a managed template, such as @AWS-Reboot@.
--
-- 'templateVersion', 'describeManagedJobTemplateResponse_templateVersion' - The version for a managed template.
--
-- 'httpStatus', 'describeManagedJobTemplateResponse_httpStatus' - The response's http status code.
newDescribeManagedJobTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeManagedJobTemplateResponse
newDescribeManagedJobTemplateResponse pHttpStatus_ =
  DescribeManagedJobTemplateResponse'
    { description =
        Prelude.Nothing,
      document = Prelude.Nothing,
      documentParameters = Prelude.Nothing,
      environments = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      templateName = Prelude.Nothing,
      templateVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique description of a managed template.
describeManagedJobTemplateResponse_description :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeManagedJobTemplateResponse_description = Lens.lens (\DescribeManagedJobTemplateResponse' {description} -> description) (\s@DescribeManagedJobTemplateResponse' {} a -> s {description = a} :: DescribeManagedJobTemplateResponse)

-- | The document schema for a managed job template.
describeManagedJobTemplateResponse_document :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeManagedJobTemplateResponse_document = Lens.lens (\DescribeManagedJobTemplateResponse' {document} -> document) (\s@DescribeManagedJobTemplateResponse' {} a -> s {document = a} :: DescribeManagedJobTemplateResponse)

-- | A map of key-value pairs that you can use as guidance to specify the
-- inputs for creating a job from a managed template.
--
-- @documentParameters@ can only be used when creating jobs from Amazon Web
-- Services managed templates. This parameter can\'t be used with custom
-- job templates or to create jobs from them.
describeManagedJobTemplateResponse_documentParameters :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe [DocumentParameter])
describeManagedJobTemplateResponse_documentParameters = Lens.lens (\DescribeManagedJobTemplateResponse' {documentParameters} -> documentParameters) (\s@DescribeManagedJobTemplateResponse' {} a -> s {documentParameters = a} :: DescribeManagedJobTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of environments that are supported with the managed job template.
describeManagedJobTemplateResponse_environments :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe [Prelude.Text])
describeManagedJobTemplateResponse_environments = Lens.lens (\DescribeManagedJobTemplateResponse' {environments} -> environments) (\s@DescribeManagedJobTemplateResponse' {} a -> s {environments = a} :: DescribeManagedJobTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique Amazon Resource Name (ARN) of the managed template.
describeManagedJobTemplateResponse_templateArn :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeManagedJobTemplateResponse_templateArn = Lens.lens (\DescribeManagedJobTemplateResponse' {templateArn} -> templateArn) (\s@DescribeManagedJobTemplateResponse' {} a -> s {templateArn = a} :: DescribeManagedJobTemplateResponse)

-- | The unique name of a managed template, such as @AWS-Reboot@.
describeManagedJobTemplateResponse_templateName :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeManagedJobTemplateResponse_templateName = Lens.lens (\DescribeManagedJobTemplateResponse' {templateName} -> templateName) (\s@DescribeManagedJobTemplateResponse' {} a -> s {templateName = a} :: DescribeManagedJobTemplateResponse)

-- | The version for a managed template.
describeManagedJobTemplateResponse_templateVersion :: Lens.Lens' DescribeManagedJobTemplateResponse (Prelude.Maybe Prelude.Text)
describeManagedJobTemplateResponse_templateVersion = Lens.lens (\DescribeManagedJobTemplateResponse' {templateVersion} -> templateVersion) (\s@DescribeManagedJobTemplateResponse' {} a -> s {templateVersion = a} :: DescribeManagedJobTemplateResponse)

-- | The response's http status code.
describeManagedJobTemplateResponse_httpStatus :: Lens.Lens' DescribeManagedJobTemplateResponse Prelude.Int
describeManagedJobTemplateResponse_httpStatus = Lens.lens (\DescribeManagedJobTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeManagedJobTemplateResponse' {} a -> s {httpStatus = a} :: DescribeManagedJobTemplateResponse)

instance
  Prelude.NFData
    DescribeManagedJobTemplateResponse
  where
  rnf DescribeManagedJobTemplateResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf documentParameters
      `Prelude.seq` Prelude.rnf environments
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateVersion
      `Prelude.seq` Prelude.rnf httpStatus
