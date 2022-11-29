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
-- Module      : Amazonka.SMS.GenerateTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an CloudFormation template based on the current launch
-- configuration and writes it to an Amazon S3 object in the customer’s
-- Amazon S3 bucket.
module Amazonka.SMS.GenerateTemplate
  ( -- * Creating a Request
    GenerateTemplate (..),
    newGenerateTemplate,

    -- * Request Lenses
    generateTemplate_templateFormat,
    generateTemplate_appId,

    -- * Destructuring the Response
    GenerateTemplateResponse (..),
    newGenerateTemplateResponse,

    -- * Response Lenses
    generateTemplateResponse_s3Location,
    generateTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGenerateTemplate' smart constructor.
data GenerateTemplate = GenerateTemplate'
  { -- | The format for generating the CloudFormation template.
    templateFormat :: Prelude.Maybe OutputFormat,
    -- | The ID of the application associated with the CloudFormation template.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateFormat', 'generateTemplate_templateFormat' - The format for generating the CloudFormation template.
--
-- 'appId', 'generateTemplate_appId' - The ID of the application associated with the CloudFormation template.
newGenerateTemplate ::
  GenerateTemplate
newGenerateTemplate =
  GenerateTemplate'
    { templateFormat = Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | The format for generating the CloudFormation template.
generateTemplate_templateFormat :: Lens.Lens' GenerateTemplate (Prelude.Maybe OutputFormat)
generateTemplate_templateFormat = Lens.lens (\GenerateTemplate' {templateFormat} -> templateFormat) (\s@GenerateTemplate' {} a -> s {templateFormat = a} :: GenerateTemplate)

-- | The ID of the application associated with the CloudFormation template.
generateTemplate_appId :: Lens.Lens' GenerateTemplate (Prelude.Maybe Prelude.Text)
generateTemplate_appId = Lens.lens (\GenerateTemplate' {appId} -> appId) (\s@GenerateTemplate' {} a -> s {appId = a} :: GenerateTemplate)

instance Core.AWSRequest GenerateTemplate where
  type
    AWSResponse GenerateTemplate =
      GenerateTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateTemplateResponse'
            Prelude.<$> (x Core..?> "s3Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateTemplate where
  hashWithSalt _salt GenerateTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateFormat
      `Prelude.hashWithSalt` appId

instance Prelude.NFData GenerateTemplate where
  rnf GenerateTemplate' {..} =
    Prelude.rnf templateFormat
      `Prelude.seq` Prelude.rnf appId

instance Core.ToHeaders GenerateTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GenerateTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GenerateTemplate where
  toJSON GenerateTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("templateFormat" Core..=)
              Prelude.<$> templateFormat,
            ("appId" Core..=) Prelude.<$> appId
          ]
      )

instance Core.ToPath GenerateTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery GenerateTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateTemplateResponse' smart constructor.
data GenerateTemplateResponse = GenerateTemplateResponse'
  { -- | The location of the Amazon S3 object.
    s3Location :: Prelude.Maybe S3Location,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'generateTemplateResponse_s3Location' - The location of the Amazon S3 object.
--
-- 'httpStatus', 'generateTemplateResponse_httpStatus' - The response's http status code.
newGenerateTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateTemplateResponse
newGenerateTemplateResponse pHttpStatus_ =
  GenerateTemplateResponse'
    { s3Location =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The location of the Amazon S3 object.
generateTemplateResponse_s3Location :: Lens.Lens' GenerateTemplateResponse (Prelude.Maybe S3Location)
generateTemplateResponse_s3Location = Lens.lens (\GenerateTemplateResponse' {s3Location} -> s3Location) (\s@GenerateTemplateResponse' {} a -> s {s3Location = a} :: GenerateTemplateResponse)

-- | The response's http status code.
generateTemplateResponse_httpStatus :: Lens.Lens' GenerateTemplateResponse Prelude.Int
generateTemplateResponse_httpStatus = Lens.lens (\GenerateTemplateResponse' {httpStatus} -> httpStatus) (\s@GenerateTemplateResponse' {} a -> s {httpStatus = a} :: GenerateTemplateResponse)

instance Prelude.NFData GenerateTemplateResponse where
  rnf GenerateTemplateResponse' {..} =
    Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf httpStatus
