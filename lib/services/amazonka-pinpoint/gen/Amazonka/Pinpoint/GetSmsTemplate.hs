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
-- Module      : Amazonka.Pinpoint.GetSmsTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through the SMS channel.
module Amazonka.Pinpoint.GetSmsTemplate
  ( -- * Creating a Request
    GetSmsTemplate (..),
    newGetSmsTemplate,

    -- * Request Lenses
    getSmsTemplate_version,
    getSmsTemplate_templateName,

    -- * Destructuring the Response
    GetSmsTemplateResponse (..),
    newGetSmsTemplateResponse,

    -- * Response Lenses
    getSmsTemplateResponse_httpStatus,
    getSmsTemplateResponse_sMSTemplateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSmsTemplate' smart constructor.
data GetSmsTemplate = GetSmsTemplate'
  { -- | The unique identifier for the version of the message template to update,
    -- retrieve information about, or delete. To retrieve identifiers and other
    -- information for all the versions of a template, use the Template
    -- Versions resource.
    --
    -- If specified, this value must match the identifier for an existing
    -- template version. If specified for an update operation, this value must
    -- match the identifier for the latest existing version of the template.
    -- This restriction helps ensure that race conditions don\'t occur.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint does
    -- the following:
    --
    -- -   For a get operation, retrieves information about the active version
    --     of the template.
    --
    -- -   For an update operation, saves the updates to (overwrites) the
    --     latest existing version of the template, if the create-new-version
    --     parameter isn\'t used or is set to false.
    --
    -- -   For a delete operation, deletes the template, including all versions
    --     of the template.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSmsTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getSmsTemplate_version' - The unique identifier for the version of the message template to update,
-- retrieve information about, or delete. To retrieve identifiers and other
-- information for all the versions of a template, use the Template
-- Versions resource.
--
-- If specified, this value must match the identifier for an existing
-- template version. If specified for an update operation, this value must
-- match the identifier for the latest existing version of the template.
-- This restriction helps ensure that race conditions don\'t occur.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint does
-- the following:
--
-- -   For a get operation, retrieves information about the active version
--     of the template.
--
-- -   For an update operation, saves the updates to (overwrites) the
--     latest existing version of the template, if the create-new-version
--     parameter isn\'t used or is set to false.
--
-- -   For a delete operation, deletes the template, including all versions
--     of the template.
--
-- 'templateName', 'getSmsTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newGetSmsTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  GetSmsTemplate
newGetSmsTemplate pTemplateName_ =
  GetSmsTemplate'
    { version = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update,
-- retrieve information about, or delete. To retrieve identifiers and other
-- information for all the versions of a template, use the Template
-- Versions resource.
--
-- If specified, this value must match the identifier for an existing
-- template version. If specified for an update operation, this value must
-- match the identifier for the latest existing version of the template.
-- This restriction helps ensure that race conditions don\'t occur.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint does
-- the following:
--
-- -   For a get operation, retrieves information about the active version
--     of the template.
--
-- -   For an update operation, saves the updates to (overwrites) the
--     latest existing version of the template, if the create-new-version
--     parameter isn\'t used or is set to false.
--
-- -   For a delete operation, deletes the template, including all versions
--     of the template.
getSmsTemplate_version :: Lens.Lens' GetSmsTemplate (Prelude.Maybe Prelude.Text)
getSmsTemplate_version = Lens.lens (\GetSmsTemplate' {version} -> version) (\s@GetSmsTemplate' {} a -> s {version = a} :: GetSmsTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getSmsTemplate_templateName :: Lens.Lens' GetSmsTemplate Prelude.Text
getSmsTemplate_templateName = Lens.lens (\GetSmsTemplate' {templateName} -> templateName) (\s@GetSmsTemplate' {} a -> s {templateName = a} :: GetSmsTemplate)

instance Core.AWSRequest GetSmsTemplate where
  type
    AWSResponse GetSmsTemplate =
      GetSmsTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSmsTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetSmsTemplate where
  hashWithSalt _salt GetSmsTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetSmsTemplate where
  rnf GetSmsTemplate' {..} =
    Prelude.rnf version `Prelude.seq`
      Prelude.rnf templateName

instance Data.ToHeaders GetSmsTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSmsTemplate where
  toPath GetSmsTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/sms"]

instance Data.ToQuery GetSmsTemplate where
  toQuery GetSmsTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newGetSmsTemplateResponse' smart constructor.
data GetSmsTemplateResponse = GetSmsTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    sMSTemplateResponse :: SMSTemplateResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSmsTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSmsTemplateResponse_httpStatus' - The response's http status code.
--
-- 'sMSTemplateResponse', 'getSmsTemplateResponse_sMSTemplateResponse' - Undocumented member.
newGetSmsTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sMSTemplateResponse'
  SMSTemplateResponse ->
  GetSmsTemplateResponse
newGetSmsTemplateResponse
  pHttpStatus_
  pSMSTemplateResponse_ =
    GetSmsTemplateResponse'
      { httpStatus = pHttpStatus_,
        sMSTemplateResponse = pSMSTemplateResponse_
      }

-- | The response's http status code.
getSmsTemplateResponse_httpStatus :: Lens.Lens' GetSmsTemplateResponse Prelude.Int
getSmsTemplateResponse_httpStatus = Lens.lens (\GetSmsTemplateResponse' {httpStatus} -> httpStatus) (\s@GetSmsTemplateResponse' {} a -> s {httpStatus = a} :: GetSmsTemplateResponse)

-- | Undocumented member.
getSmsTemplateResponse_sMSTemplateResponse :: Lens.Lens' GetSmsTemplateResponse SMSTemplateResponse
getSmsTemplateResponse_sMSTemplateResponse = Lens.lens (\GetSmsTemplateResponse' {sMSTemplateResponse} -> sMSTemplateResponse) (\s@GetSmsTemplateResponse' {} a -> s {sMSTemplateResponse = a} :: GetSmsTemplateResponse)

instance Prelude.NFData GetSmsTemplateResponse where
  rnf GetSmsTemplateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf sMSTemplateResponse
