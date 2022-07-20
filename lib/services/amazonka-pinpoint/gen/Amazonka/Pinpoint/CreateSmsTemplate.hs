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
-- Module      : Amazonka.Pinpoint.CreateSmsTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the SMS
-- channel.
module Amazonka.Pinpoint.CreateSmsTemplate
  ( -- * Creating a Request
    CreateSmsTemplate (..),
    newCreateSmsTemplate,

    -- * Request Lenses
    createSmsTemplate_templateName,
    createSmsTemplate_sMSTemplateRequest,

    -- * Destructuring the Response
    CreateSmsTemplateResponse (..),
    newCreateSmsTemplateResponse,

    -- * Response Lenses
    createSmsTemplateResponse_httpStatus,
    createSmsTemplateResponse_createTemplateMessageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSmsTemplate' smart constructor.
data CreateSmsTemplate = CreateSmsTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    sMSTemplateRequest :: SMSTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSmsTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createSmsTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'sMSTemplateRequest', 'createSmsTemplate_sMSTemplateRequest' - Undocumented member.
newCreateSmsTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'sMSTemplateRequest'
  SMSTemplateRequest ->
  CreateSmsTemplate
newCreateSmsTemplate
  pTemplateName_
  pSMSTemplateRequest_ =
    CreateSmsTemplate'
      { templateName = pTemplateName_,
        sMSTemplateRequest = pSMSTemplateRequest_
      }

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
createSmsTemplate_templateName :: Lens.Lens' CreateSmsTemplate Prelude.Text
createSmsTemplate_templateName = Lens.lens (\CreateSmsTemplate' {templateName} -> templateName) (\s@CreateSmsTemplate' {} a -> s {templateName = a} :: CreateSmsTemplate)

-- | Undocumented member.
createSmsTemplate_sMSTemplateRequest :: Lens.Lens' CreateSmsTemplate SMSTemplateRequest
createSmsTemplate_sMSTemplateRequest = Lens.lens (\CreateSmsTemplate' {sMSTemplateRequest} -> sMSTemplateRequest) (\s@CreateSmsTemplate' {} a -> s {sMSTemplateRequest = a} :: CreateSmsTemplate)

instance Core.AWSRequest CreateSmsTemplate where
  type
    AWSResponse CreateSmsTemplate =
      CreateSmsTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSmsTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateSmsTemplate where
  hashWithSalt _salt CreateSmsTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` sMSTemplateRequest

instance Prelude.NFData CreateSmsTemplate where
  rnf CreateSmsTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf sMSTemplateRequest

instance Core.ToHeaders CreateSmsTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSmsTemplate where
  toJSON CreateSmsTemplate' {..} =
    Core.toJSON sMSTemplateRequest

instance Core.ToPath CreateSmsTemplate where
  toPath CreateSmsTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/sms"]

instance Core.ToQuery CreateSmsTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSmsTemplateResponse' smart constructor.
data CreateSmsTemplateResponse = CreateSmsTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    createTemplateMessageBody :: CreateTemplateMessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSmsTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSmsTemplateResponse_httpStatus' - The response's http status code.
--
-- 'createTemplateMessageBody', 'createSmsTemplateResponse_createTemplateMessageBody' - Undocumented member.
newCreateSmsTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateSmsTemplateResponse
newCreateSmsTemplateResponse
  pHttpStatus_
  pCreateTemplateMessageBody_ =
    CreateSmsTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        createTemplateMessageBody =
          pCreateTemplateMessageBody_
      }

-- | The response's http status code.
createSmsTemplateResponse_httpStatus :: Lens.Lens' CreateSmsTemplateResponse Prelude.Int
createSmsTemplateResponse_httpStatus = Lens.lens (\CreateSmsTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateSmsTemplateResponse' {} a -> s {httpStatus = a} :: CreateSmsTemplateResponse)

-- | Undocumented member.
createSmsTemplateResponse_createTemplateMessageBody :: Lens.Lens' CreateSmsTemplateResponse CreateTemplateMessageBody
createSmsTemplateResponse_createTemplateMessageBody = Lens.lens (\CreateSmsTemplateResponse' {createTemplateMessageBody} -> createTemplateMessageBody) (\s@CreateSmsTemplateResponse' {} a -> s {createTemplateMessageBody = a} :: CreateSmsTemplateResponse)

instance Prelude.NFData CreateSmsTemplateResponse where
  rnf CreateSmsTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTemplateMessageBody
