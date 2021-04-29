{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.CreateSmsTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the SMS
-- channel.
module Network.AWS.Pinpoint.CreateSmsTemplate
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSmsTemplate' smart constructor.
data CreateSmsTemplate = CreateSmsTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    sMSTemplateRequest :: SMSTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreateSmsTemplate where
  type Rs CreateSmsTemplate = CreateSmsTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSmsTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable CreateSmsTemplate

instance Prelude.NFData CreateSmsTemplate

instance Prelude.ToHeaders CreateSmsTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateSmsTemplate where
  toJSON CreateSmsTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SMSTemplateRequest"
                  Prelude..= sMSTemplateRequest
              )
          ]
      )

instance Prelude.ToPath CreateSmsTemplate where
  toPath CreateSmsTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Prelude.toBS templateName, "/sms"]

instance Prelude.ToQuery CreateSmsTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSmsTemplateResponse' smart constructor.
data CreateSmsTemplateResponse = CreateSmsTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    createTemplateMessageBody :: CreateTemplateMessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateSmsTemplateResponse
