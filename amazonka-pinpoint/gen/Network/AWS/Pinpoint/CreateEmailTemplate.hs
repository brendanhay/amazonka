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
-- Module      : Network.AWS.Pinpoint.CreateEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the email
-- channel.
module Network.AWS.Pinpoint.CreateEmailTemplate
  ( -- * Creating a Request
    CreateEmailTemplate (..),
    newCreateEmailTemplate,

    -- * Request Lenses
    createEmailTemplate_templateName,
    createEmailTemplate_emailTemplateRequest,

    -- * Destructuring the Response
    CreateEmailTemplateResponse (..),
    newCreateEmailTemplateResponse,

    -- * Response Lenses
    createEmailTemplateResponse_httpStatus,
    createEmailTemplateResponse_createTemplateMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEmailTemplate' smart constructor.
data CreateEmailTemplate = CreateEmailTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    emailTemplateRequest :: EmailTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createEmailTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'emailTemplateRequest', 'createEmailTemplate_emailTemplateRequest' - Undocumented member.
newCreateEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'emailTemplateRequest'
  EmailTemplateRequest ->
  CreateEmailTemplate
newCreateEmailTemplate
  pTemplateName_
  pEmailTemplateRequest_ =
    CreateEmailTemplate'
      { templateName = pTemplateName_,
        emailTemplateRequest = pEmailTemplateRequest_
      }

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
createEmailTemplate_templateName :: Lens.Lens' CreateEmailTemplate Prelude.Text
createEmailTemplate_templateName = Lens.lens (\CreateEmailTemplate' {templateName} -> templateName) (\s@CreateEmailTemplate' {} a -> s {templateName = a} :: CreateEmailTemplate)

-- | Undocumented member.
createEmailTemplate_emailTemplateRequest :: Lens.Lens' CreateEmailTemplate EmailTemplateRequest
createEmailTemplate_emailTemplateRequest = Lens.lens (\CreateEmailTemplate' {emailTemplateRequest} -> emailTemplateRequest) (\s@CreateEmailTemplate' {} a -> s {emailTemplateRequest = a} :: CreateEmailTemplate)

instance Prelude.AWSRequest CreateEmailTemplate where
  type
    Rs CreateEmailTemplate =
      CreateEmailTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable CreateEmailTemplate

instance Prelude.NFData CreateEmailTemplate

instance Prelude.ToHeaders CreateEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateEmailTemplate where
  toJSON CreateEmailTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EmailTemplateRequest"
                  Prelude..= emailTemplateRequest
              )
          ]
      )

instance Prelude.ToPath CreateEmailTemplate where
  toPath CreateEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v1/templates/",
        Prelude.toBS templateName,
        "/email"
      ]

instance Prelude.ToQuery CreateEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEmailTemplateResponse' smart constructor.
data CreateEmailTemplateResponse = CreateEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    createTemplateMessageBody :: CreateTemplateMessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEmailTemplateResponse_httpStatus' - The response's http status code.
--
-- 'createTemplateMessageBody', 'createEmailTemplateResponse_createTemplateMessageBody' - Undocumented member.
newCreateEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateEmailTemplateResponse
newCreateEmailTemplateResponse
  pHttpStatus_
  pCreateTemplateMessageBody_ =
    CreateEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        createTemplateMessageBody =
          pCreateTemplateMessageBody_
      }

-- | The response's http status code.
createEmailTemplateResponse_httpStatus :: Lens.Lens' CreateEmailTemplateResponse Prelude.Int
createEmailTemplateResponse_httpStatus = Lens.lens (\CreateEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateEmailTemplateResponse' {} a -> s {httpStatus = a} :: CreateEmailTemplateResponse)

-- | Undocumented member.
createEmailTemplateResponse_createTemplateMessageBody :: Lens.Lens' CreateEmailTemplateResponse CreateTemplateMessageBody
createEmailTemplateResponse_createTemplateMessageBody = Lens.lens (\CreateEmailTemplateResponse' {createTemplateMessageBody} -> createTemplateMessageBody) (\s@CreateEmailTemplateResponse' {} a -> s {createTemplateMessageBody = a} :: CreateEmailTemplateResponse)

instance Prelude.NFData CreateEmailTemplateResponse
