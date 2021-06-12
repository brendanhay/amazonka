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
-- Module      : Network.AWS.Pinpoint.CreatePushTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through a push
-- notification channel.
module Network.AWS.Pinpoint.CreatePushTemplate
  ( -- * Creating a Request
    CreatePushTemplate (..),
    newCreatePushTemplate,

    -- * Request Lenses
    createPushTemplate_templateName,
    createPushTemplate_pushNotificationTemplateRequest,

    -- * Destructuring the Response
    CreatePushTemplateResponse (..),
    newCreatePushTemplateResponse,

    -- * Response Lenses
    createPushTemplateResponse_httpStatus,
    createPushTemplateResponse_createTemplateMessageBody,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePushTemplate' smart constructor.
data CreatePushTemplate = CreatePushTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Core.Text,
    pushNotificationTemplateRequest :: PushNotificationTemplateRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePushTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createPushTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'pushNotificationTemplateRequest', 'createPushTemplate_pushNotificationTemplateRequest' - Undocumented member.
newCreatePushTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'pushNotificationTemplateRequest'
  PushNotificationTemplateRequest ->
  CreatePushTemplate
newCreatePushTemplate
  pTemplateName_
  pPushNotificationTemplateRequest_ =
    CreatePushTemplate'
      { templateName = pTemplateName_,
        pushNotificationTemplateRequest =
          pPushNotificationTemplateRequest_
      }

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
createPushTemplate_templateName :: Lens.Lens' CreatePushTemplate Core.Text
createPushTemplate_templateName = Lens.lens (\CreatePushTemplate' {templateName} -> templateName) (\s@CreatePushTemplate' {} a -> s {templateName = a} :: CreatePushTemplate)

-- | Undocumented member.
createPushTemplate_pushNotificationTemplateRequest :: Lens.Lens' CreatePushTemplate PushNotificationTemplateRequest
createPushTemplate_pushNotificationTemplateRequest = Lens.lens (\CreatePushTemplate' {pushNotificationTemplateRequest} -> pushNotificationTemplateRequest) (\s@CreatePushTemplate' {} a -> s {pushNotificationTemplateRequest = a} :: CreatePushTemplate)

instance Core.AWSRequest CreatePushTemplate where
  type
    AWSResponse CreatePushTemplate =
      CreatePushTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePushTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable CreatePushTemplate

instance Core.NFData CreatePushTemplate

instance Core.ToHeaders CreatePushTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePushTemplate where
  toJSON CreatePushTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "PushNotificationTemplateRequest"
                  Core..= pushNotificationTemplateRequest
              )
          ]
      )

instance Core.ToPath CreatePushTemplate where
  toPath CreatePushTemplate' {..} =
    Core.mconcat
      ["/v1/templates/", Core.toBS templateName, "/push"]

instance Core.ToQuery CreatePushTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePushTemplateResponse' smart constructor.
data CreatePushTemplateResponse = CreatePushTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    createTemplateMessageBody :: CreateTemplateMessageBody
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePushTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPushTemplateResponse_httpStatus' - The response's http status code.
--
-- 'createTemplateMessageBody', 'createPushTemplateResponse_createTemplateMessageBody' - Undocumented member.
newCreatePushTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreatePushTemplateResponse
newCreatePushTemplateResponse
  pHttpStatus_
  pCreateTemplateMessageBody_ =
    CreatePushTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        createTemplateMessageBody =
          pCreateTemplateMessageBody_
      }

-- | The response's http status code.
createPushTemplateResponse_httpStatus :: Lens.Lens' CreatePushTemplateResponse Core.Int
createPushTemplateResponse_httpStatus = Lens.lens (\CreatePushTemplateResponse' {httpStatus} -> httpStatus) (\s@CreatePushTemplateResponse' {} a -> s {httpStatus = a} :: CreatePushTemplateResponse)

-- | Undocumented member.
createPushTemplateResponse_createTemplateMessageBody :: Lens.Lens' CreatePushTemplateResponse CreateTemplateMessageBody
createPushTemplateResponse_createTemplateMessageBody = Lens.lens (\CreatePushTemplateResponse' {createTemplateMessageBody} -> createTemplateMessageBody) (\s@CreatePushTemplateResponse' {} a -> s {createTemplateMessageBody = a} :: CreatePushTemplateResponse)

instance Core.NFData CreatePushTemplateResponse
