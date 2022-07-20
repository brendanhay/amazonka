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
-- Module      : Amazonka.Pinpoint.CreatePushTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through a push
-- notification channel.
module Amazonka.Pinpoint.CreatePushTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePushTemplate' smart constructor.
data CreatePushTemplate = CreatePushTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    pushNotificationTemplateRequest :: PushNotificationTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
createPushTemplate_templateName :: Lens.Lens' CreatePushTemplate Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreatePushTemplate where
  hashWithSalt _salt CreatePushTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` pushNotificationTemplateRequest

instance Prelude.NFData CreatePushTemplate where
  rnf CreatePushTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf pushNotificationTemplateRequest

instance Core.ToHeaders CreatePushTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePushTemplate where
  toJSON CreatePushTemplate' {..} =
    Core.toJSON pushNotificationTemplateRequest

instance Core.ToPath CreatePushTemplate where
  toPath CreatePushTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/push"]

instance Core.ToQuery CreatePushTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePushTemplateResponse' smart constructor.
data CreatePushTemplateResponse = CreatePushTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    createTemplateMessageBody :: CreateTemplateMessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
createPushTemplateResponse_httpStatus :: Lens.Lens' CreatePushTemplateResponse Prelude.Int
createPushTemplateResponse_httpStatus = Lens.lens (\CreatePushTemplateResponse' {httpStatus} -> httpStatus) (\s@CreatePushTemplateResponse' {} a -> s {httpStatus = a} :: CreatePushTemplateResponse)

-- | Undocumented member.
createPushTemplateResponse_createTemplateMessageBody :: Lens.Lens' CreatePushTemplateResponse CreateTemplateMessageBody
createPushTemplateResponse_createTemplateMessageBody = Lens.lens (\CreatePushTemplateResponse' {createTemplateMessageBody} -> createTemplateMessageBody) (\s@CreatePushTemplateResponse' {} a -> s {createTemplateMessageBody = a} :: CreatePushTemplateResponse)

instance Prelude.NFData CreatePushTemplateResponse where
  rnf CreatePushTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTemplateMessageBody
