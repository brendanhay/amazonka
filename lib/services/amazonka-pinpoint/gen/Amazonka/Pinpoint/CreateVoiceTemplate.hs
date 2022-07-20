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
-- Module      : Amazonka.Pinpoint.CreateVoiceTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a message template for messages that are sent through the voice
-- channel.
module Amazonka.Pinpoint.CreateVoiceTemplate
  ( -- * Creating a Request
    CreateVoiceTemplate (..),
    newCreateVoiceTemplate,

    -- * Request Lenses
    createVoiceTemplate_templateName,
    createVoiceTemplate_voiceTemplateRequest,

    -- * Destructuring the Response
    CreateVoiceTemplateResponse (..),
    newCreateVoiceTemplateResponse,

    -- * Response Lenses
    createVoiceTemplateResponse_httpStatus,
    createVoiceTemplateResponse_createTemplateMessageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVoiceTemplate' smart constructor.
data CreateVoiceTemplate = CreateVoiceTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    voiceTemplateRequest :: VoiceTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createVoiceTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'voiceTemplateRequest', 'createVoiceTemplate_voiceTemplateRequest' - Undocumented member.
newCreateVoiceTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'voiceTemplateRequest'
  VoiceTemplateRequest ->
  CreateVoiceTemplate
newCreateVoiceTemplate
  pTemplateName_
  pVoiceTemplateRequest_ =
    CreateVoiceTemplate'
      { templateName = pTemplateName_,
        voiceTemplateRequest = pVoiceTemplateRequest_
      }

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
createVoiceTemplate_templateName :: Lens.Lens' CreateVoiceTemplate Prelude.Text
createVoiceTemplate_templateName = Lens.lens (\CreateVoiceTemplate' {templateName} -> templateName) (\s@CreateVoiceTemplate' {} a -> s {templateName = a} :: CreateVoiceTemplate)

-- | Undocumented member.
createVoiceTemplate_voiceTemplateRequest :: Lens.Lens' CreateVoiceTemplate VoiceTemplateRequest
createVoiceTemplate_voiceTemplateRequest = Lens.lens (\CreateVoiceTemplate' {voiceTemplateRequest} -> voiceTemplateRequest) (\s@CreateVoiceTemplate' {} a -> s {voiceTemplateRequest = a} :: CreateVoiceTemplate)

instance Core.AWSRequest CreateVoiceTemplate where
  type
    AWSResponse CreateVoiceTemplate =
      CreateVoiceTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVoiceTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateVoiceTemplate where
  hashWithSalt _salt CreateVoiceTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` voiceTemplateRequest

instance Prelude.NFData CreateVoiceTemplate where
  rnf CreateVoiceTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf voiceTemplateRequest

instance Core.ToHeaders CreateVoiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateVoiceTemplate where
  toJSON CreateVoiceTemplate' {..} =
    Core.toJSON voiceTemplateRequest

instance Core.ToPath CreateVoiceTemplate where
  toPath CreateVoiceTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/voice"]

instance Core.ToQuery CreateVoiceTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVoiceTemplateResponse' smart constructor.
data CreateVoiceTemplateResponse = CreateVoiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    createTemplateMessageBody :: CreateTemplateMessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVoiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'createTemplateMessageBody', 'createVoiceTemplateResponse_createTemplateMessageBody' - Undocumented member.
newCreateVoiceTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTemplateMessageBody'
  CreateTemplateMessageBody ->
  CreateVoiceTemplateResponse
newCreateVoiceTemplateResponse
  pHttpStatus_
  pCreateTemplateMessageBody_ =
    CreateVoiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        createTemplateMessageBody =
          pCreateTemplateMessageBody_
      }

-- | The response's http status code.
createVoiceTemplateResponse_httpStatus :: Lens.Lens' CreateVoiceTemplateResponse Prelude.Int
createVoiceTemplateResponse_httpStatus = Lens.lens (\CreateVoiceTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateVoiceTemplateResponse' {} a -> s {httpStatus = a} :: CreateVoiceTemplateResponse)

-- | Undocumented member.
createVoiceTemplateResponse_createTemplateMessageBody :: Lens.Lens' CreateVoiceTemplateResponse CreateTemplateMessageBody
createVoiceTemplateResponse_createTemplateMessageBody = Lens.lens (\CreateVoiceTemplateResponse' {createTemplateMessageBody} -> createTemplateMessageBody) (\s@CreateVoiceTemplateResponse' {} a -> s {createTemplateMessageBody = a} :: CreateVoiceTemplateResponse)

instance Prelude.NFData CreateVoiceTemplateResponse where
  rnf CreateVoiceTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTemplateMessageBody
