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
-- Module      : Network.AWS.Pinpoint.UpdateVoiceTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through
-- the voice channel.
module Network.AWS.Pinpoint.UpdateVoiceTemplate
  ( -- * Creating a Request
    UpdateVoiceTemplate (..),
    newUpdateVoiceTemplate,

    -- * Request Lenses
    updateVoiceTemplate_version,
    updateVoiceTemplate_createNewVersion,
    updateVoiceTemplate_templateName,
    updateVoiceTemplate_voiceTemplateRequest,

    -- * Destructuring the Response
    UpdateVoiceTemplateResponse (..),
    newUpdateVoiceTemplateResponse,

    -- * Response Lenses
    updateVoiceTemplateResponse_httpStatus,
    updateVoiceTemplateResponse_messageBody,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateVoiceTemplate' smart constructor.
data UpdateVoiceTemplate = UpdateVoiceTemplate'
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
    version :: Core.Maybe Core.Text,
    -- | Specifies whether to save the updates as a new version of the message
    -- template. Valid values are: true, save the updates as a new version;
    -- and, false, save the updates to (overwrite) the latest existing version
    -- of the template.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
    -- the updates to (overwrites) the latest existing version of the template.
    -- If you specify a value of true for this parameter, don\'t specify a
    -- value for the version parameter. Otherwise, an error will occur.
    createNewVersion :: Core.Maybe Core.Bool,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Core.Text,
    voiceTemplateRequest :: VoiceTemplateRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVoiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateVoiceTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'createNewVersion', 'updateVoiceTemplate_createNewVersion' - Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
--
-- 'templateName', 'updateVoiceTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'voiceTemplateRequest', 'updateVoiceTemplate_voiceTemplateRequest' - Undocumented member.
newUpdateVoiceTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'voiceTemplateRequest'
  VoiceTemplateRequest ->
  UpdateVoiceTemplate
newUpdateVoiceTemplate
  pTemplateName_
  pVoiceTemplateRequest_ =
    UpdateVoiceTemplate'
      { version = Core.Nothing,
        createNewVersion = Core.Nothing,
        templateName = pTemplateName_,
        voiceTemplateRequest = pVoiceTemplateRequest_
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
updateVoiceTemplate_version :: Lens.Lens' UpdateVoiceTemplate (Core.Maybe Core.Text)
updateVoiceTemplate_version = Lens.lens (\UpdateVoiceTemplate' {version} -> version) (\s@UpdateVoiceTemplate' {} a -> s {version = a} :: UpdateVoiceTemplate)

-- | Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
updateVoiceTemplate_createNewVersion :: Lens.Lens' UpdateVoiceTemplate (Core.Maybe Core.Bool)
updateVoiceTemplate_createNewVersion = Lens.lens (\UpdateVoiceTemplate' {createNewVersion} -> createNewVersion) (\s@UpdateVoiceTemplate' {} a -> s {createNewVersion = a} :: UpdateVoiceTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
updateVoiceTemplate_templateName :: Lens.Lens' UpdateVoiceTemplate Core.Text
updateVoiceTemplate_templateName = Lens.lens (\UpdateVoiceTemplate' {templateName} -> templateName) (\s@UpdateVoiceTemplate' {} a -> s {templateName = a} :: UpdateVoiceTemplate)

-- | Undocumented member.
updateVoiceTemplate_voiceTemplateRequest :: Lens.Lens' UpdateVoiceTemplate VoiceTemplateRequest
updateVoiceTemplate_voiceTemplateRequest = Lens.lens (\UpdateVoiceTemplate' {voiceTemplateRequest} -> voiceTemplateRequest) (\s@UpdateVoiceTemplate' {} a -> s {voiceTemplateRequest = a} :: UpdateVoiceTemplate)

instance Core.AWSRequest UpdateVoiceTemplate where
  type
    AWSResponse UpdateVoiceTemplate =
      UpdateVoiceTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable UpdateVoiceTemplate

instance Core.NFData UpdateVoiceTemplate

instance Core.ToHeaders UpdateVoiceTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateVoiceTemplate where
  toJSON UpdateVoiceTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "VoiceTemplateRequest"
                  Core..= voiceTemplateRequest
              )
          ]
      )

instance Core.ToPath UpdateVoiceTemplate where
  toPath UpdateVoiceTemplate' {..} =
    Core.mconcat
      ["/v1/templates/", Core.toBS templateName, "/voice"]

instance Core.ToQuery UpdateVoiceTemplate where
  toQuery UpdateVoiceTemplate' {..} =
    Core.mconcat
      [ "version" Core.=: version,
        "create-new-version" Core.=: createNewVersion
      ]

-- | /See:/ 'newUpdateVoiceTemplateResponse' smart constructor.
data UpdateVoiceTemplateResponse = UpdateVoiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    messageBody :: MessageBody
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateVoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVoiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updateVoiceTemplateResponse_messageBody' - Undocumented member.
newUpdateVoiceTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateVoiceTemplateResponse
newUpdateVoiceTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    UpdateVoiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
updateVoiceTemplateResponse_httpStatus :: Lens.Lens' UpdateVoiceTemplateResponse Core.Int
updateVoiceTemplateResponse_httpStatus = Lens.lens (\UpdateVoiceTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceTemplateResponse' {} a -> s {httpStatus = a} :: UpdateVoiceTemplateResponse)

-- | Undocumented member.
updateVoiceTemplateResponse_messageBody :: Lens.Lens' UpdateVoiceTemplateResponse MessageBody
updateVoiceTemplateResponse_messageBody = Lens.lens (\UpdateVoiceTemplateResponse' {messageBody} -> messageBody) (\s@UpdateVoiceTemplateResponse' {} a -> s {messageBody = a} :: UpdateVoiceTemplateResponse)

instance Core.NFData UpdateVoiceTemplateResponse
