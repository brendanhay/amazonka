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
-- Module      : Amazonka.Pinpoint.UpdateVoiceTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through
-- the voice channel.
module Amazonka.Pinpoint.UpdateVoiceTemplate
  ( -- * Creating a Request
    UpdateVoiceTemplate (..),
    newUpdateVoiceTemplate,

    -- * Request Lenses
    updateVoiceTemplate_createNewVersion,
    updateVoiceTemplate_version,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVoiceTemplate' smart constructor.
data UpdateVoiceTemplate = UpdateVoiceTemplate'
  { -- | Specifies whether to save the updates as a new version of the message
    -- template. Valid values are: true, save the updates as a new version;
    -- and, false, save the updates to (overwrite) the latest existing version
    -- of the template.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
    -- the updates to (overwrites) the latest existing version of the template.
    -- If you specify a value of true for this parameter, don\'t specify a
    -- value for the version parameter. Otherwise, an error will occur.
    createNewVersion :: Prelude.Maybe Prelude.Bool,
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
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    voiceTemplateRequest :: VoiceTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'templateName', 'updateVoiceTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'voiceTemplateRequest', 'updateVoiceTemplate_voiceTemplateRequest' - Undocumented member.
newUpdateVoiceTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'voiceTemplateRequest'
  VoiceTemplateRequest ->
  UpdateVoiceTemplate
newUpdateVoiceTemplate
  pTemplateName_
  pVoiceTemplateRequest_ =
    UpdateVoiceTemplate'
      { createNewVersion =
          Prelude.Nothing,
        version = Prelude.Nothing,
        templateName = pTemplateName_,
        voiceTemplateRequest = pVoiceTemplateRequest_
      }

-- | Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
updateVoiceTemplate_createNewVersion :: Lens.Lens' UpdateVoiceTemplate (Prelude.Maybe Prelude.Bool)
updateVoiceTemplate_createNewVersion = Lens.lens (\UpdateVoiceTemplate' {createNewVersion} -> createNewVersion) (\s@UpdateVoiceTemplate' {} a -> s {createNewVersion = a} :: UpdateVoiceTemplate)

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
updateVoiceTemplate_version :: Lens.Lens' UpdateVoiceTemplate (Prelude.Maybe Prelude.Text)
updateVoiceTemplate_version = Lens.lens (\UpdateVoiceTemplate' {version} -> version) (\s@UpdateVoiceTemplate' {} a -> s {version = a} :: UpdateVoiceTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
updateVoiceTemplate_templateName :: Lens.Lens' UpdateVoiceTemplate Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateVoiceTemplate where
  hashWithSalt _salt UpdateVoiceTemplate' {..} =
    _salt `Prelude.hashWithSalt` createNewVersion
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` voiceTemplateRequest

instance Prelude.NFData UpdateVoiceTemplate where
  rnf UpdateVoiceTemplate' {..} =
    Prelude.rnf createNewVersion
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf voiceTemplateRequest

instance Core.ToHeaders UpdateVoiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateVoiceTemplate where
  toJSON UpdateVoiceTemplate' {..} =
    Core.toJSON voiceTemplateRequest

instance Core.ToPath UpdateVoiceTemplate where
  toPath UpdateVoiceTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/voice"]

instance Core.ToQuery UpdateVoiceTemplate where
  toQuery UpdateVoiceTemplate' {..} =
    Prelude.mconcat
      [ "create-new-version" Core.=: createNewVersion,
        "version" Core.=: version
      ]

-- | /See:/ 'newUpdateVoiceTemplateResponse' smart constructor.
data UpdateVoiceTemplateResponse = UpdateVoiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
updateVoiceTemplateResponse_httpStatus :: Lens.Lens' UpdateVoiceTemplateResponse Prelude.Int
updateVoiceTemplateResponse_httpStatus = Lens.lens (\UpdateVoiceTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceTemplateResponse' {} a -> s {httpStatus = a} :: UpdateVoiceTemplateResponse)

-- | Undocumented member.
updateVoiceTemplateResponse_messageBody :: Lens.Lens' UpdateVoiceTemplateResponse MessageBody
updateVoiceTemplateResponse_messageBody = Lens.lens (\UpdateVoiceTemplateResponse' {messageBody} -> messageBody) (\s@UpdateVoiceTemplateResponse' {} a -> s {messageBody = a} :: UpdateVoiceTemplateResponse)

instance Prelude.NFData UpdateVoiceTemplateResponse where
  rnf UpdateVoiceTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageBody
