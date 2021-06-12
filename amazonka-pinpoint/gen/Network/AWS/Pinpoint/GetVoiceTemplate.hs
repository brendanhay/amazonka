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
-- Module      : Network.AWS.Pinpoint.GetVoiceTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through the voice channel.
module Network.AWS.Pinpoint.GetVoiceTemplate
  ( -- * Creating a Request
    GetVoiceTemplate (..),
    newGetVoiceTemplate,

    -- * Request Lenses
    getVoiceTemplate_version,
    getVoiceTemplate_templateName,

    -- * Destructuring the Response
    GetVoiceTemplateResponse (..),
    newGetVoiceTemplateResponse,

    -- * Response Lenses
    getVoiceTemplateResponse_httpStatus,
    getVoiceTemplateResponse_voiceTemplateResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetVoiceTemplate' smart constructor.
data GetVoiceTemplate = GetVoiceTemplate'
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
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetVoiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getVoiceTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'getVoiceTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newGetVoiceTemplate ::
  -- | 'templateName'
  Core.Text ->
  GetVoiceTemplate
newGetVoiceTemplate pTemplateName_ =
  GetVoiceTemplate'
    { version = Core.Nothing,
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
getVoiceTemplate_version :: Lens.Lens' GetVoiceTemplate (Core.Maybe Core.Text)
getVoiceTemplate_version = Lens.lens (\GetVoiceTemplate' {version} -> version) (\s@GetVoiceTemplate' {} a -> s {version = a} :: GetVoiceTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getVoiceTemplate_templateName :: Lens.Lens' GetVoiceTemplate Core.Text
getVoiceTemplate_templateName = Lens.lens (\GetVoiceTemplate' {templateName} -> templateName) (\s@GetVoiceTemplate' {} a -> s {templateName = a} :: GetVoiceTemplate)

instance Core.AWSRequest GetVoiceTemplate where
  type
    AWSResponse GetVoiceTemplate =
      GetVoiceTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetVoiceTemplate

instance Core.NFData GetVoiceTemplate

instance Core.ToHeaders GetVoiceTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetVoiceTemplate where
  toPath GetVoiceTemplate' {..} =
    Core.mconcat
      ["/v1/templates/", Core.toBS templateName, "/voice"]

instance Core.ToQuery GetVoiceTemplate where
  toQuery GetVoiceTemplate' {..} =
    Core.mconcat ["version" Core.=: version]

-- | /See:/ 'newGetVoiceTemplateResponse' smart constructor.
data GetVoiceTemplateResponse = GetVoiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    voiceTemplateResponse :: VoiceTemplateResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetVoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getVoiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'voiceTemplateResponse', 'getVoiceTemplateResponse_voiceTemplateResponse' - Undocumented member.
newGetVoiceTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'voiceTemplateResponse'
  VoiceTemplateResponse ->
  GetVoiceTemplateResponse
newGetVoiceTemplateResponse
  pHttpStatus_
  pVoiceTemplateResponse_ =
    GetVoiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        voiceTemplateResponse = pVoiceTemplateResponse_
      }

-- | The response's http status code.
getVoiceTemplateResponse_httpStatus :: Lens.Lens' GetVoiceTemplateResponse Core.Int
getVoiceTemplateResponse_httpStatus = Lens.lens (\GetVoiceTemplateResponse' {httpStatus} -> httpStatus) (\s@GetVoiceTemplateResponse' {} a -> s {httpStatus = a} :: GetVoiceTemplateResponse)

-- | Undocumented member.
getVoiceTemplateResponse_voiceTemplateResponse :: Lens.Lens' GetVoiceTemplateResponse VoiceTemplateResponse
getVoiceTemplateResponse_voiceTemplateResponse = Lens.lens (\GetVoiceTemplateResponse' {voiceTemplateResponse} -> voiceTemplateResponse) (\s@GetVoiceTemplateResponse' {} a -> s {voiceTemplateResponse = a} :: GetVoiceTemplateResponse)

instance Core.NFData GetVoiceTemplateResponse
