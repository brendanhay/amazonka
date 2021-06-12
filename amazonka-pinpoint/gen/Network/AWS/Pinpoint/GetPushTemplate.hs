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
-- Module      : Network.AWS.Pinpoint.GetPushTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through a push notification channel.
module Network.AWS.Pinpoint.GetPushTemplate
  ( -- * Creating a Request
    GetPushTemplate (..),
    newGetPushTemplate,

    -- * Request Lenses
    getPushTemplate_version,
    getPushTemplate_templateName,

    -- * Destructuring the Response
    GetPushTemplateResponse (..),
    newGetPushTemplateResponse,

    -- * Response Lenses
    getPushTemplateResponse_httpStatus,
    getPushTemplateResponse_pushNotificationTemplateResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPushTemplate' smart constructor.
data GetPushTemplate = GetPushTemplate'
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
-- Create a value of 'GetPushTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getPushTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'getPushTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newGetPushTemplate ::
  -- | 'templateName'
  Core.Text ->
  GetPushTemplate
newGetPushTemplate pTemplateName_ =
  GetPushTemplate'
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
getPushTemplate_version :: Lens.Lens' GetPushTemplate (Core.Maybe Core.Text)
getPushTemplate_version = Lens.lens (\GetPushTemplate' {version} -> version) (\s@GetPushTemplate' {} a -> s {version = a} :: GetPushTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getPushTemplate_templateName :: Lens.Lens' GetPushTemplate Core.Text
getPushTemplate_templateName = Lens.lens (\GetPushTemplate' {templateName} -> templateName) (\s@GetPushTemplate' {} a -> s {templateName = a} :: GetPushTemplate)

instance Core.AWSRequest GetPushTemplate where
  type
    AWSResponse GetPushTemplate =
      GetPushTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPushTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetPushTemplate

instance Core.NFData GetPushTemplate

instance Core.ToHeaders GetPushTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetPushTemplate where
  toPath GetPushTemplate' {..} =
    Core.mconcat
      ["/v1/templates/", Core.toBS templateName, "/push"]

instance Core.ToQuery GetPushTemplate where
  toQuery GetPushTemplate' {..} =
    Core.mconcat ["version" Core.=: version]

-- | /See:/ 'newGetPushTemplateResponse' smart constructor.
data GetPushTemplateResponse = GetPushTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    pushNotificationTemplateResponse :: PushNotificationTemplateResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPushTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPushTemplateResponse_httpStatus' - The response's http status code.
--
-- 'pushNotificationTemplateResponse', 'getPushTemplateResponse_pushNotificationTemplateResponse' - Undocumented member.
newGetPushTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'pushNotificationTemplateResponse'
  PushNotificationTemplateResponse ->
  GetPushTemplateResponse
newGetPushTemplateResponse
  pHttpStatus_
  pPushNotificationTemplateResponse_ =
    GetPushTemplateResponse'
      { httpStatus = pHttpStatus_,
        pushNotificationTemplateResponse =
          pPushNotificationTemplateResponse_
      }

-- | The response's http status code.
getPushTemplateResponse_httpStatus :: Lens.Lens' GetPushTemplateResponse Core.Int
getPushTemplateResponse_httpStatus = Lens.lens (\GetPushTemplateResponse' {httpStatus} -> httpStatus) (\s@GetPushTemplateResponse' {} a -> s {httpStatus = a} :: GetPushTemplateResponse)

-- | Undocumented member.
getPushTemplateResponse_pushNotificationTemplateResponse :: Lens.Lens' GetPushTemplateResponse PushNotificationTemplateResponse
getPushTemplateResponse_pushNotificationTemplateResponse = Lens.lens (\GetPushTemplateResponse' {pushNotificationTemplateResponse} -> pushNotificationTemplateResponse) (\s@GetPushTemplateResponse' {} a -> s {pushNotificationTemplateResponse = a} :: GetPushTemplateResponse)

instance Core.NFData GetPushTemplateResponse
