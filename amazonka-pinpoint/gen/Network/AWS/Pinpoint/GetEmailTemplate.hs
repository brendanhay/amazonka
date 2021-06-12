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
-- Module      : Network.AWS.Pinpoint.GetEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through the email channel.
module Network.AWS.Pinpoint.GetEmailTemplate
  ( -- * Creating a Request
    GetEmailTemplate (..),
    newGetEmailTemplate,

    -- * Request Lenses
    getEmailTemplate_version,
    getEmailTemplate_templateName,

    -- * Destructuring the Response
    GetEmailTemplateResponse (..),
    newGetEmailTemplateResponse,

    -- * Response Lenses
    getEmailTemplateResponse_httpStatus,
    getEmailTemplateResponse_emailTemplateResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEmailTemplate' smart constructor.
data GetEmailTemplate = GetEmailTemplate'
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
-- Create a value of 'GetEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getEmailTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'getEmailTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newGetEmailTemplate ::
  -- | 'templateName'
  Core.Text ->
  GetEmailTemplate
newGetEmailTemplate pTemplateName_ =
  GetEmailTemplate'
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
getEmailTemplate_version :: Lens.Lens' GetEmailTemplate (Core.Maybe Core.Text)
getEmailTemplate_version = Lens.lens (\GetEmailTemplate' {version} -> version) (\s@GetEmailTemplate' {} a -> s {version = a} :: GetEmailTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getEmailTemplate_templateName :: Lens.Lens' GetEmailTemplate Core.Text
getEmailTemplate_templateName = Lens.lens (\GetEmailTemplate' {templateName} -> templateName) (\s@GetEmailTemplate' {} a -> s {templateName = a} :: GetEmailTemplate)

instance Core.AWSRequest GetEmailTemplate where
  type
    AWSResponse GetEmailTemplate =
      GetEmailTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetEmailTemplate

instance Core.NFData GetEmailTemplate

instance Core.ToHeaders GetEmailTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetEmailTemplate where
  toPath GetEmailTemplate' {..} =
    Core.mconcat
      ["/v1/templates/", Core.toBS templateName, "/email"]

instance Core.ToQuery GetEmailTemplate where
  toQuery GetEmailTemplate' {..} =
    Core.mconcat ["version" Core.=: version]

-- | /See:/ 'newGetEmailTemplateResponse' smart constructor.
data GetEmailTemplateResponse = GetEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    emailTemplateResponse :: EmailTemplateResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEmailTemplateResponse_httpStatus' - The response's http status code.
--
-- 'emailTemplateResponse', 'getEmailTemplateResponse_emailTemplateResponse' - Undocumented member.
newGetEmailTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'emailTemplateResponse'
  EmailTemplateResponse ->
  GetEmailTemplateResponse
newGetEmailTemplateResponse
  pHttpStatus_
  pEmailTemplateResponse_ =
    GetEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        emailTemplateResponse = pEmailTemplateResponse_
      }

-- | The response's http status code.
getEmailTemplateResponse_httpStatus :: Lens.Lens' GetEmailTemplateResponse Core.Int
getEmailTemplateResponse_httpStatus = Lens.lens (\GetEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@GetEmailTemplateResponse' {} a -> s {httpStatus = a} :: GetEmailTemplateResponse)

-- | Undocumented member.
getEmailTemplateResponse_emailTemplateResponse :: Lens.Lens' GetEmailTemplateResponse EmailTemplateResponse
getEmailTemplateResponse_emailTemplateResponse = Lens.lens (\GetEmailTemplateResponse' {emailTemplateResponse} -> emailTemplateResponse) (\s@GetEmailTemplateResponse' {} a -> s {emailTemplateResponse = a} :: GetEmailTemplateResponse)

instance Core.NFData GetEmailTemplateResponse
