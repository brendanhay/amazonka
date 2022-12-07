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
-- Module      : Amazonka.Pinpoint.GetEmailTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through the email channel.
module Amazonka.Pinpoint.GetEmailTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetEmailTemplate
newGetEmailTemplate pTemplateName_ =
  GetEmailTemplate'
    { version = Prelude.Nothing,
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
getEmailTemplate_version :: Lens.Lens' GetEmailTemplate (Prelude.Maybe Prelude.Text)
getEmailTemplate_version = Lens.lens (\GetEmailTemplate' {version} -> version) (\s@GetEmailTemplate' {} a -> s {version = a} :: GetEmailTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getEmailTemplate_templateName :: Lens.Lens' GetEmailTemplate Prelude.Text
getEmailTemplate_templateName = Lens.lens (\GetEmailTemplate' {templateName} -> templateName) (\s@GetEmailTemplate' {} a -> s {templateName = a} :: GetEmailTemplate)

instance Core.AWSRequest GetEmailTemplate where
  type
    AWSResponse GetEmailTemplate =
      GetEmailTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetEmailTemplate where
  hashWithSalt _salt GetEmailTemplate' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetEmailTemplate where
  rnf GetEmailTemplate' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders GetEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEmailTemplate where
  toPath GetEmailTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/email"]

instance Data.ToQuery GetEmailTemplate where
  toQuery GetEmailTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newGetEmailTemplateResponse' smart constructor.
data GetEmailTemplateResponse = GetEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    emailTemplateResponse :: EmailTemplateResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getEmailTemplateResponse_httpStatus :: Lens.Lens' GetEmailTemplateResponse Prelude.Int
getEmailTemplateResponse_httpStatus = Lens.lens (\GetEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@GetEmailTemplateResponse' {} a -> s {httpStatus = a} :: GetEmailTemplateResponse)

-- | Undocumented member.
getEmailTemplateResponse_emailTemplateResponse :: Lens.Lens' GetEmailTemplateResponse EmailTemplateResponse
getEmailTemplateResponse_emailTemplateResponse = Lens.lens (\GetEmailTemplateResponse' {emailTemplateResponse} -> emailTemplateResponse) (\s@GetEmailTemplateResponse' {} a -> s {emailTemplateResponse = a} :: GetEmailTemplateResponse)

instance Prelude.NFData GetEmailTemplateResponse where
  rnf GetEmailTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf emailTemplateResponse
