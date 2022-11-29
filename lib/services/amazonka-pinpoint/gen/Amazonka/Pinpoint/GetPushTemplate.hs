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
-- Module      : Amazonka.Pinpoint.GetPushTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through a push notification channel.
module Amazonka.Pinpoint.GetPushTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetPushTemplate
newGetPushTemplate pTemplateName_ =
  GetPushTemplate'
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
getPushTemplate_version :: Lens.Lens' GetPushTemplate (Prelude.Maybe Prelude.Text)
getPushTemplate_version = Lens.lens (\GetPushTemplate' {version} -> version) (\s@GetPushTemplate' {} a -> s {version = a} :: GetPushTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getPushTemplate_templateName :: Lens.Lens' GetPushTemplate Prelude.Text
getPushTemplate_templateName = Lens.lens (\GetPushTemplate' {templateName} -> templateName) (\s@GetPushTemplate' {} a -> s {templateName = a} :: GetPushTemplate)

instance Core.AWSRequest GetPushTemplate where
  type
    AWSResponse GetPushTemplate =
      GetPushTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPushTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetPushTemplate where
  hashWithSalt _salt GetPushTemplate' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetPushTemplate where
  rnf GetPushTemplate' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName

instance Core.ToHeaders GetPushTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetPushTemplate where
  toPath GetPushTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/push"]

instance Core.ToQuery GetPushTemplate where
  toQuery GetPushTemplate' {..} =
    Prelude.mconcat ["version" Core.=: version]

-- | /See:/ 'newGetPushTemplateResponse' smart constructor.
data GetPushTemplateResponse = GetPushTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    pushNotificationTemplateResponse :: PushNotificationTemplateResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getPushTemplateResponse_httpStatus :: Lens.Lens' GetPushTemplateResponse Prelude.Int
getPushTemplateResponse_httpStatus = Lens.lens (\GetPushTemplateResponse' {httpStatus} -> httpStatus) (\s@GetPushTemplateResponse' {} a -> s {httpStatus = a} :: GetPushTemplateResponse)

-- | Undocumented member.
getPushTemplateResponse_pushNotificationTemplateResponse :: Lens.Lens' GetPushTemplateResponse PushNotificationTemplateResponse
getPushTemplateResponse_pushNotificationTemplateResponse = Lens.lens (\GetPushTemplateResponse' {pushNotificationTemplateResponse} -> pushNotificationTemplateResponse) (\s@GetPushTemplateResponse' {} a -> s {pushNotificationTemplateResponse = a} :: GetPushTemplateResponse)

instance Prelude.NFData GetPushTemplateResponse where
  rnf GetPushTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf pushNotificationTemplateResponse
