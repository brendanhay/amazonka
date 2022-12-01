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
-- Module      : Amazonka.Pinpoint.GetInAppTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- sent through the in-app channel.
module Amazonka.Pinpoint.GetInAppTemplate
  ( -- * Creating a Request
    GetInAppTemplate (..),
    newGetInAppTemplate,

    -- * Request Lenses
    getInAppTemplate_version,
    getInAppTemplate_templateName,

    -- * Destructuring the Response
    GetInAppTemplateResponse (..),
    newGetInAppTemplateResponse,

    -- * Response Lenses
    getInAppTemplateResponse_httpStatus,
    getInAppTemplateResponse_inAppTemplateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInAppTemplate' smart constructor.
data GetInAppTemplate = GetInAppTemplate'
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
-- Create a value of 'GetInAppTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getInAppTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'getInAppTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newGetInAppTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  GetInAppTemplate
newGetInAppTemplate pTemplateName_ =
  GetInAppTemplate'
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
getInAppTemplate_version :: Lens.Lens' GetInAppTemplate (Prelude.Maybe Prelude.Text)
getInAppTemplate_version = Lens.lens (\GetInAppTemplate' {version} -> version) (\s@GetInAppTemplate' {} a -> s {version = a} :: GetInAppTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getInAppTemplate_templateName :: Lens.Lens' GetInAppTemplate Prelude.Text
getInAppTemplate_templateName = Lens.lens (\GetInAppTemplate' {templateName} -> templateName) (\s@GetInAppTemplate' {} a -> s {templateName = a} :: GetInAppTemplate)

instance Core.AWSRequest GetInAppTemplate where
  type
    AWSResponse GetInAppTemplate =
      GetInAppTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInAppTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetInAppTemplate where
  hashWithSalt _salt GetInAppTemplate' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetInAppTemplate where
  rnf GetInAppTemplate' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName

instance Core.ToHeaders GetInAppTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetInAppTemplate where
  toPath GetInAppTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/inapp"]

instance Core.ToQuery GetInAppTemplate where
  toQuery GetInAppTemplate' {..} =
    Prelude.mconcat ["version" Core.=: version]

-- | /See:/ 'newGetInAppTemplateResponse' smart constructor.
data GetInAppTemplateResponse = GetInAppTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    inAppTemplateResponse :: InAppTemplateResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInAppTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getInAppTemplateResponse_httpStatus' - The response's http status code.
--
-- 'inAppTemplateResponse', 'getInAppTemplateResponse_inAppTemplateResponse' - Undocumented member.
newGetInAppTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inAppTemplateResponse'
  InAppTemplateResponse ->
  GetInAppTemplateResponse
newGetInAppTemplateResponse
  pHttpStatus_
  pInAppTemplateResponse_ =
    GetInAppTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        inAppTemplateResponse = pInAppTemplateResponse_
      }

-- | The response's http status code.
getInAppTemplateResponse_httpStatus :: Lens.Lens' GetInAppTemplateResponse Prelude.Int
getInAppTemplateResponse_httpStatus = Lens.lens (\GetInAppTemplateResponse' {httpStatus} -> httpStatus) (\s@GetInAppTemplateResponse' {} a -> s {httpStatus = a} :: GetInAppTemplateResponse)

-- | Undocumented member.
getInAppTemplateResponse_inAppTemplateResponse :: Lens.Lens' GetInAppTemplateResponse InAppTemplateResponse
getInAppTemplateResponse_inAppTemplateResponse = Lens.lens (\GetInAppTemplateResponse' {inAppTemplateResponse} -> inAppTemplateResponse) (\s@GetInAppTemplateResponse' {} a -> s {inAppTemplateResponse = a} :: GetInAppTemplateResponse)

instance Prelude.NFData GetInAppTemplateResponse where
  rnf GetInAppTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf inAppTemplateResponse
