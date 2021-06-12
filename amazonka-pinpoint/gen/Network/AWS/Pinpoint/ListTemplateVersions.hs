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
-- Module      : Network.AWS.Pinpoint.ListTemplateVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the versions of a specific message
-- template.
module Network.AWS.Pinpoint.ListTemplateVersions
  ( -- * Creating a Request
    ListTemplateVersions (..),
    newListTemplateVersions,

    -- * Request Lenses
    listTemplateVersions_nextToken,
    listTemplateVersions_pageSize,
    listTemplateVersions_templateName,
    listTemplateVersions_templateType,

    -- * Destructuring the Response
    ListTemplateVersionsResponse (..),
    newListTemplateVersionsResponse,

    -- * Response Lenses
    listTemplateVersionsResponse_httpStatus,
    listTemplateVersionsResponse_templateVersionsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTemplateVersions' smart constructor.
data ListTemplateVersions = ListTemplateVersions'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. Valid
    -- values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateVersions_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'listTemplateVersions_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'templateName', 'listTemplateVersions_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'templateType', 'listTemplateVersions_templateType' - The type of channel that the message template is designed for. Valid
-- values are: EMAIL, PUSH, SMS, and VOICE.
newListTemplateVersions ::
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  Core.Text ->
  ListTemplateVersions
newListTemplateVersions pTemplateName_ pTemplateType_ =
  ListTemplateVersions'
    { nextToken = Core.Nothing,
      pageSize = Core.Nothing,
      templateName = pTemplateName_,
      templateType = pTemplateType_
    }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listTemplateVersions_nextToken :: Lens.Lens' ListTemplateVersions (Core.Maybe Core.Text)
listTemplateVersions_nextToken = Lens.lens (\ListTemplateVersions' {nextToken} -> nextToken) (\s@ListTemplateVersions' {} a -> s {nextToken = a} :: ListTemplateVersions)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listTemplateVersions_pageSize :: Lens.Lens' ListTemplateVersions (Core.Maybe Core.Text)
listTemplateVersions_pageSize = Lens.lens (\ListTemplateVersions' {pageSize} -> pageSize) (\s@ListTemplateVersions' {} a -> s {pageSize = a} :: ListTemplateVersions)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
listTemplateVersions_templateName :: Lens.Lens' ListTemplateVersions Core.Text
listTemplateVersions_templateName = Lens.lens (\ListTemplateVersions' {templateName} -> templateName) (\s@ListTemplateVersions' {} a -> s {templateName = a} :: ListTemplateVersions)

-- | The type of channel that the message template is designed for. Valid
-- values are: EMAIL, PUSH, SMS, and VOICE.
listTemplateVersions_templateType :: Lens.Lens' ListTemplateVersions Core.Text
listTemplateVersions_templateType = Lens.lens (\ListTemplateVersions' {templateType} -> templateType) (\s@ListTemplateVersions' {} a -> s {templateType = a} :: ListTemplateVersions)

instance Core.AWSRequest ListTemplateVersions where
  type
    AWSResponse ListTemplateVersions =
      ListTemplateVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateVersionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable ListTemplateVersions

instance Core.NFData ListTemplateVersions

instance Core.ToHeaders ListTemplateVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListTemplateVersions where
  toPath ListTemplateVersions' {..} =
    Core.mconcat
      [ "/v1/templates/",
        Core.toBS templateName,
        "/",
        Core.toBS templateType,
        "/versions"
      ]

instance Core.ToQuery ListTemplateVersions where
  toQuery ListTemplateVersions' {..} =
    Core.mconcat
      [ "next-token" Core.=: nextToken,
        "page-size" Core.=: pageSize
      ]

-- | /See:/ 'newListTemplateVersionsResponse' smart constructor.
data ListTemplateVersionsResponse = ListTemplateVersionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    templateVersionsResponse :: TemplateVersionsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTemplateVersionsResponse_httpStatus' - The response's http status code.
--
-- 'templateVersionsResponse', 'listTemplateVersionsResponse_templateVersionsResponse' - Undocumented member.
newListTemplateVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'templateVersionsResponse'
  TemplateVersionsResponse ->
  ListTemplateVersionsResponse
newListTemplateVersionsResponse
  pHttpStatus_
  pTemplateVersionsResponse_ =
    ListTemplateVersionsResponse'
      { httpStatus =
          pHttpStatus_,
        templateVersionsResponse =
          pTemplateVersionsResponse_
      }

-- | The response's http status code.
listTemplateVersionsResponse_httpStatus :: Lens.Lens' ListTemplateVersionsResponse Core.Int
listTemplateVersionsResponse_httpStatus = Lens.lens (\ListTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@ListTemplateVersionsResponse' {} a -> s {httpStatus = a} :: ListTemplateVersionsResponse)

-- | Undocumented member.
listTemplateVersionsResponse_templateVersionsResponse :: Lens.Lens' ListTemplateVersionsResponse TemplateVersionsResponse
listTemplateVersionsResponse_templateVersionsResponse = Lens.lens (\ListTemplateVersionsResponse' {templateVersionsResponse} -> templateVersionsResponse) (\s@ListTemplateVersionsResponse' {} a -> s {templateVersionsResponse = a} :: ListTemplateVersionsResponse)

instance Core.NFData ListTemplateVersionsResponse
