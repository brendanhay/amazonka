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
-- Module      : Network.AWS.Pinpoint.ListTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the message templates that are
-- associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.ListTemplates
  ( -- * Creating a Request
    ListTemplates (..),
    newListTemplates,

    -- * Request Lenses
    listTemplates_nextToken,
    listTemplates_pageSize,
    listTemplates_templateType,
    listTemplates_prefix,

    -- * Destructuring the Response
    ListTemplatesResponse (..),
    newListTemplatesResponse,

    -- * Response Lenses
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templatesResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The type of message template to include in the results. Valid values
    -- are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in
    -- the results, don\'t include this parameter in your request.
    templateType :: Core.Maybe Core.Text,
    -- | The substring to match in the names of the message templates to include
    -- in the results. If you specify this value, Amazon Pinpoint returns only
    -- those templates whose names begin with the value that you specify.
    prefix :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplates_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'listTemplates_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'templateType', 'listTemplates_templateType' - The type of message template to include in the results. Valid values
-- are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in
-- the results, don\'t include this parameter in your request.
--
-- 'prefix', 'listTemplates_prefix' - The substring to match in the names of the message templates to include
-- in the results. If you specify this value, Amazon Pinpoint returns only
-- those templates whose names begin with the value that you specify.
newListTemplates ::
  ListTemplates
newListTemplates =
  ListTemplates'
    { nextToken = Core.Nothing,
      pageSize = Core.Nothing,
      templateType = Core.Nothing,
      prefix = Core.Nothing
    }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listTemplates_nextToken :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
listTemplates_nextToken = Lens.lens (\ListTemplates' {nextToken} -> nextToken) (\s@ListTemplates' {} a -> s {nextToken = a} :: ListTemplates)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listTemplates_pageSize :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
listTemplates_pageSize = Lens.lens (\ListTemplates' {pageSize} -> pageSize) (\s@ListTemplates' {} a -> s {pageSize = a} :: ListTemplates)

-- | The type of message template to include in the results. Valid values
-- are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in
-- the results, don\'t include this parameter in your request.
listTemplates_templateType :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
listTemplates_templateType = Lens.lens (\ListTemplates' {templateType} -> templateType) (\s@ListTemplates' {} a -> s {templateType = a} :: ListTemplates)

-- | The substring to match in the names of the message templates to include
-- in the results. If you specify this value, Amazon Pinpoint returns only
-- those templates whose names begin with the value that you specify.
listTemplates_prefix :: Lens.Lens' ListTemplates (Core.Maybe Core.Text)
listTemplates_prefix = Lens.lens (\ListTemplates' {prefix} -> prefix) (\s@ListTemplates' {} a -> s {prefix = a} :: ListTemplates)

instance Core.AWSRequest ListTemplates where
  type
    AWSResponse ListTemplates =
      ListTemplatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable ListTemplates

instance Core.NFData ListTemplates

instance Core.ToHeaders ListTemplates where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListTemplates where
  toPath = Core.const "/v1/templates"

instance Core.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Core.mconcat
      [ "next-token" Core.=: nextToken,
        "page-size" Core.=: pageSize,
        "template-type" Core.=: templateType,
        "prefix" Core.=: prefix
      ]

-- | /See:/ 'newListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    templatesResponse :: TemplatesResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'templatesResponse', 'listTemplatesResponse_templatesResponse' - Undocumented member.
newListTemplatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'templatesResponse'
  TemplatesResponse ->
  ListTemplatesResponse
newListTemplatesResponse
  pHttpStatus_
  pTemplatesResponse_ =
    ListTemplatesResponse'
      { httpStatus = pHttpStatus_,
        templatesResponse = pTemplatesResponse_
      }

-- | The response's http status code.
listTemplatesResponse_httpStatus :: Lens.Lens' ListTemplatesResponse Core.Int
listTemplatesResponse_httpStatus = Lens.lens (\ListTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListTemplatesResponse' {} a -> s {httpStatus = a} :: ListTemplatesResponse)

-- | Undocumented member.
listTemplatesResponse_templatesResponse :: Lens.Lens' ListTemplatesResponse TemplatesResponse
listTemplatesResponse_templatesResponse = Lens.lens (\ListTemplatesResponse' {templatesResponse} -> templatesResponse) (\s@ListTemplatesResponse' {} a -> s {templatesResponse = a} :: ListTemplatesResponse)

instance Core.NFData ListTemplatesResponse
