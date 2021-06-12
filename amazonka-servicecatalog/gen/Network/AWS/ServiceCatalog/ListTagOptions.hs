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
-- Module      : Network.AWS.ServiceCatalog.ListTagOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified TagOptions or all TagOptions.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListTagOptions
  ( -- * Creating a Request
    ListTagOptions (..),
    newListTagOptions,

    -- * Request Lenses
    listTagOptions_pageSize,
    listTagOptions_pageToken,
    listTagOptions_filters,

    -- * Destructuring the Response
    ListTagOptionsResponse (..),
    newListTagOptionsResponse,

    -- * Response Lenses
    listTagOptionsResponse_pageToken,
    listTagOptionsResponse_tagOptionDetails,
    listTagOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListTagOptions' smart constructor.
data ListTagOptions = ListTagOptions'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The search filters. If no search filters are specified, the output
    -- includes all TagOptions.
    filters :: Core.Maybe ListTagOptionsFilters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listTagOptions_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listTagOptions_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'filters', 'listTagOptions_filters' - The search filters. If no search filters are specified, the output
-- includes all TagOptions.
newListTagOptions ::
  ListTagOptions
newListTagOptions =
  ListTagOptions'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      filters = Core.Nothing
    }

-- | The maximum number of items to return with this call.
listTagOptions_pageSize :: Lens.Lens' ListTagOptions (Core.Maybe Core.Natural)
listTagOptions_pageSize = Lens.lens (\ListTagOptions' {pageSize} -> pageSize) (\s@ListTagOptions' {} a -> s {pageSize = a} :: ListTagOptions)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listTagOptions_pageToken :: Lens.Lens' ListTagOptions (Core.Maybe Core.Text)
listTagOptions_pageToken = Lens.lens (\ListTagOptions' {pageToken} -> pageToken) (\s@ListTagOptions' {} a -> s {pageToken = a} :: ListTagOptions)

-- | The search filters. If no search filters are specified, the output
-- includes all TagOptions.
listTagOptions_filters :: Lens.Lens' ListTagOptions (Core.Maybe ListTagOptionsFilters)
listTagOptions_filters = Lens.lens (\ListTagOptions' {filters} -> filters) (\s@ListTagOptions' {} a -> s {filters = a} :: ListTagOptions)

instance Core.AWSPager ListTagOptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagOptionsResponse_pageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagOptionsResponse_tagOptionDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTagOptions_pageToken
          Lens..~ rs
          Lens.^? listTagOptionsResponse_pageToken Core.. Lens._Just

instance Core.AWSRequest ListTagOptions where
  type
    AWSResponse ListTagOptions =
      ListTagOptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagOptionsResponse'
            Core.<$> (x Core..?> "PageToken")
            Core.<*> (x Core..?> "TagOptionDetails" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagOptions

instance Core.NFData ListTagOptions

instance Core.ToHeaders ListTagOptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListTagOptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagOptions where
  toJSON ListTagOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListTagOptions where
  toPath = Core.const "/"

instance Core.ToQuery ListTagOptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagOptionsResponse' smart constructor.
data ListTagOptionsResponse = ListTagOptionsResponse'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | Information about the TagOptions.
    tagOptionDetails :: Core.Maybe [TagOptionDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'listTagOptionsResponse_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'tagOptionDetails', 'listTagOptionsResponse_tagOptionDetails' - Information about the TagOptions.
--
-- 'httpStatus', 'listTagOptionsResponse_httpStatus' - The response's http status code.
newListTagOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagOptionsResponse
newListTagOptionsResponse pHttpStatus_ =
  ListTagOptionsResponse'
    { pageToken = Core.Nothing,
      tagOptionDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listTagOptionsResponse_pageToken :: Lens.Lens' ListTagOptionsResponse (Core.Maybe Core.Text)
listTagOptionsResponse_pageToken = Lens.lens (\ListTagOptionsResponse' {pageToken} -> pageToken) (\s@ListTagOptionsResponse' {} a -> s {pageToken = a} :: ListTagOptionsResponse)

-- | Information about the TagOptions.
listTagOptionsResponse_tagOptionDetails :: Lens.Lens' ListTagOptionsResponse (Core.Maybe [TagOptionDetail])
listTagOptionsResponse_tagOptionDetails = Lens.lens (\ListTagOptionsResponse' {tagOptionDetails} -> tagOptionDetails) (\s@ListTagOptionsResponse' {} a -> s {tagOptionDetails = a} :: ListTagOptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagOptionsResponse_httpStatus :: Lens.Lens' ListTagOptionsResponse Core.Int
listTagOptionsResponse_httpStatus = Lens.lens (\ListTagOptionsResponse' {httpStatus} -> httpStatus) (\s@ListTagOptionsResponse' {} a -> s {httpStatus = a} :: ListTagOptionsResponse)

instance Core.NFData ListTagOptionsResponse
