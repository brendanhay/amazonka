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
-- Module      : Network.AWS.GuardDuty.ListFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of the current filters.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFilters
  ( -- * Creating a Request
    ListFilters (..),
    newListFilters,

    -- * Request Lenses
    listFilters_nextToken,
    listFilters_maxResults,
    listFilters_detectorId,

    -- * Destructuring the Response
    ListFiltersResponse (..),
    newListFiltersResponse,

    -- * Response Lenses
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filterNames,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFilters' smart constructor.
data ListFilters = ListFilters'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 50. The maximum value is
    -- 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFilters_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'maxResults', 'listFilters_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
--
-- 'detectorId', 'listFilters_detectorId' - The unique ID of the detector that the filter is associated with.
newListFilters ::
  -- | 'detectorId'
  Core.Text ->
  ListFilters
newListFilters pDetectorId_ =
  ListFilters'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listFilters_nextToken :: Lens.Lens' ListFilters (Core.Maybe Core.Text)
listFilters_nextToken = Lens.lens (\ListFilters' {nextToken} -> nextToken) (\s@ListFilters' {} a -> s {nextToken = a} :: ListFilters)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
listFilters_maxResults :: Lens.Lens' ListFilters (Core.Maybe Core.Natural)
listFilters_maxResults = Lens.lens (\ListFilters' {maxResults} -> maxResults) (\s@ListFilters' {} a -> s {maxResults = a} :: ListFilters)

-- | The unique ID of the detector that the filter is associated with.
listFilters_detectorId :: Lens.Lens' ListFilters Core.Text
listFilters_detectorId = Lens.lens (\ListFilters' {detectorId} -> detectorId) (\s@ListFilters' {} a -> s {detectorId = a} :: ListFilters)

instance Core.AWSPager ListFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFiltersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listFiltersResponse_filterNames) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFilters_nextToken
          Lens..~ rs
          Lens.^? listFiltersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListFilters where
  type AWSResponse ListFilters = ListFiltersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFiltersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "filterNames" Core..!@ Core.mempty)
      )

instance Core.Hashable ListFilters

instance Core.NFData ListFilters

instance Core.ToHeaders ListFilters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListFilters where
  toPath ListFilters' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/filter"]

instance Core.ToQuery ListFilters where
  toQuery ListFilters' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of filter names.
    filterNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFiltersResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'listFiltersResponse_httpStatus' - The response's http status code.
--
-- 'filterNames', 'listFiltersResponse_filterNames' - A list of filter names.
newListFiltersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFiltersResponse
newListFiltersResponse pHttpStatus_ =
  ListFiltersResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      filterNames = Core.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listFiltersResponse_nextToken :: Lens.Lens' ListFiltersResponse (Core.Maybe Core.Text)
listFiltersResponse_nextToken = Lens.lens (\ListFiltersResponse' {nextToken} -> nextToken) (\s@ListFiltersResponse' {} a -> s {nextToken = a} :: ListFiltersResponse)

-- | The response's http status code.
listFiltersResponse_httpStatus :: Lens.Lens' ListFiltersResponse Core.Int
listFiltersResponse_httpStatus = Lens.lens (\ListFiltersResponse' {httpStatus} -> httpStatus) (\s@ListFiltersResponse' {} a -> s {httpStatus = a} :: ListFiltersResponse)

-- | A list of filter names.
listFiltersResponse_filterNames :: Lens.Lens' ListFiltersResponse [Core.Text]
listFiltersResponse_filterNames = Lens.lens (\ListFiltersResponse' {filterNames} -> filterNames) (\s@ListFiltersResponse' {} a -> s {filterNames = a} :: ListFiltersResponse) Core.. Lens._Coerce

instance Core.NFData ListFiltersResponse
