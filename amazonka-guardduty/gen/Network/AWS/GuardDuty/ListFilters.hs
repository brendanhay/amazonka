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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFilters' smart constructor.
data ListFilters = ListFilters'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 50. The maximum value is
    -- 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListFilters
newListFilters pDetectorId_ =
  ListFilters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listFilters_nextToken :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Text)
listFilters_nextToken = Lens.lens (\ListFilters' {nextToken} -> nextToken) (\s@ListFilters' {} a -> s {nextToken = a} :: ListFilters)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
listFilters_maxResults :: Lens.Lens' ListFilters (Prelude.Maybe Prelude.Natural)
listFilters_maxResults = Lens.lens (\ListFilters' {maxResults} -> maxResults) (\s@ListFilters' {} a -> s {maxResults = a} :: ListFilters)

-- | The unique ID of the detector that the filter is associated with.
listFilters_detectorId :: Lens.Lens' ListFilters Prelude.Text
listFilters_detectorId = Lens.lens (\ListFilters' {detectorId} -> detectorId) (\s@ListFilters' {} a -> s {detectorId = a} :: ListFilters)

instance Core.AWSPager ListFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFiltersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listFiltersResponse_filterNames) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFilters_nextToken
          Lens..~ rs
          Lens.^? listFiltersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFilters where
  type AWSResponse ListFilters = ListFiltersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFiltersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "filterNames" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListFilters

instance Prelude.NFData ListFilters

instance Core.ToHeaders ListFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListFilters where
  toPath ListFilters' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/filter"]

instance Core.ToQuery ListFilters where
  toQuery ListFilters' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of filter names.
    filterNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListFiltersResponse
newListFiltersResponse pHttpStatus_ =
  ListFiltersResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      filterNames = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listFiltersResponse_nextToken :: Lens.Lens' ListFiltersResponse (Prelude.Maybe Prelude.Text)
listFiltersResponse_nextToken = Lens.lens (\ListFiltersResponse' {nextToken} -> nextToken) (\s@ListFiltersResponse' {} a -> s {nextToken = a} :: ListFiltersResponse)

-- | The response's http status code.
listFiltersResponse_httpStatus :: Lens.Lens' ListFiltersResponse Prelude.Int
listFiltersResponse_httpStatus = Lens.lens (\ListFiltersResponse' {httpStatus} -> httpStatus) (\s@ListFiltersResponse' {} a -> s {httpStatus = a} :: ListFiltersResponse)

-- | A list of filter names.
listFiltersResponse_filterNames :: Lens.Lens' ListFiltersResponse [Prelude.Text]
listFiltersResponse_filterNames = Lens.lens (\ListFiltersResponse' {filterNames} -> filterNames) (\s@ListFiltersResponse' {} a -> s {filterNames = a} :: ListFiltersResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListFiltersResponse
