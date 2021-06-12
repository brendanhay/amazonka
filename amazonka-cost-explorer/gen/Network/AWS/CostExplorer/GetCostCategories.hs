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
-- Module      : Network.AWS.CostExplorer.GetCostCategories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of Cost Category names and values incurred cost.
--
-- If some Cost Category names and values are not associated with any cost,
-- they will not be returned by this API.
module Network.AWS.CostExplorer.GetCostCategories
  ( -- * Creating a Request
    GetCostCategories (..),
    newGetCostCategories,

    -- * Request Lenses
    getCostCategories_maxResults,
    getCostCategories_searchString,
    getCostCategories_nextPageToken,
    getCostCategories_costCategoryName,
    getCostCategories_sortBy,
    getCostCategories_filter,
    getCostCategories_timePeriod,

    -- * Destructuring the Response
    GetCostCategoriesResponse (..),
    newGetCostCategoriesResponse,

    -- * Response Lenses
    getCostCategoriesResponse_costCategoryValues,
    getCostCategoriesResponse_nextPageToken,
    getCostCategoriesResponse_costCategoryNames,
    getCostCategoriesResponse_httpStatus,
    getCostCategoriesResponse_returnSize,
    getCostCategoriesResponse_totalSize,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCostCategories' smart constructor.
data GetCostCategories = GetCostCategories'
  { -- | This field is only used when @SortBy@ is provided in the request.
    --
    -- The maximum number of objects that to be returned for this request. If
    -- @MaxResults@ is not specified with @SortBy@, the request will return
    -- 1000 results as the default value for this parameter.
    --
    -- For @GetCostCategories@, MaxResults has an upper limit of 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | The value that you want to search the filter values for.
    --
    -- If you do not specify a @CostCategoryName@, @SearchString@ will be used
    -- to filter Cost Category names that match the @SearchString@ pattern. If
    -- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
    -- filter Cost Category values that match the @SearchString@ pattern.
    searchString :: Core.Maybe Core.Text,
    -- | If the number of objects that are still available for retrieval exceeds
    -- the limit, AWS returns a NextPageToken value in the response. To
    -- retrieve the next batch of objects, provide the NextPageToken from the
    -- prior call in your next request.
    nextPageToken :: Core.Maybe Core.Text,
    costCategoryName :: Core.Maybe Core.Text,
    -- | The value by which you want to sort the data.
    --
    -- The key represents cost and usage metrics. The following values are
    -- supported:
    --
    -- -   @BlendedCost@
    --
    -- -   @UnblendedCost@
    --
    -- -   @AmortizedCost@
    --
    -- -   @NetAmortizedCost@
    --
    -- -   @NetUnblendedCost@
    --
    -- -   @UsageQuantity@
    --
    -- -   @NormalizedUsageAmount@
    --
    -- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
    --
    -- When using @SortBy@, @NextPageToken@ and @SearchString@ are not
    -- supported.
    sortBy :: Core.Maybe [SortDefinition],
    filter' :: Core.Maybe Expression,
    timePeriod :: DateInterval
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCostCategories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCostCategories_maxResults' - This field is only used when @SortBy@ is provided in the request.
--
-- The maximum number of objects that to be returned for this request. If
-- @MaxResults@ is not specified with @SortBy@, the request will return
-- 1000 results as the default value for this parameter.
--
-- For @GetCostCategories@, MaxResults has an upper limit of 1000.
--
-- 'searchString', 'getCostCategories_searchString' - The value that you want to search the filter values for.
--
-- If you do not specify a @CostCategoryName@, @SearchString@ will be used
-- to filter Cost Category names that match the @SearchString@ pattern. If
-- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
-- filter Cost Category values that match the @SearchString@ pattern.
--
-- 'nextPageToken', 'getCostCategories_nextPageToken' - If the number of objects that are still available for retrieval exceeds
-- the limit, AWS returns a NextPageToken value in the response. To
-- retrieve the next batch of objects, provide the NextPageToken from the
-- prior call in your next request.
--
-- 'costCategoryName', 'getCostCategories_costCategoryName' - Undocumented member.
--
-- 'sortBy', 'getCostCategories_sortBy' - The value by which you want to sort the data.
--
-- The key represents cost and usage metrics. The following values are
-- supported:
--
-- -   @BlendedCost@
--
-- -   @UnblendedCost@
--
-- -   @AmortizedCost@
--
-- -   @NetAmortizedCost@
--
-- -   @NetUnblendedCost@
--
-- -   @UsageQuantity@
--
-- -   @NormalizedUsageAmount@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- When using @SortBy@, @NextPageToken@ and @SearchString@ are not
-- supported.
--
-- 'filter'', 'getCostCategories_filter' - Undocumented member.
--
-- 'timePeriod', 'getCostCategories_timePeriod' - Undocumented member.
newGetCostCategories ::
  -- | 'timePeriod'
  DateInterval ->
  GetCostCategories
newGetCostCategories pTimePeriod_ =
  GetCostCategories'
    { maxResults = Core.Nothing,
      searchString = Core.Nothing,
      nextPageToken = Core.Nothing,
      costCategoryName = Core.Nothing,
      sortBy = Core.Nothing,
      filter' = Core.Nothing,
      timePeriod = pTimePeriod_
    }

-- | This field is only used when @SortBy@ is provided in the request.
--
-- The maximum number of objects that to be returned for this request. If
-- @MaxResults@ is not specified with @SortBy@, the request will return
-- 1000 results as the default value for this parameter.
--
-- For @GetCostCategories@, MaxResults has an upper limit of 1000.
getCostCategories_maxResults :: Lens.Lens' GetCostCategories (Core.Maybe Core.Natural)
getCostCategories_maxResults = Lens.lens (\GetCostCategories' {maxResults} -> maxResults) (\s@GetCostCategories' {} a -> s {maxResults = a} :: GetCostCategories)

-- | The value that you want to search the filter values for.
--
-- If you do not specify a @CostCategoryName@, @SearchString@ will be used
-- to filter Cost Category names that match the @SearchString@ pattern. If
-- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
-- filter Cost Category values that match the @SearchString@ pattern.
getCostCategories_searchString :: Lens.Lens' GetCostCategories (Core.Maybe Core.Text)
getCostCategories_searchString = Lens.lens (\GetCostCategories' {searchString} -> searchString) (\s@GetCostCategories' {} a -> s {searchString = a} :: GetCostCategories)

-- | If the number of objects that are still available for retrieval exceeds
-- the limit, AWS returns a NextPageToken value in the response. To
-- retrieve the next batch of objects, provide the NextPageToken from the
-- prior call in your next request.
getCostCategories_nextPageToken :: Lens.Lens' GetCostCategories (Core.Maybe Core.Text)
getCostCategories_nextPageToken = Lens.lens (\GetCostCategories' {nextPageToken} -> nextPageToken) (\s@GetCostCategories' {} a -> s {nextPageToken = a} :: GetCostCategories)

-- | Undocumented member.
getCostCategories_costCategoryName :: Lens.Lens' GetCostCategories (Core.Maybe Core.Text)
getCostCategories_costCategoryName = Lens.lens (\GetCostCategories' {costCategoryName} -> costCategoryName) (\s@GetCostCategories' {} a -> s {costCategoryName = a} :: GetCostCategories)

-- | The value by which you want to sort the data.
--
-- The key represents cost and usage metrics. The following values are
-- supported:
--
-- -   @BlendedCost@
--
-- -   @UnblendedCost@
--
-- -   @AmortizedCost@
--
-- -   @NetAmortizedCost@
--
-- -   @NetUnblendedCost@
--
-- -   @UsageQuantity@
--
-- -   @NormalizedUsageAmount@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- When using @SortBy@, @NextPageToken@ and @SearchString@ are not
-- supported.
getCostCategories_sortBy :: Lens.Lens' GetCostCategories (Core.Maybe [SortDefinition])
getCostCategories_sortBy = Lens.lens (\GetCostCategories' {sortBy} -> sortBy) (\s@GetCostCategories' {} a -> s {sortBy = a} :: GetCostCategories) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getCostCategories_filter :: Lens.Lens' GetCostCategories (Core.Maybe Expression)
getCostCategories_filter = Lens.lens (\GetCostCategories' {filter'} -> filter') (\s@GetCostCategories' {} a -> s {filter' = a} :: GetCostCategories)

-- | Undocumented member.
getCostCategories_timePeriod :: Lens.Lens' GetCostCategories DateInterval
getCostCategories_timePeriod = Lens.lens (\GetCostCategories' {timePeriod} -> timePeriod) (\s@GetCostCategories' {} a -> s {timePeriod = a} :: GetCostCategories)

instance Core.AWSRequest GetCostCategories where
  type
    AWSResponse GetCostCategories =
      GetCostCategoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostCategoriesResponse'
            Core.<$> ( x Core..?> "CostCategoryValues"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (x Core..?> "CostCategoryNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ReturnSize")
            Core.<*> (x Core..:> "TotalSize")
      )

instance Core.Hashable GetCostCategories

instance Core.NFData GetCostCategories

instance Core.ToHeaders GetCostCategories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetCostCategories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCostCategories where
  toJSON GetCostCategories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("SearchString" Core..=) Core.<$> searchString,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("CostCategoryName" Core..=)
              Core.<$> costCategoryName,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance Core.ToPath GetCostCategories where
  toPath = Core.const "/"

instance Core.ToQuery GetCostCategories where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCostCategoriesResponse' smart constructor.
data GetCostCategoriesResponse = GetCostCategoriesResponse'
  { -- | The Cost Category values.
    --
    -- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
    -- specified in the request.
    costCategoryValues :: Core.Maybe [Core.Text],
    -- | If the number of objects that are still available for retrieval exceeds
    -- the limit, AWS returns a NextPageToken value in the response. To
    -- retrieve the next batch of objects, provide the marker from the prior
    -- call in your next request.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The names of the Cost Categories.
    costCategoryNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The number of objects returned.
    returnSize :: Core.Int,
    -- | The total number of objects.
    totalSize :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCostCategoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryValues', 'getCostCategoriesResponse_costCategoryValues' - The Cost Category values.
--
-- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
-- specified in the request.
--
-- 'nextPageToken', 'getCostCategoriesResponse_nextPageToken' - If the number of objects that are still available for retrieval exceeds
-- the limit, AWS returns a NextPageToken value in the response. To
-- retrieve the next batch of objects, provide the marker from the prior
-- call in your next request.
--
-- 'costCategoryNames', 'getCostCategoriesResponse_costCategoryNames' - The names of the Cost Categories.
--
-- 'httpStatus', 'getCostCategoriesResponse_httpStatus' - The response's http status code.
--
-- 'returnSize', 'getCostCategoriesResponse_returnSize' - The number of objects returned.
--
-- 'totalSize', 'getCostCategoriesResponse_totalSize' - The total number of objects.
newGetCostCategoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'returnSize'
  Core.Int ->
  -- | 'totalSize'
  Core.Int ->
  GetCostCategoriesResponse
newGetCostCategoriesResponse
  pHttpStatus_
  pReturnSize_
  pTotalSize_ =
    GetCostCategoriesResponse'
      { costCategoryValues =
          Core.Nothing,
        nextPageToken = Core.Nothing,
        costCategoryNames = Core.Nothing,
        httpStatus = pHttpStatus_,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | The Cost Category values.
--
-- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
-- specified in the request.
getCostCategoriesResponse_costCategoryValues :: Lens.Lens' GetCostCategoriesResponse (Core.Maybe [Core.Text])
getCostCategoriesResponse_costCategoryValues = Lens.lens (\GetCostCategoriesResponse' {costCategoryValues} -> costCategoryValues) (\s@GetCostCategoriesResponse' {} a -> s {costCategoryValues = a} :: GetCostCategoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | If the number of objects that are still available for retrieval exceeds
-- the limit, AWS returns a NextPageToken value in the response. To
-- retrieve the next batch of objects, provide the marker from the prior
-- call in your next request.
getCostCategoriesResponse_nextPageToken :: Lens.Lens' GetCostCategoriesResponse (Core.Maybe Core.Text)
getCostCategoriesResponse_nextPageToken = Lens.lens (\GetCostCategoriesResponse' {nextPageToken} -> nextPageToken) (\s@GetCostCategoriesResponse' {} a -> s {nextPageToken = a} :: GetCostCategoriesResponse)

-- | The names of the Cost Categories.
getCostCategoriesResponse_costCategoryNames :: Lens.Lens' GetCostCategoriesResponse (Core.Maybe [Core.Text])
getCostCategoriesResponse_costCategoryNames = Lens.lens (\GetCostCategoriesResponse' {costCategoryNames} -> costCategoryNames) (\s@GetCostCategoriesResponse' {} a -> s {costCategoryNames = a} :: GetCostCategoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCostCategoriesResponse_httpStatus :: Lens.Lens' GetCostCategoriesResponse Core.Int
getCostCategoriesResponse_httpStatus = Lens.lens (\GetCostCategoriesResponse' {httpStatus} -> httpStatus) (\s@GetCostCategoriesResponse' {} a -> s {httpStatus = a} :: GetCostCategoriesResponse)

-- | The number of objects returned.
getCostCategoriesResponse_returnSize :: Lens.Lens' GetCostCategoriesResponse Core.Int
getCostCategoriesResponse_returnSize = Lens.lens (\GetCostCategoriesResponse' {returnSize} -> returnSize) (\s@GetCostCategoriesResponse' {} a -> s {returnSize = a} :: GetCostCategoriesResponse)

-- | The total number of objects.
getCostCategoriesResponse_totalSize :: Lens.Lens' GetCostCategoriesResponse Core.Int
getCostCategoriesResponse_totalSize = Lens.lens (\GetCostCategoriesResponse' {totalSize} -> totalSize) (\s@GetCostCategoriesResponse' {} a -> s {totalSize = a} :: GetCostCategoriesResponse)

instance Core.NFData GetCostCategoriesResponse
