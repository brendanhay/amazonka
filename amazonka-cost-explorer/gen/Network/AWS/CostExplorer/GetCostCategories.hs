{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The value that you want to search the filter values for.
    --
    -- If you do not specify a @CostCategoryName@, @SearchString@ will be used
    -- to filter Cost Category names that match the @SearchString@ pattern. If
    -- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
    -- filter Cost Category values that match the @SearchString@ pattern.
    searchString :: Prelude.Maybe Prelude.Text,
    -- | If the number of objects that are still available for retrieval exceeds
    -- the limit, AWS returns a NextPageToken value in the response. To
    -- retrieve the next batch of objects, provide the NextPageToken from the
    -- prior call in your next request.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    costCategoryName :: Prelude.Maybe Prelude.Text,
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
    sortBy :: Prelude.Maybe [SortDefinition],
    filter' :: Prelude.Maybe Expression,
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { maxResults = Prelude.Nothing,
      searchString = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      costCategoryName = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | This field is only used when @SortBy@ is provided in the request.
--
-- The maximum number of objects that to be returned for this request. If
-- @MaxResults@ is not specified with @SortBy@, the request will return
-- 1000 results as the default value for this parameter.
--
-- For @GetCostCategories@, MaxResults has an upper limit of 1000.
getCostCategories_maxResults :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Natural)
getCostCategories_maxResults = Lens.lens (\GetCostCategories' {maxResults} -> maxResults) (\s@GetCostCategories' {} a -> s {maxResults = a} :: GetCostCategories)

-- | The value that you want to search the filter values for.
--
-- If you do not specify a @CostCategoryName@, @SearchString@ will be used
-- to filter Cost Category names that match the @SearchString@ pattern. If
-- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
-- filter Cost Category values that match the @SearchString@ pattern.
getCostCategories_searchString :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Text)
getCostCategories_searchString = Lens.lens (\GetCostCategories' {searchString} -> searchString) (\s@GetCostCategories' {} a -> s {searchString = a} :: GetCostCategories)

-- | If the number of objects that are still available for retrieval exceeds
-- the limit, AWS returns a NextPageToken value in the response. To
-- retrieve the next batch of objects, provide the NextPageToken from the
-- prior call in your next request.
getCostCategories_nextPageToken :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Text)
getCostCategories_nextPageToken = Lens.lens (\GetCostCategories' {nextPageToken} -> nextPageToken) (\s@GetCostCategories' {} a -> s {nextPageToken = a} :: GetCostCategories)

-- | Undocumented member.
getCostCategories_costCategoryName :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Text)
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
getCostCategories_sortBy :: Lens.Lens' GetCostCategories (Prelude.Maybe [SortDefinition])
getCostCategories_sortBy = Lens.lens (\GetCostCategories' {sortBy} -> sortBy) (\s@GetCostCategories' {} a -> s {sortBy = a} :: GetCostCategories) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
getCostCategories_filter :: Lens.Lens' GetCostCategories (Prelude.Maybe Expression)
getCostCategories_filter = Lens.lens (\GetCostCategories' {filter'} -> filter') (\s@GetCostCategories' {} a -> s {filter' = a} :: GetCostCategories)

-- | Undocumented member.
getCostCategories_timePeriod :: Lens.Lens' GetCostCategories DateInterval
getCostCategories_timePeriod = Lens.lens (\GetCostCategories' {timePeriod} -> timePeriod) (\s@GetCostCategories' {} a -> s {timePeriod = a} :: GetCostCategories)

instance Prelude.AWSRequest GetCostCategories where
  type Rs GetCostCategories = GetCostCategoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostCategoriesResponse'
            Prelude.<$> ( x Prelude..?> "CostCategoryValues"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextPageToken")
            Prelude.<*> ( x Prelude..?> "CostCategoryNames"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ReturnSize")
            Prelude.<*> (x Prelude..:> "TotalSize")
      )

instance Prelude.Hashable GetCostCategories

instance Prelude.NFData GetCostCategories

instance Prelude.ToHeaders GetCostCategories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.GetCostCategories" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCostCategories where
  toJSON GetCostCategories' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("SearchString" Prelude..=) Prelude.<$> searchString,
            ("NextPageToken" Prelude..=)
              Prelude.<$> nextPageToken,
            ("CostCategoryName" Prelude..=)
              Prelude.<$> costCategoryName,
            ("SortBy" Prelude..=) Prelude.<$> sortBy,
            ("Filter" Prelude..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Prelude..= timePeriod)
          ]
      )

instance Prelude.ToPath GetCostCategories where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCostCategories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostCategoriesResponse' smart constructor.
data GetCostCategoriesResponse = GetCostCategoriesResponse'
  { -- | The Cost Category values.
    --
    -- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
    -- specified in the request.
    costCategoryValues :: Prelude.Maybe [Prelude.Text],
    -- | If the number of objects that are still available for retrieval exceeds
    -- the limit, AWS returns a NextPageToken value in the response. To
    -- retrieve the next batch of objects, provide the marker from the prior
    -- call in your next request.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The names of the Cost Categories.
    costCategoryNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The number of objects returned.
    returnSize :: Prelude.Int,
    -- | The total number of objects.
    totalSize :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'returnSize'
  Prelude.Int ->
  -- | 'totalSize'
  Prelude.Int ->
  GetCostCategoriesResponse
newGetCostCategoriesResponse
  pHttpStatus_
  pReturnSize_
  pTotalSize_ =
    GetCostCategoriesResponse'
      { costCategoryValues =
          Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        costCategoryNames = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | The Cost Category values.
--
-- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
-- specified in the request.
getCostCategoriesResponse_costCategoryValues :: Lens.Lens' GetCostCategoriesResponse (Prelude.Maybe [Prelude.Text])
getCostCategoriesResponse_costCategoryValues = Lens.lens (\GetCostCategoriesResponse' {costCategoryValues} -> costCategoryValues) (\s@GetCostCategoriesResponse' {} a -> s {costCategoryValues = a} :: GetCostCategoriesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If the number of objects that are still available for retrieval exceeds
-- the limit, AWS returns a NextPageToken value in the response. To
-- retrieve the next batch of objects, provide the marker from the prior
-- call in your next request.
getCostCategoriesResponse_nextPageToken :: Lens.Lens' GetCostCategoriesResponse (Prelude.Maybe Prelude.Text)
getCostCategoriesResponse_nextPageToken = Lens.lens (\GetCostCategoriesResponse' {nextPageToken} -> nextPageToken) (\s@GetCostCategoriesResponse' {} a -> s {nextPageToken = a} :: GetCostCategoriesResponse)

-- | The names of the Cost Categories.
getCostCategoriesResponse_costCategoryNames :: Lens.Lens' GetCostCategoriesResponse (Prelude.Maybe [Prelude.Text])
getCostCategoriesResponse_costCategoryNames = Lens.lens (\GetCostCategoriesResponse' {costCategoryNames} -> costCategoryNames) (\s@GetCostCategoriesResponse' {} a -> s {costCategoryNames = a} :: GetCostCategoriesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getCostCategoriesResponse_httpStatus :: Lens.Lens' GetCostCategoriesResponse Prelude.Int
getCostCategoriesResponse_httpStatus = Lens.lens (\GetCostCategoriesResponse' {httpStatus} -> httpStatus) (\s@GetCostCategoriesResponse' {} a -> s {httpStatus = a} :: GetCostCategoriesResponse)

-- | The number of objects returned.
getCostCategoriesResponse_returnSize :: Lens.Lens' GetCostCategoriesResponse Prelude.Int
getCostCategoriesResponse_returnSize = Lens.lens (\GetCostCategoriesResponse' {returnSize} -> returnSize) (\s@GetCostCategoriesResponse' {} a -> s {returnSize = a} :: GetCostCategoriesResponse)

-- | The total number of objects.
getCostCategoriesResponse_totalSize :: Lens.Lens' GetCostCategoriesResponse Prelude.Int
getCostCategoriesResponse_totalSize = Lens.lens (\GetCostCategoriesResponse' {totalSize} -> totalSize) (\s@GetCostCategoriesResponse' {} a -> s {totalSize = a} :: GetCostCategoriesResponse)

instance Prelude.NFData GetCostCategoriesResponse
