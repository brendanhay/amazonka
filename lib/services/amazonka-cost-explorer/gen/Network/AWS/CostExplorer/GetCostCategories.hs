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
-- Module      : Amazonka.CostExplorer.GetCostCategories
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
module Amazonka.CostExplorer.GetCostCategories
  ( -- * Creating a Request
    GetCostCategories (..),
    newGetCostCategories,

    -- * Request Lenses
    getCostCategories_nextPageToken,
    getCostCategories_searchString,
    getCostCategories_costCategoryName,
    getCostCategories_filter,
    getCostCategories_maxResults,
    getCostCategories_sortBy,
    getCostCategories_timePeriod,

    -- * Destructuring the Response
    GetCostCategoriesResponse (..),
    newGetCostCategoriesResponse,

    -- * Response Lenses
    getCostCategoriesResponse_nextPageToken,
    getCostCategoriesResponse_costCategoryNames,
    getCostCategoriesResponse_costCategoryValues,
    getCostCategoriesResponse_httpStatus,
    getCostCategoriesResponse_returnSize,
    getCostCategoriesResponse_totalSize,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCostCategories' smart constructor.
data GetCostCategories = GetCostCategories'
  { -- | If the number of objects that are still available for retrieval exceeds
    -- the limit, Amazon Web Services returns a NextPageToken value in the
    -- response. To retrieve the next batch of objects, provide the
    -- NextPageToken from the prior call in your next request.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The value that you want to search the filter values for.
    --
    -- If you do not specify a @CostCategoryName@, @SearchString@ will be used
    -- to filter Cost Category names that match the @SearchString@ pattern. If
    -- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
    -- filter Cost Category values that match the @SearchString@ pattern.
    searchString :: Prelude.Maybe Prelude.Text,
    costCategoryName :: Prelude.Maybe Prelude.Text,
    filter' :: Prelude.Maybe Expression,
    -- | This field is only used when @SortBy@ is provided in the request.
    --
    -- The maximum number of objects that to be returned for this request. If
    -- @MaxResults@ is not specified with @SortBy@, the request will return
    -- 1000 results as the default value for this parameter.
    --
    -- For @GetCostCategories@, MaxResults has an upper limit of 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostCategories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostCategories_nextPageToken' - If the number of objects that are still available for retrieval exceeds
-- the limit, Amazon Web Services returns a NextPageToken value in the
-- response. To retrieve the next batch of objects, provide the
-- NextPageToken from the prior call in your next request.
--
-- 'searchString', 'getCostCategories_searchString' - The value that you want to search the filter values for.
--
-- If you do not specify a @CostCategoryName@, @SearchString@ will be used
-- to filter Cost Category names that match the @SearchString@ pattern. If
-- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
-- filter Cost Category values that match the @SearchString@ pattern.
--
-- 'costCategoryName', 'getCostCategories_costCategoryName' - Undocumented member.
--
-- 'filter'', 'getCostCategories_filter' - Undocumented member.
--
-- 'maxResults', 'getCostCategories_maxResults' - This field is only used when @SortBy@ is provided in the request.
--
-- The maximum number of objects that to be returned for this request. If
-- @MaxResults@ is not specified with @SortBy@, the request will return
-- 1000 results as the default value for this parameter.
--
-- For @GetCostCategories@, MaxResults has an upper limit of 1000.
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
-- 'timePeriod', 'getCostCategories_timePeriod' - Undocumented member.
newGetCostCategories ::
  -- | 'timePeriod'
  DateInterval ->
  GetCostCategories
newGetCostCategories pTimePeriod_ =
  GetCostCategories'
    { nextPageToken = Prelude.Nothing,
      searchString = Prelude.Nothing,
      costCategoryName = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | If the number of objects that are still available for retrieval exceeds
-- the limit, Amazon Web Services returns a NextPageToken value in the
-- response. To retrieve the next batch of objects, provide the
-- NextPageToken from the prior call in your next request.
getCostCategories_nextPageToken :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Text)
getCostCategories_nextPageToken = Lens.lens (\GetCostCategories' {nextPageToken} -> nextPageToken) (\s@GetCostCategories' {} a -> s {nextPageToken = a} :: GetCostCategories)

-- | The value that you want to search the filter values for.
--
-- If you do not specify a @CostCategoryName@, @SearchString@ will be used
-- to filter Cost Category names that match the @SearchString@ pattern. If
-- you do specifiy a @CostCategoryName@, @SearchString@ will be used to
-- filter Cost Category values that match the @SearchString@ pattern.
getCostCategories_searchString :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Text)
getCostCategories_searchString = Lens.lens (\GetCostCategories' {searchString} -> searchString) (\s@GetCostCategories' {} a -> s {searchString = a} :: GetCostCategories)

-- | Undocumented member.
getCostCategories_costCategoryName :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Text)
getCostCategories_costCategoryName = Lens.lens (\GetCostCategories' {costCategoryName} -> costCategoryName) (\s@GetCostCategories' {} a -> s {costCategoryName = a} :: GetCostCategories)

-- | Undocumented member.
getCostCategories_filter :: Lens.Lens' GetCostCategories (Prelude.Maybe Expression)
getCostCategories_filter = Lens.lens (\GetCostCategories' {filter'} -> filter') (\s@GetCostCategories' {} a -> s {filter' = a} :: GetCostCategories)

-- | This field is only used when @SortBy@ is provided in the request.
--
-- The maximum number of objects that to be returned for this request. If
-- @MaxResults@ is not specified with @SortBy@, the request will return
-- 1000 results as the default value for this parameter.
--
-- For @GetCostCategories@, MaxResults has an upper limit of 1000.
getCostCategories_maxResults :: Lens.Lens' GetCostCategories (Prelude.Maybe Prelude.Natural)
getCostCategories_maxResults = Lens.lens (\GetCostCategories' {maxResults} -> maxResults) (\s@GetCostCategories' {} a -> s {maxResults = a} :: GetCostCategories)

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
getCostCategories_sortBy = Lens.lens (\GetCostCategories' {sortBy} -> sortBy) (\s@GetCostCategories' {} a -> s {sortBy = a} :: GetCostCategories) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> ( x Core..?> "CostCategoryNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "CostCategoryValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ReturnSize")
            Prelude.<*> (x Core..:> "TotalSize")
      )

instance Prelude.Hashable GetCostCategories

instance Prelude.NFData GetCostCategories

instance Core.ToHeaders GetCostCategories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetCostCategories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCostCategories where
  toJSON GetCostCategories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("SearchString" Core..=) Prelude.<$> searchString,
            ("CostCategoryName" Core..=)
              Prelude.<$> costCategoryName,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            Prelude.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance Core.ToPath GetCostCategories where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCostCategories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostCategoriesResponse' smart constructor.
data GetCostCategoriesResponse = GetCostCategoriesResponse'
  { -- | If the number of objects that are still available for retrieval exceeds
    -- the limit, Amazon Web Services returns a NextPageToken value in the
    -- response. To retrieve the next batch of objects, provide the marker from
    -- the prior call in your next request.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The names of the Cost Categories.
    costCategoryNames :: Prelude.Maybe [Prelude.Text],
    -- | The Cost Category values.
    --
    -- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
    -- specified in the request.
    costCategoryValues :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The number of objects returned.
    returnSize :: Prelude.Int,
    -- | The total number of objects.
    totalSize :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostCategoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostCategoriesResponse_nextPageToken' - If the number of objects that are still available for retrieval exceeds
-- the limit, Amazon Web Services returns a NextPageToken value in the
-- response. To retrieve the next batch of objects, provide the marker from
-- the prior call in your next request.
--
-- 'costCategoryNames', 'getCostCategoriesResponse_costCategoryNames' - The names of the Cost Categories.
--
-- 'costCategoryValues', 'getCostCategoriesResponse_costCategoryValues' - The Cost Category values.
--
-- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
-- specified in the request.
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
      { nextPageToken =
          Prelude.Nothing,
        costCategoryNames = Prelude.Nothing,
        costCategoryValues = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | If the number of objects that are still available for retrieval exceeds
-- the limit, Amazon Web Services returns a NextPageToken value in the
-- response. To retrieve the next batch of objects, provide the marker from
-- the prior call in your next request.
getCostCategoriesResponse_nextPageToken :: Lens.Lens' GetCostCategoriesResponse (Prelude.Maybe Prelude.Text)
getCostCategoriesResponse_nextPageToken = Lens.lens (\GetCostCategoriesResponse' {nextPageToken} -> nextPageToken) (\s@GetCostCategoriesResponse' {} a -> s {nextPageToken = a} :: GetCostCategoriesResponse)

-- | The names of the Cost Categories.
getCostCategoriesResponse_costCategoryNames :: Lens.Lens' GetCostCategoriesResponse (Prelude.Maybe [Prelude.Text])
getCostCategoriesResponse_costCategoryNames = Lens.lens (\GetCostCategoriesResponse' {costCategoryNames} -> costCategoryNames) (\s@GetCostCategoriesResponse' {} a -> s {costCategoryNames = a} :: GetCostCategoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Cost Category values.
--
-- @CostCategoryValues@ are not returned if @CostCategoryName@ is not
-- specified in the request.
getCostCategoriesResponse_costCategoryValues :: Lens.Lens' GetCostCategoriesResponse (Prelude.Maybe [Prelude.Text])
getCostCategoriesResponse_costCategoryValues = Lens.lens (\GetCostCategoriesResponse' {costCategoryValues} -> costCategoryValues) (\s@GetCostCategoriesResponse' {} a -> s {costCategoryValues = a} :: GetCostCategoriesResponse) Prelude.. Lens.mapping Lens.coerced

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
