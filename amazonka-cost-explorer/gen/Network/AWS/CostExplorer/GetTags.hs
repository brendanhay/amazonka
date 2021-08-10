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
-- Module      : Network.AWS.CostExplorer.GetTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for available tag keys and tag values for a specified period.
-- You can search the tag values for an arbitrary string.
module Network.AWS.CostExplorer.GetTags
  ( -- * Creating a Request
    GetTags (..),
    newGetTags,

    -- * Request Lenses
    getTags_maxResults,
    getTags_searchString,
    getTags_tagKey,
    getTags_nextPageToken,
    getTags_sortBy,
    getTags_filter,
    getTags_timePeriod,

    -- * Destructuring the Response
    GetTagsResponse (..),
    newGetTagsResponse,

    -- * Response Lenses
    getTagsResponse_nextPageToken,
    getTagsResponse_httpStatus,
    getTagsResponse_tags,
    getTagsResponse_returnSize,
    getTagsResponse_totalSize,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTags' smart constructor.
data GetTags = GetTags'
  { -- | This field is only used when SortBy is provided in the request. The
    -- maximum number of objects that to be returned for this request. If
    -- MaxResults is not specified with SortBy, the request will return 1000
    -- results as the default value for this parameter.
    --
    -- For @GetTags@, MaxResults has an upper limit of 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The value that you want to search for.
    searchString :: Prelude.Maybe Prelude.Text,
    -- | The key of the tag that you want to return values for.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
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
    -- | The start and end dates for retrieving the dimension values. The start
    -- date is inclusive, but the end date is exclusive. For example, if
    -- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
    -- usage data is retrieved from @2017-01-01@ up to and including
    -- @2017-04-30@ but not including @2017-05-01@.
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getTags_maxResults' - This field is only used when SortBy is provided in the request. The
-- maximum number of objects that to be returned for this request. If
-- MaxResults is not specified with SortBy, the request will return 1000
-- results as the default value for this parameter.
--
-- For @GetTags@, MaxResults has an upper limit of 1000.
--
-- 'searchString', 'getTags_searchString' - The value that you want to search for.
--
-- 'tagKey', 'getTags_tagKey' - The key of the tag that you want to return values for.
--
-- 'nextPageToken', 'getTags_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'sortBy', 'getTags_sortBy' - The value by which you want to sort the data.
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
-- 'filter'', 'getTags_filter' - Undocumented member.
--
-- 'timePeriod', 'getTags_timePeriod' - The start and end dates for retrieving the dimension values. The start
-- date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
newGetTags ::
  -- | 'timePeriod'
  DateInterval ->
  GetTags
newGetTags pTimePeriod_ =
  GetTags'
    { maxResults = Prelude.Nothing,
      searchString = Prelude.Nothing,
      tagKey = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | This field is only used when SortBy is provided in the request. The
-- maximum number of objects that to be returned for this request. If
-- MaxResults is not specified with SortBy, the request will return 1000
-- results as the default value for this parameter.
--
-- For @GetTags@, MaxResults has an upper limit of 1000.
getTags_maxResults :: Lens.Lens' GetTags (Prelude.Maybe Prelude.Natural)
getTags_maxResults = Lens.lens (\GetTags' {maxResults} -> maxResults) (\s@GetTags' {} a -> s {maxResults = a} :: GetTags)

-- | The value that you want to search for.
getTags_searchString :: Lens.Lens' GetTags (Prelude.Maybe Prelude.Text)
getTags_searchString = Lens.lens (\GetTags' {searchString} -> searchString) (\s@GetTags' {} a -> s {searchString = a} :: GetTags)

-- | The key of the tag that you want to return values for.
getTags_tagKey :: Lens.Lens' GetTags (Prelude.Maybe Prelude.Text)
getTags_tagKey = Lens.lens (\GetTags' {tagKey} -> tagKey) (\s@GetTags' {} a -> s {tagKey = a} :: GetTags)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getTags_nextPageToken :: Lens.Lens' GetTags (Prelude.Maybe Prelude.Text)
getTags_nextPageToken = Lens.lens (\GetTags' {nextPageToken} -> nextPageToken) (\s@GetTags' {} a -> s {nextPageToken = a} :: GetTags)

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
getTags_sortBy :: Lens.Lens' GetTags (Prelude.Maybe [SortDefinition])
getTags_sortBy = Lens.lens (\GetTags' {sortBy} -> sortBy) (\s@GetTags' {} a -> s {sortBy = a} :: GetTags) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getTags_filter :: Lens.Lens' GetTags (Prelude.Maybe Expression)
getTags_filter = Lens.lens (\GetTags' {filter'} -> filter') (\s@GetTags' {} a -> s {filter' = a} :: GetTags)

-- | The start and end dates for retrieving the dimension values. The start
-- date is inclusive, but the end date is exclusive. For example, if
-- @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the cost and
-- usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
getTags_timePeriod :: Lens.Lens' GetTags DateInterval
getTags_timePeriod = Lens.lens (\GetTags' {timePeriod} -> timePeriod) (\s@GetTags' {} a -> s {timePeriod = a} :: GetTags)

instance Core.AWSRequest GetTags where
  type AWSResponse GetTags = GetTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "ReturnSize")
            Prelude.<*> (x Core..:> "TotalSize")
      )

instance Prelude.Hashable GetTags

instance Prelude.NFData GetTags

instance Core.ToHeaders GetTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTags where
  toJSON GetTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SearchString" Core..=) Prelude.<$> searchString,
            ("TagKey" Core..=) Prelude.<$> tagKey,
            ("NextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("Filter" Core..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance Core.ToPath GetTags where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The token for the next set of retrievable results. AWS provides the
    -- token when the response from a previous call has more results than the
    -- maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The tags that match your request.
    tags :: [Prelude.Text],
    -- | The number of query results that AWS returns at a time.
    returnSize :: Prelude.Int,
    -- | The total number of query results.
    totalSize :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getTagsResponse_nextPageToken' - The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
--
-- 'httpStatus', 'getTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'getTagsResponse_tags' - The tags that match your request.
--
-- 'returnSize', 'getTagsResponse_returnSize' - The number of query results that AWS returns at a time.
--
-- 'totalSize', 'getTagsResponse_totalSize' - The total number of query results.
newGetTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'returnSize'
  Prelude.Int ->
  -- | 'totalSize'
  Prelude.Int ->
  GetTagsResponse
newGetTagsResponse
  pHttpStatus_
  pReturnSize_
  pTotalSize_ =
    GetTagsResponse'
      { nextPageToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        tags = Prelude.mempty,
        returnSize = pReturnSize_,
        totalSize = pTotalSize_
      }

-- | The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
getTagsResponse_nextPageToken :: Lens.Lens' GetTagsResponse (Prelude.Maybe Prelude.Text)
getTagsResponse_nextPageToken = Lens.lens (\GetTagsResponse' {nextPageToken} -> nextPageToken) (\s@GetTagsResponse' {} a -> s {nextPageToken = a} :: GetTagsResponse)

-- | The response's http status code.
getTagsResponse_httpStatus :: Lens.Lens' GetTagsResponse Prelude.Int
getTagsResponse_httpStatus = Lens.lens (\GetTagsResponse' {httpStatus} -> httpStatus) (\s@GetTagsResponse' {} a -> s {httpStatus = a} :: GetTagsResponse)

-- | The tags that match your request.
getTagsResponse_tags :: Lens.Lens' GetTagsResponse [Prelude.Text]
getTagsResponse_tags = Lens.lens (\GetTagsResponse' {tags} -> tags) (\s@GetTagsResponse' {} a -> s {tags = a} :: GetTagsResponse) Prelude.. Lens._Coerce

-- | The number of query results that AWS returns at a time.
getTagsResponse_returnSize :: Lens.Lens' GetTagsResponse Prelude.Int
getTagsResponse_returnSize = Lens.lens (\GetTagsResponse' {returnSize} -> returnSize) (\s@GetTagsResponse' {} a -> s {returnSize = a} :: GetTagsResponse)

-- | The total number of query results.
getTagsResponse_totalSize :: Lens.Lens' GetTagsResponse Prelude.Int
getTagsResponse_totalSize = Lens.lens (\GetTagsResponse' {totalSize} -> totalSize) (\s@GetTagsResponse' {} a -> s {totalSize = a} :: GetTagsResponse)

instance Prelude.NFData GetTagsResponse
