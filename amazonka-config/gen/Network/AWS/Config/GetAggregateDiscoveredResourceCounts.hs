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
-- Module      : Network.AWS.Config.GetAggregateDiscoveredResourceCounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource counts across accounts and regions that are present
-- in your AWS Config aggregator. You can request the resource counts by
-- providing filters and GroupByKey.
--
-- For example, if the input contains accountID 12345678910 and region
-- us-east-1 in filters, the API returns the count of resources in account
-- ID 12345678910 and region us-east-1. If the input contains ACCOUNT_ID as
-- a GroupByKey, the API returns resource counts for all source accounts
-- that are present in your aggregator.
module Network.AWS.Config.GetAggregateDiscoveredResourceCounts
  ( -- * Creating a Request
    GetAggregateDiscoveredResourceCounts (..),
    newGetAggregateDiscoveredResourceCounts,

    -- * Request Lenses
    getAggregateDiscoveredResourceCounts_nextToken,
    getAggregateDiscoveredResourceCounts_filters,
    getAggregateDiscoveredResourceCounts_groupByKey,
    getAggregateDiscoveredResourceCounts_limit,
    getAggregateDiscoveredResourceCounts_configurationAggregatorName,

    -- * Destructuring the Response
    GetAggregateDiscoveredResourceCountsResponse (..),
    newGetAggregateDiscoveredResourceCountsResponse,

    -- * Response Lenses
    getAggregateDiscoveredResourceCountsResponse_nextToken,
    getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts,
    getAggregateDiscoveredResourceCountsResponse_groupByKey,
    getAggregateDiscoveredResourceCountsResponse_httpStatus,
    getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAggregateDiscoveredResourceCounts' smart constructor.
data GetAggregateDiscoveredResourceCounts = GetAggregateDiscoveredResourceCounts'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results based on the @ResourceCountFilters@ object.
    filters :: Core.Maybe ResourceCountFilters,
    -- | The key to group the resource counts.
    groupByKey :: Core.Maybe ResourceCountGroupKey,
    -- | The maximum number of GroupedResourceCount objects returned on each
    -- page. The default is 1000. You cannot specify a number greater than
    -- 1000. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateDiscoveredResourceCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateDiscoveredResourceCounts_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'filters', 'getAggregateDiscoveredResourceCounts_filters' - Filters the results based on the @ResourceCountFilters@ object.
--
-- 'groupByKey', 'getAggregateDiscoveredResourceCounts_groupByKey' - The key to group the resource counts.
--
-- 'limit', 'getAggregateDiscoveredResourceCounts_limit' - The maximum number of GroupedResourceCount objects returned on each
-- page. The default is 1000. You cannot specify a number greater than
-- 1000. If you specify 0, AWS Config uses the default.
--
-- 'configurationAggregatorName', 'getAggregateDiscoveredResourceCounts_configurationAggregatorName' - The name of the configuration aggregator.
newGetAggregateDiscoveredResourceCounts ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  GetAggregateDiscoveredResourceCounts
newGetAggregateDiscoveredResourceCounts
  pConfigurationAggregatorName_ =
    GetAggregateDiscoveredResourceCounts'
      { nextToken =
          Core.Nothing,
        filters = Core.Nothing,
        groupByKey = Core.Nothing,
        limit = Core.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateDiscoveredResourceCounts_nextToken :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Core.Text)
getAggregateDiscoveredResourceCounts_nextToken = Lens.lens (\GetAggregateDiscoveredResourceCounts' {nextToken} -> nextToken) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {nextToken = a} :: GetAggregateDiscoveredResourceCounts)

-- | Filters the results based on the @ResourceCountFilters@ object.
getAggregateDiscoveredResourceCounts_filters :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe ResourceCountFilters)
getAggregateDiscoveredResourceCounts_filters = Lens.lens (\GetAggregateDiscoveredResourceCounts' {filters} -> filters) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {filters = a} :: GetAggregateDiscoveredResourceCounts)

-- | The key to group the resource counts.
getAggregateDiscoveredResourceCounts_groupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe ResourceCountGroupKey)
getAggregateDiscoveredResourceCounts_groupByKey = Lens.lens (\GetAggregateDiscoveredResourceCounts' {groupByKey} -> groupByKey) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {groupByKey = a} :: GetAggregateDiscoveredResourceCounts)

-- | The maximum number of GroupedResourceCount objects returned on each
-- page. The default is 1000. You cannot specify a number greater than
-- 1000. If you specify 0, AWS Config uses the default.
getAggregateDiscoveredResourceCounts_limit :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Core.Natural)
getAggregateDiscoveredResourceCounts_limit = Lens.lens (\GetAggregateDiscoveredResourceCounts' {limit} -> limit) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {limit = a} :: GetAggregateDiscoveredResourceCounts)

-- | The name of the configuration aggregator.
getAggregateDiscoveredResourceCounts_configurationAggregatorName :: Lens.Lens' GetAggregateDiscoveredResourceCounts Core.Text
getAggregateDiscoveredResourceCounts_configurationAggregatorName = Lens.lens (\GetAggregateDiscoveredResourceCounts' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {configurationAggregatorName = a} :: GetAggregateDiscoveredResourceCounts)

instance
  Core.AWSRequest
    GetAggregateDiscoveredResourceCounts
  where
  type
    AWSResponse GetAggregateDiscoveredResourceCounts =
      GetAggregateDiscoveredResourceCountsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateDiscoveredResourceCountsResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "GroupedResourceCounts"
                           Core..!@ Core.mempty
                       )
              Core.<*> (x Core..?> "GroupByKey")
              Core.<*> (Core.pure (Core.fromEnum s))
              Core.<*> (x Core..:> "TotalDiscoveredResources")
      )

instance
  Core.Hashable
    GetAggregateDiscoveredResourceCounts

instance
  Core.NFData
    GetAggregateDiscoveredResourceCounts

instance
  Core.ToHeaders
    GetAggregateDiscoveredResourceCounts
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetAggregateDiscoveredResourceCounts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetAggregateDiscoveredResourceCounts
  where
  toJSON GetAggregateDiscoveredResourceCounts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Filters" Core..=) Core.<$> filters,
            ("GroupByKey" Core..=) Core.<$> groupByKey,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              )
          ]
      )

instance
  Core.ToPath
    GetAggregateDiscoveredResourceCounts
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetAggregateDiscoveredResourceCounts
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAggregateDiscoveredResourceCountsResponse' smart constructor.
data GetAggregateDiscoveredResourceCountsResponse = GetAggregateDiscoveredResourceCountsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of GroupedResourceCount objects.
    groupedResourceCounts :: Core.Maybe [GroupedResourceCount],
    -- | The key passed into the request object. If @GroupByKey@ is not provided,
    -- the result will be empty.
    groupByKey :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The total number of resources that are present in an aggregator with the
    -- filters that you provide.
    totalDiscoveredResources :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateDiscoveredResourceCountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateDiscoveredResourceCountsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'groupedResourceCounts', 'getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts' - Returns a list of GroupedResourceCount objects.
--
-- 'groupByKey', 'getAggregateDiscoveredResourceCountsResponse_groupByKey' - The key passed into the request object. If @GroupByKey@ is not provided,
-- the result will be empty.
--
-- 'httpStatus', 'getAggregateDiscoveredResourceCountsResponse_httpStatus' - The response's http status code.
--
-- 'totalDiscoveredResources', 'getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources' - The total number of resources that are present in an aggregator with the
-- filters that you provide.
newGetAggregateDiscoveredResourceCountsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'totalDiscoveredResources'
  Core.Integer ->
  GetAggregateDiscoveredResourceCountsResponse
newGetAggregateDiscoveredResourceCountsResponse
  pHttpStatus_
  pTotalDiscoveredResources_ =
    GetAggregateDiscoveredResourceCountsResponse'
      { nextToken =
          Core.Nothing,
        groupedResourceCounts =
          Core.Nothing,
        groupByKey = Core.Nothing,
        httpStatus = pHttpStatus_,
        totalDiscoveredResources =
          pTotalDiscoveredResources_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateDiscoveredResourceCountsResponse_nextToken :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe Core.Text)
getAggregateDiscoveredResourceCountsResponse_nextToken = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {nextToken} -> nextToken) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {nextToken = a} :: GetAggregateDiscoveredResourceCountsResponse)

-- | Returns a list of GroupedResourceCount objects.
getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe [GroupedResourceCount])
getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {groupedResourceCounts} -> groupedResourceCounts) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {groupedResourceCounts = a} :: GetAggregateDiscoveredResourceCountsResponse) Core.. Lens.mapping Lens._Coerce

-- | The key passed into the request object. If @GroupByKey@ is not provided,
-- the result will be empty.
getAggregateDiscoveredResourceCountsResponse_groupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe Core.Text)
getAggregateDiscoveredResourceCountsResponse_groupByKey = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {groupByKey} -> groupByKey) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {groupByKey = a} :: GetAggregateDiscoveredResourceCountsResponse)

-- | The response's http status code.
getAggregateDiscoveredResourceCountsResponse_httpStatus :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Core.Int
getAggregateDiscoveredResourceCountsResponse_httpStatus = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {httpStatus} -> httpStatus) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {httpStatus = a} :: GetAggregateDiscoveredResourceCountsResponse)

-- | The total number of resources that are present in an aggregator with the
-- filters that you provide.
getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Core.Integer
getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {totalDiscoveredResources} -> totalDiscoveredResources) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {totalDiscoveredResources = a} :: GetAggregateDiscoveredResourceCountsResponse)

instance
  Core.NFData
    GetAggregateDiscoveredResourceCountsResponse
