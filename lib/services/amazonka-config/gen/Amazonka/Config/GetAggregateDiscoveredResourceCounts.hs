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
-- Module      : Amazonka.Config.GetAggregateDiscoveredResourceCounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource counts across accounts and regions that are present
-- in your Config aggregator. You can request the resource counts by
-- providing filters and GroupByKey.
--
-- For example, if the input contains accountID 12345678910 and region
-- us-east-1 in filters, the API returns the count of resources in account
-- ID 12345678910 and region us-east-1. If the input contains ACCOUNT_ID as
-- a GroupByKey, the API returns resource counts for all source accounts
-- that are present in your aggregator.
module Amazonka.Config.GetAggregateDiscoveredResourceCounts
  ( -- * Creating a Request
    GetAggregateDiscoveredResourceCounts (..),
    newGetAggregateDiscoveredResourceCounts,

    -- * Request Lenses
    getAggregateDiscoveredResourceCounts_filters,
    getAggregateDiscoveredResourceCounts_groupByKey,
    getAggregateDiscoveredResourceCounts_limit,
    getAggregateDiscoveredResourceCounts_nextToken,
    getAggregateDiscoveredResourceCounts_configurationAggregatorName,

    -- * Destructuring the Response
    GetAggregateDiscoveredResourceCountsResponse (..),
    newGetAggregateDiscoveredResourceCountsResponse,

    -- * Response Lenses
    getAggregateDiscoveredResourceCountsResponse_groupByKey,
    getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts,
    getAggregateDiscoveredResourceCountsResponse_nextToken,
    getAggregateDiscoveredResourceCountsResponse_httpStatus,
    getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAggregateDiscoveredResourceCounts' smart constructor.
data GetAggregateDiscoveredResourceCounts = GetAggregateDiscoveredResourceCounts'
  { -- | Filters the results based on the @ResourceCountFilters@ object.
    filters :: Prelude.Maybe ResourceCountFilters,
    -- | The key to group the resource counts.
    groupByKey :: Prelude.Maybe ResourceCountGroupKey,
    -- | The maximum number of GroupedResourceCount objects returned on each
    -- page. The default is 1000. You cannot specify a number greater than
    -- 1000. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAggregateDiscoveredResourceCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'getAggregateDiscoveredResourceCounts_filters' - Filters the results based on the @ResourceCountFilters@ object.
--
-- 'groupByKey', 'getAggregateDiscoveredResourceCounts_groupByKey' - The key to group the resource counts.
--
-- 'limit', 'getAggregateDiscoveredResourceCounts_limit' - The maximum number of GroupedResourceCount objects returned on each
-- page. The default is 1000. You cannot specify a number greater than
-- 1000. If you specify 0, Config uses the default.
--
-- 'nextToken', 'getAggregateDiscoveredResourceCounts_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregatorName', 'getAggregateDiscoveredResourceCounts_configurationAggregatorName' - The name of the configuration aggregator.
newGetAggregateDiscoveredResourceCounts ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  GetAggregateDiscoveredResourceCounts
newGetAggregateDiscoveredResourceCounts
  pConfigurationAggregatorName_ =
    GetAggregateDiscoveredResourceCounts'
      { filters =
          Prelude.Nothing,
        groupByKey = Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the results based on the @ResourceCountFilters@ object.
getAggregateDiscoveredResourceCounts_filters :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Prelude.Maybe ResourceCountFilters)
getAggregateDiscoveredResourceCounts_filters = Lens.lens (\GetAggregateDiscoveredResourceCounts' {filters} -> filters) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {filters = a} :: GetAggregateDiscoveredResourceCounts)

-- | The key to group the resource counts.
getAggregateDiscoveredResourceCounts_groupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Prelude.Maybe ResourceCountGroupKey)
getAggregateDiscoveredResourceCounts_groupByKey = Lens.lens (\GetAggregateDiscoveredResourceCounts' {groupByKey} -> groupByKey) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {groupByKey = a} :: GetAggregateDiscoveredResourceCounts)

-- | The maximum number of GroupedResourceCount objects returned on each
-- page. The default is 1000. You cannot specify a number greater than
-- 1000. If you specify 0, Config uses the default.
getAggregateDiscoveredResourceCounts_limit :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Prelude.Maybe Prelude.Natural)
getAggregateDiscoveredResourceCounts_limit = Lens.lens (\GetAggregateDiscoveredResourceCounts' {limit} -> limit) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {limit = a} :: GetAggregateDiscoveredResourceCounts)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateDiscoveredResourceCounts_nextToken :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Prelude.Maybe Prelude.Text)
getAggregateDiscoveredResourceCounts_nextToken = Lens.lens (\GetAggregateDiscoveredResourceCounts' {nextToken} -> nextToken) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {nextToken = a} :: GetAggregateDiscoveredResourceCounts)

-- | The name of the configuration aggregator.
getAggregateDiscoveredResourceCounts_configurationAggregatorName :: Lens.Lens' GetAggregateDiscoveredResourceCounts Prelude.Text
getAggregateDiscoveredResourceCounts_configurationAggregatorName = Lens.lens (\GetAggregateDiscoveredResourceCounts' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateDiscoveredResourceCounts' {} a -> s {configurationAggregatorName = a} :: GetAggregateDiscoveredResourceCounts)

instance
  Core.AWSRequest
    GetAggregateDiscoveredResourceCounts
  where
  type
    AWSResponse GetAggregateDiscoveredResourceCounts =
      GetAggregateDiscoveredResourceCountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateDiscoveredResourceCountsResponse'
            Prelude.<$> (x Data..?> "GroupByKey")
              Prelude.<*> ( x Data..?> "GroupedResourceCounts"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "TotalDiscoveredResources")
      )

instance
  Prelude.Hashable
    GetAggregateDiscoveredResourceCounts
  where
  hashWithSalt
    _salt
    GetAggregateDiscoveredResourceCounts' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` groupByKey
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` configurationAggregatorName

instance
  Prelude.NFData
    GetAggregateDiscoveredResourceCounts
  where
  rnf GetAggregateDiscoveredResourceCounts' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf groupByKey
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf configurationAggregatorName

instance
  Data.ToHeaders
    GetAggregateDiscoveredResourceCounts
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetAggregateDiscoveredResourceCounts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetAggregateDiscoveredResourceCounts
  where
  toJSON GetAggregateDiscoveredResourceCounts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("GroupByKey" Data..=) Prelude.<$> groupByKey,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Data..= configurationAggregatorName
              )
          ]
      )

instance
  Data.ToPath
    GetAggregateDiscoveredResourceCounts
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetAggregateDiscoveredResourceCounts
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAggregateDiscoveredResourceCountsResponse' smart constructor.
data GetAggregateDiscoveredResourceCountsResponse = GetAggregateDiscoveredResourceCountsResponse'
  { -- | The key passed into the request object. If @GroupByKey@ is not provided,
    -- the result will be empty.
    groupByKey :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of GroupedResourceCount objects.
    groupedResourceCounts :: Prelude.Maybe [GroupedResourceCount],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The total number of resources that are present in an aggregator with the
    -- filters that you provide.
    totalDiscoveredResources :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAggregateDiscoveredResourceCountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupByKey', 'getAggregateDiscoveredResourceCountsResponse_groupByKey' - The key passed into the request object. If @GroupByKey@ is not provided,
-- the result will be empty.
--
-- 'groupedResourceCounts', 'getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts' - Returns a list of GroupedResourceCount objects.
--
-- 'nextToken', 'getAggregateDiscoveredResourceCountsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'getAggregateDiscoveredResourceCountsResponse_httpStatus' - The response's http status code.
--
-- 'totalDiscoveredResources', 'getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources' - The total number of resources that are present in an aggregator with the
-- filters that you provide.
newGetAggregateDiscoveredResourceCountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'totalDiscoveredResources'
  Prelude.Integer ->
  GetAggregateDiscoveredResourceCountsResponse
newGetAggregateDiscoveredResourceCountsResponse
  pHttpStatus_
  pTotalDiscoveredResources_ =
    GetAggregateDiscoveredResourceCountsResponse'
      { groupByKey =
          Prelude.Nothing,
        groupedResourceCounts =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        totalDiscoveredResources =
          pTotalDiscoveredResources_
      }

-- | The key passed into the request object. If @GroupByKey@ is not provided,
-- the result will be empty.
getAggregateDiscoveredResourceCountsResponse_groupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Prelude.Maybe Prelude.Text)
getAggregateDiscoveredResourceCountsResponse_groupByKey = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {groupByKey} -> groupByKey) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {groupByKey = a} :: GetAggregateDiscoveredResourceCountsResponse)

-- | Returns a list of GroupedResourceCount objects.
getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Prelude.Maybe [GroupedResourceCount])
getAggregateDiscoveredResourceCountsResponse_groupedResourceCounts = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {groupedResourceCounts} -> groupedResourceCounts) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {groupedResourceCounts = a} :: GetAggregateDiscoveredResourceCountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateDiscoveredResourceCountsResponse_nextToken :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Prelude.Maybe Prelude.Text)
getAggregateDiscoveredResourceCountsResponse_nextToken = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {nextToken} -> nextToken) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {nextToken = a} :: GetAggregateDiscoveredResourceCountsResponse)

-- | The response's http status code.
getAggregateDiscoveredResourceCountsResponse_httpStatus :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Prelude.Int
getAggregateDiscoveredResourceCountsResponse_httpStatus = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {httpStatus} -> httpStatus) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {httpStatus = a} :: GetAggregateDiscoveredResourceCountsResponse)

-- | The total number of resources that are present in an aggregator with the
-- filters that you provide.
getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Prelude.Integer
getAggregateDiscoveredResourceCountsResponse_totalDiscoveredResources = Lens.lens (\GetAggregateDiscoveredResourceCountsResponse' {totalDiscoveredResources} -> totalDiscoveredResources) (\s@GetAggregateDiscoveredResourceCountsResponse' {} a -> s {totalDiscoveredResources = a} :: GetAggregateDiscoveredResourceCountsResponse)

instance
  Prelude.NFData
    GetAggregateDiscoveredResourceCountsResponse
  where
  rnf GetAggregateDiscoveredResourceCountsResponse' {..} =
    Prelude.rnf groupByKey
      `Prelude.seq` Prelude.rnf groupedResourceCounts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf totalDiscoveredResources
