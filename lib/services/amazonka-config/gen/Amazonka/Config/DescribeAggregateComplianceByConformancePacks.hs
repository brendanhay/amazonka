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
-- Module      : Amazonka.Config.DescribeAggregateComplianceByConformancePacks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the conformance packs and their associated compliance
-- status with the count of compliant and noncompliant Config rules within
-- each conformance pack. Also returns the total rule count which includes
-- compliant rules, noncompliant rules, and rules that cannot be evaluated
-- due to insufficient data.
--
-- The results can return an empty result page, but if you have a
-- @nextToken@, the results are displayed on the next page.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeAggregateComplianceByConformancePacks
  ( -- * Creating a Request
    DescribeAggregateComplianceByConformancePacks (..),
    newDescribeAggregateComplianceByConformancePacks,

    -- * Request Lenses
    describeAggregateComplianceByConformancePacks_filters,
    describeAggregateComplianceByConformancePacks_limit,
    describeAggregateComplianceByConformancePacks_nextToken,
    describeAggregateComplianceByConformancePacks_configurationAggregatorName,

    -- * Destructuring the Response
    DescribeAggregateComplianceByConformancePacksResponse (..),
    newDescribeAggregateComplianceByConformancePacksResponse,

    -- * Response Lenses
    describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks,
    describeAggregateComplianceByConformancePacksResponse_nextToken,
    describeAggregateComplianceByConformancePacksResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAggregateComplianceByConformancePacks' smart constructor.
data DescribeAggregateComplianceByConformancePacks = DescribeAggregateComplianceByConformancePacks'
  { -- | Filters the result by @AggregateConformancePackComplianceFilters@
    -- object.
    filters :: Prelude.Maybe AggregateConformancePackComplianceFilters,
    -- | The maximum number of conformance packs compliance details returned on
    -- each page. The default is maximum. If you specify 0, Config uses the
    -- default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregateComplianceByConformancePacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeAggregateComplianceByConformancePacks_filters' - Filters the result by @AggregateConformancePackComplianceFilters@
-- object.
--
-- 'limit', 'describeAggregateComplianceByConformancePacks_limit' - The maximum number of conformance packs compliance details returned on
-- each page. The default is maximum. If you specify 0, Config uses the
-- default.
--
-- 'nextToken', 'describeAggregateComplianceByConformancePacks_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregatorName', 'describeAggregateComplianceByConformancePacks_configurationAggregatorName' - The name of the configuration aggregator.
newDescribeAggregateComplianceByConformancePacks ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  DescribeAggregateComplianceByConformancePacks
newDescribeAggregateComplianceByConformancePacks
  pConfigurationAggregatorName_ =
    DescribeAggregateComplianceByConformancePacks'
      { filters =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | Filters the result by @AggregateConformancePackComplianceFilters@
-- object.
describeAggregateComplianceByConformancePacks_filters :: Lens.Lens' DescribeAggregateComplianceByConformancePacks (Prelude.Maybe AggregateConformancePackComplianceFilters)
describeAggregateComplianceByConformancePacks_filters = Lens.lens (\DescribeAggregateComplianceByConformancePacks' {filters} -> filters) (\s@DescribeAggregateComplianceByConformancePacks' {} a -> s {filters = a} :: DescribeAggregateComplianceByConformancePacks)

-- | The maximum number of conformance packs compliance details returned on
-- each page. The default is maximum. If you specify 0, Config uses the
-- default.
describeAggregateComplianceByConformancePacks_limit :: Lens.Lens' DescribeAggregateComplianceByConformancePacks (Prelude.Maybe Prelude.Natural)
describeAggregateComplianceByConformancePacks_limit = Lens.lens (\DescribeAggregateComplianceByConformancePacks' {limit} -> limit) (\s@DescribeAggregateComplianceByConformancePacks' {} a -> s {limit = a} :: DescribeAggregateComplianceByConformancePacks)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConformancePacks_nextToken :: Lens.Lens' DescribeAggregateComplianceByConformancePacks (Prelude.Maybe Prelude.Text)
describeAggregateComplianceByConformancePacks_nextToken = Lens.lens (\DescribeAggregateComplianceByConformancePacks' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConformancePacks' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConformancePacks)

-- | The name of the configuration aggregator.
describeAggregateComplianceByConformancePacks_configurationAggregatorName :: Lens.Lens' DescribeAggregateComplianceByConformancePacks Prelude.Text
describeAggregateComplianceByConformancePacks_configurationAggregatorName = Lens.lens (\DescribeAggregateComplianceByConformancePacks' {configurationAggregatorName} -> configurationAggregatorName) (\s@DescribeAggregateComplianceByConformancePacks' {} a -> s {configurationAggregatorName = a} :: DescribeAggregateComplianceByConformancePacks)

instance
  Core.AWSPager
    DescribeAggregateComplianceByConformancePacks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAggregateComplianceByConformancePacksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeAggregateComplianceByConformancePacks_nextToken
          Lens..~ rs
          Lens.^? describeAggregateComplianceByConformancePacksResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAggregateComplianceByConformancePacks
  where
  type
    AWSResponse
      DescribeAggregateComplianceByConformancePacks =
      DescribeAggregateComplianceByConformancePacksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregateComplianceByConformancePacksResponse'
            Prelude.<$> ( x
                            Data..?> "AggregateComplianceByConformancePacks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAggregateComplianceByConformancePacks
  where
  hashWithSalt
    _salt
    DescribeAggregateComplianceByConformancePacks' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` configurationAggregatorName

instance
  Prelude.NFData
    DescribeAggregateComplianceByConformancePacks
  where
  rnf
    DescribeAggregateComplianceByConformancePacks' {..} =
      Prelude.rnf filters
        `Prelude.seq` Prelude.rnf limit
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf configurationAggregatorName

instance
  Data.ToHeaders
    DescribeAggregateComplianceByConformancePacks
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeAggregateComplianceByConformancePacks" ::
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
    DescribeAggregateComplianceByConformancePacks
  where
  toJSON
    DescribeAggregateComplianceByConformancePacks' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Filters" Data..=) Prelude.<$> filters,
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
    DescribeAggregateComplianceByConformancePacks
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAggregateComplianceByConformancePacks
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAggregateComplianceByConformancePacksResponse' smart constructor.
data DescribeAggregateComplianceByConformancePacksResponse = DescribeAggregateComplianceByConformancePacksResponse'
  { -- | Returns the @AggregateComplianceByConformancePack@ object.
    aggregateComplianceByConformancePacks :: Prelude.Maybe [AggregateComplianceByConformancePack],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregateComplianceByConformancePacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregateComplianceByConformancePacks', 'describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks' - Returns the @AggregateComplianceByConformancePack@ object.
--
-- 'nextToken', 'describeAggregateComplianceByConformancePacksResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeAggregateComplianceByConformancePacksResponse_httpStatus' - The response's http status code.
newDescribeAggregateComplianceByConformancePacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAggregateComplianceByConformancePacksResponse
newDescribeAggregateComplianceByConformancePacksResponse
  pHttpStatus_ =
    DescribeAggregateComplianceByConformancePacksResponse'
      { aggregateComplianceByConformancePacks =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns the @AggregateComplianceByConformancePack@ object.
describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks :: Lens.Lens' DescribeAggregateComplianceByConformancePacksResponse (Prelude.Maybe [AggregateComplianceByConformancePack])
describeAggregateComplianceByConformancePacksResponse_aggregateComplianceByConformancePacks = Lens.lens (\DescribeAggregateComplianceByConformancePacksResponse' {aggregateComplianceByConformancePacks} -> aggregateComplianceByConformancePacks) (\s@DescribeAggregateComplianceByConformancePacksResponse' {} a -> s {aggregateComplianceByConformancePacks = a} :: DescribeAggregateComplianceByConformancePacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregateComplianceByConformancePacksResponse_nextToken :: Lens.Lens' DescribeAggregateComplianceByConformancePacksResponse (Prelude.Maybe Prelude.Text)
describeAggregateComplianceByConformancePacksResponse_nextToken = Lens.lens (\DescribeAggregateComplianceByConformancePacksResponse' {nextToken} -> nextToken) (\s@DescribeAggregateComplianceByConformancePacksResponse' {} a -> s {nextToken = a} :: DescribeAggregateComplianceByConformancePacksResponse)

-- | The response's http status code.
describeAggregateComplianceByConformancePacksResponse_httpStatus :: Lens.Lens' DescribeAggregateComplianceByConformancePacksResponse Prelude.Int
describeAggregateComplianceByConformancePacksResponse_httpStatus = Lens.lens (\DescribeAggregateComplianceByConformancePacksResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregateComplianceByConformancePacksResponse' {} a -> s {httpStatus = a} :: DescribeAggregateComplianceByConformancePacksResponse)

instance
  Prelude.NFData
    DescribeAggregateComplianceByConformancePacksResponse
  where
  rnf
    DescribeAggregateComplianceByConformancePacksResponse' {..} =
      Prelude.rnf aggregateComplianceByConformancePacks
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
