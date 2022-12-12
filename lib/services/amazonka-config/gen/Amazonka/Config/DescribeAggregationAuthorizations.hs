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
-- Module      : Amazonka.Config.DescribeAggregationAuthorizations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of authorizations granted to various aggregator accounts
-- and regions.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeAggregationAuthorizations
  ( -- * Creating a Request
    DescribeAggregationAuthorizations (..),
    newDescribeAggregationAuthorizations,

    -- * Request Lenses
    describeAggregationAuthorizations_limit,
    describeAggregationAuthorizations_nextToken,

    -- * Destructuring the Response
    DescribeAggregationAuthorizationsResponse (..),
    newDescribeAggregationAuthorizationsResponse,

    -- * Response Lenses
    describeAggregationAuthorizationsResponse_aggregationAuthorizations,
    describeAggregationAuthorizationsResponse_nextToken,
    describeAggregationAuthorizationsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAggregationAuthorizations' smart constructor.
data DescribeAggregationAuthorizations = DescribeAggregationAuthorizations'
  { -- | The maximum number of AggregationAuthorizations returned on each page.
    -- The default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregationAuthorizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeAggregationAuthorizations_limit' - The maximum number of AggregationAuthorizations returned on each page.
-- The default is maximum. If you specify 0, Config uses the default.
--
-- 'nextToken', 'describeAggregationAuthorizations_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribeAggregationAuthorizations ::
  DescribeAggregationAuthorizations
newDescribeAggregationAuthorizations =
  DescribeAggregationAuthorizations'
    { limit =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of AggregationAuthorizations returned on each page.
-- The default is maximum. If you specify 0, Config uses the default.
describeAggregationAuthorizations_limit :: Lens.Lens' DescribeAggregationAuthorizations (Prelude.Maybe Prelude.Natural)
describeAggregationAuthorizations_limit = Lens.lens (\DescribeAggregationAuthorizations' {limit} -> limit) (\s@DescribeAggregationAuthorizations' {} a -> s {limit = a} :: DescribeAggregationAuthorizations)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregationAuthorizations_nextToken :: Lens.Lens' DescribeAggregationAuthorizations (Prelude.Maybe Prelude.Text)
describeAggregationAuthorizations_nextToken = Lens.lens (\DescribeAggregationAuthorizations' {nextToken} -> nextToken) (\s@DescribeAggregationAuthorizations' {} a -> s {nextToken = a} :: DescribeAggregationAuthorizations)

instance
  Core.AWSPager
    DescribeAggregationAuthorizations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAggregationAuthorizationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAggregationAuthorizationsResponse_aggregationAuthorizations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAggregationAuthorizations_nextToken
          Lens..~ rs
          Lens.^? describeAggregationAuthorizationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeAggregationAuthorizations
  where
  type
    AWSResponse DescribeAggregationAuthorizations =
      DescribeAggregationAuthorizationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregationAuthorizationsResponse'
            Prelude.<$> ( x Data..?> "AggregationAuthorizations"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAggregationAuthorizations
  where
  hashWithSalt
    _salt
    DescribeAggregationAuthorizations' {..} =
      _salt `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeAggregationAuthorizations
  where
  rnf DescribeAggregationAuthorizations' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeAggregationAuthorizations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeAggregationAuthorizations" ::
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
    DescribeAggregationAuthorizations
  where
  toJSON DescribeAggregationAuthorizations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribeAggregationAuthorizations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAggregationAuthorizations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAggregationAuthorizationsResponse' smart constructor.
data DescribeAggregationAuthorizationsResponse = DescribeAggregationAuthorizationsResponse'
  { -- | Returns a list of authorizations granted to various aggregator accounts
    -- and regions.
    aggregationAuthorizations :: Prelude.Maybe [AggregationAuthorization],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAggregationAuthorizationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationAuthorizations', 'describeAggregationAuthorizationsResponse_aggregationAuthorizations' - Returns a list of authorizations granted to various aggregator accounts
-- and regions.
--
-- 'nextToken', 'describeAggregationAuthorizationsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeAggregationAuthorizationsResponse_httpStatus' - The response's http status code.
newDescribeAggregationAuthorizationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAggregationAuthorizationsResponse
newDescribeAggregationAuthorizationsResponse
  pHttpStatus_ =
    DescribeAggregationAuthorizationsResponse'
      { aggregationAuthorizations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of authorizations granted to various aggregator accounts
-- and regions.
describeAggregationAuthorizationsResponse_aggregationAuthorizations :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Prelude.Maybe [AggregationAuthorization])
describeAggregationAuthorizationsResponse_aggregationAuthorizations = Lens.lens (\DescribeAggregationAuthorizationsResponse' {aggregationAuthorizations} -> aggregationAuthorizations) (\s@DescribeAggregationAuthorizationsResponse' {} a -> s {aggregationAuthorizations = a} :: DescribeAggregationAuthorizationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregationAuthorizationsResponse_nextToken :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Prelude.Maybe Prelude.Text)
describeAggregationAuthorizationsResponse_nextToken = Lens.lens (\DescribeAggregationAuthorizationsResponse' {nextToken} -> nextToken) (\s@DescribeAggregationAuthorizationsResponse' {} a -> s {nextToken = a} :: DescribeAggregationAuthorizationsResponse)

-- | The response's http status code.
describeAggregationAuthorizationsResponse_httpStatus :: Lens.Lens' DescribeAggregationAuthorizationsResponse Prelude.Int
describeAggregationAuthorizationsResponse_httpStatus = Lens.lens (\DescribeAggregationAuthorizationsResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregationAuthorizationsResponse' {} a -> s {httpStatus = a} :: DescribeAggregationAuthorizationsResponse)

instance
  Prelude.NFData
    DescribeAggregationAuthorizationsResponse
  where
  rnf DescribeAggregationAuthorizationsResponse' {..} =
    Prelude.rnf aggregationAuthorizations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
