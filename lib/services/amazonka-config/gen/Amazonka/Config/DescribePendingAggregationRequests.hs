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
-- Module      : Amazonka.Config.DescribePendingAggregationRequests
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all pending aggregation requests.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribePendingAggregationRequests
  ( -- * Creating a Request
    DescribePendingAggregationRequests (..),
    newDescribePendingAggregationRequests,

    -- * Request Lenses
    describePendingAggregationRequests_limit,
    describePendingAggregationRequests_nextToken,

    -- * Destructuring the Response
    DescribePendingAggregationRequestsResponse (..),
    newDescribePendingAggregationRequestsResponse,

    -- * Response Lenses
    describePendingAggregationRequestsResponse_nextToken,
    describePendingAggregationRequestsResponse_pendingAggregationRequests,
    describePendingAggregationRequestsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePendingAggregationRequests' smart constructor.
data DescribePendingAggregationRequests = DescribePendingAggregationRequests'
  { -- | The maximum number of evaluation results returned on each page. The
    -- default is maximum. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePendingAggregationRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describePendingAggregationRequests_limit' - The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, Config uses the default.
--
-- 'nextToken', 'describePendingAggregationRequests_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribePendingAggregationRequests ::
  DescribePendingAggregationRequests
newDescribePendingAggregationRequests =
  DescribePendingAggregationRequests'
    { limit =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, Config uses the default.
describePendingAggregationRequests_limit :: Lens.Lens' DescribePendingAggregationRequests (Prelude.Maybe Prelude.Natural)
describePendingAggregationRequests_limit = Lens.lens (\DescribePendingAggregationRequests' {limit} -> limit) (\s@DescribePendingAggregationRequests' {} a -> s {limit = a} :: DescribePendingAggregationRequests)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describePendingAggregationRequests_nextToken :: Lens.Lens' DescribePendingAggregationRequests (Prelude.Maybe Prelude.Text)
describePendingAggregationRequests_nextToken = Lens.lens (\DescribePendingAggregationRequests' {nextToken} -> nextToken) (\s@DescribePendingAggregationRequests' {} a -> s {nextToken = a} :: DescribePendingAggregationRequests)

instance
  Core.AWSPager
    DescribePendingAggregationRequests
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePendingAggregationRequestsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePendingAggregationRequestsResponse_pendingAggregationRequests
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describePendingAggregationRequests_nextToken
              Lens..~ rs
              Lens.^? describePendingAggregationRequestsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribePendingAggregationRequests
  where
  type
    AWSResponse DescribePendingAggregationRequests =
      DescribePendingAggregationRequestsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePendingAggregationRequestsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "PendingAggregationRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePendingAggregationRequests
  where
  hashWithSalt
    _salt
    DescribePendingAggregationRequests' {..} =
      _salt
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribePendingAggregationRequests
  where
  rnf DescribePendingAggregationRequests' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribePendingAggregationRequests
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribePendingAggregationRequests" ::
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
    DescribePendingAggregationRequests
  where
  toJSON DescribePendingAggregationRequests' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribePendingAggregationRequests
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribePendingAggregationRequests
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePendingAggregationRequestsResponse' smart constructor.
data DescribePendingAggregationRequestsResponse = DescribePendingAggregationRequestsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a PendingAggregationRequests object.
    pendingAggregationRequests :: Prelude.Maybe [PendingAggregationRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePendingAggregationRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePendingAggregationRequestsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'pendingAggregationRequests', 'describePendingAggregationRequestsResponse_pendingAggregationRequests' - Returns a PendingAggregationRequests object.
--
-- 'httpStatus', 'describePendingAggregationRequestsResponse_httpStatus' - The response's http status code.
newDescribePendingAggregationRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePendingAggregationRequestsResponse
newDescribePendingAggregationRequestsResponse
  pHttpStatus_ =
    DescribePendingAggregationRequestsResponse'
      { nextToken =
          Prelude.Nothing,
        pendingAggregationRequests =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describePendingAggregationRequestsResponse_nextToken :: Lens.Lens' DescribePendingAggregationRequestsResponse (Prelude.Maybe Prelude.Text)
describePendingAggregationRequestsResponse_nextToken = Lens.lens (\DescribePendingAggregationRequestsResponse' {nextToken} -> nextToken) (\s@DescribePendingAggregationRequestsResponse' {} a -> s {nextToken = a} :: DescribePendingAggregationRequestsResponse)

-- | Returns a PendingAggregationRequests object.
describePendingAggregationRequestsResponse_pendingAggregationRequests :: Lens.Lens' DescribePendingAggregationRequestsResponse (Prelude.Maybe [PendingAggregationRequest])
describePendingAggregationRequestsResponse_pendingAggregationRequests = Lens.lens (\DescribePendingAggregationRequestsResponse' {pendingAggregationRequests} -> pendingAggregationRequests) (\s@DescribePendingAggregationRequestsResponse' {} a -> s {pendingAggregationRequests = a} :: DescribePendingAggregationRequestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePendingAggregationRequestsResponse_httpStatus :: Lens.Lens' DescribePendingAggregationRequestsResponse Prelude.Int
describePendingAggregationRequestsResponse_httpStatus = Lens.lens (\DescribePendingAggregationRequestsResponse' {httpStatus} -> httpStatus) (\s@DescribePendingAggregationRequestsResponse' {} a -> s {httpStatus = a} :: DescribePendingAggregationRequestsResponse)

instance
  Prelude.NFData
    DescribePendingAggregationRequestsResponse
  where
  rnf DescribePendingAggregationRequestsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf pendingAggregationRequests `Prelude.seq`
        Prelude.rnf httpStatus
