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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribePools
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified pools or all pools associated with your Amazon
-- Web Services account.
--
-- If you specify pool IDs, the output includes information for only the
-- specified pools. If you specify filters, the output includes information
-- for only those pools that meet the filter criteria. If you don\'t
-- specify pool IDs or filters, the output includes information for all
-- pools.
--
-- If you specify a pool ID that isn\'t valid, an Error is returned.
--
-- A pool is a collection of phone numbers and SenderIds. A pool can
-- include one or more phone numbers and SenderIds that are associated with
-- your Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribePools
  ( -- * Creating a Request
    DescribePools (..),
    newDescribePools,

    -- * Request Lenses
    describePools_nextToken,
    describePools_poolIds,
    describePools_filters,
    describePools_maxResults,

    -- * Destructuring the Response
    DescribePoolsResponse (..),
    newDescribePoolsResponse,

    -- * Response Lenses
    describePoolsResponse_nextToken,
    describePoolsResponse_pools,
    describePoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePools' smart constructor.
data DescribePools = DescribePools'
  { -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of pools to find. This is an array of strings that
    -- can be either the PoolId or PoolArn.
    poolIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of PoolFilter objects to filter the results.
    filters :: Prelude.Maybe [PoolFilter],
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePools_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
--
-- 'poolIds', 'describePools_poolIds' - The unique identifier of pools to find. This is an array of strings that
-- can be either the PoolId or PoolArn.
--
-- 'filters', 'describePools_filters' - An array of PoolFilter objects to filter the results.
--
-- 'maxResults', 'describePools_maxResults' - The maximum number of results to return per each request.
newDescribePools ::
  DescribePools
newDescribePools =
  DescribePools'
    { nextToken = Prelude.Nothing,
      poolIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describePools_nextToken :: Lens.Lens' DescribePools (Prelude.Maybe Prelude.Text)
describePools_nextToken = Lens.lens (\DescribePools' {nextToken} -> nextToken) (\s@DescribePools' {} a -> s {nextToken = a} :: DescribePools)

-- | The unique identifier of pools to find. This is an array of strings that
-- can be either the PoolId or PoolArn.
describePools_poolIds :: Lens.Lens' DescribePools (Prelude.Maybe [Prelude.Text])
describePools_poolIds = Lens.lens (\DescribePools' {poolIds} -> poolIds) (\s@DescribePools' {} a -> s {poolIds = a} :: DescribePools) Prelude.. Lens.mapping Lens.coerced

-- | An array of PoolFilter objects to filter the results.
describePools_filters :: Lens.Lens' DescribePools (Prelude.Maybe [PoolFilter])
describePools_filters = Lens.lens (\DescribePools' {filters} -> filters) (\s@DescribePools' {} a -> s {filters = a} :: DescribePools) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per each request.
describePools_maxResults :: Lens.Lens' DescribePools (Prelude.Maybe Prelude.Natural)
describePools_maxResults = Lens.lens (\DescribePools' {maxResults} -> maxResults) (\s@DescribePools' {} a -> s {maxResults = a} :: DescribePools)

instance Core.AWSPager DescribePools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePoolsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePoolsResponse_pools Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePools_nextToken
          Lens..~ rs
          Lens.^? describePoolsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribePools where
  type
    AWSResponse DescribePools =
      DescribePoolsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePoolsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Pools" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePools where
  hashWithSalt _salt DescribePools' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` poolIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribePools where
  rnf DescribePools' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf poolIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribePools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribePools" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePools where
  toJSON DescribePools' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PoolIds" Data..=) Prelude.<$> poolIds,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribePools where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePools where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePoolsResponse' smart constructor.
data DescribePoolsResponse = DescribePoolsResponse'
  { -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of PoolInformation objects that contain the details for the
    -- requested pools.
    pools :: Prelude.Maybe [PoolInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePoolsResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'pools', 'describePoolsResponse_pools' - An array of PoolInformation objects that contain the details for the
-- requested pools.
--
-- 'httpStatus', 'describePoolsResponse_httpStatus' - The response's http status code.
newDescribePoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePoolsResponse
newDescribePoolsResponse pHttpStatus_ =
  DescribePoolsResponse'
    { nextToken = Prelude.Nothing,
      pools = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describePoolsResponse_nextToken :: Lens.Lens' DescribePoolsResponse (Prelude.Maybe Prelude.Text)
describePoolsResponse_nextToken = Lens.lens (\DescribePoolsResponse' {nextToken} -> nextToken) (\s@DescribePoolsResponse' {} a -> s {nextToken = a} :: DescribePoolsResponse)

-- | An array of PoolInformation objects that contain the details for the
-- requested pools.
describePoolsResponse_pools :: Lens.Lens' DescribePoolsResponse (Prelude.Maybe [PoolInformation])
describePoolsResponse_pools = Lens.lens (\DescribePoolsResponse' {pools} -> pools) (\s@DescribePoolsResponse' {} a -> s {pools = a} :: DescribePoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePoolsResponse_httpStatus :: Lens.Lens' DescribePoolsResponse Prelude.Int
describePoolsResponse_httpStatus = Lens.lens (\DescribePoolsResponse' {httpStatus} -> httpStatus) (\s@DescribePoolsResponse' {} a -> s {httpStatus = a} :: DescribePoolsResponse)

instance Prelude.NFData DescribePoolsResponse where
  rnf DescribePoolsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pools
      `Prelude.seq` Prelude.rnf httpStatus
