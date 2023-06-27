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
-- Module      : Amazonka.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Spot Fleet requests.
--
-- Spot Fleet requests are deleted 48 hours after they are canceled and
-- their instances are terminated.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeSpotFleetRequests
  ( -- * Creating a Request
    DescribeSpotFleetRequests (..),
    newDescribeSpotFleetRequests,

    -- * Request Lenses
    describeSpotFleetRequests_dryRun,
    describeSpotFleetRequests_maxResults,
    describeSpotFleetRequests_nextToken,
    describeSpotFleetRequests_spotFleetRequestIds,

    -- * Destructuring the Response
    DescribeSpotFleetRequestsResponse (..),
    newDescribeSpotFleetRequestsResponse,

    -- * Response Lenses
    describeSpotFleetRequestsResponse_nextToken,
    describeSpotFleetRequestsResponse_spotFleetRequestConfigs,
    describeSpotFleetRequestsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeSpotFleetRequests.
--
-- /See:/ 'newDescribeSpotFleetRequests' smart constructor.
data DescribeSpotFleetRequests = DescribeSpotFleetRequests'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Spot Fleet requests.
    spotFleetRequestIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotFleetRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSpotFleetRequests_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotFleetRequests_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeSpotFleetRequests_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'spotFleetRequestIds', 'describeSpotFleetRequests_spotFleetRequestIds' - The IDs of the Spot Fleet requests.
newDescribeSpotFleetRequests ::
  DescribeSpotFleetRequests
newDescribeSpotFleetRequests =
  DescribeSpotFleetRequests'
    { dryRun =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      spotFleetRequestIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotFleetRequests_dryRun :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe Prelude.Bool)
describeSpotFleetRequests_dryRun = Lens.lens (\DescribeSpotFleetRequests' {dryRun} -> dryRun) (\s@DescribeSpotFleetRequests' {} a -> s {dryRun = a} :: DescribeSpotFleetRequests)

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeSpotFleetRequests_maxResults :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe Prelude.Int)
describeSpotFleetRequests_maxResults = Lens.lens (\DescribeSpotFleetRequests' {maxResults} -> maxResults) (\s@DescribeSpotFleetRequests' {} a -> s {maxResults = a} :: DescribeSpotFleetRequests)

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeSpotFleetRequests_nextToken :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe Prelude.Text)
describeSpotFleetRequests_nextToken = Lens.lens (\DescribeSpotFleetRequests' {nextToken} -> nextToken) (\s@DescribeSpotFleetRequests' {} a -> s {nextToken = a} :: DescribeSpotFleetRequests)

-- | The IDs of the Spot Fleet requests.
describeSpotFleetRequests_spotFleetRequestIds :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe [Prelude.Text])
describeSpotFleetRequests_spotFleetRequestIds = Lens.lens (\DescribeSpotFleetRequests' {spotFleetRequestIds} -> spotFleetRequestIds) (\s@DescribeSpotFleetRequests' {} a -> s {spotFleetRequestIds = a} :: DescribeSpotFleetRequests) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeSpotFleetRequests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSpotFleetRequestsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSpotFleetRequestsResponse_spotFleetRequestConfigs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSpotFleetRequests_nextToken
          Lens..~ rs
          Lens.^? describeSpotFleetRequestsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSpotFleetRequests where
  type
    AWSResponse DescribeSpotFleetRequests =
      DescribeSpotFleetRequestsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetRequestsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "spotFleetRequestConfigSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSpotFleetRequests where
  hashWithSalt _salt DescribeSpotFleetRequests' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` spotFleetRequestIds

instance Prelude.NFData DescribeSpotFleetRequests where
  rnf DescribeSpotFleetRequests' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spotFleetRequestIds

instance Data.ToHeaders DescribeSpotFleetRequests where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSpotFleetRequests where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSpotFleetRequests where
  toQuery DescribeSpotFleetRequests' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSpotFleetRequests" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "SpotFleetRequestId"
              Prelude.<$> spotFleetRequestIds
          )
      ]

-- | Contains the output of DescribeSpotFleetRequests.
--
-- /See:/ 'newDescribeSpotFleetRequestsResponse' smart constructor.
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
  { -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the configuration of your Spot Fleet.
    spotFleetRequestConfigs :: Prelude.Maybe [SpotFleetRequestConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotFleetRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetRequestsResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'spotFleetRequestConfigs', 'describeSpotFleetRequestsResponse_spotFleetRequestConfigs' - Information about the configuration of your Spot Fleet.
--
-- 'httpStatus', 'describeSpotFleetRequestsResponse_httpStatus' - The response's http status code.
newDescribeSpotFleetRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSpotFleetRequestsResponse
newDescribeSpotFleetRequestsResponse pHttpStatus_ =
  DescribeSpotFleetRequestsResponse'
    { nextToken =
        Prelude.Nothing,
      spotFleetRequestConfigs =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeSpotFleetRequestsResponse_nextToken :: Lens.Lens' DescribeSpotFleetRequestsResponse (Prelude.Maybe Prelude.Text)
describeSpotFleetRequestsResponse_nextToken = Lens.lens (\DescribeSpotFleetRequestsResponse' {nextToken} -> nextToken) (\s@DescribeSpotFleetRequestsResponse' {} a -> s {nextToken = a} :: DescribeSpotFleetRequestsResponse)

-- | Information about the configuration of your Spot Fleet.
describeSpotFleetRequestsResponse_spotFleetRequestConfigs :: Lens.Lens' DescribeSpotFleetRequestsResponse (Prelude.Maybe [SpotFleetRequestConfig])
describeSpotFleetRequestsResponse_spotFleetRequestConfigs = Lens.lens (\DescribeSpotFleetRequestsResponse' {spotFleetRequestConfigs} -> spotFleetRequestConfigs) (\s@DescribeSpotFleetRequestsResponse' {} a -> s {spotFleetRequestConfigs = a} :: DescribeSpotFleetRequestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSpotFleetRequestsResponse_httpStatus :: Lens.Lens' DescribeSpotFleetRequestsResponse Prelude.Int
describeSpotFleetRequestsResponse_httpStatus = Lens.lens (\DescribeSpotFleetRequestsResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotFleetRequestsResponse' {} a -> s {httpStatus = a} :: DescribeSpotFleetRequestsResponse)

instance
  Prelude.NFData
    DescribeSpotFleetRequestsResponse
  where
  rnf DescribeSpotFleetRequestsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spotFleetRequestConfigs
      `Prelude.seq` Prelude.rnf httpStatus
