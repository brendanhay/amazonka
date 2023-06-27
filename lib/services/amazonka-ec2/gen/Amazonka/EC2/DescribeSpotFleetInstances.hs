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
-- Module      : Amazonka.EC2.DescribeSpotFleetInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified Spot Fleet.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeSpotFleetInstances
  ( -- * Creating a Request
    DescribeSpotFleetInstances (..),
    newDescribeSpotFleetInstances,

    -- * Request Lenses
    describeSpotFleetInstances_dryRun,
    describeSpotFleetInstances_maxResults,
    describeSpotFleetInstances_nextToken,
    describeSpotFleetInstances_spotFleetRequestId,

    -- * Destructuring the Response
    DescribeSpotFleetInstancesResponse (..),
    newDescribeSpotFleetInstancesResponse,

    -- * Response Lenses
    describeSpotFleetInstancesResponse_activeInstances,
    describeSpotFleetInstancesResponse_nextToken,
    describeSpotFleetInstancesResponse_spotFleetRequestId,
    describeSpotFleetInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeSpotFleetInstances.
--
-- /See:/ 'newDescribeSpotFleetInstances' smart constructor.
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotFleetInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSpotFleetInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotFleetInstances_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeSpotFleetInstances_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'spotFleetRequestId', 'describeSpotFleetInstances_spotFleetRequestId' - The ID of the Spot Fleet request.
newDescribeSpotFleetInstances ::
  -- | 'spotFleetRequestId'
  Prelude.Text ->
  DescribeSpotFleetInstances
newDescribeSpotFleetInstances pSpotFleetRequestId_ =
  DescribeSpotFleetInstances'
    { dryRun =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      spotFleetRequestId = pSpotFleetRequestId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotFleetInstances_dryRun :: Lens.Lens' DescribeSpotFleetInstances (Prelude.Maybe Prelude.Bool)
describeSpotFleetInstances_dryRun = Lens.lens (\DescribeSpotFleetInstances' {dryRun} -> dryRun) (\s@DescribeSpotFleetInstances' {} a -> s {dryRun = a} :: DescribeSpotFleetInstances)

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeSpotFleetInstances_maxResults :: Lens.Lens' DescribeSpotFleetInstances (Prelude.Maybe Prelude.Natural)
describeSpotFleetInstances_maxResults = Lens.lens (\DescribeSpotFleetInstances' {maxResults} -> maxResults) (\s@DescribeSpotFleetInstances' {} a -> s {maxResults = a} :: DescribeSpotFleetInstances)

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeSpotFleetInstances_nextToken :: Lens.Lens' DescribeSpotFleetInstances (Prelude.Maybe Prelude.Text)
describeSpotFleetInstances_nextToken = Lens.lens (\DescribeSpotFleetInstances' {nextToken} -> nextToken) (\s@DescribeSpotFleetInstances' {} a -> s {nextToken = a} :: DescribeSpotFleetInstances)

-- | The ID of the Spot Fleet request.
describeSpotFleetInstances_spotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstances Prelude.Text
describeSpotFleetInstances_spotFleetRequestId = Lens.lens (\DescribeSpotFleetInstances' {spotFleetRequestId} -> spotFleetRequestId) (\s@DescribeSpotFleetInstances' {} a -> s {spotFleetRequestId = a} :: DescribeSpotFleetInstances)

instance Core.AWSPager DescribeSpotFleetInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSpotFleetInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSpotFleetInstancesResponse_activeInstances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSpotFleetInstances_nextToken
          Lens..~ rs
          Lens.^? describeSpotFleetInstancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSpotFleetInstances where
  type
    AWSResponse DescribeSpotFleetInstances =
      DescribeSpotFleetInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetInstancesResponse'
            Prelude.<$> ( x
                            Data..@? "activeInstanceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (x Data..@? "spotFleetRequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSpotFleetInstances where
  hashWithSalt _salt DescribeSpotFleetInstances' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` spotFleetRequestId

instance Prelude.NFData DescribeSpotFleetInstances where
  rnf DescribeSpotFleetInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spotFleetRequestId

instance Data.ToHeaders DescribeSpotFleetInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSpotFleetInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSpotFleetInstances where
  toQuery DescribeSpotFleetInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSpotFleetInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SpotFleetRequestId" Data.=: spotFleetRequestId
      ]

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'newDescribeSpotFleetInstancesResponse' smart constructor.
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
  { -- | The running instances. This list is refreshed periodically and might be
    -- out of date.
    activeInstances :: Prelude.Maybe [ActiveInstance],
    -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotFleetInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeInstances', 'describeSpotFleetInstancesResponse_activeInstances' - The running instances. This list is refreshed periodically and might be
-- out of date.
--
-- 'nextToken', 'describeSpotFleetInstancesResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'spotFleetRequestId', 'describeSpotFleetInstancesResponse_spotFleetRequestId' - The ID of the Spot Fleet request.
--
-- 'httpStatus', 'describeSpotFleetInstancesResponse_httpStatus' - The response's http status code.
newDescribeSpotFleetInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSpotFleetInstancesResponse
newDescribeSpotFleetInstancesResponse pHttpStatus_ =
  DescribeSpotFleetInstancesResponse'
    { activeInstances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      spotFleetRequestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The running instances. This list is refreshed periodically and might be
-- out of date.
describeSpotFleetInstancesResponse_activeInstances :: Lens.Lens' DescribeSpotFleetInstancesResponse (Prelude.Maybe [ActiveInstance])
describeSpotFleetInstancesResponse_activeInstances = Lens.lens (\DescribeSpotFleetInstancesResponse' {activeInstances} -> activeInstances) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {activeInstances = a} :: DescribeSpotFleetInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeSpotFleetInstancesResponse_nextToken :: Lens.Lens' DescribeSpotFleetInstancesResponse (Prelude.Maybe Prelude.Text)
describeSpotFleetInstancesResponse_nextToken = Lens.lens (\DescribeSpotFleetInstancesResponse' {nextToken} -> nextToken) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {nextToken = a} :: DescribeSpotFleetInstancesResponse)

-- | The ID of the Spot Fleet request.
describeSpotFleetInstancesResponse_spotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstancesResponse (Prelude.Maybe Prelude.Text)
describeSpotFleetInstancesResponse_spotFleetRequestId = Lens.lens (\DescribeSpotFleetInstancesResponse' {spotFleetRequestId} -> spotFleetRequestId) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {spotFleetRequestId = a} :: DescribeSpotFleetInstancesResponse)

-- | The response's http status code.
describeSpotFleetInstancesResponse_httpStatus :: Lens.Lens' DescribeSpotFleetInstancesResponse Prelude.Int
describeSpotFleetInstancesResponse_httpStatus = Lens.lens (\DescribeSpotFleetInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {httpStatus = a} :: DescribeSpotFleetInstancesResponse)

instance
  Prelude.NFData
    DescribeSpotFleetInstancesResponse
  where
  rnf DescribeSpotFleetInstancesResponse' {..} =
    Prelude.rnf activeInstances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spotFleetRequestId
      `Prelude.seq` Prelude.rnf httpStatus
