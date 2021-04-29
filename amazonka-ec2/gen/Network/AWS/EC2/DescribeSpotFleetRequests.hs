{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.DescribeSpotFleetRequests
  ( -- * Creating a Request
    DescribeSpotFleetRequests (..),
    newDescribeSpotFleetRequests,

    -- * Request Lenses
    describeSpotFleetRequests_nextToken,
    describeSpotFleetRequests_dryRun,
    describeSpotFleetRequests_maxResults,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotFleetRequests.
--
-- /See:/ 'newDescribeSpotFleetRequests' smart constructor.
data DescribeSpotFleetRequests = DescribeSpotFleetRequests'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The IDs of the Spot Fleet requests.
    spotFleetRequestIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotFleetRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetRequests_nextToken' - The token for the next set of results.
--
-- 'dryRun', 'describeSpotFleetRequests_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotFleetRequests_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'spotFleetRequestIds', 'describeSpotFleetRequests_spotFleetRequestIds' - The IDs of the Spot Fleet requests.
newDescribeSpotFleetRequests ::
  DescribeSpotFleetRequests
newDescribeSpotFleetRequests =
  DescribeSpotFleetRequests'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      spotFleetRequestIds = Prelude.Nothing
    }

-- | The token for the next set of results.
describeSpotFleetRequests_nextToken :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe Prelude.Text)
describeSpotFleetRequests_nextToken = Lens.lens (\DescribeSpotFleetRequests' {nextToken} -> nextToken) (\s@DescribeSpotFleetRequests' {} a -> s {nextToken = a} :: DescribeSpotFleetRequests)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotFleetRequests_dryRun :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe Prelude.Bool)
describeSpotFleetRequests_dryRun = Lens.lens (\DescribeSpotFleetRequests' {dryRun} -> dryRun) (\s@DescribeSpotFleetRequests' {} a -> s {dryRun = a} :: DescribeSpotFleetRequests)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeSpotFleetRequests_maxResults :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe Prelude.Int)
describeSpotFleetRequests_maxResults = Lens.lens (\DescribeSpotFleetRequests' {maxResults} -> maxResults) (\s@DescribeSpotFleetRequests' {} a -> s {maxResults = a} :: DescribeSpotFleetRequests)

-- | The IDs of the Spot Fleet requests.
describeSpotFleetRequests_spotFleetRequestIds :: Lens.Lens' DescribeSpotFleetRequests (Prelude.Maybe [Prelude.Text])
describeSpotFleetRequests_spotFleetRequestIds = Lens.lens (\DescribeSpotFleetRequests' {spotFleetRequestIds} -> spotFleetRequestIds) (\s@DescribeSpotFleetRequests' {} a -> s {spotFleetRequestIds = a} :: DescribeSpotFleetRequests) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeSpotFleetRequests where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeSpotFleetRequestsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeSpotFleetRequestsResponse_spotFleetRequestConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeSpotFleetRequests_nextToken
          Lens..~ rs
          Lens.^? describeSpotFleetRequestsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeSpotFleetRequests where
  type
    Rs DescribeSpotFleetRequests =
      DescribeSpotFleetRequestsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetRequestsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "spotFleetRequestConfigSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSpotFleetRequests

instance Prelude.NFData DescribeSpotFleetRequests

instance Prelude.ToHeaders DescribeSpotFleetRequests where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeSpotFleetRequests where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSpotFleetRequests where
  toQuery DescribeSpotFleetRequests' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeSpotFleetRequests" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          ( Prelude.toQueryList "SpotFleetRequestId"
              Prelude.<$> spotFleetRequestIds
          )
      ]

-- | Contains the output of DescribeSpotFleetRequests.
--
-- /See:/ 'newDescribeSpotFleetRequestsResponse' smart constructor.
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the configuration of your Spot Fleet.
    spotFleetRequestConfigs :: Prelude.Maybe [SpotFleetRequestConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotFleetRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetRequestsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
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

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
describeSpotFleetRequestsResponse_nextToken :: Lens.Lens' DescribeSpotFleetRequestsResponse (Prelude.Maybe Prelude.Text)
describeSpotFleetRequestsResponse_nextToken = Lens.lens (\DescribeSpotFleetRequestsResponse' {nextToken} -> nextToken) (\s@DescribeSpotFleetRequestsResponse' {} a -> s {nextToken = a} :: DescribeSpotFleetRequestsResponse)

-- | Information about the configuration of your Spot Fleet.
describeSpotFleetRequestsResponse_spotFleetRequestConfigs :: Lens.Lens' DescribeSpotFleetRequestsResponse (Prelude.Maybe [SpotFleetRequestConfig])
describeSpotFleetRequestsResponse_spotFleetRequestConfigs = Lens.lens (\DescribeSpotFleetRequestsResponse' {spotFleetRequestConfigs} -> spotFleetRequestConfigs) (\s@DescribeSpotFleetRequestsResponse' {} a -> s {spotFleetRequestConfigs = a} :: DescribeSpotFleetRequestsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeSpotFleetRequestsResponse_httpStatus :: Lens.Lens' DescribeSpotFleetRequestsResponse Prelude.Int
describeSpotFleetRequestsResponse_httpStatus = Lens.lens (\DescribeSpotFleetRequestsResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotFleetRequestsResponse' {} a -> s {httpStatus = a} :: DescribeSpotFleetRequestsResponse)

instance
  Prelude.NFData
    DescribeSpotFleetRequestsResponse
