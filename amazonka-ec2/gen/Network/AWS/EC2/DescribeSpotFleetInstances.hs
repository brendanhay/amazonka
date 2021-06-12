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
-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified Spot Fleet.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetInstances
  ( -- * Creating a Request
    DescribeSpotFleetInstances (..),
    newDescribeSpotFleetInstances,

    -- * Request Lenses
    describeSpotFleetInstances_nextToken,
    describeSpotFleetInstances_dryRun,
    describeSpotFleetInstances_maxResults,
    describeSpotFleetInstances_spotFleetRequestId,

    -- * Destructuring the Response
    DescribeSpotFleetInstancesResponse (..),
    newDescribeSpotFleetInstancesResponse,

    -- * Response Lenses
    describeSpotFleetInstancesResponse_nextToken,
    describeSpotFleetInstancesResponse_activeInstances,
    describeSpotFleetInstancesResponse_spotFleetRequestId,
    describeSpotFleetInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotFleetInstances.
--
-- /See:/ 'newDescribeSpotFleetInstances' smart constructor.
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSpotFleetInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetInstances_nextToken' - The token for the next set of results.
--
-- 'dryRun', 'describeSpotFleetInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotFleetInstances_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'spotFleetRequestId', 'describeSpotFleetInstances_spotFleetRequestId' - The ID of the Spot Fleet request.
newDescribeSpotFleetInstances ::
  -- | 'spotFleetRequestId'
  Core.Text ->
  DescribeSpotFleetInstances
newDescribeSpotFleetInstances pSpotFleetRequestId_ =
  DescribeSpotFleetInstances'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      spotFleetRequestId = pSpotFleetRequestId_
    }

-- | The token for the next set of results.
describeSpotFleetInstances_nextToken :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Text)
describeSpotFleetInstances_nextToken = Lens.lens (\DescribeSpotFleetInstances' {nextToken} -> nextToken) (\s@DescribeSpotFleetInstances' {} a -> s {nextToken = a} :: DescribeSpotFleetInstances)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotFleetInstances_dryRun :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Bool)
describeSpotFleetInstances_dryRun = Lens.lens (\DescribeSpotFleetInstances' {dryRun} -> dryRun) (\s@DescribeSpotFleetInstances' {} a -> s {dryRun = a} :: DescribeSpotFleetInstances)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeSpotFleetInstances_maxResults :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Natural)
describeSpotFleetInstances_maxResults = Lens.lens (\DescribeSpotFleetInstances' {maxResults} -> maxResults) (\s@DescribeSpotFleetInstances' {} a -> s {maxResults = a} :: DescribeSpotFleetInstances)

-- | The ID of the Spot Fleet request.
describeSpotFleetInstances_spotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstances Core.Text
describeSpotFleetInstances_spotFleetRequestId = Lens.lens (\DescribeSpotFleetInstances' {spotFleetRequestId} -> spotFleetRequestId) (\s@DescribeSpotFleetInstances' {} a -> s {spotFleetRequestId = a} :: DescribeSpotFleetInstances)

instance Core.AWSPager DescribeSpotFleetInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSpotFleetInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSpotFleetInstancesResponse_activeInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSpotFleetInstances_nextToken
          Lens..~ rs
          Lens.^? describeSpotFleetInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeSpotFleetInstances where
  type
    AWSResponse DescribeSpotFleetInstances =
      DescribeSpotFleetInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetInstancesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "activeInstanceSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "spotFleetRequestId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSpotFleetInstances

instance Core.NFData DescribeSpotFleetInstances

instance Core.ToHeaders DescribeSpotFleetInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSpotFleetInstances where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSpotFleetInstances where
  toQuery DescribeSpotFleetInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSpotFleetInstances" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "SpotFleetRequestId" Core.=: spotFleetRequestId
      ]

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'newDescribeSpotFleetInstancesResponse' smart constructor.
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The running instances. This list is refreshed periodically and might be
    -- out of date.
    activeInstances :: Core.Maybe [ActiveInstance],
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSpotFleetInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetInstancesResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
--
-- 'activeInstances', 'describeSpotFleetInstancesResponse_activeInstances' - The running instances. This list is refreshed periodically and might be
-- out of date.
--
-- 'spotFleetRequestId', 'describeSpotFleetInstancesResponse_spotFleetRequestId' - The ID of the Spot Fleet request.
--
-- 'httpStatus', 'describeSpotFleetInstancesResponse_httpStatus' - The response's http status code.
newDescribeSpotFleetInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSpotFleetInstancesResponse
newDescribeSpotFleetInstancesResponse pHttpStatus_ =
  DescribeSpotFleetInstancesResponse'
    { nextToken =
        Core.Nothing,
      activeInstances = Core.Nothing,
      spotFleetRequestId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
describeSpotFleetInstancesResponse_nextToken :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe Core.Text)
describeSpotFleetInstancesResponse_nextToken = Lens.lens (\DescribeSpotFleetInstancesResponse' {nextToken} -> nextToken) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {nextToken = a} :: DescribeSpotFleetInstancesResponse)

-- | The running instances. This list is refreshed periodically and might be
-- out of date.
describeSpotFleetInstancesResponse_activeInstances :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe [ActiveInstance])
describeSpotFleetInstancesResponse_activeInstances = Lens.lens (\DescribeSpotFleetInstancesResponse' {activeInstances} -> activeInstances) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {activeInstances = a} :: DescribeSpotFleetInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Spot Fleet request.
describeSpotFleetInstancesResponse_spotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe Core.Text)
describeSpotFleetInstancesResponse_spotFleetRequestId = Lens.lens (\DescribeSpotFleetInstancesResponse' {spotFleetRequestId} -> spotFleetRequestId) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {spotFleetRequestId = a} :: DescribeSpotFleetInstancesResponse)

-- | The response's http status code.
describeSpotFleetInstancesResponse_httpStatus :: Lens.Lens' DescribeSpotFleetInstancesResponse Core.Int
describeSpotFleetInstancesResponse_httpStatus = Lens.lens (\DescribeSpotFleetInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotFleetInstancesResponse' {} a -> s {httpStatus = a} :: DescribeSpotFleetInstancesResponse)

instance
  Core.NFData
    DescribeSpotFleetInstancesResponse
