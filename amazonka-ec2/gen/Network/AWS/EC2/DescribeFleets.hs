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
-- Module      : Network.AWS.EC2.DescribeFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EC2 Fleets or all of your EC2 Fleets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet.html#monitor-ec2-fleet Monitoring your EC2 Fleet>
-- in the /Amazon EC2 User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFleets
  ( -- * Creating a Request
    DescribeFleets (..),
    newDescribeFleets,

    -- * Request Lenses
    describeFleets_nextToken,
    describeFleets_dryRun,
    describeFleets_maxResults,
    describeFleets_filters,
    describeFleets_fleetIds,

    -- * Destructuring the Response
    DescribeFleetsResponse (..),
    newDescribeFleetsResponse,

    -- * Response Lenses
    describeFleetsResponse_nextToken,
    describeFleetsResponse_fleets,
    describeFleetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
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
    maxResults :: Core.Maybe Core.Int,
    -- | The filters.
    --
    -- -   @activity-status@ - The progress of the EC2 Fleet ( @error@ |
    --     @pending-fulfillment@ | @pending-termination@ | @fulfilled@).
    --
    -- -   @excess-capacity-termination-policy@ - Indicates whether to
    --     terminate running instances if the target capacity is decreased
    --     below the current EC2 Fleet size (@true@ | @false@).
    --
    -- -   @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ |
    --     @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ |
    --     @modifying@).
    --
    -- -   @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should
    --     replace unhealthy instances (@true@ | @false@).
    --
    -- -   @type@ - The type of request (@instant@ | @request@ | @maintain@).
    filters :: Core.Maybe [Filter],
    -- | The ID of the EC2 Fleets.
    fleetIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleets_nextToken' - The token for the next set of results.
--
-- 'dryRun', 'describeFleets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeFleets_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'filters', 'describeFleets_filters' - The filters.
--
-- -   @activity-status@ - The progress of the EC2 Fleet ( @error@ |
--     @pending-fulfillment@ | @pending-termination@ | @fulfilled@).
--
-- -   @excess-capacity-termination-policy@ - Indicates whether to
--     terminate running instances if the target capacity is decreased
--     below the current EC2 Fleet size (@true@ | @false@).
--
-- -   @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ |
--     @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ |
--     @modifying@).
--
-- -   @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should
--     replace unhealthy instances (@true@ | @false@).
--
-- -   @type@ - The type of request (@instant@ | @request@ | @maintain@).
--
-- 'fleetIds', 'describeFleets_fleetIds' - The ID of the EC2 Fleets.
newDescribeFleets ::
  DescribeFleets
newDescribeFleets =
  DescribeFleets'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      fleetIds = Core.Nothing
    }

-- | The token for the next set of results.
describeFleets_nextToken :: Lens.Lens' DescribeFleets (Core.Maybe Core.Text)
describeFleets_nextToken = Lens.lens (\DescribeFleets' {nextToken} -> nextToken) (\s@DescribeFleets' {} a -> s {nextToken = a} :: DescribeFleets)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFleets_dryRun :: Lens.Lens' DescribeFleets (Core.Maybe Core.Bool)
describeFleets_dryRun = Lens.lens (\DescribeFleets' {dryRun} -> dryRun) (\s@DescribeFleets' {} a -> s {dryRun = a} :: DescribeFleets)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeFleets_maxResults :: Lens.Lens' DescribeFleets (Core.Maybe Core.Int)
describeFleets_maxResults = Lens.lens (\DescribeFleets' {maxResults} -> maxResults) (\s@DescribeFleets' {} a -> s {maxResults = a} :: DescribeFleets)

-- | The filters.
--
-- -   @activity-status@ - The progress of the EC2 Fleet ( @error@ |
--     @pending-fulfillment@ | @pending-termination@ | @fulfilled@).
--
-- -   @excess-capacity-termination-policy@ - Indicates whether to
--     terminate running instances if the target capacity is decreased
--     below the current EC2 Fleet size (@true@ | @false@).
--
-- -   @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ |
--     @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ |
--     @modifying@).
--
-- -   @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should
--     replace unhealthy instances (@true@ | @false@).
--
-- -   @type@ - The type of request (@instant@ | @request@ | @maintain@).
describeFleets_filters :: Lens.Lens' DescribeFleets (Core.Maybe [Filter])
describeFleets_filters = Lens.lens (\DescribeFleets' {filters} -> filters) (\s@DescribeFleets' {} a -> s {filters = a} :: DescribeFleets) Core.. Lens.mapping Lens._Coerce

-- | The ID of the EC2 Fleets.
describeFleets_fleetIds :: Lens.Lens' DescribeFleets (Core.Maybe [Core.Text])
describeFleets_fleetIds = Lens.lens (\DescribeFleets' {fleetIds} -> fleetIds) (\s@DescribeFleets' {} a -> s {fleetIds = a} :: DescribeFleets) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFleetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFleetsResponse_fleets Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeFleets_nextToken
          Lens..~ rs
          Lens.^? describeFleetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeFleets where
  type
    AWSResponse DescribeFleets =
      DescribeFleetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFleetsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "fleetSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFleets

instance Core.NFData DescribeFleets

instance Core.ToHeaders DescribeFleets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeFleets where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFleets where
  toQuery DescribeFleets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeFleets" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          (Core.toQueryList "FleetId" Core.<$> fleetIds)
      ]

-- | /See:/ 'newDescribeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the EC2 Fleets.
    fleets :: Core.Maybe [FleetData],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetsResponse_nextToken' - The token for the next set of results.
--
-- 'fleets', 'describeFleetsResponse_fleets' - Information about the EC2 Fleets.
--
-- 'httpStatus', 'describeFleetsResponse_httpStatus' - The response's http status code.
newDescribeFleetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeFleetsResponse
newDescribeFleetsResponse pHttpStatus_ =
  DescribeFleetsResponse'
    { nextToken = Core.Nothing,
      fleets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results.
describeFleetsResponse_nextToken :: Lens.Lens' DescribeFleetsResponse (Core.Maybe Core.Text)
describeFleetsResponse_nextToken = Lens.lens (\DescribeFleetsResponse' {nextToken} -> nextToken) (\s@DescribeFleetsResponse' {} a -> s {nextToken = a} :: DescribeFleetsResponse)

-- | Information about the EC2 Fleets.
describeFleetsResponse_fleets :: Lens.Lens' DescribeFleetsResponse (Core.Maybe [FleetData])
describeFleetsResponse_fleets = Lens.lens (\DescribeFleetsResponse' {fleets} -> fleets) (\s@DescribeFleetsResponse' {} a -> s {fleets = a} :: DescribeFleetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFleetsResponse_httpStatus :: Lens.Lens' DescribeFleetsResponse Core.Int
describeFleetsResponse_httpStatus = Lens.lens (\DescribeFleetsResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetsResponse' {} a -> s {httpStatus = a} :: DescribeFleetsResponse)

instance Core.NFData DescribeFleetsResponse
