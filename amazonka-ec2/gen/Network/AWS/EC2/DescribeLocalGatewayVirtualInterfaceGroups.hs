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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interface groups.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
  ( -- * Creating a Request
    DescribeLocalGatewayVirtualInterfaceGroups (..),
    newDescribeLocalGatewayVirtualInterfaceGroups,

    -- * Request Lenses
    describeLocalGatewayVirtualInterfaceGroups_nextToken,
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,

    -- * Destructuring the Response
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (..),
    newDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- * Response Lenses
    describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups,
    describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaceGroups' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroups = DescribeLocalGatewayVirtualInterfaceGroups'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters.
    --
    -- -   @local-gateway-id@ - The ID of a local gateway.
    --
    -- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
    --     interface.
    --
    -- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
    --     interface group.
    filters :: Core.Maybe [Filter],
    -- | The IDs of the virtual interface groups.
    localGatewayVirtualInterfaceGroupIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayVirtualInterfaceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGatewayVirtualInterfaceGroups_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeLocalGatewayVirtualInterfaceGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLocalGatewayVirtualInterfaceGroups_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeLocalGatewayVirtualInterfaceGroups_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
--     interface group.
--
-- 'localGatewayVirtualInterfaceGroupIds', 'describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds' - The IDs of the virtual interface groups.
newDescribeLocalGatewayVirtualInterfaceGroups ::
  DescribeLocalGatewayVirtualInterfaceGroups
newDescribeLocalGatewayVirtualInterfaceGroups =
  DescribeLocalGatewayVirtualInterfaceGroups'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      localGatewayVirtualInterfaceGroupIds =
        Core.Nothing
    }

-- | The token for the next page of results.
describeLocalGatewayVirtualInterfaceGroups_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe Core.Text)
describeLocalGatewayVirtualInterfaceGroups_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayVirtualInterfaceGroups_dryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe Core.Bool)
describeLocalGatewayVirtualInterfaceGroups_dryRun = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {dryRun} -> dryRun) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {dryRun = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayVirtualInterfaceGroups_maxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe Core.Natural)
describeLocalGatewayVirtualInterfaceGroups_maxResults = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {maxResults} -> maxResults) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {maxResults = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
--     interface group.
describeLocalGatewayVirtualInterfaceGroups_filters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe [Filter])
describeLocalGatewayVirtualInterfaceGroups_filters = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {filters} -> filters) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {filters = a} :: DescribeLocalGatewayVirtualInterfaceGroups) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the virtual interface groups.
describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Core.Maybe [Core.Text])
describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {localGatewayVirtualInterfaceGroupIds} -> localGatewayVirtualInterfaceGroupIds) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {localGatewayVirtualInterfaceGroupIds = a} :: DescribeLocalGatewayVirtualInterfaceGroups) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLocalGatewayVirtualInterfaceGroups_nextToken
          Lens..~ rs
            Lens.^? describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  type
    AWSResponse
      DescribeLocalGatewayVirtualInterfaceGroups =
      DescribeLocalGatewayVirtualInterfaceGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfaceGroupsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "localGatewayVirtualInterfaceGroupSet"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeLocalGatewayVirtualInterfaceGroups

instance
  Core.NFData
    DescribeLocalGatewayVirtualInterfaceGroups

instance
  Core.ToHeaders
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toQuery
    DescribeLocalGatewayVirtualInterfaceGroups' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DescribeLocalGatewayVirtualInterfaceGroups" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "NextToken" Core.=: nextToken,
          "DryRun" Core.=: dryRun,
          "MaxResults" Core.=: maxResults,
          Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
          Core.toQuery
            ( Core.toQueryList
                "LocalGatewayVirtualInterfaceGroupId"
                Core.<$> localGatewayVirtualInterfaceGroupIds
            )
        ]

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaceGroupsResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroupsResponse = DescribeLocalGatewayVirtualInterfaceGroupsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The virtual interface groups.
    localGatewayVirtualInterfaceGroups :: Core.Maybe [LocalGatewayVirtualInterfaceGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayVirtualInterfaceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'localGatewayVirtualInterfaceGroups', 'describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups' - The virtual interface groups.
--
-- 'httpStatus', 'describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayVirtualInterfaceGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLocalGatewayVirtualInterfaceGroupsResponse
newDescribeLocalGatewayVirtualInterfaceGroupsResponse
  pHttpStatus_ =
    DescribeLocalGatewayVirtualInterfaceGroupsResponse'
      { nextToken =
          Core.Nothing,
        localGatewayVirtualInterfaceGroups =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Core.Maybe Core.Text)
describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)

-- | The virtual interface groups.
describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Core.Maybe [LocalGatewayVirtualInterfaceGroup])
describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {localGatewayVirtualInterfaceGroups} -> localGatewayVirtualInterfaceGroups) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {localGatewayVirtualInterfaceGroups = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse Core.Int
describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)

instance
  Core.NFData
    DescribeLocalGatewayVirtualInterfaceGroupsResponse
