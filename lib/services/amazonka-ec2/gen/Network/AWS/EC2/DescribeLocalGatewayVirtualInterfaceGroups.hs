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
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_nextToken,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaceGroups' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroups = DescribeLocalGatewayVirtualInterfaceGroups'
  { -- | One or more filters.
    --
    -- -   @local-gateway-id@ - The ID of a local gateway.
    --
    -- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
    --     interface.
    --
    -- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
    --     interface group.
    filters :: Prelude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the virtual interface groups.
    localGatewayVirtualInterfaceGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayVirtualInterfaceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'describeLocalGatewayVirtualInterfaceGroups_nextToken' - The token for the next page of results.
--
-- 'localGatewayVirtualInterfaceGroupIds', 'describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds' - The IDs of the virtual interface groups.
--
-- 'dryRun', 'describeLocalGatewayVirtualInterfaceGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLocalGatewayVirtualInterfaceGroups_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeLocalGatewayVirtualInterfaceGroups ::
  DescribeLocalGatewayVirtualInterfaceGroups
newDescribeLocalGatewayVirtualInterfaceGroups =
  DescribeLocalGatewayVirtualInterfaceGroups'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      localGatewayVirtualInterfaceGroupIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
--     interface group.
describeLocalGatewayVirtualInterfaceGroups_filters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe [Filter])
describeLocalGatewayVirtualInterfaceGroups_filters = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {filters} -> filters) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {filters = a} :: DescribeLocalGatewayVirtualInterfaceGroups) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeLocalGatewayVirtualInterfaceGroups_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe Prelude.Text)
describeLocalGatewayVirtualInterfaceGroups_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | The IDs of the virtual interface groups.
describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe [Prelude.Text])
describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {localGatewayVirtualInterfaceGroupIds} -> localGatewayVirtualInterfaceGroupIds) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {localGatewayVirtualInterfaceGroupIds = a} :: DescribeLocalGatewayVirtualInterfaceGroups) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayVirtualInterfaceGroups_dryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe Prelude.Bool)
describeLocalGatewayVirtualInterfaceGroups_dryRun = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {dryRun} -> dryRun) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {dryRun = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayVirtualInterfaceGroups_maxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe Prelude.Natural)
describeLocalGatewayVirtualInterfaceGroups_maxResults = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {maxResults} -> maxResults) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {maxResults = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

instance
  Core.AWSPager
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLocalGatewayVirtualInterfaceGroups_nextToken
          Lens..~ rs
            Lens.^? describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "localGatewayVirtualInterfaceGroupSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayVirtualInterfaceGroups

instance
  Prelude.NFData
    DescribeLocalGatewayVirtualInterfaceGroups

instance
  Core.ToHeaders
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toQuery
    DescribeLocalGatewayVirtualInterfaceGroups' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "DescribeLocalGatewayVirtualInterfaceGroups" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
          "NextToken" Core.=: nextToken,
          Core.toQuery
            ( Core.toQueryList
                "LocalGatewayVirtualInterfaceGroupId"
                Prelude.<$> localGatewayVirtualInterfaceGroupIds
            ),
          "DryRun" Core.=: dryRun,
          "MaxResults" Core.=: maxResults
        ]

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaceGroupsResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroupsResponse = DescribeLocalGatewayVirtualInterfaceGroupsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The virtual interface groups.
    localGatewayVirtualInterfaceGroups :: Prelude.Maybe [LocalGatewayVirtualInterfaceGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeLocalGatewayVirtualInterfaceGroupsResponse
newDescribeLocalGatewayVirtualInterfaceGroupsResponse
  pHttpStatus_ =
    DescribeLocalGatewayVirtualInterfaceGroupsResponse'
      { nextToken =
          Prelude.Nothing,
        localGatewayVirtualInterfaceGroups =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)

-- | The virtual interface groups.
describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Prelude.Maybe [LocalGatewayVirtualInterfaceGroup])
describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {localGatewayVirtualInterfaceGroups} -> localGatewayVirtualInterfaceGroups) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {localGatewayVirtualInterfaceGroups = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse Prelude.Int
describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayVirtualInterfaceGroupsResponse
