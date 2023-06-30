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
-- Module      : Amazonka.EC2.DescribeLocalGatewayVirtualInterfaceGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interface groups.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeLocalGatewayVirtualInterfaceGroups
  ( -- * Creating a Request
    DescribeLocalGatewayVirtualInterfaceGroups (..),
    newDescribeLocalGatewayVirtualInterfaceGroups,

    -- * Request Lenses
    describeLocalGatewayVirtualInterfaceGroups_dryRun,
    describeLocalGatewayVirtualInterfaceGroups_filters,
    describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds,
    describeLocalGatewayVirtualInterfaceGroups_maxResults,
    describeLocalGatewayVirtualInterfaceGroups_nextToken,

    -- * Destructuring the Response
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (..),
    newDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- * Response Lenses
    describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups,
    describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken,
    describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaceGroups' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroups = DescribeLocalGatewayVirtualInterfaceGroups'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @local-gateway-id@ - The ID of a local gateway.
    --
    -- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
    --     interface group.
    --
    -- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
    --     interface.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     local gateway virtual interface group.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the virtual interface groups.
    localGatewayVirtualInterfaceGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'dryRun', 'describeLocalGatewayVirtualInterfaceGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeLocalGatewayVirtualInterfaceGroups_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
--     interface group.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway virtual interface group.
--
-- 'localGatewayVirtualInterfaceGroupIds', 'describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds' - The IDs of the virtual interface groups.
--
-- 'maxResults', 'describeLocalGatewayVirtualInterfaceGroups_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeLocalGatewayVirtualInterfaceGroups_nextToken' - The token for the next page of results.
newDescribeLocalGatewayVirtualInterfaceGroups ::
  DescribeLocalGatewayVirtualInterfaceGroups
newDescribeLocalGatewayVirtualInterfaceGroups =
  DescribeLocalGatewayVirtualInterfaceGroups'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      localGatewayVirtualInterfaceGroupIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayVirtualInterfaceGroups_dryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe Prelude.Bool)
describeLocalGatewayVirtualInterfaceGroups_dryRun = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {dryRun} -> dryRun) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {dryRun = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-virtual-interface-group-id@ - The ID of the virtual
--     interface group.
--
-- -   @local-gateway-virtual-interface-id@ - The ID of the virtual
--     interface.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway virtual interface group.
describeLocalGatewayVirtualInterfaceGroups_filters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe [Filter])
describeLocalGatewayVirtualInterfaceGroups_filters = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {filters} -> filters) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {filters = a} :: DescribeLocalGatewayVirtualInterfaceGroups) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the virtual interface groups.
describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe [Prelude.Text])
describeLocalGatewayVirtualInterfaceGroups_localGatewayVirtualInterfaceGroupIds = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {localGatewayVirtualInterfaceGroupIds} -> localGatewayVirtualInterfaceGroupIds) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {localGatewayVirtualInterfaceGroupIds = a} :: DescribeLocalGatewayVirtualInterfaceGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayVirtualInterfaceGroups_maxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe Prelude.Natural)
describeLocalGatewayVirtualInterfaceGroups_maxResults = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {maxResults} -> maxResults) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {maxResults = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

-- | The token for the next page of results.
describeLocalGatewayVirtualInterfaceGroups_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Prelude.Maybe Prelude.Text)
describeLocalGatewayVirtualInterfaceGroups_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroups' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaceGroups' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroups)

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
        Prelude.Just
          Prelude.$ rq
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfaceGroupsResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayVirtualInterfaceGroupSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  hashWithSalt
    _salt
    DescribeLocalGatewayVirtualInterfaceGroups' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  rnf DescribeLocalGatewayVirtualInterfaceGroups' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeLocalGatewayVirtualInterfaceGroups
  where
  toQuery
    DescribeLocalGatewayVirtualInterfaceGroups' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeLocalGatewayVirtualInterfaceGroups" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          Data.toQuery
            ( Data.toQueryList
                "LocalGatewayVirtualInterfaceGroupId"
                Prelude.<$> localGatewayVirtualInterfaceGroupIds
            ),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeLocalGatewayVirtualInterfaceGroupsResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroupsResponse = DescribeLocalGatewayVirtualInterfaceGroupsResponse'
  { -- | The virtual interface groups.
    localGatewayVirtualInterfaceGroups :: Prelude.Maybe [LocalGatewayVirtualInterfaceGroup],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'localGatewayVirtualInterfaceGroups', 'describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups' - The virtual interface groups.
--
-- 'nextToken', 'describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayVirtualInterfaceGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayVirtualInterfaceGroupsResponse
newDescribeLocalGatewayVirtualInterfaceGroupsResponse
  pHttpStatus_ =
    DescribeLocalGatewayVirtualInterfaceGroupsResponse'
      { localGatewayVirtualInterfaceGroups =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The virtual interface groups.
describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Prelude.Maybe [LocalGatewayVirtualInterfaceGroup])
describeLocalGatewayVirtualInterfaceGroupsResponse_localGatewayVirtualInterfaceGroups = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {localGatewayVirtualInterfaceGroups} -> localGatewayVirtualInterfaceGroups) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {localGatewayVirtualInterfaceGroups = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayVirtualInterfaceGroupsResponse_nextToken = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)

-- | The response's http status code.
describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse Prelude.Int
describeLocalGatewayVirtualInterfaceGroupsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayVirtualInterfaceGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayVirtualInterfaceGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayVirtualInterfaceGroupsResponse
  where
  rnf
    DescribeLocalGatewayVirtualInterfaceGroupsResponse' {..} =
      Prelude.rnf localGatewayVirtualInterfaceGroups
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
