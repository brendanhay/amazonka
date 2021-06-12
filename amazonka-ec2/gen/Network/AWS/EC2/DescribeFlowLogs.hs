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
-- Module      : Network.AWS.EC2.DescribeFlowLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more flow logs. To view the information in your flow
-- logs (the log streams for the network interfaces), you must use the
-- CloudWatch Logs console or the CloudWatch Logs API.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFlowLogs
  ( -- * Creating a Request
    DescribeFlowLogs (..),
    newDescribeFlowLogs,

    -- * Request Lenses
    describeFlowLogs_nextToken,
    describeFlowLogs_dryRun,
    describeFlowLogs_maxResults,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_filter,

    -- * Destructuring the Response
    DescribeFlowLogsResponse (..),
    newDescribeFlowLogsResponse,

    -- * Response Lenses
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
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
    maxResults :: Core.Maybe Core.Int,
    -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
    --     @FAILED@).
    --
    -- -   @log-destination-type@ - The type of destination to which the flow
    --     log publishes data. Possible destination types include
    --     @cloud-watch-logs@ and @s3@.
    --
    -- -   @flow-log-id@ - The ID of the flow log.
    --
    -- -   @log-group-name@ - The name of the log group.
    --
    -- -   @resource-id@ - The ID of the VPC, subnet, or network interface.
    --
    -- -   @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filter' :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFlowLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFlowLogs_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeFlowLogs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeFlowLogs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'flowLogIds', 'describeFlowLogs_flowLogIds' - One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- 'filter'', 'describeFlowLogs_filter' - One or more filters.
--
-- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
--     @FAILED@).
--
-- -   @log-destination-type@ - The type of destination to which the flow
--     log publishes data. Possible destination types include
--     @cloud-watch-logs@ and @s3@.
--
-- -   @flow-log-id@ - The ID of the flow log.
--
-- -   @log-group-name@ - The name of the log group.
--
-- -   @resource-id@ - The ID of the VPC, subnet, or network interface.
--
-- -   @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
newDescribeFlowLogs ::
  DescribeFlowLogs
newDescribeFlowLogs =
  DescribeFlowLogs'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      flowLogIds = Core.Nothing,
      filter' = Core.Nothing
    }

-- | The token for the next page of results.
describeFlowLogs_nextToken :: Lens.Lens' DescribeFlowLogs (Core.Maybe Core.Text)
describeFlowLogs_nextToken = Lens.lens (\DescribeFlowLogs' {nextToken} -> nextToken) (\s@DescribeFlowLogs' {} a -> s {nextToken = a} :: DescribeFlowLogs)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFlowLogs_dryRun :: Lens.Lens' DescribeFlowLogs (Core.Maybe Core.Bool)
describeFlowLogs_dryRun = Lens.lens (\DescribeFlowLogs' {dryRun} -> dryRun) (\s@DescribeFlowLogs' {} a -> s {dryRun = a} :: DescribeFlowLogs)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeFlowLogs_maxResults :: Lens.Lens' DescribeFlowLogs (Core.Maybe Core.Int)
describeFlowLogs_maxResults = Lens.lens (\DescribeFlowLogs' {maxResults} -> maxResults) (\s@DescribeFlowLogs' {} a -> s {maxResults = a} :: DescribeFlowLogs)

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
describeFlowLogs_flowLogIds :: Lens.Lens' DescribeFlowLogs (Core.Maybe [Core.Text])
describeFlowLogs_flowLogIds = Lens.lens (\DescribeFlowLogs' {flowLogIds} -> flowLogIds) (\s@DescribeFlowLogs' {} a -> s {flowLogIds = a} :: DescribeFlowLogs) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
--     @FAILED@).
--
-- -   @log-destination-type@ - The type of destination to which the flow
--     log publishes data. Possible destination types include
--     @cloud-watch-logs@ and @s3@.
--
-- -   @flow-log-id@ - The ID of the flow log.
--
-- -   @log-group-name@ - The name of the log group.
--
-- -   @resource-id@ - The ID of the VPC, subnet, or network interface.
--
-- -   @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
describeFlowLogs_filter :: Lens.Lens' DescribeFlowLogs (Core.Maybe [Filter])
describeFlowLogs_filter = Lens.lens (\DescribeFlowLogs' {filter'} -> filter') (\s@DescribeFlowLogs' {} a -> s {filter' = a} :: DescribeFlowLogs) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeFlowLogs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFlowLogsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFlowLogsResponse_flowLogs Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeFlowLogs_nextToken
          Lens..~ rs
          Lens.^? describeFlowLogsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeFlowLogs where
  type
    AWSResponse DescribeFlowLogs =
      DescribeFlowLogsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFlowLogsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "flowLogSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFlowLogs

instance Core.NFData DescribeFlowLogs

instance Core.ToHeaders DescribeFlowLogs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeFlowLogs where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFlowLogs where
  toQuery DescribeFlowLogs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeFlowLogs" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "FlowLogId" Core.<$> flowLogIds),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filter')
      ]

-- | /See:/ 'newDescribeFlowLogsResponse' smart constructor.
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the flow logs.
    flowLogs :: Core.Maybe [FlowLog],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFlowLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFlowLogsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'flowLogs', 'describeFlowLogsResponse_flowLogs' - Information about the flow logs.
--
-- 'httpStatus', 'describeFlowLogsResponse_httpStatus' - The response's http status code.
newDescribeFlowLogsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeFlowLogsResponse
newDescribeFlowLogsResponse pHttpStatus_ =
  DescribeFlowLogsResponse'
    { nextToken = Core.Nothing,
      flowLogs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeFlowLogsResponse_nextToken :: Lens.Lens' DescribeFlowLogsResponse (Core.Maybe Core.Text)
describeFlowLogsResponse_nextToken = Lens.lens (\DescribeFlowLogsResponse' {nextToken} -> nextToken) (\s@DescribeFlowLogsResponse' {} a -> s {nextToken = a} :: DescribeFlowLogsResponse)

-- | Information about the flow logs.
describeFlowLogsResponse_flowLogs :: Lens.Lens' DescribeFlowLogsResponse (Core.Maybe [FlowLog])
describeFlowLogsResponse_flowLogs = Lens.lens (\DescribeFlowLogsResponse' {flowLogs} -> flowLogs) (\s@DescribeFlowLogsResponse' {} a -> s {flowLogs = a} :: DescribeFlowLogsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFlowLogsResponse_httpStatus :: Lens.Lens' DescribeFlowLogsResponse Core.Int
describeFlowLogsResponse_httpStatus = Lens.lens (\DescribeFlowLogsResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowLogsResponse' {} a -> s {httpStatus = a} :: DescribeFlowLogsResponse)

instance Core.NFData DescribeFlowLogsResponse
