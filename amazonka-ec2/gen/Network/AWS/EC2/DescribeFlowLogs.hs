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
    describeFlowLogs_maxResults,
    describeFlowLogs_dryRun,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: Prelude.Maybe [Prelude.Text],
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
    filter' :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'describeFlowLogs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'dryRun', 'describeFlowLogs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      flowLogIds = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | The token for the next page of results.
describeFlowLogs_nextToken :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Text)
describeFlowLogs_nextToken = Lens.lens (\DescribeFlowLogs' {nextToken} -> nextToken) (\s@DescribeFlowLogs' {} a -> s {nextToken = a} :: DescribeFlowLogs)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeFlowLogs_maxResults :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Int)
describeFlowLogs_maxResults = Lens.lens (\DescribeFlowLogs' {maxResults} -> maxResults) (\s@DescribeFlowLogs' {} a -> s {maxResults = a} :: DescribeFlowLogs)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFlowLogs_dryRun :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Bool)
describeFlowLogs_dryRun = Lens.lens (\DescribeFlowLogs' {dryRun} -> dryRun) (\s@DescribeFlowLogs' {} a -> s {dryRun = a} :: DescribeFlowLogs)

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
describeFlowLogs_flowLogIds :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe [Prelude.Text])
describeFlowLogs_flowLogIds = Lens.lens (\DescribeFlowLogs' {flowLogIds} -> flowLogIds) (\s@DescribeFlowLogs' {} a -> s {flowLogIds = a} :: DescribeFlowLogs) Prelude.. Lens.mapping Lens._Coerce

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
describeFlowLogs_filter :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe [Filter])
describeFlowLogs_filter = Lens.lens (\DescribeFlowLogs' {filter'} -> filter') (\s@DescribeFlowLogs' {} a -> s {filter' = a} :: DescribeFlowLogs) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeFlowLogs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFlowLogsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFlowLogsResponse_flowLogs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFlowLogs_nextToken
          Lens..~ rs
          Lens.^? describeFlowLogsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFlowLogs where
  type
    AWSResponse DescribeFlowLogs =
      DescribeFlowLogsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFlowLogsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "flowLogSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFlowLogs

instance Prelude.NFData DescribeFlowLogs

instance Core.ToHeaders DescribeFlowLogs where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeFlowLogs where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFlowLogs where
  toQuery DescribeFlowLogs' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeFlowLogs" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "FlowLogId"
              Prelude.<$> flowLogIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filter')
      ]

-- | /See:/ 'newDescribeFlowLogsResponse' smart constructor.
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the flow logs.
    flowLogs :: Prelude.Maybe [FlowLog],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeFlowLogsResponse
newDescribeFlowLogsResponse pHttpStatus_ =
  DescribeFlowLogsResponse'
    { nextToken =
        Prelude.Nothing,
      flowLogs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeFlowLogsResponse_nextToken :: Lens.Lens' DescribeFlowLogsResponse (Prelude.Maybe Prelude.Text)
describeFlowLogsResponse_nextToken = Lens.lens (\DescribeFlowLogsResponse' {nextToken} -> nextToken) (\s@DescribeFlowLogsResponse' {} a -> s {nextToken = a} :: DescribeFlowLogsResponse)

-- | Information about the flow logs.
describeFlowLogsResponse_flowLogs :: Lens.Lens' DescribeFlowLogsResponse (Prelude.Maybe [FlowLog])
describeFlowLogsResponse_flowLogs = Lens.lens (\DescribeFlowLogsResponse' {flowLogs} -> flowLogs) (\s@DescribeFlowLogsResponse' {} a -> s {flowLogs = a} :: DescribeFlowLogsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeFlowLogsResponse_httpStatus :: Lens.Lens' DescribeFlowLogsResponse Prelude.Int
describeFlowLogsResponse_httpStatus = Lens.lens (\DescribeFlowLogsResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowLogsResponse' {} a -> s {httpStatus = a} :: DescribeFlowLogsResponse)

instance Prelude.NFData DescribeFlowLogsResponse
