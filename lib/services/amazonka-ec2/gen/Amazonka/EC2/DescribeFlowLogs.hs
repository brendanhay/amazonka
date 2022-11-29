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
-- Module      : Amazonka.EC2.DescribeFlowLogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more flow logs.
--
-- To view the published flow log records, you must view the log
-- destination. For example, the CloudWatch Logs log group, the Amazon S3
-- bucket, or the Kinesis Data Firehose delivery stream.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeFlowLogs
  ( -- * Creating a Request
    DescribeFlowLogs (..),
    newDescribeFlowLogs,

    -- * Request Lenses
    describeFlowLogs_nextToken,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_dryRun,
    describeFlowLogs_filter,
    describeFlowLogs_maxResults,

    -- * Destructuring the Response
    DescribeFlowLogsResponse (..),
    newDescribeFlowLogsResponse,

    -- * Response Lenses
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
    --     @FAILED@).
    --
    -- -   @log-destination-type@ - The type of destination for the flow log
    --     data (@cloud-watch-logs@ | @s3@ | @kinesis-data-firehose@).
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
    filter' :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'flowLogIds', 'describeFlowLogs_flowLogIds' - One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- 'dryRun', 'describeFlowLogs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filter'', 'describeFlowLogs_filter' - One or more filters.
--
-- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
--     @FAILED@).
--
-- -   @log-destination-type@ - The type of destination for the flow log
--     data (@cloud-watch-logs@ | @s3@ | @kinesis-data-firehose@).
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
--
-- 'maxResults', 'describeFlowLogs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeFlowLogs ::
  DescribeFlowLogs
newDescribeFlowLogs =
  DescribeFlowLogs'
    { nextToken = Prelude.Nothing,
      flowLogIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeFlowLogs_nextToken :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Text)
describeFlowLogs_nextToken = Lens.lens (\DescribeFlowLogs' {nextToken} -> nextToken) (\s@DescribeFlowLogs' {} a -> s {nextToken = a} :: DescribeFlowLogs)

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
describeFlowLogs_flowLogIds :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe [Prelude.Text])
describeFlowLogs_flowLogIds = Lens.lens (\DescribeFlowLogs' {flowLogIds} -> flowLogIds) (\s@DescribeFlowLogs' {} a -> s {flowLogIds = a} :: DescribeFlowLogs) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFlowLogs_dryRun :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Bool)
describeFlowLogs_dryRun = Lens.lens (\DescribeFlowLogs' {dryRun} -> dryRun) (\s@DescribeFlowLogs' {} a -> s {dryRun = a} :: DescribeFlowLogs)

-- | One or more filters.
--
-- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
--     @FAILED@).
--
-- -   @log-destination-type@ - The type of destination for the flow log
--     data (@cloud-watch-logs@ | @s3@ | @kinesis-data-firehose@).
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
describeFlowLogs_filter = Lens.lens (\DescribeFlowLogs' {filter'} -> filter') (\s@DescribeFlowLogs' {} a -> s {filter' = a} :: DescribeFlowLogs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeFlowLogs_maxResults :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Int)
describeFlowLogs_maxResults = Lens.lens (\DescribeFlowLogs' {maxResults} -> maxResults) (\s@DescribeFlowLogs' {} a -> s {maxResults = a} :: DescribeFlowLogs)

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
  request overrides =
    Request.postQuery (overrides defaultService)
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

instance Prelude.Hashable DescribeFlowLogs where
  hashWithSalt _salt DescribeFlowLogs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` flowLogIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeFlowLogs where
  rnf DescribeFlowLogs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf flowLogIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

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
        Core.toQuery
          ( Core.toQueryList "FlowLogId"
              Prelude.<$> flowLogIds
          ),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filter'),
        "MaxResults" Core.=: maxResults
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
describeFlowLogsResponse_flowLogs = Lens.lens (\DescribeFlowLogsResponse' {flowLogs} -> flowLogs) (\s@DescribeFlowLogsResponse' {} a -> s {flowLogs = a} :: DescribeFlowLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFlowLogsResponse_httpStatus :: Lens.Lens' DescribeFlowLogsResponse Prelude.Int
describeFlowLogsResponse_httpStatus = Lens.lens (\DescribeFlowLogsResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowLogsResponse' {} a -> s {httpStatus = a} :: DescribeFlowLogsResponse)

instance Prelude.NFData DescribeFlowLogsResponse where
  rnf DescribeFlowLogsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf flowLogs
      `Prelude.seq` Prelude.rnf httpStatus
