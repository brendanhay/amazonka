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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeFlowLogs_dryRun,
    describeFlowLogs_filter,
    describeFlowLogs_flowLogIds,
    describeFlowLogs_maxResults,
    describeFlowLogs_nextToken,

    -- * Destructuring the Response
    DescribeFlowLogsResponse (..),
    newDescribeFlowLogsResponse,

    -- * Response Lenses
    describeFlowLogsResponse_flowLogs,
    describeFlowLogsResponse_nextToken,
    describeFlowLogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { -- | Checks whether you have the required permissions for the action, without
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
    -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to request the next page of items. Pagination continues from
    -- the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'flowLogIds', 'describeFlowLogs_flowLogIds' - One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- 'maxResults', 'describeFlowLogs_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeFlowLogs_nextToken' - The token to request the next page of items. Pagination continues from
-- the end of the items returned by the previous request.
newDescribeFlowLogs ::
  DescribeFlowLogs
newDescribeFlowLogs =
  DescribeFlowLogs'
    { dryRun = Prelude.Nothing,
      filter' = Prelude.Nothing,
      flowLogIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

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

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
describeFlowLogs_flowLogIds :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe [Prelude.Text])
describeFlowLogs_flowLogIds = Lens.lens (\DescribeFlowLogs' {flowLogIds} -> flowLogIds) (\s@DescribeFlowLogs' {} a -> s {flowLogIds = a} :: DescribeFlowLogs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeFlowLogs_maxResults :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Int)
describeFlowLogs_maxResults = Lens.lens (\DescribeFlowLogs' {maxResults} -> maxResults) (\s@DescribeFlowLogs' {} a -> s {maxResults = a} :: DescribeFlowLogs)

-- | The token to request the next page of items. Pagination continues from
-- the end of the items returned by the previous request.
describeFlowLogs_nextToken :: Lens.Lens' DescribeFlowLogs (Prelude.Maybe Prelude.Text)
describeFlowLogs_nextToken = Lens.lens (\DescribeFlowLogs' {nextToken} -> nextToken) (\s@DescribeFlowLogs' {} a -> s {nextToken = a} :: DescribeFlowLogs)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..@? "flowLogSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFlowLogs where
  hashWithSalt _salt DescribeFlowLogs' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` flowLogIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFlowLogs where
  rnf DescribeFlowLogs' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf flowLogIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFlowLogs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFlowLogs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFlowLogs where
  toQuery DescribeFlowLogs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeFlowLogs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filter'),
        Data.toQuery
          ( Data.toQueryList "FlowLogId"
              Prelude.<$> flowLogIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeFlowLogsResponse' smart constructor.
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
  { -- | Information about the flow logs.
    flowLogs :: Prelude.Maybe [FlowLog],
    -- | The token to request the next page of items. This value is @null@ when
    -- there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'flowLogs', 'describeFlowLogsResponse_flowLogs' - Information about the flow logs.
--
-- 'nextToken', 'describeFlowLogsResponse_nextToken' - The token to request the next page of items. This value is @null@ when
-- there are no more items to return.
--
-- 'httpStatus', 'describeFlowLogsResponse_httpStatus' - The response's http status code.
newDescribeFlowLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFlowLogsResponse
newDescribeFlowLogsResponse pHttpStatus_ =
  DescribeFlowLogsResponse'
    { flowLogs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the flow logs.
describeFlowLogsResponse_flowLogs :: Lens.Lens' DescribeFlowLogsResponse (Prelude.Maybe [FlowLog])
describeFlowLogsResponse_flowLogs = Lens.lens (\DescribeFlowLogsResponse' {flowLogs} -> flowLogs) (\s@DescribeFlowLogsResponse' {} a -> s {flowLogs = a} :: DescribeFlowLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to request the next page of items. This value is @null@ when
-- there are no more items to return.
describeFlowLogsResponse_nextToken :: Lens.Lens' DescribeFlowLogsResponse (Prelude.Maybe Prelude.Text)
describeFlowLogsResponse_nextToken = Lens.lens (\DescribeFlowLogsResponse' {nextToken} -> nextToken) (\s@DescribeFlowLogsResponse' {} a -> s {nextToken = a} :: DescribeFlowLogsResponse)

-- | The response's http status code.
describeFlowLogsResponse_httpStatus :: Lens.Lens' DescribeFlowLogsResponse Prelude.Int
describeFlowLogsResponse_httpStatus = Lens.lens (\DescribeFlowLogsResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowLogsResponse' {} a -> s {httpStatus = a} :: DescribeFlowLogsResponse)

instance Prelude.NFData DescribeFlowLogsResponse where
  rnf DescribeFlowLogsResponse' {..} =
    Prelude.rnf flowLogs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
