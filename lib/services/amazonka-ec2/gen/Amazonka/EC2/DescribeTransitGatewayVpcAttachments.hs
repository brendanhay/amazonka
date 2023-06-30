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
-- Module      : Amazonka.EC2.DescribeTransitGatewayVpcAttachments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more VPC attachments. By default, all VPC attachments
-- are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayVpcAttachments
  ( -- * Creating a Request
    DescribeTransitGatewayVpcAttachments (..),
    newDescribeTransitGatewayVpcAttachments,

    -- * Request Lenses
    describeTransitGatewayVpcAttachments_dryRun,
    describeTransitGatewayVpcAttachments_filters,
    describeTransitGatewayVpcAttachments_maxResults,
    describeTransitGatewayVpcAttachments_nextToken,
    describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds,

    -- * Destructuring the Response
    DescribeTransitGatewayVpcAttachmentsResponse (..),
    newDescribeTransitGatewayVpcAttachmentsResponse,

    -- * Response Lenses
    describeTransitGatewayVpcAttachmentsResponse_nextToken,
    describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments,
    describeTransitGatewayVpcAttachmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayVpcAttachments' smart constructor.
data DescribeTransitGatewayVpcAttachments = DescribeTransitGatewayVpcAttachments'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @state@ - The state of the attachment. Valid values are @available@
    --     | @deleted@ | @deleting@ | @failed@ | @failing@ |
    --     @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@
    --     | @rollingBack@ | @rejected@ | @rejecting@.
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the attachment.
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    --
    -- -   @vpc-id@ - The ID of the VPC.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the attachments.
    transitGatewayAttachmentIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayVpcAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTransitGatewayVpcAttachments_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTransitGatewayVpcAttachments_filters' - One or more filters. The possible values are:
--
-- -   @state@ - The state of the attachment. Valid values are @available@
--     | @deleted@ | @deleting@ | @failed@ | @failing@ |
--     @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@
--     | @rollingBack@ | @rejected@ | @rejecting@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @vpc-id@ - The ID of the VPC.
--
-- 'maxResults', 'describeTransitGatewayVpcAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTransitGatewayVpcAttachments_nextToken' - The token for the next page of results.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds' - The IDs of the attachments.
newDescribeTransitGatewayVpcAttachments ::
  DescribeTransitGatewayVpcAttachments
newDescribeTransitGatewayVpcAttachments =
  DescribeTransitGatewayVpcAttachments'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transitGatewayAttachmentIds =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayVpcAttachments_dryRun :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe Prelude.Bool)
describeTransitGatewayVpcAttachments_dryRun = Lens.lens (\DescribeTransitGatewayVpcAttachments' {dryRun} -> dryRun) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {dryRun = a} :: DescribeTransitGatewayVpcAttachments)

-- | One or more filters. The possible values are:
--
-- -   @state@ - The state of the attachment. Valid values are @available@
--     | @deleted@ | @deleting@ | @failed@ | @failing@ |
--     @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@
--     | @rollingBack@ | @rejected@ | @rejecting@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @vpc-id@ - The ID of the VPC.
describeTransitGatewayVpcAttachments_filters :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe [Filter])
describeTransitGatewayVpcAttachments_filters = Lens.lens (\DescribeTransitGatewayVpcAttachments' {filters} -> filters) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {filters = a} :: DescribeTransitGatewayVpcAttachments) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayVpcAttachments_maxResults :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe Prelude.Natural)
describeTransitGatewayVpcAttachments_maxResults = Lens.lens (\DescribeTransitGatewayVpcAttachments' {maxResults} -> maxResults) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {maxResults = a} :: DescribeTransitGatewayVpcAttachments)

-- | The token for the next page of results.
describeTransitGatewayVpcAttachments_nextToken :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe Prelude.Text)
describeTransitGatewayVpcAttachments_nextToken = Lens.lens (\DescribeTransitGatewayVpcAttachments' {nextToken} -> nextToken) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {nextToken = a} :: DescribeTransitGatewayVpcAttachments)

-- | The IDs of the attachments.
describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe [Prelude.Text])
describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayVpcAttachments' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayVpcAttachments) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeTransitGatewayVpcAttachments
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayVpcAttachmentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeTransitGatewayVpcAttachments_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayVpcAttachmentsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayVpcAttachments
  where
  type
    AWSResponse DescribeTransitGatewayVpcAttachments =
      DescribeTransitGatewayVpcAttachmentsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayVpcAttachmentsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "transitGatewayVpcAttachments"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayVpcAttachments
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayVpcAttachments' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayAttachmentIds

instance
  Prelude.NFData
    DescribeTransitGatewayVpcAttachments
  where
  rnf DescribeTransitGatewayVpcAttachments' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentIds

instance
  Data.ToHeaders
    DescribeTransitGatewayVpcAttachments
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeTransitGatewayVpcAttachments
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTransitGatewayVpcAttachments
  where
  toQuery DescribeTransitGatewayVpcAttachments' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTransitGatewayVpcAttachments" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "TransitGatewayAttachmentIds"
              Prelude.<$> transitGatewayAttachmentIds
          )
      ]

-- | /See:/ 'newDescribeTransitGatewayVpcAttachmentsResponse' smart constructor.
data DescribeTransitGatewayVpcAttachmentsResponse = DescribeTransitGatewayVpcAttachmentsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC attachments.
    transitGatewayVpcAttachments :: Prelude.Maybe [TransitGatewayVpcAttachment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayVpcAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayVpcAttachmentsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayVpcAttachments', 'describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments' - Information about the VPC attachments.
--
-- 'httpStatus', 'describeTransitGatewayVpcAttachmentsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayVpcAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayVpcAttachmentsResponse
newDescribeTransitGatewayVpcAttachmentsResponse
  pHttpStatus_ =
    DescribeTransitGatewayVpcAttachmentsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayVpcAttachments =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayVpcAttachmentsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayVpcAttachmentsResponse_nextToken = Lens.lens (\DescribeTransitGatewayVpcAttachmentsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayVpcAttachmentsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayVpcAttachmentsResponse)

-- | Information about the VPC attachments.
describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse (Prelude.Maybe [TransitGatewayVpcAttachment])
describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments = Lens.lens (\DescribeTransitGatewayVpcAttachmentsResponse' {transitGatewayVpcAttachments} -> transitGatewayVpcAttachments) (\s@DescribeTransitGatewayVpcAttachmentsResponse' {} a -> s {transitGatewayVpcAttachments = a} :: DescribeTransitGatewayVpcAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayVpcAttachmentsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse Prelude.Int
describeTransitGatewayVpcAttachmentsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayVpcAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayVpcAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayVpcAttachmentsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayVpcAttachmentsResponse
  where
  rnf DescribeTransitGatewayVpcAttachmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayVpcAttachments
      `Prelude.seq` Prelude.rnf httpStatus
