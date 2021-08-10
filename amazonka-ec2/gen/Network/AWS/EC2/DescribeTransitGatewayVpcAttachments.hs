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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayVpcAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more VPC attachments. By default, all VPC attachments
-- are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayVpcAttachments
  ( -- * Creating a Request
    DescribeTransitGatewayVpcAttachments (..),
    newDescribeTransitGatewayVpcAttachments,

    -- * Request Lenses
    describeTransitGatewayVpcAttachments_nextToken,
    describeTransitGatewayVpcAttachments_dryRun,
    describeTransitGatewayVpcAttachments_maxResults,
    describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayVpcAttachments_filters,

    -- * Destructuring the Response
    DescribeTransitGatewayVpcAttachmentsResponse (..),
    newDescribeTransitGatewayVpcAttachmentsResponse,

    -- * Response Lenses
    describeTransitGatewayVpcAttachmentsResponse_nextToken,
    describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments,
    describeTransitGatewayVpcAttachmentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGatewayVpcAttachments' smart constructor.
data DescribeTransitGatewayVpcAttachments = DescribeTransitGatewayVpcAttachments'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the attachments.
    transitGatewayAttachmentIds :: Prelude.Maybe [Prelude.Text],
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
    filters :: Prelude.Maybe [Filter]
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
-- 'nextToken', 'describeTransitGatewayVpcAttachments_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGatewayVpcAttachments_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayVpcAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds' - The IDs of the attachments.
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
newDescribeTransitGatewayVpcAttachments ::
  DescribeTransitGatewayVpcAttachments
newDescribeTransitGatewayVpcAttachments =
  DescribeTransitGatewayVpcAttachments'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      transitGatewayAttachmentIds =
        Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayVpcAttachments_nextToken :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe Prelude.Text)
describeTransitGatewayVpcAttachments_nextToken = Lens.lens (\DescribeTransitGatewayVpcAttachments' {nextToken} -> nextToken) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {nextToken = a} :: DescribeTransitGatewayVpcAttachments)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayVpcAttachments_dryRun :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe Prelude.Bool)
describeTransitGatewayVpcAttachments_dryRun = Lens.lens (\DescribeTransitGatewayVpcAttachments' {dryRun} -> dryRun) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {dryRun = a} :: DescribeTransitGatewayVpcAttachments)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayVpcAttachments_maxResults :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe Prelude.Natural)
describeTransitGatewayVpcAttachments_maxResults = Lens.lens (\DescribeTransitGatewayVpcAttachments' {maxResults} -> maxResults) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {maxResults = a} :: DescribeTransitGatewayVpcAttachments)

-- | The IDs of the attachments.
describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Prelude.Maybe [Prelude.Text])
describeTransitGatewayVpcAttachments_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayVpcAttachments' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayVpcAttachments) Prelude.. Lens.mapping Lens._Coerce

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
describeTransitGatewayVpcAttachments_filters = Lens.lens (\DescribeTransitGatewayVpcAttachments' {filters} -> filters) (\s@DescribeTransitGatewayVpcAttachments' {} a -> s {filters = a} :: DescribeTransitGatewayVpcAttachments) Prelude.. Lens.mapping Lens._Coerce

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
      Prelude.Just Prelude.$
        rq
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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayVpcAttachmentsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayVpcAttachments"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayVpcAttachments

instance
  Prelude.NFData
    DescribeTransitGatewayVpcAttachments

instance
  Core.ToHeaders
    DescribeTransitGatewayVpcAttachments
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeTransitGatewayVpcAttachments
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayVpcAttachments
  where
  toQuery DescribeTransitGatewayVpcAttachments' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayVpcAttachments" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayAttachmentIds"
              Prelude.<$> transitGatewayAttachmentIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
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
describeTransitGatewayVpcAttachmentsResponse_transitGatewayVpcAttachments = Lens.lens (\DescribeTransitGatewayVpcAttachmentsResponse' {transitGatewayVpcAttachments} -> transitGatewayVpcAttachments) (\s@DescribeTransitGatewayVpcAttachmentsResponse' {} a -> s {transitGatewayVpcAttachments = a} :: DescribeTransitGatewayVpcAttachmentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTransitGatewayVpcAttachmentsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse Prelude.Int
describeTransitGatewayVpcAttachmentsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayVpcAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayVpcAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayVpcAttachmentsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayVpcAttachmentsResponse
