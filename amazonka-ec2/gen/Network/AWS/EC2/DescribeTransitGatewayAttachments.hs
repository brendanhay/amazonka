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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more attachments between resources and transit
-- gateways. By default, all attachments are described. Alternatively, you
-- can filter the results by attachment ID, attachment state, resource ID,
-- or resource owner.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayAttachments
  ( -- * Creating a Request
    DescribeTransitGatewayAttachments (..),
    newDescribeTransitGatewayAttachments,

    -- * Request Lenses
    describeTransitGatewayAttachments_nextToken,
    describeTransitGatewayAttachments_dryRun,
    describeTransitGatewayAttachments_maxResults,
    describeTransitGatewayAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayAttachments_filters,

    -- * Destructuring the Response
    DescribeTransitGatewayAttachmentsResponse (..),
    newDescribeTransitGatewayAttachmentsResponse,

    -- * Response Lenses
    describeTransitGatewayAttachmentsResponse_nextToken,
    describeTransitGatewayAttachmentsResponse_transitGatewayAttachments,
    describeTransitGatewayAttachmentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGatewayAttachments' smart constructor.
data DescribeTransitGatewayAttachments = DescribeTransitGatewayAttachments'
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
    -- -   @association.state@ - The state of the association (@associating@ |
    --     @associated@ | @disassociating@).
    --
    -- -   @association.transit-gateway-route-table-id@ - The ID of the route
    --     table for the transit gateway.
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-owner-id@ - The ID of the AWS account that owns the
    --     resource.
    --
    -- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
    --     | @direct-connect-gateway@ | @peering@ | @connect@.
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
    -- -   @transit-gateway-owner-id@ - The ID of the AWS account that owns the
    --     transit gateway.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayAttachments_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGatewayAttachments_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayAttachments_transitGatewayAttachmentIds' - The IDs of the attachments.
--
-- 'filters', 'describeTransitGatewayAttachments_filters' - One or more filters. The possible values are:
--
-- -   @association.state@ - The state of the association (@associating@ |
--     @associated@ | @disassociating@).
--
-- -   @association.transit-gateway-route-table-id@ - The ID of the route
--     table for the transit gateway.
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-owner-id@ - The ID of the AWS account that owns the
--     resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
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
-- -   @transit-gateway-owner-id@ - The ID of the AWS account that owns the
--     transit gateway.
newDescribeTransitGatewayAttachments ::
  DescribeTransitGatewayAttachments
newDescribeTransitGatewayAttachments =
  DescribeTransitGatewayAttachments'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      transitGatewayAttachmentIds =
        Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayAttachments_nextToken :: Lens.Lens' DescribeTransitGatewayAttachments (Prelude.Maybe Prelude.Text)
describeTransitGatewayAttachments_nextToken = Lens.lens (\DescribeTransitGatewayAttachments' {nextToken} -> nextToken) (\s@DescribeTransitGatewayAttachments' {} a -> s {nextToken = a} :: DescribeTransitGatewayAttachments)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayAttachments_dryRun :: Lens.Lens' DescribeTransitGatewayAttachments (Prelude.Maybe Prelude.Bool)
describeTransitGatewayAttachments_dryRun = Lens.lens (\DescribeTransitGatewayAttachments' {dryRun} -> dryRun) (\s@DescribeTransitGatewayAttachments' {} a -> s {dryRun = a} :: DescribeTransitGatewayAttachments)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayAttachments_maxResults :: Lens.Lens' DescribeTransitGatewayAttachments (Prelude.Maybe Prelude.Natural)
describeTransitGatewayAttachments_maxResults = Lens.lens (\DescribeTransitGatewayAttachments' {maxResults} -> maxResults) (\s@DescribeTransitGatewayAttachments' {} a -> s {maxResults = a} :: DescribeTransitGatewayAttachments)

-- | The IDs of the attachments.
describeTransitGatewayAttachments_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayAttachments (Prelude.Maybe [Prelude.Text])
describeTransitGatewayAttachments_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayAttachments' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayAttachments' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayAttachments) Prelude.. Lens.mapping Lens._Coerce

-- | One or more filters. The possible values are:
--
-- -   @association.state@ - The state of the association (@associating@ |
--     @associated@ | @disassociating@).
--
-- -   @association.transit-gateway-route-table-id@ - The ID of the route
--     table for the transit gateway.
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-owner-id@ - The ID of the AWS account that owns the
--     resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
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
-- -   @transit-gateway-owner-id@ - The ID of the AWS account that owns the
--     transit gateway.
describeTransitGatewayAttachments_filters :: Lens.Lens' DescribeTransitGatewayAttachments (Prelude.Maybe [Filter])
describeTransitGatewayAttachments_filters = Lens.lens (\DescribeTransitGatewayAttachments' {filters} -> filters) (\s@DescribeTransitGatewayAttachments' {} a -> s {filters = a} :: DescribeTransitGatewayAttachments) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeTransitGatewayAttachments
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayAttachmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayAttachmentsResponse_transitGatewayAttachments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTransitGatewayAttachments_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayAttachmentsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayAttachments
  where
  type
    AWSResponse DescribeTransitGatewayAttachments =
      DescribeTransitGatewayAttachmentsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayAttachmentsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayAttachments"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayAttachments

instance
  Prelude.NFData
    DescribeTransitGatewayAttachments

instance
  Core.ToHeaders
    DescribeTransitGatewayAttachments
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeTransitGatewayAttachments
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayAttachments
  where
  toQuery DescribeTransitGatewayAttachments' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayAttachments" ::
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

-- | /See:/ 'newDescribeTransitGatewayAttachmentsResponse' smart constructor.
data DescribeTransitGatewayAttachmentsResponse = DescribeTransitGatewayAttachmentsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the attachments.
    transitGatewayAttachments :: Prelude.Maybe [TransitGatewayAttachment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayAttachmentsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayAttachments', 'describeTransitGatewayAttachmentsResponse_transitGatewayAttachments' - Information about the attachments.
--
-- 'httpStatus', 'describeTransitGatewayAttachmentsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayAttachmentsResponse
newDescribeTransitGatewayAttachmentsResponse
  pHttpStatus_ =
    DescribeTransitGatewayAttachmentsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayAttachments =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayAttachmentsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayAttachmentsResponse_nextToken = Lens.lens (\DescribeTransitGatewayAttachmentsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayAttachmentsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayAttachmentsResponse)

-- | Information about the attachments.
describeTransitGatewayAttachmentsResponse_transitGatewayAttachments :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Prelude.Maybe [TransitGatewayAttachment])
describeTransitGatewayAttachmentsResponse_transitGatewayAttachments = Lens.lens (\DescribeTransitGatewayAttachmentsResponse' {transitGatewayAttachments} -> transitGatewayAttachments) (\s@DescribeTransitGatewayAttachmentsResponse' {} a -> s {transitGatewayAttachments = a} :: DescribeTransitGatewayAttachmentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTransitGatewayAttachmentsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse Prelude.Int
describeTransitGatewayAttachmentsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayAttachmentsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayAttachmentsResponse
