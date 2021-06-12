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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your transit gateway peering attachments.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
  ( -- * Creating a Request
    DescribeTransitGatewayPeeringAttachments (..),
    newDescribeTransitGatewayPeeringAttachments,

    -- * Request Lenses
    describeTransitGatewayPeeringAttachments_nextToken,
    describeTransitGatewayPeeringAttachments_dryRun,
    describeTransitGatewayPeeringAttachments_maxResults,
    describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds,
    describeTransitGatewayPeeringAttachments_filters,

    -- * Destructuring the Response
    DescribeTransitGatewayPeeringAttachmentsResponse (..),
    newDescribeTransitGatewayPeeringAttachmentsResponse,

    -- * Response Lenses
    describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments,
    describeTransitGatewayPeeringAttachmentsResponse_nextToken,
    describeTransitGatewayPeeringAttachmentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGatewayPeeringAttachments' smart constructor.
data DescribeTransitGatewayPeeringAttachments = DescribeTransitGatewayPeeringAttachments'
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
    -- | One or more IDs of the transit gateway peering attachments.
    transitGatewayAttachmentIds :: Core.Maybe [Core.Text],
    -- | One or more filters. The possible values are:
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the transit gateway
    --     attachment.
    --
    -- -   @local-owner-id@ - The ID of your AWS account.
    --
    -- -   @remote-owner-id@ - The ID of the AWS account in the remote Region
    --     that owns the transit gateway.
    --
    -- -   @state@ - The state of the peering attachment. Valid values are
    --     @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ |
    --     @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@
    --     | @rollingBack@ | @rejected@ | @rejecting@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources that have a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayPeeringAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayPeeringAttachments_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGatewayPeeringAttachments_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayPeeringAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds' - One or more IDs of the transit gateway peering attachments.
--
-- 'filters', 'describeTransitGatewayPeeringAttachments_filters' - One or more filters. The possible values are:
--
-- -   @transit-gateway-attachment-id@ - The ID of the transit gateway
--     attachment.
--
-- -   @local-owner-id@ - The ID of your AWS account.
--
-- -   @remote-owner-id@ - The ID of the AWS account in the remote Region
--     that owns the transit gateway.
--
-- -   @state@ - The state of the peering attachment. Valid values are
--     @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ |
--     @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@
--     | @rollingBack@ | @rejected@ | @rejecting@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
newDescribeTransitGatewayPeeringAttachments ::
  DescribeTransitGatewayPeeringAttachments
newDescribeTransitGatewayPeeringAttachments =
  DescribeTransitGatewayPeeringAttachments'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      transitGatewayAttachmentIds =
        Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayPeeringAttachments_nextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe Core.Text)
describeTransitGatewayPeeringAttachments_nextToken = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {nextToken} -> nextToken) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {nextToken = a} :: DescribeTransitGatewayPeeringAttachments)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayPeeringAttachments_dryRun :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe Core.Bool)
describeTransitGatewayPeeringAttachments_dryRun = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {dryRun} -> dryRun) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {dryRun = a} :: DescribeTransitGatewayPeeringAttachments)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayPeeringAttachments_maxResults :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe Core.Natural)
describeTransitGatewayPeeringAttachments_maxResults = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {maxResults} -> maxResults) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {maxResults = a} :: DescribeTransitGatewayPeeringAttachments)

-- | One or more IDs of the transit gateway peering attachments.
describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe [Core.Text])
describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayPeeringAttachments) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. The possible values are:
--
-- -   @transit-gateway-attachment-id@ - The ID of the transit gateway
--     attachment.
--
-- -   @local-owner-id@ - The ID of your AWS account.
--
-- -   @remote-owner-id@ - The ID of the AWS account in the remote Region
--     that owns the transit gateway.
--
-- -   @state@ - The state of the peering attachment. Valid values are
--     @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ |
--     @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@
--     | @rollingBack@ | @rejected@ | @rejecting@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources that have a tag with a specific key,
--     regardless of the tag value.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
describeTransitGatewayPeeringAttachments_filters :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe [Filter])
describeTransitGatewayPeeringAttachments_filters = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {filters} -> filters) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {filters = a} :: DescribeTransitGatewayPeeringAttachments) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeTransitGatewayPeeringAttachments
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayPeeringAttachmentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTransitGatewayPeeringAttachments_nextToken
          Lens..~ rs
            Lens.^? describeTransitGatewayPeeringAttachmentsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayPeeringAttachments
  where
  type
    AWSResponse
      DescribeTransitGatewayPeeringAttachments =
      DescribeTransitGatewayPeeringAttachmentsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayPeeringAttachmentsResponse'
            Core.<$> ( x Core..@? "transitGatewayPeeringAttachments"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
              Core.<*> (x Core..@? "nextToken")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeTransitGatewayPeeringAttachments

instance
  Core.NFData
    DescribeTransitGatewayPeeringAttachments

instance
  Core.ToHeaders
    DescribeTransitGatewayPeeringAttachments
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeTransitGatewayPeeringAttachments
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayPeeringAttachments
  where
  toQuery DescribeTransitGatewayPeeringAttachments' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayPeeringAttachments" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayAttachmentIds"
              Core.<$> transitGatewayAttachmentIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeTransitGatewayPeeringAttachmentsResponse' smart constructor.
data DescribeTransitGatewayPeeringAttachmentsResponse = DescribeTransitGatewayPeeringAttachmentsResponse'
  { -- | The transit gateway peering attachments.
    transitGatewayPeeringAttachments :: Core.Maybe [TransitGatewayPeeringAttachment],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayPeeringAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeeringAttachments', 'describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments' - The transit gateway peering attachments.
--
-- 'nextToken', 'describeTransitGatewayPeeringAttachmentsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeTransitGatewayPeeringAttachmentsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayPeeringAttachmentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTransitGatewayPeeringAttachmentsResponse
newDescribeTransitGatewayPeeringAttachmentsResponse
  pHttpStatus_ =
    DescribeTransitGatewayPeeringAttachmentsResponse'
      { transitGatewayPeeringAttachments =
          Core.Nothing,
        nextToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachments.
describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Core.Maybe [TransitGatewayPeeringAttachment])
describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments = Lens.lens (\DescribeTransitGatewayPeeringAttachmentsResponse' {transitGatewayPeeringAttachments} -> transitGatewayPeeringAttachments) (\s@DescribeTransitGatewayPeeringAttachmentsResponse' {} a -> s {transitGatewayPeeringAttachments = a} :: DescribeTransitGatewayPeeringAttachmentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayPeeringAttachmentsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Core.Maybe Core.Text)
describeTransitGatewayPeeringAttachmentsResponse_nextToken = Lens.lens (\DescribeTransitGatewayPeeringAttachmentsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayPeeringAttachmentsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)

-- | The response's http status code.
describeTransitGatewayPeeringAttachmentsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse Core.Int
describeTransitGatewayPeeringAttachmentsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayPeeringAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayPeeringAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)

instance
  Core.NFData
    DescribeTransitGatewayPeeringAttachmentsResponse
