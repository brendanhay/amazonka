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
-- Module      : Amazonka.EC2.DescribeTransitGatewayPeeringAttachments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your transit gateway peering attachments.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayPeeringAttachments
  ( -- * Creating a Request
    DescribeTransitGatewayPeeringAttachments (..),
    newDescribeTransitGatewayPeeringAttachments,

    -- * Request Lenses
    describeTransitGatewayPeeringAttachments_dryRun,
    describeTransitGatewayPeeringAttachments_filters,
    describeTransitGatewayPeeringAttachments_maxResults,
    describeTransitGatewayPeeringAttachments_nextToken,
    describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds,

    -- * Destructuring the Response
    DescribeTransitGatewayPeeringAttachmentsResponse (..),
    newDescribeTransitGatewayPeeringAttachmentsResponse,

    -- * Response Lenses
    describeTransitGatewayPeeringAttachmentsResponse_nextToken,
    describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments,
    describeTransitGatewayPeeringAttachmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayPeeringAttachments' smart constructor.
data DescribeTransitGatewayPeeringAttachments = DescribeTransitGatewayPeeringAttachments'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the transit gateway
    --     attachment.
    --
    -- -   @local-owner-id@ - The ID of your Amazon Web Services account.
    --
    -- -   @remote-owner-id@ - The ID of the Amazon Web Services account in the
    --     remote Region that owns the transit gateway.
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more IDs of the transit gateway peering attachments.
    transitGatewayAttachmentIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayPeeringAttachments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTransitGatewayPeeringAttachments_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTransitGatewayPeeringAttachments_filters' - One or more filters. The possible values are:
--
-- -   @transit-gateway-attachment-id@ - The ID of the transit gateway
--     attachment.
--
-- -   @local-owner-id@ - The ID of your Amazon Web Services account.
--
-- -   @remote-owner-id@ - The ID of the Amazon Web Services account in the
--     remote Region that owns the transit gateway.
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
--
-- 'maxResults', 'describeTransitGatewayPeeringAttachments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTransitGatewayPeeringAttachments_nextToken' - The token for the next page of results.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds' - One or more IDs of the transit gateway peering attachments.
newDescribeTransitGatewayPeeringAttachments ::
  DescribeTransitGatewayPeeringAttachments
newDescribeTransitGatewayPeeringAttachments =
  DescribeTransitGatewayPeeringAttachments'
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
describeTransitGatewayPeeringAttachments_dryRun :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Prelude.Maybe Prelude.Bool)
describeTransitGatewayPeeringAttachments_dryRun = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {dryRun} -> dryRun) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {dryRun = a} :: DescribeTransitGatewayPeeringAttachments)

-- | One or more filters. The possible values are:
--
-- -   @transit-gateway-attachment-id@ - The ID of the transit gateway
--     attachment.
--
-- -   @local-owner-id@ - The ID of your Amazon Web Services account.
--
-- -   @remote-owner-id@ - The ID of the Amazon Web Services account in the
--     remote Region that owns the transit gateway.
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
describeTransitGatewayPeeringAttachments_filters :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Prelude.Maybe [Filter])
describeTransitGatewayPeeringAttachments_filters = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {filters} -> filters) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {filters = a} :: DescribeTransitGatewayPeeringAttachments) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayPeeringAttachments_maxResults :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Prelude.Maybe Prelude.Natural)
describeTransitGatewayPeeringAttachments_maxResults = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {maxResults} -> maxResults) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {maxResults = a} :: DescribeTransitGatewayPeeringAttachments)

-- | The token for the next page of results.
describeTransitGatewayPeeringAttachments_nextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Prelude.Maybe Prelude.Text)
describeTransitGatewayPeeringAttachments_nextToken = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {nextToken} -> nextToken) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {nextToken = a} :: DescribeTransitGatewayPeeringAttachments)

-- | One or more IDs of the transit gateway peering attachments.
describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Prelude.Maybe [Prelude.Text])
describeTransitGatewayPeeringAttachments_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayPeeringAttachments' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayPeeringAttachments' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayPeeringAttachments) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeTransitGatewayPeeringAttachments
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayPeeringAttachmentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeTransitGatewayPeeringAttachments_nextToken
              Lens..~ rs
              Lens.^? describeTransitGatewayPeeringAttachmentsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayPeeringAttachments
  where
  type
    AWSResponse
      DescribeTransitGatewayPeeringAttachments =
      DescribeTransitGatewayPeeringAttachmentsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayPeeringAttachmentsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "transitGatewayPeeringAttachments"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayPeeringAttachments
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayPeeringAttachments' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayAttachmentIds

instance
  Prelude.NFData
    DescribeTransitGatewayPeeringAttachments
  where
  rnf DescribeTransitGatewayPeeringAttachments' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf transitGatewayAttachmentIds

instance
  Data.ToHeaders
    DescribeTransitGatewayPeeringAttachments
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeTransitGatewayPeeringAttachments
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTransitGatewayPeeringAttachments
  where
  toQuery DescribeTransitGatewayPeeringAttachments' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTransitGatewayPeeringAttachments" ::
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

-- | /See:/ 'newDescribeTransitGatewayPeeringAttachmentsResponse' smart constructor.
data DescribeTransitGatewayPeeringAttachmentsResponse = DescribeTransitGatewayPeeringAttachmentsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway peering attachments.
    transitGatewayPeeringAttachments :: Prelude.Maybe [TransitGatewayPeeringAttachment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayPeeringAttachmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayPeeringAttachmentsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayPeeringAttachments', 'describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments' - The transit gateway peering attachments.
--
-- 'httpStatus', 'describeTransitGatewayPeeringAttachmentsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayPeeringAttachmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayPeeringAttachmentsResponse
newDescribeTransitGatewayPeeringAttachmentsResponse
  pHttpStatus_ =
    DescribeTransitGatewayPeeringAttachmentsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayPeeringAttachments =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayPeeringAttachmentsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayPeeringAttachmentsResponse_nextToken = Lens.lens (\DescribeTransitGatewayPeeringAttachmentsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayPeeringAttachmentsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)

-- | The transit gateway peering attachments.
describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Prelude.Maybe [TransitGatewayPeeringAttachment])
describeTransitGatewayPeeringAttachmentsResponse_transitGatewayPeeringAttachments = Lens.lens (\DescribeTransitGatewayPeeringAttachmentsResponse' {transitGatewayPeeringAttachments} -> transitGatewayPeeringAttachments) (\s@DescribeTransitGatewayPeeringAttachmentsResponse' {} a -> s {transitGatewayPeeringAttachments = a} :: DescribeTransitGatewayPeeringAttachmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayPeeringAttachmentsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse Prelude.Int
describeTransitGatewayPeeringAttachmentsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayPeeringAttachmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayPeeringAttachmentsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayPeeringAttachmentsResponse
  where
  rnf
    DescribeTransitGatewayPeeringAttachmentsResponse' {..} =
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf transitGatewayPeeringAttachments `Prelude.seq`
          Prelude.rnf httpStatus
