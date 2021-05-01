{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayConnects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Connect attachments.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayConnects
  ( -- * Creating a Request
    DescribeTransitGatewayConnects (..),
    newDescribeTransitGatewayConnects,

    -- * Request Lenses
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_maxResults,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,
    describeTransitGatewayConnects_filters,

    -- * Destructuring the Response
    DescribeTransitGatewayConnectsResponse (..),
    newDescribeTransitGatewayConnectsResponse,

    -- * Response Lenses
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGatewayConnects' smart constructor.
data DescribeTransitGatewayConnects = DescribeTransitGatewayConnects'
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
    -- -   @options.protocol@ - The tunnel protocol (@gre@).
    --
    -- -   @state@ - The state of the attachment (@initiating@ |
    --     @initiatingRequest@ | @pendingAcceptance@ | @rollingBack@ |
    --     @pending@ | @available@ | @modifying@ | @deleting@ | @deleted@ |
    --     @failed@ | @rejected@ | @rejecting@ | @failing@).
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the Connect attachment.
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    --
    -- -   @transport-transit-gateway-attachment-id@ - The ID of the transit
    --     gateway attachment from which the Connect attachment was created.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayConnects_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGatewayConnects_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayConnects_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayConnects_transitGatewayAttachmentIds' - The IDs of the attachments.
--
-- 'filters', 'describeTransitGatewayConnects_filters' - One or more filters. The possible values are:
--
-- -   @options.protocol@ - The tunnel protocol (@gre@).
--
-- -   @state@ - The state of the attachment (@initiating@ |
--     @initiatingRequest@ | @pendingAcceptance@ | @rollingBack@ |
--     @pending@ | @available@ | @modifying@ | @deleting@ | @deleted@ |
--     @failed@ | @rejected@ | @rejecting@ | @failing@).
--
-- -   @transit-gateway-attachment-id@ - The ID of the Connect attachment.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @transport-transit-gateway-attachment-id@ - The ID of the transit
--     gateway attachment from which the Connect attachment was created.
newDescribeTransitGatewayConnects ::
  DescribeTransitGatewayConnects
newDescribeTransitGatewayConnects =
  DescribeTransitGatewayConnects'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      transitGatewayAttachmentIds =
        Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayConnects_nextToken :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnects_nextToken = Lens.lens (\DescribeTransitGatewayConnects' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnects' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnects)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayConnects_dryRun :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Bool)
describeTransitGatewayConnects_dryRun = Lens.lens (\DescribeTransitGatewayConnects' {dryRun} -> dryRun) (\s@DescribeTransitGatewayConnects' {} a -> s {dryRun = a} :: DescribeTransitGatewayConnects)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayConnects_maxResults :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Natural)
describeTransitGatewayConnects_maxResults = Lens.lens (\DescribeTransitGatewayConnects' {maxResults} -> maxResults) (\s@DescribeTransitGatewayConnects' {} a -> s {maxResults = a} :: DescribeTransitGatewayConnects)

-- | The IDs of the attachments.
describeTransitGatewayConnects_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe [Prelude.Text])
describeTransitGatewayConnects_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayConnects' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayConnects' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayConnects) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more filters. The possible values are:
--
-- -   @options.protocol@ - The tunnel protocol (@gre@).
--
-- -   @state@ - The state of the attachment (@initiating@ |
--     @initiatingRequest@ | @pendingAcceptance@ | @rollingBack@ |
--     @pending@ | @available@ | @modifying@ | @deleting@ | @deleted@ |
--     @failed@ | @rejected@ | @rejecting@ | @failing@).
--
-- -   @transit-gateway-attachment-id@ - The ID of the Connect attachment.
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @transport-transit-gateway-attachment-id@ - The ID of the transit
--     gateway attachment from which the Connect attachment was created.
describeTransitGatewayConnects_filters :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe [Filter])
describeTransitGatewayConnects_filters = Lens.lens (\DescribeTransitGatewayConnects' {filters} -> filters) (\s@DescribeTransitGatewayConnects' {} a -> s {filters = a} :: DescribeTransitGatewayConnects) Prelude.. Lens.mapping Prelude._Coerce

instance
  Pager.AWSPager
    DescribeTransitGatewayConnects
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeTransitGatewayConnectsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeTransitGatewayConnectsResponse_transitGatewayConnects
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeTransitGatewayConnects_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayConnectsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeTransitGatewayConnects
  where
  type
    Rs DescribeTransitGatewayConnects =
      DescribeTransitGatewayConnectsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayConnectsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "transitGatewayConnectSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayConnects

instance
  Prelude.NFData
    DescribeTransitGatewayConnects

instance
  Prelude.ToHeaders
    DescribeTransitGatewayConnects
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeTransitGatewayConnects
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeTransitGatewayConnects
  where
  toQuery DescribeTransitGatewayConnects' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeTransitGatewayConnects" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          ( Prelude.toQueryList "TransitGatewayAttachmentIds"
              Prelude.<$> transitGatewayAttachmentIds
          ),
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeTransitGatewayConnectsResponse' smart constructor.
data DescribeTransitGatewayConnectsResponse = DescribeTransitGatewayConnectsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Connect attachments.
    transitGatewayConnects :: Prelude.Maybe [TransitGatewayConnect],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayConnectsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayConnects', 'describeTransitGatewayConnectsResponse_transitGatewayConnects' - Information about the Connect attachments.
--
-- 'httpStatus', 'describeTransitGatewayConnectsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayConnectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayConnectsResponse
newDescribeTransitGatewayConnectsResponse
  pHttpStatus_ =
    DescribeTransitGatewayConnectsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayConnects =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayConnectsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayConnectsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnectsResponse_nextToken = Lens.lens (\DescribeTransitGatewayConnectsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnectsResponse)

-- | Information about the Connect attachments.
describeTransitGatewayConnectsResponse_transitGatewayConnects :: Lens.Lens' DescribeTransitGatewayConnectsResponse (Prelude.Maybe [TransitGatewayConnect])
describeTransitGatewayConnectsResponse_transitGatewayConnects = Lens.lens (\DescribeTransitGatewayConnectsResponse' {transitGatewayConnects} -> transitGatewayConnects) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {transitGatewayConnects = a} :: DescribeTransitGatewayConnectsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTransitGatewayConnectsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayConnectsResponse Prelude.Int
describeTransitGatewayConnectsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayConnectsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayConnectsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayConnectsResponse
