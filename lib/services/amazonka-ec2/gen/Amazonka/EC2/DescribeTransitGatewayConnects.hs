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
-- Module      : Amazonka.EC2.DescribeTransitGatewayConnects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Connect attachments.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayConnects
  ( -- * Creating a Request
    DescribeTransitGatewayConnects (..),
    newDescribeTransitGatewayConnects,

    -- * Request Lenses
    describeTransitGatewayConnects_filters,
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_maxResults,

    -- * Destructuring the Response
    DescribeTransitGatewayConnectsResponse (..),
    newDescribeTransitGatewayConnectsResponse,

    -- * Response Lenses
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayConnects' smart constructor.
data DescribeTransitGatewayConnects = DescribeTransitGatewayConnects'
  { -- | One or more filters. The possible values are:
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
    filters :: Prelude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the attachments.
    transitGatewayAttachmentIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'nextToken', 'describeTransitGatewayConnects_nextToken' - The token for the next page of results.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayConnects_transitGatewayAttachmentIds' - The IDs of the attachments.
--
-- 'dryRun', 'describeTransitGatewayConnects_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayConnects_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeTransitGatewayConnects ::
  DescribeTransitGatewayConnects
newDescribeTransitGatewayConnects =
  DescribeTransitGatewayConnects'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transitGatewayAttachmentIds =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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
describeTransitGatewayConnects_filters = Lens.lens (\DescribeTransitGatewayConnects' {filters} -> filters) (\s@DescribeTransitGatewayConnects' {} a -> s {filters = a} :: DescribeTransitGatewayConnects) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeTransitGatewayConnects_nextToken :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnects_nextToken = Lens.lens (\DescribeTransitGatewayConnects' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnects' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnects)

-- | The IDs of the attachments.
describeTransitGatewayConnects_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe [Prelude.Text])
describeTransitGatewayConnects_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayConnects' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayConnects' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayConnects) Prelude.. Lens.mapping Lens.coerced

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

instance Core.AWSPager DescribeTransitGatewayConnects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayConnectsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayConnectsResponse_transitGatewayConnects
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTransitGatewayConnects_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayConnectsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayConnects
  where
  type
    AWSResponse DescribeTransitGatewayConnects =
      DescribeTransitGatewayConnectsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayConnectsResponse'
            Prelude.<$> ( x Core..@? "transitGatewayConnectSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayConnects
  where
  hashWithSalt
    salt'
    DescribeTransitGatewayConnects' {..} =
      salt' `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentIds
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters

instance
  Prelude.NFData
    DescribeTransitGatewayConnects
  where
  rnf DescribeTransitGatewayConnects' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentIds
      `Prelude.seq` Prelude.rnf nextToken

instance
  Core.ToHeaders
    DescribeTransitGatewayConnects
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTransitGatewayConnects where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTransitGatewayConnects where
  toQuery DescribeTransitGatewayConnects' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayConnects" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayAttachmentIds"
              Prelude.<$> transitGatewayAttachmentIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeTransitGatewayConnectsResponse' smart constructor.
data DescribeTransitGatewayConnectsResponse = DescribeTransitGatewayConnectsResponse'
  { -- | Information about the Connect attachments.
    transitGatewayConnects :: Prelude.Maybe [TransitGatewayConnect],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnects', 'describeTransitGatewayConnectsResponse_transitGatewayConnects' - Information about the Connect attachments.
--
-- 'nextToken', 'describeTransitGatewayConnectsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeTransitGatewayConnectsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayConnectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayConnectsResponse
newDescribeTransitGatewayConnectsResponse
  pHttpStatus_ =
    DescribeTransitGatewayConnectsResponse'
      { transitGatewayConnects =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the Connect attachments.
describeTransitGatewayConnectsResponse_transitGatewayConnects :: Lens.Lens' DescribeTransitGatewayConnectsResponse (Prelude.Maybe [TransitGatewayConnect])
describeTransitGatewayConnectsResponse_transitGatewayConnects = Lens.lens (\DescribeTransitGatewayConnectsResponse' {transitGatewayConnects} -> transitGatewayConnects) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {transitGatewayConnects = a} :: DescribeTransitGatewayConnectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayConnectsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayConnectsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnectsResponse_nextToken = Lens.lens (\DescribeTransitGatewayConnectsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnectsResponse)

-- | The response's http status code.
describeTransitGatewayConnectsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayConnectsResponse Prelude.Int
describeTransitGatewayConnectsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayConnectsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayConnectsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayConnectsResponse
  where
  rnf DescribeTransitGatewayConnectsResponse' {..} =
    Prelude.rnf transitGatewayConnects
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
