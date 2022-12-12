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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeTransitGatewayConnects_dryRun,
    describeTransitGatewayConnects_filters,
    describeTransitGatewayConnects_maxResults,
    describeTransitGatewayConnects_nextToken,
    describeTransitGatewayConnects_transitGatewayAttachmentIds,

    -- * Destructuring the Response
    DescribeTransitGatewayConnectsResponse (..),
    newDescribeTransitGatewayConnectsResponse,

    -- * Response Lenses
    describeTransitGatewayConnectsResponse_nextToken,
    describeTransitGatewayConnectsResponse_transitGatewayConnects,
    describeTransitGatewayConnectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayConnects' smart constructor.
data DescribeTransitGatewayConnects = DescribeTransitGatewayConnects'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
-- Create a value of 'DescribeTransitGatewayConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTransitGatewayConnects_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'maxResults', 'describeTransitGatewayConnects_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTransitGatewayConnects_nextToken' - The token for the next page of results.
--
-- 'transitGatewayAttachmentIds', 'describeTransitGatewayConnects_transitGatewayAttachmentIds' - The IDs of the attachments.
newDescribeTransitGatewayConnects ::
  DescribeTransitGatewayConnects
newDescribeTransitGatewayConnects =
  DescribeTransitGatewayConnects'
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
describeTransitGatewayConnects_dryRun :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Bool)
describeTransitGatewayConnects_dryRun = Lens.lens (\DescribeTransitGatewayConnects' {dryRun} -> dryRun) (\s@DescribeTransitGatewayConnects' {} a -> s {dryRun = a} :: DescribeTransitGatewayConnects)

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

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayConnects_maxResults :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Natural)
describeTransitGatewayConnects_maxResults = Lens.lens (\DescribeTransitGatewayConnects' {maxResults} -> maxResults) (\s@DescribeTransitGatewayConnects' {} a -> s {maxResults = a} :: DescribeTransitGatewayConnects)

-- | The token for the next page of results.
describeTransitGatewayConnects_nextToken :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnects_nextToken = Lens.lens (\DescribeTransitGatewayConnects' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnects' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnects)

-- | The IDs of the attachments.
describeTransitGatewayConnects_transitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayConnects (Prelude.Maybe [Prelude.Text])
describeTransitGatewayConnects_transitGatewayAttachmentIds = Lens.lens (\DescribeTransitGatewayConnects' {transitGatewayAttachmentIds} -> transitGatewayAttachmentIds) (\s@DescribeTransitGatewayConnects' {} a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayConnects) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayConnectsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "transitGatewayConnectSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayConnects
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayConnects' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayAttachmentIds

instance
  Prelude.NFData
    DescribeTransitGatewayConnects
  where
  rnf DescribeTransitGatewayConnects' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentIds

instance
  Data.ToHeaders
    DescribeTransitGatewayConnects
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTransitGatewayConnects where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTransitGatewayConnects where
  toQuery DescribeTransitGatewayConnects' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTransitGatewayConnects" ::
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describeTransitGatewayConnectsResponse_transitGatewayConnects = Lens.lens (\DescribeTransitGatewayConnectsResponse' {transitGatewayConnects} -> transitGatewayConnects) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {transitGatewayConnects = a} :: DescribeTransitGatewayConnectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayConnectsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayConnectsResponse Prelude.Int
describeTransitGatewayConnectsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayConnectsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayConnectsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayConnectsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayConnectsResponse
  where
  rnf DescribeTransitGatewayConnectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayConnects
      `Prelude.seq` Prelude.rnf httpStatus
