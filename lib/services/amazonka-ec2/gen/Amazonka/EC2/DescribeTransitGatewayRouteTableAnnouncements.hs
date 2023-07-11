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
-- Module      : Amazonka.EC2.DescribeTransitGatewayRouteTableAnnouncements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway route table advertisements.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayRouteTableAnnouncements
  ( -- * Creating a Request
    DescribeTransitGatewayRouteTableAnnouncements (..),
    newDescribeTransitGatewayRouteTableAnnouncements,

    -- * Request Lenses
    describeTransitGatewayRouteTableAnnouncements_dryRun,
    describeTransitGatewayRouteTableAnnouncements_filters,
    describeTransitGatewayRouteTableAnnouncements_maxResults,
    describeTransitGatewayRouteTableAnnouncements_nextToken,
    describeTransitGatewayRouteTableAnnouncements_transitGatewayRouteTableAnnouncementIds,

    -- * Destructuring the Response
    DescribeTransitGatewayRouteTableAnnouncementsResponse (..),
    newDescribeTransitGatewayRouteTableAnnouncementsResponse,

    -- * Response Lenses
    describeTransitGatewayRouteTableAnnouncementsResponse_nextToken,
    describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements,
    describeTransitGatewayRouteTableAnnouncementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayRouteTableAnnouncements' smart constructor.
data DescribeTransitGatewayRouteTableAnnouncements = DescribeTransitGatewayRouteTableAnnouncements'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters associated with the transit gateway policy table.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the transit gateway route tables that are being advertised.
    transitGatewayRouteTableAnnouncementIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayRouteTableAnnouncements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTransitGatewayRouteTableAnnouncements_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTransitGatewayRouteTableAnnouncements_filters' - The filters associated with the transit gateway policy table.
--
-- 'maxResults', 'describeTransitGatewayRouteTableAnnouncements_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTransitGatewayRouteTableAnnouncements_nextToken' - The token for the next page of results.
--
-- 'transitGatewayRouteTableAnnouncementIds', 'describeTransitGatewayRouteTableAnnouncements_transitGatewayRouteTableAnnouncementIds' - The IDs of the transit gateway route tables that are being advertised.
newDescribeTransitGatewayRouteTableAnnouncements ::
  DescribeTransitGatewayRouteTableAnnouncements
newDescribeTransitGatewayRouteTableAnnouncements =
  DescribeTransitGatewayRouteTableAnnouncements'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transitGatewayRouteTableAnnouncementIds =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayRouteTableAnnouncements_dryRun :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncements (Prelude.Maybe Prelude.Bool)
describeTransitGatewayRouteTableAnnouncements_dryRun = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncements' {dryRun} -> dryRun) (\s@DescribeTransitGatewayRouteTableAnnouncements' {} a -> s {dryRun = a} :: DescribeTransitGatewayRouteTableAnnouncements)

-- | The filters associated with the transit gateway policy table.
describeTransitGatewayRouteTableAnnouncements_filters :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncements (Prelude.Maybe [Filter])
describeTransitGatewayRouteTableAnnouncements_filters = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncements' {filters} -> filters) (\s@DescribeTransitGatewayRouteTableAnnouncements' {} a -> s {filters = a} :: DescribeTransitGatewayRouteTableAnnouncements) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayRouteTableAnnouncements_maxResults :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncements (Prelude.Maybe Prelude.Natural)
describeTransitGatewayRouteTableAnnouncements_maxResults = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncements' {maxResults} -> maxResults) (\s@DescribeTransitGatewayRouteTableAnnouncements' {} a -> s {maxResults = a} :: DescribeTransitGatewayRouteTableAnnouncements)

-- | The token for the next page of results.
describeTransitGatewayRouteTableAnnouncements_nextToken :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncements (Prelude.Maybe Prelude.Text)
describeTransitGatewayRouteTableAnnouncements_nextToken = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncements' {nextToken} -> nextToken) (\s@DescribeTransitGatewayRouteTableAnnouncements' {} a -> s {nextToken = a} :: DescribeTransitGatewayRouteTableAnnouncements)

-- | The IDs of the transit gateway route tables that are being advertised.
describeTransitGatewayRouteTableAnnouncements_transitGatewayRouteTableAnnouncementIds :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncements (Prelude.Maybe [Prelude.Text])
describeTransitGatewayRouteTableAnnouncements_transitGatewayRouteTableAnnouncementIds = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncements' {transitGatewayRouteTableAnnouncementIds} -> transitGatewayRouteTableAnnouncementIds) (\s@DescribeTransitGatewayRouteTableAnnouncements' {} a -> s {transitGatewayRouteTableAnnouncementIds = a} :: DescribeTransitGatewayRouteTableAnnouncements) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeTransitGatewayRouteTableAnnouncements
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayRouteTableAnnouncementsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeTransitGatewayRouteTableAnnouncements_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayRouteTableAnnouncementsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayRouteTableAnnouncements
  where
  type
    AWSResponse
      DescribeTransitGatewayRouteTableAnnouncements =
      DescribeTransitGatewayRouteTableAnnouncementsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayRouteTableAnnouncementsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "transitGatewayRouteTableAnnouncements"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayRouteTableAnnouncements
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayRouteTableAnnouncements' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayRouteTableAnnouncementIds

instance
  Prelude.NFData
    DescribeTransitGatewayRouteTableAnnouncements
  where
  rnf
    DescribeTransitGatewayRouteTableAnnouncements' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf filters
        `Prelude.seq` Prelude.rnf maxResults
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncementIds

instance
  Data.ToHeaders
    DescribeTransitGatewayRouteTableAnnouncements
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeTransitGatewayRouteTableAnnouncements
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTransitGatewayRouteTableAnnouncements
  where
  toQuery
    DescribeTransitGatewayRouteTableAnnouncements' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeTransitGatewayRouteTableAnnouncements" ::
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
            ( Data.toQueryList
                "TransitGatewayRouteTableAnnouncementIds"
                Prelude.<$> transitGatewayRouteTableAnnouncementIds
            )
        ]

-- | /See:/ 'newDescribeTransitGatewayRouteTableAnnouncementsResponse' smart constructor.
data DescribeTransitGatewayRouteTableAnnouncementsResponse = DescribeTransitGatewayRouteTableAnnouncementsResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Describes the transit gateway route table announcement.
    transitGatewayRouteTableAnnouncements :: Prelude.Maybe [TransitGatewayRouteTableAnnouncement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayRouteTableAnnouncementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayRouteTableAnnouncementsResponse_nextToken' - The token for the next page of results.
--
-- 'transitGatewayRouteTableAnnouncements', 'describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements' - Describes the transit gateway route table announcement.
--
-- 'httpStatus', 'describeTransitGatewayRouteTableAnnouncementsResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayRouteTableAnnouncementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayRouteTableAnnouncementsResponse
newDescribeTransitGatewayRouteTableAnnouncementsResponse
  pHttpStatus_ =
    DescribeTransitGatewayRouteTableAnnouncementsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayRouteTableAnnouncements =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token for the next page of results.
describeTransitGatewayRouteTableAnnouncementsResponse_nextToken :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncementsResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayRouteTableAnnouncementsResponse_nextToken = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncementsResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayRouteTableAnnouncementsResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayRouteTableAnnouncementsResponse)

-- | Describes the transit gateway route table announcement.
describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncementsResponse (Prelude.Maybe [TransitGatewayRouteTableAnnouncement])
describeTransitGatewayRouteTableAnnouncementsResponse_transitGatewayRouteTableAnnouncements = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncementsResponse' {transitGatewayRouteTableAnnouncements} -> transitGatewayRouteTableAnnouncements) (\s@DescribeTransitGatewayRouteTableAnnouncementsResponse' {} a -> s {transitGatewayRouteTableAnnouncements = a} :: DescribeTransitGatewayRouteTableAnnouncementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayRouteTableAnnouncementsResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayRouteTableAnnouncementsResponse Prelude.Int
describeTransitGatewayRouteTableAnnouncementsResponse_httpStatus = Lens.lens (\DescribeTransitGatewayRouteTableAnnouncementsResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayRouteTableAnnouncementsResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayRouteTableAnnouncementsResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayRouteTableAnnouncementsResponse
  where
  rnf
    DescribeTransitGatewayRouteTableAnnouncementsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncements
        `Prelude.seq` Prelude.rnf httpStatus
