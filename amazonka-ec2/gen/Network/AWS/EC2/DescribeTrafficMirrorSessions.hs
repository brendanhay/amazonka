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
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror sessions. By default, all Traffic
-- Mirror sessions are described. Alternatively, you can filter the
-- results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorSessions
  ( -- * Creating a Request
    DescribeTrafficMirrorSessions (..),
    newDescribeTrafficMirrorSessions,

    -- * Request Lenses
    describeTrafficMirrorSessions_nextToken,
    describeTrafficMirrorSessions_trafficMirrorSessionIds,
    describeTrafficMirrorSessions_dryRun,
    describeTrafficMirrorSessions_maxResults,
    describeTrafficMirrorSessions_filters,

    -- * Destructuring the Response
    DescribeTrafficMirrorSessionsResponse (..),
    newDescribeTrafficMirrorSessionsResponse,

    -- * Response Lenses
    describeTrafficMirrorSessionsResponse_nextToken,
    describeTrafficMirrorSessionsResponse_trafficMirrorSessions,
    describeTrafficMirrorSessionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTrafficMirrorSessions' smart constructor.
data DescribeTrafficMirrorSessions = DescribeTrafficMirrorSessions'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters. The possible values are:
    --
    -- -   @description@: The Traffic Mirror session description.
    --
    -- -   @network-interface-id@: The ID of the Traffic Mirror session network
    --     interface.
    --
    -- -   @owner-id@: The ID of the account that owns the Traffic Mirror
    --     session.
    --
    -- -   @packet-length@: The assigned number of packets to mirror.
    --
    -- -   @session-number@: The assigned session number.
    --
    -- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
    --
    -- -   @traffic-mirror-session-id@: The ID of the Traffic Mirror session.
    --
    -- -   @traffic-mirror-target-id@: The ID of the Traffic Mirror target.
    --
    -- -   @virtual-network-id@: The virtual network ID of the Traffic Mirror
    --     session.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficMirrorSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrafficMirrorSessions_nextToken' - The token for the next page of results.
--
-- 'trafficMirrorSessionIds', 'describeTrafficMirrorSessions_trafficMirrorSessionIds' - The ID of the Traffic Mirror session.
--
-- 'dryRun', 'describeTrafficMirrorSessions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTrafficMirrorSessions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeTrafficMirrorSessions_filters' - One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror session description.
--
-- -   @network-interface-id@: The ID of the Traffic Mirror session network
--     interface.
--
-- -   @owner-id@: The ID of the account that owns the Traffic Mirror
--     session.
--
-- -   @packet-length@: The assigned number of packets to mirror.
--
-- -   @session-number@: The assigned session number.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
--
-- -   @traffic-mirror-session-id@: The ID of the Traffic Mirror session.
--
-- -   @traffic-mirror-target-id@: The ID of the Traffic Mirror target.
--
-- -   @virtual-network-id@: The virtual network ID of the Traffic Mirror
--     session.
newDescribeTrafficMirrorSessions ::
  DescribeTrafficMirrorSessions
newDescribeTrafficMirrorSessions =
  DescribeTrafficMirrorSessions'
    { nextToken =
        Prelude.Nothing,
      trafficMirrorSessionIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTrafficMirrorSessions_nextToken :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe Prelude.Text)
describeTrafficMirrorSessions_nextToken = Lens.lens (\DescribeTrafficMirrorSessions' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorSessions' {} a -> s {nextToken = a} :: DescribeTrafficMirrorSessions)

-- | The ID of the Traffic Mirror session.
describeTrafficMirrorSessions_trafficMirrorSessionIds :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe [Prelude.Text])
describeTrafficMirrorSessions_trafficMirrorSessionIds = Lens.lens (\DescribeTrafficMirrorSessions' {trafficMirrorSessionIds} -> trafficMirrorSessionIds) (\s@DescribeTrafficMirrorSessions' {} a -> s {trafficMirrorSessionIds = a} :: DescribeTrafficMirrorSessions) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrafficMirrorSessions_dryRun :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe Prelude.Bool)
describeTrafficMirrorSessions_dryRun = Lens.lens (\DescribeTrafficMirrorSessions' {dryRun} -> dryRun) (\s@DescribeTrafficMirrorSessions' {} a -> s {dryRun = a} :: DescribeTrafficMirrorSessions)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrafficMirrorSessions_maxResults :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe Prelude.Natural)
describeTrafficMirrorSessions_maxResults = Lens.lens (\DescribeTrafficMirrorSessions' {maxResults} -> maxResults) (\s@DescribeTrafficMirrorSessions' {} a -> s {maxResults = a} :: DescribeTrafficMirrorSessions)

-- | One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror session description.
--
-- -   @network-interface-id@: The ID of the Traffic Mirror session network
--     interface.
--
-- -   @owner-id@: The ID of the account that owns the Traffic Mirror
--     session.
--
-- -   @packet-length@: The assigned number of packets to mirror.
--
-- -   @session-number@: The assigned session number.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
--
-- -   @traffic-mirror-session-id@: The ID of the Traffic Mirror session.
--
-- -   @traffic-mirror-target-id@: The ID of the Traffic Mirror target.
--
-- -   @virtual-network-id@: The virtual network ID of the Traffic Mirror
--     session.
describeTrafficMirrorSessions_filters :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe [Filter])
describeTrafficMirrorSessions_filters = Lens.lens (\DescribeTrafficMirrorSessions' {filters} -> filters) (\s@DescribeTrafficMirrorSessions' {} a -> s {filters = a} :: DescribeTrafficMirrorSessions) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeTrafficMirrorSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorSessionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorSessionsResponse_trafficMirrorSessions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTrafficMirrorSessions_nextToken
          Lens..~ rs
          Lens.^? describeTrafficMirrorSessionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTrafficMirrorSessions
  where
  type
    AWSResponse DescribeTrafficMirrorSessions =
      DescribeTrafficMirrorSessionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorSessionsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "trafficMirrorSessionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrafficMirrorSessions

instance Prelude.NFData DescribeTrafficMirrorSessions

instance Core.ToHeaders DescribeTrafficMirrorSessions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTrafficMirrorSessions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTrafficMirrorSessions where
  toQuery DescribeTrafficMirrorSessions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTrafficMirrorSessions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "TrafficMirrorSessionId"
              Prelude.<$> trafficMirrorSessionIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeTrafficMirrorSessionsResponse' smart constructor.
data DescribeTrafficMirrorSessionsResponse = DescribeTrafficMirrorSessionsResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Describes one or more Traffic Mirror sessions. By default, all Traffic
    -- Mirror sessions are described. Alternatively, you can filter the
    -- results.
    trafficMirrorSessions :: Prelude.Maybe [TrafficMirrorSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficMirrorSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrafficMirrorSessionsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
--
-- 'trafficMirrorSessions', 'describeTrafficMirrorSessionsResponse_trafficMirrorSessions' - Describes one or more Traffic Mirror sessions. By default, all Traffic
-- Mirror sessions are described. Alternatively, you can filter the
-- results.
--
-- 'httpStatus', 'describeTrafficMirrorSessionsResponse_httpStatus' - The response's http status code.
newDescribeTrafficMirrorSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrafficMirrorSessionsResponse
newDescribeTrafficMirrorSessionsResponse pHttpStatus_ =
  DescribeTrafficMirrorSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      trafficMirrorSessions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
describeTrafficMirrorSessionsResponse_nextToken :: Lens.Lens' DescribeTrafficMirrorSessionsResponse (Prelude.Maybe Prelude.Text)
describeTrafficMirrorSessionsResponse_nextToken = Lens.lens (\DescribeTrafficMirrorSessionsResponse' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorSessionsResponse' {} a -> s {nextToken = a} :: DescribeTrafficMirrorSessionsResponse)

-- | Describes one or more Traffic Mirror sessions. By default, all Traffic
-- Mirror sessions are described. Alternatively, you can filter the
-- results.
describeTrafficMirrorSessionsResponse_trafficMirrorSessions :: Lens.Lens' DescribeTrafficMirrorSessionsResponse (Prelude.Maybe [TrafficMirrorSession])
describeTrafficMirrorSessionsResponse_trafficMirrorSessions = Lens.lens (\DescribeTrafficMirrorSessionsResponse' {trafficMirrorSessions} -> trafficMirrorSessions) (\s@DescribeTrafficMirrorSessionsResponse' {} a -> s {trafficMirrorSessions = a} :: DescribeTrafficMirrorSessionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTrafficMirrorSessionsResponse_httpStatus :: Lens.Lens' DescribeTrafficMirrorSessionsResponse Prelude.Int
describeTrafficMirrorSessionsResponse_httpStatus = Lens.lens (\DescribeTrafficMirrorSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficMirrorSessionsResponse' {} a -> s {httpStatus = a} :: DescribeTrafficMirrorSessionsResponse)

instance
  Prelude.NFData
    DescribeTrafficMirrorSessionsResponse
