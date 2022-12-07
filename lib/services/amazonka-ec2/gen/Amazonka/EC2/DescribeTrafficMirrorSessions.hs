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
-- Module      : Amazonka.EC2.DescribeTrafficMirrorSessions
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.DescribeTrafficMirrorSessions
  ( -- * Creating a Request
    DescribeTrafficMirrorSessions (..),
    newDescribeTrafficMirrorSessions,

    -- * Request Lenses
    describeTrafficMirrorSessions_nextToken,
    describeTrafficMirrorSessions_trafficMirrorSessionIds,
    describeTrafficMirrorSessions_filters,
    describeTrafficMirrorSessions_dryRun,
    describeTrafficMirrorSessions_maxResults,

    -- * Destructuring the Response
    DescribeTrafficMirrorSessionsResponse (..),
    newDescribeTrafficMirrorSessionsResponse,

    -- * Response Lenses
    describeTrafficMirrorSessionsResponse_nextToken,
    describeTrafficMirrorSessionsResponse_trafficMirrorSessions,
    describeTrafficMirrorSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrafficMirrorSessions' smart constructor.
data DescribeTrafficMirrorSessions = DescribeTrafficMirrorSessions'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionIds :: Prelude.Maybe [Prelude.Text],
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
    filters :: Prelude.Maybe [Filter],
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
--
-- 'dryRun', 'describeTrafficMirrorSessions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTrafficMirrorSessions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeTrafficMirrorSessions ::
  DescribeTrafficMirrorSessions
newDescribeTrafficMirrorSessions =
  DescribeTrafficMirrorSessions'
    { nextToken =
        Prelude.Nothing,
      trafficMirrorSessionIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTrafficMirrorSessions_nextToken :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe Prelude.Text)
describeTrafficMirrorSessions_nextToken = Lens.lens (\DescribeTrafficMirrorSessions' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorSessions' {} a -> s {nextToken = a} :: DescribeTrafficMirrorSessions)

-- | The ID of the Traffic Mirror session.
describeTrafficMirrorSessions_trafficMirrorSessionIds :: Lens.Lens' DescribeTrafficMirrorSessions (Prelude.Maybe [Prelude.Text])
describeTrafficMirrorSessions_trafficMirrorSessionIds = Lens.lens (\DescribeTrafficMirrorSessions' {trafficMirrorSessionIds} -> trafficMirrorSessionIds) (\s@DescribeTrafficMirrorSessions' {} a -> s {trafficMirrorSessionIds = a} :: DescribeTrafficMirrorSessions) Prelude.. Lens.mapping Lens.coerced

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
describeTrafficMirrorSessions_filters = Lens.lens (\DescribeTrafficMirrorSessions' {filters} -> filters) (\s@DescribeTrafficMirrorSessions' {} a -> s {filters = a} :: DescribeTrafficMirrorSessions) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorSessionsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "trafficMirrorSessionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrafficMirrorSessions
  where
  hashWithSalt _salt DescribeTrafficMirrorSessions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` trafficMirrorSessionIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeTrafficMirrorSessions where
  rnf DescribeTrafficMirrorSessions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficMirrorSessionIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeTrafficMirrorSessions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTrafficMirrorSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrafficMirrorSessions where
  toQuery DescribeTrafficMirrorSessions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTrafficMirrorSessions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "TrafficMirrorSessionId"
              Prelude.<$> trafficMirrorSessionIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults
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
describeTrafficMirrorSessionsResponse_trafficMirrorSessions = Lens.lens (\DescribeTrafficMirrorSessionsResponse' {trafficMirrorSessions} -> trafficMirrorSessions) (\s@DescribeTrafficMirrorSessionsResponse' {} a -> s {trafficMirrorSessions = a} :: DescribeTrafficMirrorSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrafficMirrorSessionsResponse_httpStatus :: Lens.Lens' DescribeTrafficMirrorSessionsResponse Prelude.Int
describeTrafficMirrorSessionsResponse_httpStatus = Lens.lens (\DescribeTrafficMirrorSessionsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficMirrorSessionsResponse' {} a -> s {httpStatus = a} :: DescribeTrafficMirrorSessionsResponse)

instance
  Prelude.NFData
    DescribeTrafficMirrorSessionsResponse
  where
  rnf DescribeTrafficMirrorSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficMirrorSessions
      `Prelude.seq` Prelude.rnf httpStatus
