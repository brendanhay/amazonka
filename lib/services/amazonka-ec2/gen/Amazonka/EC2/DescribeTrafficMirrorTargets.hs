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
-- Module      : Amazonka.EC2.DescribeTrafficMirrorTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about one or more Traffic Mirror targets.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTrafficMirrorTargets
  ( -- * Creating a Request
    DescribeTrafficMirrorTargets (..),
    newDescribeTrafficMirrorTargets,

    -- * Request Lenses
    describeTrafficMirrorTargets_dryRun,
    describeTrafficMirrorTargets_filters,
    describeTrafficMirrorTargets_maxResults,
    describeTrafficMirrorTargets_nextToken,
    describeTrafficMirrorTargets_trafficMirrorTargetIds,

    -- * Destructuring the Response
    DescribeTrafficMirrorTargetsResponse (..),
    newDescribeTrafficMirrorTargetsResponse,

    -- * Response Lenses
    describeTrafficMirrorTargetsResponse_nextToken,
    describeTrafficMirrorTargetsResponse_trafficMirrorTargets,
    describeTrafficMirrorTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrafficMirrorTargets' smart constructor.
data DescribeTrafficMirrorTargets = DescribeTrafficMirrorTargets'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @description@: The Traffic Mirror target description.
    --
    -- -   @network-interface-id@: The ID of the Traffic Mirror session network
    --     interface.
    --
    -- -   @network-load-balancer-arn@: The Amazon Resource Name (ARN) of the
    --     Network Load Balancer that is associated with the session.
    --
    -- -   @owner-id@: The ID of the account that owns the Traffic Mirror
    --     session.
    --
    -- -   @traffic-mirror-target-id@: The ID of the Traffic Mirror target.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror targets.
    trafficMirrorTargetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficMirrorTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTrafficMirrorTargets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTrafficMirrorTargets_filters' - One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror target description.
--
-- -   @network-interface-id@: The ID of the Traffic Mirror session network
--     interface.
--
-- -   @network-load-balancer-arn@: The Amazon Resource Name (ARN) of the
--     Network Load Balancer that is associated with the session.
--
-- -   @owner-id@: The ID of the account that owns the Traffic Mirror
--     session.
--
-- -   @traffic-mirror-target-id@: The ID of the Traffic Mirror target.
--
-- 'maxResults', 'describeTrafficMirrorTargets_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTrafficMirrorTargets_nextToken' - The token for the next page of results.
--
-- 'trafficMirrorTargetIds', 'describeTrafficMirrorTargets_trafficMirrorTargetIds' - The ID of the Traffic Mirror targets.
newDescribeTrafficMirrorTargets ::
  DescribeTrafficMirrorTargets
newDescribeTrafficMirrorTargets =
  DescribeTrafficMirrorTargets'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      trafficMirrorTargetIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrafficMirrorTargets_dryRun :: Lens.Lens' DescribeTrafficMirrorTargets (Prelude.Maybe Prelude.Bool)
describeTrafficMirrorTargets_dryRun = Lens.lens (\DescribeTrafficMirrorTargets' {dryRun} -> dryRun) (\s@DescribeTrafficMirrorTargets' {} a -> s {dryRun = a} :: DescribeTrafficMirrorTargets)

-- | One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror target description.
--
-- -   @network-interface-id@: The ID of the Traffic Mirror session network
--     interface.
--
-- -   @network-load-balancer-arn@: The Amazon Resource Name (ARN) of the
--     Network Load Balancer that is associated with the session.
--
-- -   @owner-id@: The ID of the account that owns the Traffic Mirror
--     session.
--
-- -   @traffic-mirror-target-id@: The ID of the Traffic Mirror target.
describeTrafficMirrorTargets_filters :: Lens.Lens' DescribeTrafficMirrorTargets (Prelude.Maybe [Filter])
describeTrafficMirrorTargets_filters = Lens.lens (\DescribeTrafficMirrorTargets' {filters} -> filters) (\s@DescribeTrafficMirrorTargets' {} a -> s {filters = a} :: DescribeTrafficMirrorTargets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrafficMirrorTargets_maxResults :: Lens.Lens' DescribeTrafficMirrorTargets (Prelude.Maybe Prelude.Natural)
describeTrafficMirrorTargets_maxResults = Lens.lens (\DescribeTrafficMirrorTargets' {maxResults} -> maxResults) (\s@DescribeTrafficMirrorTargets' {} a -> s {maxResults = a} :: DescribeTrafficMirrorTargets)

-- | The token for the next page of results.
describeTrafficMirrorTargets_nextToken :: Lens.Lens' DescribeTrafficMirrorTargets (Prelude.Maybe Prelude.Text)
describeTrafficMirrorTargets_nextToken = Lens.lens (\DescribeTrafficMirrorTargets' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorTargets' {} a -> s {nextToken = a} :: DescribeTrafficMirrorTargets)

-- | The ID of the Traffic Mirror targets.
describeTrafficMirrorTargets_trafficMirrorTargetIds :: Lens.Lens' DescribeTrafficMirrorTargets (Prelude.Maybe [Prelude.Text])
describeTrafficMirrorTargets_trafficMirrorTargetIds = Lens.lens (\DescribeTrafficMirrorTargets' {trafficMirrorTargetIds} -> trafficMirrorTargetIds) (\s@DescribeTrafficMirrorTargets' {} a -> s {trafficMirrorTargetIds = a} :: DescribeTrafficMirrorTargets) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeTrafficMirrorTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorTargetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorTargetsResponse_trafficMirrorTargets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTrafficMirrorTargets_nextToken
          Lens..~ rs
          Lens.^? describeTrafficMirrorTargetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTrafficMirrorTargets where
  type
    AWSResponse DescribeTrafficMirrorTargets =
      DescribeTrafficMirrorTargetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorTargetsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "trafficMirrorTargetSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrafficMirrorTargets
  where
  hashWithSalt _salt DescribeTrafficMirrorTargets' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` trafficMirrorTargetIds

instance Prelude.NFData DescribeTrafficMirrorTargets where
  rnf DescribeTrafficMirrorTargets' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficMirrorTargetIds

instance Data.ToHeaders DescribeTrafficMirrorTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTrafficMirrorTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrafficMirrorTargets where
  toQuery DescribeTrafficMirrorTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTrafficMirrorTargets" ::
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
          ( Data.toQueryList "TrafficMirrorTargetId"
              Prelude.<$> trafficMirrorTargetIds
          )
      ]

-- | /See:/ 'newDescribeTrafficMirrorTargetsResponse' smart constructor.
data DescribeTrafficMirrorTargetsResponse = DescribeTrafficMirrorTargetsResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more Traffic Mirror targets.
    trafficMirrorTargets :: Prelude.Maybe [TrafficMirrorTarget],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficMirrorTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrafficMirrorTargetsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
--
-- 'trafficMirrorTargets', 'describeTrafficMirrorTargetsResponse_trafficMirrorTargets' - Information about one or more Traffic Mirror targets.
--
-- 'httpStatus', 'describeTrafficMirrorTargetsResponse_httpStatus' - The response's http status code.
newDescribeTrafficMirrorTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrafficMirrorTargetsResponse
newDescribeTrafficMirrorTargetsResponse pHttpStatus_ =
  DescribeTrafficMirrorTargetsResponse'
    { nextToken =
        Prelude.Nothing,
      trafficMirrorTargets =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
describeTrafficMirrorTargetsResponse_nextToken :: Lens.Lens' DescribeTrafficMirrorTargetsResponse (Prelude.Maybe Prelude.Text)
describeTrafficMirrorTargetsResponse_nextToken = Lens.lens (\DescribeTrafficMirrorTargetsResponse' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorTargetsResponse' {} a -> s {nextToken = a} :: DescribeTrafficMirrorTargetsResponse)

-- | Information about one or more Traffic Mirror targets.
describeTrafficMirrorTargetsResponse_trafficMirrorTargets :: Lens.Lens' DescribeTrafficMirrorTargetsResponse (Prelude.Maybe [TrafficMirrorTarget])
describeTrafficMirrorTargetsResponse_trafficMirrorTargets = Lens.lens (\DescribeTrafficMirrorTargetsResponse' {trafficMirrorTargets} -> trafficMirrorTargets) (\s@DescribeTrafficMirrorTargetsResponse' {} a -> s {trafficMirrorTargets = a} :: DescribeTrafficMirrorTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrafficMirrorTargetsResponse_httpStatus :: Lens.Lens' DescribeTrafficMirrorTargetsResponse Prelude.Int
describeTrafficMirrorTargetsResponse_httpStatus = Lens.lens (\DescribeTrafficMirrorTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficMirrorTargetsResponse' {} a -> s {httpStatus = a} :: DescribeTrafficMirrorTargetsResponse)

instance
  Prelude.NFData
    DescribeTrafficMirrorTargetsResponse
  where
  rnf DescribeTrafficMirrorTargetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficMirrorTargets
      `Prelude.seq` Prelude.rnf httpStatus
