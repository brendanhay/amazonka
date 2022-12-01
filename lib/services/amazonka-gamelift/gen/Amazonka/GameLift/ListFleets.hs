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
-- Module      : Amazonka.GameLift.ListFleets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of fleet resources in an Amazon Web Services
-- Region. You can call this operation to get fleets in a previously
-- selected default Region (see
-- <https://docs.aws.amazon.com/credref/latest/refdocs/setting-global-region.html>or
-- specify a Region in your request. You can filter the result set to find
-- only those fleets that are deployed with a specific build or script. For
-- fleets that have multiple locations, this operation retrieves fleets
-- based on their home Region only.
--
-- This operation can be used in the following ways:
--
-- -   To get a list of all fleets in a Region, don\'t provide a build or
--     script identifier.
--
-- -   To get a list of all fleets where a specific custom game build is
--     deployed, provide the build ID.
--
-- -   To get a list of all Realtime Servers fleets with a specific
--     configuration script, provide the script ID.
--
-- Use the pagination parameters to retrieve results as a set of sequential
-- pages.
--
-- If successful, a list of fleet IDs that match the request parameters is
-- returned. A NextToken value is also returned if there are more result
-- pages to retrieve.
--
-- Fleet resources are not listed in a particular order.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
--
-- __Related actions__
--
-- CreateFleet | UpdateFleetCapacity | PutScalingPolicy |
-- DescribeEC2InstanceLimits | DescribeFleetAttributes |
-- DescribeFleetLocationAttributes | UpdateFleetAttributes |
-- StopFleetActions | DeleteFleet |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- This operation returns paginated results.
module Amazonka.GameLift.ListFleets
  ( -- * Creating a Request
    ListFleets (..),
    newListFleets,

    -- * Request Lenses
    listFleets_nextToken,
    listFleets_buildId,
    listFleets_limit,
    listFleets_scriptId,

    -- * Destructuring the Response
    ListFleetsResponse (..),
    newListFleetsResponse,

    -- * Response Lenses
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetIds,
    listFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newListFleets' smart constructor.
data ListFleets = ListFleets'
  { -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the build to request fleets for. Use this
    -- parameter to return only fleets using a specified build. Use either the
    -- build ID or ARN value.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the Realtime script to request fleets for. Use
    -- this parameter to return only fleets using a specified script. Use
    -- either the script ID or ARN value.
    scriptId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleets_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'buildId', 'listFleets_buildId' - A unique identifier for the build to request fleets for. Use this
-- parameter to return only fleets using a specified build. Use either the
-- build ID or ARN value.
--
-- 'limit', 'listFleets_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'scriptId', 'listFleets_scriptId' - A unique identifier for the Realtime script to request fleets for. Use
-- this parameter to return only fleets using a specified script. Use
-- either the script ID or ARN value.
newListFleets ::
  ListFleets
newListFleets =
  ListFleets'
    { nextToken = Prelude.Nothing,
      buildId = Prelude.Nothing,
      limit = Prelude.Nothing,
      scriptId = Prelude.Nothing
    }

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listFleets_nextToken :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_nextToken = Lens.lens (\ListFleets' {nextToken} -> nextToken) (\s@ListFleets' {} a -> s {nextToken = a} :: ListFleets)

-- | A unique identifier for the build to request fleets for. Use this
-- parameter to return only fleets using a specified build. Use either the
-- build ID or ARN value.
listFleets_buildId :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_buildId = Lens.lens (\ListFleets' {buildId} -> buildId) (\s@ListFleets' {} a -> s {buildId = a} :: ListFleets)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listFleets_limit :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Natural)
listFleets_limit = Lens.lens (\ListFleets' {limit} -> limit) (\s@ListFleets' {} a -> s {limit = a} :: ListFleets)

-- | A unique identifier for the Realtime script to request fleets for. Use
-- this parameter to return only fleets using a specified script. Use
-- either the script ID or ARN value.
listFleets_scriptId :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_scriptId = Lens.lens (\ListFleets' {scriptId} -> scriptId) (\s@ListFleets' {} a -> s {scriptId = a} :: ListFleets)

instance Core.AWSPager ListFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_fleetIds Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFleets_nextToken
          Lens..~ rs
          Lens.^? listFleetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFleets where
  type AWSResponse ListFleets = ListFleetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "FleetIds")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFleets where
  hashWithSalt _salt ListFleets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` scriptId

instance Prelude.NFData ListFleets where
  rnf ListFleets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf scriptId

instance Core.ToHeaders ListFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListFleets" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("BuildId" Core..=) Prelude.<$> buildId,
            ("Limit" Core..=) Prelude.<$> limit,
            ("ScriptId" Core..=) Prelude.<$> scriptId
          ]
      )

instance Core.ToPath ListFleets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFleets where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A set of fleet IDs that match the list request. You can retrieve
    -- additional information about all returned fleets by passing this result
    -- set to a DescribeFleetAttributes, DescribeFleetCapacity, or
    -- DescribeFleetUtilization call.
    fleetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleetsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'fleetIds', 'listFleetsResponse_fleetIds' - A set of fleet IDs that match the list request. You can retrieve
-- additional information about all returned fleets by passing this result
-- set to a DescribeFleetAttributes, DescribeFleetCapacity, or
-- DescribeFleetUtilization call.
--
-- 'httpStatus', 'listFleetsResponse_httpStatus' - The response's http status code.
newListFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFleetsResponse
newListFleetsResponse pHttpStatus_ =
  ListFleetsResponse'
    { nextToken = Prelude.Nothing,
      fleetIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
listFleetsResponse_nextToken :: Lens.Lens' ListFleetsResponse (Prelude.Maybe Prelude.Text)
listFleetsResponse_nextToken = Lens.lens (\ListFleetsResponse' {nextToken} -> nextToken) (\s@ListFleetsResponse' {} a -> s {nextToken = a} :: ListFleetsResponse)

-- | A set of fleet IDs that match the list request. You can retrieve
-- additional information about all returned fleets by passing this result
-- set to a DescribeFleetAttributes, DescribeFleetCapacity, or
-- DescribeFleetUtilization call.
listFleetsResponse_fleetIds :: Lens.Lens' ListFleetsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listFleetsResponse_fleetIds = Lens.lens (\ListFleetsResponse' {fleetIds} -> fleetIds) (\s@ListFleetsResponse' {} a -> s {fleetIds = a} :: ListFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFleetsResponse_httpStatus :: Lens.Lens' ListFleetsResponse Prelude.Int
listFleetsResponse_httpStatus = Lens.lens (\ListFleetsResponse' {httpStatus} -> httpStatus) (\s@ListFleetsResponse' {} a -> s {httpStatus = a} :: ListFleetsResponse)

instance Prelude.NFData ListFleetsResponse where
  rnf ListFleetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetIds
      `Prelude.seq` Prelude.rnf httpStatus
