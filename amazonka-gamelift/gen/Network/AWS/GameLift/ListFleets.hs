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
-- Module      : Network.AWS.GameLift.ListFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of fleet resources for this AWS account. You can
-- filter the result set to find only those fleets that are deployed with a
-- specific build or script. Use the pagination parameters to retrieve
-- results in sequential pages.
--
-- Fleet resources are not listed in a particular order.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListFleets
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

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newListFleets' smart constructor.
data ListFleets = ListFleets'
  { -- | Token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | A unique identifier for a build to return fleets for. Use this parameter
    -- to return only fleets using a specified build. Use either the build ID
    -- or ARN value. To retrieve all fleets, do not include either a BuildId
    -- and ScriptID parameter.
    buildId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | A unique identifier for a Realtime script to return fleets for. Use this
    -- parameter to return only fleets using a specified script. Use either the
    -- script ID or ARN value. To retrieve all fleets, leave this parameter
    -- empty.
    scriptId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleets_nextToken' - Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'buildId', 'listFleets_buildId' - A unique identifier for a build to return fleets for. Use this parameter
-- to return only fleets using a specified build. Use either the build ID
-- or ARN value. To retrieve all fleets, do not include either a BuildId
-- and ScriptID parameter.
--
-- 'limit', 'listFleets_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'scriptId', 'listFleets_scriptId' - A unique identifier for a Realtime script to return fleets for. Use this
-- parameter to return only fleets using a specified script. Use either the
-- script ID or ARN value. To retrieve all fleets, leave this parameter
-- empty.
newListFleets ::
  ListFleets
newListFleets =
  ListFleets'
    { nextToken = Core.Nothing,
      buildId = Core.Nothing,
      limit = Core.Nothing,
      scriptId = Core.Nothing
    }

-- | Token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listFleets_nextToken :: Lens.Lens' ListFleets (Core.Maybe Core.Text)
listFleets_nextToken = Lens.lens (\ListFleets' {nextToken} -> nextToken) (\s@ListFleets' {} a -> s {nextToken = a} :: ListFleets)

-- | A unique identifier for a build to return fleets for. Use this parameter
-- to return only fleets using a specified build. Use either the build ID
-- or ARN value. To retrieve all fleets, do not include either a BuildId
-- and ScriptID parameter.
listFleets_buildId :: Lens.Lens' ListFleets (Core.Maybe Core.Text)
listFleets_buildId = Lens.lens (\ListFleets' {buildId} -> buildId) (\s@ListFleets' {} a -> s {buildId = a} :: ListFleets)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
listFleets_limit :: Lens.Lens' ListFleets (Core.Maybe Core.Natural)
listFleets_limit = Lens.lens (\ListFleets' {limit} -> limit) (\s@ListFleets' {} a -> s {limit = a} :: ListFleets)

-- | A unique identifier for a Realtime script to return fleets for. Use this
-- parameter to return only fleets using a specified script. Use either the
-- script ID or ARN value. To retrieve all fleets, leave this parameter
-- empty.
listFleets_scriptId :: Lens.Lens' ListFleets (Core.Maybe Core.Text)
listFleets_scriptId = Lens.lens (\ListFleets' {scriptId} -> scriptId) (\s@ListFleets' {} a -> s {scriptId = a} :: ListFleets)

instance Core.AWSPager ListFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_fleetIds Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFleets_nextToken
          Lens..~ rs
          Lens.^? listFleetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListFleets where
  type AWSResponse ListFleets = ListFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "FleetIds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListFleets

instance Core.NFData ListFleets

instance Core.ToHeaders ListFleets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ListFleets" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("BuildId" Core..=) Core.<$> buildId,
            ("Limit" Core..=) Core.<$> limit,
            ("ScriptId" Core..=) Core.<$> scriptId
          ]
      )

instance Core.ToPath ListFleets where
  toPath = Core.const "/"

instance Core.ToQuery ListFleets where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { -- | Token that indicates where to resume retrieving results on the next call
    -- to this operation. If no token is returned, these results represent the
    -- end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Set of fleet IDs matching the list request. You can retrieve additional
    -- information about all returned fleets by passing this result set to a
    -- call to DescribeFleetAttributes, DescribeFleetCapacity, or
    -- DescribeFleetUtilization.
    fleetIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleetsResponse_nextToken' - Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
--
-- 'fleetIds', 'listFleetsResponse_fleetIds' - Set of fleet IDs matching the list request. You can retrieve additional
-- information about all returned fleets by passing this result set to a
-- call to DescribeFleetAttributes, DescribeFleetCapacity, or
-- DescribeFleetUtilization.
--
-- 'httpStatus', 'listFleetsResponse_httpStatus' - The response's http status code.
newListFleetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFleetsResponse
newListFleetsResponse pHttpStatus_ =
  ListFleetsResponse'
    { nextToken = Core.Nothing,
      fleetIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call
-- to this operation. If no token is returned, these results represent the
-- end of the list.
listFleetsResponse_nextToken :: Lens.Lens' ListFleetsResponse (Core.Maybe Core.Text)
listFleetsResponse_nextToken = Lens.lens (\ListFleetsResponse' {nextToken} -> nextToken) (\s@ListFleetsResponse' {} a -> s {nextToken = a} :: ListFleetsResponse)

-- | Set of fleet IDs matching the list request. You can retrieve additional
-- information about all returned fleets by passing this result set to a
-- call to DescribeFleetAttributes, DescribeFleetCapacity, or
-- DescribeFleetUtilization.
listFleetsResponse_fleetIds :: Lens.Lens' ListFleetsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listFleetsResponse_fleetIds = Lens.lens (\ListFleetsResponse' {fleetIds} -> fleetIds) (\s@ListFleetsResponse' {} a -> s {fleetIds = a} :: ListFleetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFleetsResponse_httpStatus :: Lens.Lens' ListFleetsResponse Core.Int
listFleetsResponse_httpStatus = Lens.lens (\ListFleetsResponse' {httpStatus} -> httpStatus) (\s@ListFleetsResponse' {} a -> s {httpStatus = a} :: ListFleetsResponse)

instance Core.NFData ListFleetsResponse
