{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of fleet resources for this AWS account. You can filter the result set to find only those fleets that are deployed with a specific build or script. Use the pagination parameters to retrieve results in sequential pages.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
-- __Related operations__
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListFleets
  ( -- * Creating a request
    ListFleets (..),
    mkListFleets,

    -- ** Request lenses
    lfBuildId,
    lfNextToken,
    lfScriptId,
    lfLimit,

    -- * Destructuring the response
    ListFleetsResponse (..),
    mkListFleetsResponse,

    -- ** Response lenses
    lfrsNextToken,
    lfrsFleetIds,
    lfrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkListFleets' smart constructor.
data ListFleets = ListFleets'
  { buildId :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    scriptId :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFleets' with the minimum fields required to make a request.
--
-- * 'buildId' - A unique identifier for a build to return fleets for. Use this parameter to return only fleets using a specified build. Use either the build ID or ARN value. To retrieve all fleets, do not include either a BuildId and ScriptID parameter.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'scriptId' - A unique identifier for a Realtime script to return fleets for. Use this parameter to return only fleets using a specified script. Use either the script ID or ARN value. To retrieve all fleets, leave this parameter empty.
mkListFleets ::
  ListFleets
mkListFleets =
  ListFleets'
    { buildId = Lude.Nothing,
      nextToken = Lude.Nothing,
      scriptId = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | A unique identifier for a build to return fleets for. Use this parameter to return only fleets using a specified build. Use either the build ID or ARN value. To retrieve all fleets, do not include either a BuildId and ScriptID parameter.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfBuildId :: Lens.Lens' ListFleets (Lude.Maybe Lude.Text)
lfBuildId = Lens.lens (buildId :: ListFleets -> Lude.Maybe Lude.Text) (\s a -> s {buildId = a} :: ListFleets)
{-# DEPRECATED lfBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFleets (Lude.Maybe Lude.Text)
lfNextToken = Lens.lens (nextToken :: ListFleets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFleets)
{-# DEPRECATED lfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A unique identifier for a Realtime script to return fleets for. Use this parameter to return only fleets using a specified script. Use either the script ID or ARN value. To retrieve all fleets, leave this parameter empty.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfScriptId :: Lens.Lens' ListFleets (Lude.Maybe Lude.Text)
lfScriptId = Lens.lens (scriptId :: ListFleets -> Lude.Maybe Lude.Text) (\s a -> s {scriptId = a} :: ListFleets)
{-# DEPRECATED lfScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLimit :: Lens.Lens' ListFleets (Lude.Maybe Lude.Natural)
lfLimit = Lens.lens (limit :: ListFleets -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListFleets)
{-# DEPRECATED lfLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListFleets where
  page rq rs
    | Page.stop (rs Lens.^. lfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfrsFleetIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfNextToken Lens..~ rs Lens.^. lfrsNextToken

instance Lude.AWSRequest ListFleets where
  type Rs ListFleets = ListFleetsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FleetIds")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFleets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("GameLift.ListFleets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BuildId" Lude..=) Lude.<$> buildId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ScriptId" Lude..=) Lude.<$> scriptId,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListFleets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListFleets where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    fleetIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFleetsResponse' with the minimum fields required to make a request.
--
-- * 'fleetIds' - Set of fleet IDs matching the list request. You can retrieve additional information about all returned fleets by passing this result set to a call to 'DescribeFleetAttributes' , 'DescribeFleetCapacity' , or 'DescribeFleetUtilization' .
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkListFleetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFleetsResponse
mkListFleetsResponse pResponseStatus_ =
  ListFleetsResponse'
    { nextToken = Lude.Nothing,
      fleetIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsNextToken :: Lens.Lens' ListFleetsResponse (Lude.Maybe Lude.Text)
lfrsNextToken = Lens.lens (nextToken :: ListFleetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFleetsResponse)
{-# DEPRECATED lfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set of fleet IDs matching the list request. You can retrieve additional information about all returned fleets by passing this result set to a call to 'DescribeFleetAttributes' , 'DescribeFleetCapacity' , or 'DescribeFleetUtilization' .
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFleetIds :: Lens.Lens' ListFleetsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lfrsFleetIds = Lens.lens (fleetIds :: ListFleetsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {fleetIds = a} :: ListFleetsResponse)
{-# DEPRECATED lfrsFleetIds "Use generic-lens or generic-optics with 'fleetIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsResponseStatus :: Lens.Lens' ListFleetsResponse Lude.Int
lfrsResponseStatus = Lens.lens (responseStatus :: ListFleetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFleetsResponse)
{-# DEPRECATED lfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
