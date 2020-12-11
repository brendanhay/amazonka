{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a fleet's instances, including instance IDs. Use this operation to get details on all instances in the fleet or get details on one specific instance.
--
-- To get a specific instance, specify fleet ID and instance ID. To get all instances in a fleet, specify a fleet ID only. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, an 'Instance' object is returned for each result.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
-- __Related operations__
--
--     * 'DescribeInstances'
--
--
--     * 'GetInstanceAccess'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeInstances
  ( -- * Creating a request
    DescribeInstances (..),
    mkDescribeInstances,

    -- ** Request lenses
    diInstanceId,
    diNextToken,
    diLimit,
    diFleetId,

    -- * Destructuring the response
    DescribeInstancesResponse (..),
    mkDescribeInstancesResponse,

    -- ** Response lenses
    dirsNextToken,
    dirsInstances,
    dirsResponseStatus,
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
-- /See:/ 'mkDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { instanceId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    fleetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstances' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet to retrieve instance information for. You can use either the fleet ID or ARN value.
-- * 'instanceId' - A unique identifier for an instance to retrieve. Specify an instance ID or leave blank to retrieve all instances in the fleet.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
-- * 'nextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
mkDescribeInstances ::
  -- | 'fleetId'
  Lude.Text ->
  DescribeInstances
mkDescribeInstances pFleetId_ =
  DescribeInstances'
    { instanceId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      fleetId = pFleetId_
    }

-- | A unique identifier for an instance to retrieve. Specify an instance ID or leave blank to retrieve all instances in the fleet.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DescribeInstances (Lude.Maybe Lude.Text)
diInstanceId = Lens.lens (instanceId :: DescribeInstances -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstances)
{-# DEPRECATED diInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNextToken :: Lens.Lens' DescribeInstances (Lude.Maybe Lude.Text)
diNextToken = Lens.lens (nextToken :: DescribeInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstances)
{-# DEPRECATED diNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLimit :: Lens.Lens' DescribeInstances (Lude.Maybe Lude.Natural)
diLimit = Lens.lens (limit :: DescribeInstances -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeInstances)
{-# DEPRECATED diLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A unique identifier for a fleet to retrieve instance information for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diFleetId :: Lens.Lens' DescribeInstances Lude.Text
diFleetId = Lens.lens (fleetId :: DescribeInstances -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeInstances)
{-# DEPRECATED diFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Page.AWSPager DescribeInstances where
  page rq rs
    | Page.stop (rs Lens.^. dirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dirsInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& diNextToken Lens..~ rs Lens.^. dirsNextToken

instance Lude.AWSRequest DescribeInstances where
  type Rs DescribeInstances = DescribeInstancesResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstancesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Instances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstances where
  toJSON DescribeInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath DescribeInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstances where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    instances :: Lude.Maybe [Instance],
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

-- | Creates a value of 'DescribeInstancesResponse' with the minimum fields required to make a request.
--
-- * 'instances' - A collection of objects containing properties for each instance returned.
-- * 'nextToken' - Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkDescribeInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstancesResponse
mkDescribeInstancesResponse pResponseStatus_ =
  DescribeInstancesResponse'
    { nextToken = Lude.Nothing,
      instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsNextToken :: Lens.Lens' DescribeInstancesResponse (Lude.Maybe Lude.Text)
dirsNextToken = Lens.lens (nextToken :: DescribeInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancesResponse)
{-# DEPRECATED dirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A collection of objects containing properties for each instance returned.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsInstances :: Lens.Lens' DescribeInstancesResponse (Lude.Maybe [Instance])
dirsInstances = Lens.lens (instances :: DescribeInstancesResponse -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: DescribeInstancesResponse)
{-# DEPRECATED dirsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DescribeInstancesResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DescribeInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstancesResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
