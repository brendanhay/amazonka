{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFleetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified EC2 Fleet.
module Network.AWS.EC2.DescribeFleetInstances
  ( -- * Creating a request
    DescribeFleetInstances (..),
    mkDescribeFleetInstances,

    -- ** Request lenses
    dfifFilters,
    dfifNextToken,
    dfifFleetId,
    dfifDryRun,
    dfifMaxResults,

    -- * Destructuring the response
    DescribeFleetInstancesResponse (..),
    mkDescribeFleetInstancesResponse,

    -- ** Response lenses
    dfisrsNextToken,
    dfisrsFleetId,
    dfisrsActiveInstances,
    dfisrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFleetInstances' smart constructor.
data DescribeFleetInstances = DescribeFleetInstances'
  { -- | The filters.
    --
    --
    --     * @instance-type@ - The instance type.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the EC2 Fleet.
    fleetId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetInstances' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @instance-type@ - The instance type.
--
--
-- * 'nextToken' - The token for the next set of results.
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkDescribeFleetInstances ::
  -- | 'fleetId'
  Lude.Text ->
  DescribeFleetInstances
mkDescribeFleetInstances pFleetId_ =
  DescribeFleetInstances'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      fleetId = pFleetId_,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @instance-type@ - The instance type.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifFilters :: Lens.Lens' DescribeFleetInstances (Lude.Maybe [Filter])
dfifFilters = Lens.lens (filters :: DescribeFleetInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeFleetInstances)
{-# DEPRECATED dfifFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifNextToken :: Lens.Lens' DescribeFleetInstances (Lude.Maybe Lude.Text)
dfifNextToken = Lens.lens (nextToken :: DescribeFleetInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetInstances)
{-# DEPRECATED dfifNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifFleetId :: Lens.Lens' DescribeFleetInstances Lude.Text
dfifFleetId = Lens.lens (fleetId :: DescribeFleetInstances -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeFleetInstances)
{-# DEPRECATED dfifFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifDryRun :: Lens.Lens' DescribeFleetInstances (Lude.Maybe Lude.Bool)
dfifDryRun = Lens.lens (dryRun :: DescribeFleetInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFleetInstances)
{-# DEPRECATED dfifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfifMaxResults :: Lens.Lens' DescribeFleetInstances (Lude.Maybe Lude.Int)
dfifMaxResults = Lens.lens (maxResults :: DescribeFleetInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeFleetInstances)
{-# DEPRECATED dfifMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeFleetInstances where
  type Rs DescribeFleetInstances = DescribeFleetInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFleetInstancesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> (x Lude..@? "fleetId")
            Lude.<*> ( x Lude..@? "activeInstanceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFleetInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetInstances where
  toQuery DescribeFleetInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeFleetInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "FleetId" Lude.=: fleetId,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeFleetInstancesResponse' smart constructor.
data DescribeFleetInstancesResponse = DescribeFleetInstancesResponse'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the EC2 Fleet.
    fleetId :: Lude.Maybe Lude.Text,
    -- | The running instances. This list is refreshed periodically and might be out of date.
    activeInstances :: Lude.Maybe [ActiveInstance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'activeInstances' - The running instances. This list is refreshed periodically and might be out of date.
-- * 'responseStatus' - The response status code.
mkDescribeFleetInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetInstancesResponse
mkDescribeFleetInstancesResponse pResponseStatus_ =
  DescribeFleetInstancesResponse'
    { nextToken = Lude.Nothing,
      fleetId = Lude.Nothing,
      activeInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisrsNextToken :: Lens.Lens' DescribeFleetInstancesResponse (Lude.Maybe Lude.Text)
dfisrsNextToken = Lens.lens (nextToken :: DescribeFleetInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetInstancesResponse)
{-# DEPRECATED dfisrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisrsFleetId :: Lens.Lens' DescribeFleetInstancesResponse (Lude.Maybe Lude.Text)
dfisrsFleetId = Lens.lens (fleetId :: DescribeFleetInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DescribeFleetInstancesResponse)
{-# DEPRECATED dfisrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The running instances. This list is refreshed periodically and might be out of date.
--
-- /Note:/ Consider using 'activeInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisrsActiveInstances :: Lens.Lens' DescribeFleetInstancesResponse (Lude.Maybe [ActiveInstance])
dfisrsActiveInstances = Lens.lens (activeInstances :: DescribeFleetInstancesResponse -> Lude.Maybe [ActiveInstance]) (\s a -> s {activeInstances = a} :: DescribeFleetInstancesResponse)
{-# DEPRECATED dfisrsActiveInstances "Use generic-lens or generic-optics with 'activeInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfisrsResponseStatus :: Lens.Lens' DescribeFleetInstancesResponse Lude.Int
dfisrsResponseStatus = Lens.lens (responseStatus :: DescribeFleetInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetInstancesResponse)
{-# DEPRECATED dfisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
