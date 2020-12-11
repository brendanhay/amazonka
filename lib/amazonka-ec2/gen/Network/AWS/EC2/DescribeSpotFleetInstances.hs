{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified Spot Fleet.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetInstances
  ( -- * Creating a request
    DescribeSpotFleetInstances (..),
    mkDescribeSpotFleetInstances,

    -- ** Request lenses
    dsfiNextToken,
    dsfiDryRun,
    dsfiMaxResults,
    dsfiSpotFleetRequestId,

    -- * Destructuring the response
    DescribeSpotFleetInstancesResponse (..),
    mkDescribeSpotFleetInstancesResponse,

    -- ** Response lenses
    dsfirsNextToken,
    dsfirsSpotFleetRequestId,
    dsfirsActiveInstances,
    dsfirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeSpotFleetInstances.
--
-- /See:/ 'mkDescribeSpotFleetInstances' smart constructor.
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
  { nextToken ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    spotFleetRequestId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotFleetInstances' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
mkDescribeSpotFleetInstances ::
  -- | 'spotFleetRequestId'
  Lude.Text ->
  DescribeSpotFleetInstances
mkDescribeSpotFleetInstances pSpotFleetRequestId_ =
  DescribeSpotFleetInstances'
    { nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      spotFleetRequestId = pSpotFleetRequestId_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiNextToken :: Lens.Lens' DescribeSpotFleetInstances (Lude.Maybe Lude.Text)
dsfiNextToken = Lens.lens (nextToken :: DescribeSpotFleetInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotFleetInstances)
{-# DEPRECATED dsfiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiDryRun :: Lens.Lens' DescribeSpotFleetInstances (Lude.Maybe Lude.Bool)
dsfiDryRun = Lens.lens (dryRun :: DescribeSpotFleetInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSpotFleetInstances)
{-# DEPRECATED dsfiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiMaxResults :: Lens.Lens' DescribeSpotFleetInstances (Lude.Maybe Lude.Natural)
dsfiMaxResults = Lens.lens (maxResults :: DescribeSpotFleetInstances -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeSpotFleetInstances)
{-# DEPRECATED dsfiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstances Lude.Text
dsfiSpotFleetRequestId = Lens.lens (spotFleetRequestId :: DescribeSpotFleetInstances -> Lude.Text) (\s a -> s {spotFleetRequestId = a} :: DescribeSpotFleetInstances)
{-# DEPRECATED dsfiSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

instance Page.AWSPager DescribeSpotFleetInstances where
  page rq rs
    | Page.stop (rs Lens.^. dsfirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsfirsActiveInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsfiNextToken Lens..~ rs Lens.^. dsfirsNextToken

instance Lude.AWSRequest DescribeSpotFleetInstances where
  type
    Rs DescribeSpotFleetInstances =
      DescribeSpotFleetInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSpotFleetInstancesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> (x Lude..@? "spotFleetRequestId")
            Lude.<*> ( x Lude..@? "activeInstanceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSpotFleetInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSpotFleetInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSpotFleetInstances where
  toQuery DescribeSpotFleetInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeSpotFleetInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "SpotFleetRequestId" Lude.=: spotFleetRequestId
      ]

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'mkDescribeSpotFleetInstancesResponse' smart constructor.
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    spotFleetRequestId ::
      Lude.Maybe Lude.Text,
    activeInstances ::
      Lude.Maybe
        [ActiveInstance],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotFleetInstancesResponse' with the minimum fields required to make a request.
--
-- * 'activeInstances' - The running instances. This list is refreshed periodically and might be out of date.
-- * 'nextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
mkDescribeSpotFleetInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSpotFleetInstancesResponse
mkDescribeSpotFleetInstancesResponse pResponseStatus_ =
  DescribeSpotFleetInstancesResponse'
    { nextToken = Lude.Nothing,
      spotFleetRequestId = Lude.Nothing,
      activeInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirsNextToken :: Lens.Lens' DescribeSpotFleetInstancesResponse (Lude.Maybe Lude.Text)
dsfirsNextToken = Lens.lens (nextToken :: DescribeSpotFleetInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotFleetInstancesResponse)
{-# DEPRECATED dsfirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirsSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstancesResponse (Lude.Maybe Lude.Text)
dsfirsSpotFleetRequestId = Lens.lens (spotFleetRequestId :: DescribeSpotFleetInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {spotFleetRequestId = a} :: DescribeSpotFleetInstancesResponse)
{-# DEPRECATED dsfirsSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The running instances. This list is refreshed periodically and might be out of date.
--
-- /Note:/ Consider using 'activeInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirsActiveInstances :: Lens.Lens' DescribeSpotFleetInstancesResponse (Lude.Maybe [ActiveInstance])
dsfirsActiveInstances = Lens.lens (activeInstances :: DescribeSpotFleetInstancesResponse -> Lude.Maybe [ActiveInstance]) (\s a -> s {activeInstances = a} :: DescribeSpotFleetInstancesResponse)
{-# DEPRECATED dsfirsActiveInstances "Use generic-lens or generic-optics with 'activeInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirsResponseStatus :: Lens.Lens' DescribeSpotFleetInstancesResponse Lude.Int
dsfirsResponseStatus = Lens.lens (responseStatus :: DescribeSpotFleetInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSpotFleetInstancesResponse)
{-# DEPRECATED dsfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
