{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Spot Fleet requests.
--
-- Spot Fleet requests are deleted 48 hours after they are canceled and their instances are terminated.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetRequests
  ( -- * Creating a request
    DescribeSpotFleetRequests (..),
    mkDescribeSpotFleetRequests,

    -- ** Request lenses
    dsfrSpotFleetRequestIds,
    dsfrNextToken,
    dsfrDryRun,
    dsfrMaxResults,

    -- * Destructuring the response
    DescribeSpotFleetRequestsResponse (..),
    mkDescribeSpotFleetRequestsResponse,

    -- ** Response lenses
    dsfrrsNextToken,
    dsfrrsSpotFleetRequestConfigs,
    dsfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeSpotFleetRequests.
--
-- /See:/ 'mkDescribeSpotFleetRequests' smart constructor.
data DescribeSpotFleetRequests = DescribeSpotFleetRequests'
  { spotFleetRequestIds ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotFleetRequests' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
-- * 'spotFleetRequestIds' - The IDs of the Spot Fleet requests.
mkDescribeSpotFleetRequests ::
  DescribeSpotFleetRequests
mkDescribeSpotFleetRequests =
  DescribeSpotFleetRequests'
    { spotFleetRequestIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IDs of the Spot Fleet requests.
--
-- /Note:/ Consider using 'spotFleetRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrSpotFleetRequestIds :: Lens.Lens' DescribeSpotFleetRequests (Lude.Maybe [Lude.Text])
dsfrSpotFleetRequestIds = Lens.lens (spotFleetRequestIds :: DescribeSpotFleetRequests -> Lude.Maybe [Lude.Text]) (\s a -> s {spotFleetRequestIds = a} :: DescribeSpotFleetRequests)
{-# DEPRECATED dsfrSpotFleetRequestIds "Use generic-lens or generic-optics with 'spotFleetRequestIds' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrNextToken :: Lens.Lens' DescribeSpotFleetRequests (Lude.Maybe Lude.Text)
dsfrNextToken = Lens.lens (nextToken :: DescribeSpotFleetRequests -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotFleetRequests)
{-# DEPRECATED dsfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrDryRun :: Lens.Lens' DescribeSpotFleetRequests (Lude.Maybe Lude.Bool)
dsfrDryRun = Lens.lens (dryRun :: DescribeSpotFleetRequests -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSpotFleetRequests)
{-# DEPRECATED dsfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrMaxResults :: Lens.Lens' DescribeSpotFleetRequests (Lude.Maybe Lude.Int)
dsfrMaxResults = Lens.lens (maxResults :: DescribeSpotFleetRequests -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeSpotFleetRequests)
{-# DEPRECATED dsfrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSpotFleetRequests where
  page rq rs
    | Page.stop (rs Lens.^. dsfrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsfrrsSpotFleetRequestConfigs) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsfrNextToken Lens..~ rs Lens.^. dsfrrsNextToken

instance Lude.AWSRequest DescribeSpotFleetRequests where
  type
    Rs DescribeSpotFleetRequests =
      DescribeSpotFleetRequestsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSpotFleetRequestsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "spotFleetRequestConfigSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSpotFleetRequests where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSpotFleetRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSpotFleetRequests where
  toQuery DescribeSpotFleetRequests' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSpotFleetRequests" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "SpotFleetRequestId"
              Lude.<$> spotFleetRequestIds
          ),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeSpotFleetRequests.
--
-- /See:/ 'mkDescribeSpotFleetRequestsResponse' smart constructor.
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    spotFleetRequestConfigs ::
      Lude.Maybe
        [SpotFleetRequestConfig],
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

-- | Creates a value of 'DescribeSpotFleetRequestsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'spotFleetRequestConfigs' - Information about the configuration of your Spot Fleet.
mkDescribeSpotFleetRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSpotFleetRequestsResponse
mkDescribeSpotFleetRequestsResponse pResponseStatus_ =
  DescribeSpotFleetRequestsResponse'
    { nextToken = Lude.Nothing,
      spotFleetRequestConfigs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrsNextToken :: Lens.Lens' DescribeSpotFleetRequestsResponse (Lude.Maybe Lude.Text)
dsfrrsNextToken = Lens.lens (nextToken :: DescribeSpotFleetRequestsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotFleetRequestsResponse)
{-# DEPRECATED dsfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the configuration of your Spot Fleet.
--
-- /Note:/ Consider using 'spotFleetRequestConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrsSpotFleetRequestConfigs :: Lens.Lens' DescribeSpotFleetRequestsResponse (Lude.Maybe [SpotFleetRequestConfig])
dsfrrsSpotFleetRequestConfigs = Lens.lens (spotFleetRequestConfigs :: DescribeSpotFleetRequestsResponse -> Lude.Maybe [SpotFleetRequestConfig]) (\s a -> s {spotFleetRequestConfigs = a} :: DescribeSpotFleetRequestsResponse)
{-# DEPRECATED dsfrrsSpotFleetRequestConfigs "Use generic-lens or generic-optics with 'spotFleetRequestConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrsResponseStatus :: Lens.Lens' DescribeSpotFleetRequestsResponse Lude.Int
dsfrrsResponseStatus = Lens.lens (responseStatus :: DescribeSpotFleetRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSpotFleetRequestsResponse)
{-# DEPRECATED dsfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
