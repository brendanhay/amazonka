{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeByoipCidrs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IP address ranges that were specified in calls to 'ProvisionByoipCidr' .
--
-- To describe the address pools that were created when you provisioned the address ranges, use 'DescribePublicIpv4Pools' or 'DescribeIpv6Pools' .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeByoipCidrs
  ( -- * Creating a request
    DescribeByoipCidrs (..),
    mkDescribeByoipCidrs,

    -- ** Request lenses
    dbcsNextToken,
    dbcsDryRun,
    dbcsMaxResults,

    -- * Destructuring the response
    DescribeByoipCidrsResponse (..),
    mkDescribeByoipCidrsResponse,

    -- ** Response lenses
    dbcsrsNextToken,
    dbcsrsByoipCidrs,
    dbcsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeByoipCidrs' smart constructor.
data DescribeByoipCidrs = DescribeByoipCidrs'
  { -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeByoipCidrs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeByoipCidrs ::
  -- | 'maxResults'
  Lude.Natural ->
  DescribeByoipCidrs
mkDescribeByoipCidrs pMaxResults_ =
  DescribeByoipCidrs'
    { nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = pMaxResults_
    }

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsNextToken :: Lens.Lens' DescribeByoipCidrs (Lude.Maybe Lude.Text)
dbcsNextToken = Lens.lens (nextToken :: DescribeByoipCidrs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeByoipCidrs)
{-# DEPRECATED dbcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsDryRun :: Lens.Lens' DescribeByoipCidrs (Lude.Maybe Lude.Bool)
dbcsDryRun = Lens.lens (dryRun :: DescribeByoipCidrs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeByoipCidrs)
{-# DEPRECATED dbcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsMaxResults :: Lens.Lens' DescribeByoipCidrs Lude.Natural
dbcsMaxResults = Lens.lens (maxResults :: DescribeByoipCidrs -> Lude.Natural) (\s a -> s {maxResults = a} :: DescribeByoipCidrs)
{-# DEPRECATED dbcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeByoipCidrs where
  page rq rs
    | Page.stop (rs Lens.^. dbcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbcsrsByoipCidrs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbcsNextToken Lens..~ rs Lens.^. dbcsrsNextToken

instance Lude.AWSRequest DescribeByoipCidrs where
  type Rs DescribeByoipCidrs = DescribeByoipCidrsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeByoipCidrsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "byoipCidrSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeByoipCidrs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeByoipCidrs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeByoipCidrs where
  toQuery DescribeByoipCidrs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeByoipCidrs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeByoipCidrsResponse' smart constructor.
data DescribeByoipCidrsResponse = DescribeByoipCidrsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about your address ranges.
    byoipCidrs :: Lude.Maybe [ByoipCidr],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeByoipCidrsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'byoipCidrs' - Information about your address ranges.
-- * 'responseStatus' - The response status code.
mkDescribeByoipCidrsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeByoipCidrsResponse
mkDescribeByoipCidrsResponse pResponseStatus_ =
  DescribeByoipCidrsResponse'
    { nextToken = Lude.Nothing,
      byoipCidrs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsrsNextToken :: Lens.Lens' DescribeByoipCidrsResponse (Lude.Maybe Lude.Text)
dbcsrsNextToken = Lens.lens (nextToken :: DescribeByoipCidrsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeByoipCidrsResponse)
{-# DEPRECATED dbcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about your address ranges.
--
-- /Note:/ Consider using 'byoipCidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsrsByoipCidrs :: Lens.Lens' DescribeByoipCidrsResponse (Lude.Maybe [ByoipCidr])
dbcsrsByoipCidrs = Lens.lens (byoipCidrs :: DescribeByoipCidrsResponse -> Lude.Maybe [ByoipCidr]) (\s a -> s {byoipCidrs = a} :: DescribeByoipCidrsResponse)
{-# DEPRECATED dbcsrsByoipCidrs "Use generic-lens or generic-optics with 'byoipCidrs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsrsResponseStatus :: Lens.Lens' DescribeByoipCidrsResponse Lude.Int
dbcsrsResponseStatus = Lens.lens (responseStatus :: DescribeByoipCidrsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeByoipCidrsResponse)
{-# DEPRECATED dbcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
