{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetAssociatedIPv6PoolCidrs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the IPv6 CIDR block associations for a specified IPv6 address pool.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetAssociatedIPv6PoolCidrs
  ( -- * Creating a request
    GetAssociatedIPv6PoolCidrs (..),
    mkGetAssociatedIPv6PoolCidrs,

    -- ** Request lenses
    gaipcNextToken,
    gaipcDryRun,
    gaipcMaxResults,
    gaipcPoolId,

    -- * Destructuring the response
    GetAssociatedIPv6PoolCidrsResponse (..),
    mkGetAssociatedIPv6PoolCidrsResponse,

    -- ** Response lenses
    gaipcrsIPv6CidrAssociations,
    gaipcrsNextToken,
    gaipcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAssociatedIPv6PoolCidrs' smart constructor.
data GetAssociatedIPv6PoolCidrs = GetAssociatedIPv6PoolCidrs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    poolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssociatedIPv6PoolCidrs' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'poolId' - The ID of the IPv6 address pool.
mkGetAssociatedIPv6PoolCidrs ::
  -- | 'poolId'
  Lude.Text ->
  GetAssociatedIPv6PoolCidrs
mkGetAssociatedIPv6PoolCidrs pPoolId_ =
  GetAssociatedIPv6PoolCidrs'
    { nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      poolId = pPoolId_
    }

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcNextToken :: Lens.Lens' GetAssociatedIPv6PoolCidrs (Lude.Maybe Lude.Text)
gaipcNextToken = Lens.lens (nextToken :: GetAssociatedIPv6PoolCidrs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAssociatedIPv6PoolCidrs)
{-# DEPRECATED gaipcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcDryRun :: Lens.Lens' GetAssociatedIPv6PoolCidrs (Lude.Maybe Lude.Bool)
gaipcDryRun = Lens.lens (dryRun :: GetAssociatedIPv6PoolCidrs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetAssociatedIPv6PoolCidrs)
{-# DEPRECATED gaipcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcMaxResults :: Lens.Lens' GetAssociatedIPv6PoolCidrs (Lude.Maybe Lude.Natural)
gaipcMaxResults = Lens.lens (maxResults :: GetAssociatedIPv6PoolCidrs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetAssociatedIPv6PoolCidrs)
{-# DEPRECATED gaipcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the IPv6 address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcPoolId :: Lens.Lens' GetAssociatedIPv6PoolCidrs Lude.Text
gaipcPoolId = Lens.lens (poolId :: GetAssociatedIPv6PoolCidrs -> Lude.Text) (\s a -> s {poolId = a} :: GetAssociatedIPv6PoolCidrs)
{-# DEPRECATED gaipcPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

instance Page.AWSPager GetAssociatedIPv6PoolCidrs where
  page rq rs
    | Page.stop (rs Lens.^. gaipcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gaipcrsIPv6CidrAssociations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gaipcNextToken Lens..~ rs Lens.^. gaipcrsNextToken

instance Lude.AWSRequest GetAssociatedIPv6PoolCidrs where
  type
    Rs GetAssociatedIPv6PoolCidrs =
      GetAssociatedIPv6PoolCidrsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetAssociatedIPv6PoolCidrsResponse'
            Lude.<$> ( x Lude..@? "ipv6CidrAssociationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAssociatedIPv6PoolCidrs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAssociatedIPv6PoolCidrs where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAssociatedIPv6PoolCidrs where
  toQuery GetAssociatedIPv6PoolCidrs' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetAssociatedIpv6PoolCidrs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "PoolId" Lude.=: poolId
      ]

-- | /See:/ 'mkGetAssociatedIPv6PoolCidrsResponse' smart constructor.
data GetAssociatedIPv6PoolCidrsResponse = GetAssociatedIPv6PoolCidrsResponse'
  { ipv6CidrAssociations ::
      Lude.Maybe
        [IPv6CidrAssociation],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetAssociatedIPv6PoolCidrsResponse' with the minimum fields required to make a request.
--
-- * 'ipv6CidrAssociations' - Information about the IPv6 CIDR block associations.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetAssociatedIPv6PoolCidrsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAssociatedIPv6PoolCidrsResponse
mkGetAssociatedIPv6PoolCidrsResponse pResponseStatus_ =
  GetAssociatedIPv6PoolCidrsResponse'
    { ipv6CidrAssociations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IPv6 CIDR block associations.
--
-- /Note:/ Consider using 'ipv6CidrAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcrsIPv6CidrAssociations :: Lens.Lens' GetAssociatedIPv6PoolCidrsResponse (Lude.Maybe [IPv6CidrAssociation])
gaipcrsIPv6CidrAssociations = Lens.lens (ipv6CidrAssociations :: GetAssociatedIPv6PoolCidrsResponse -> Lude.Maybe [IPv6CidrAssociation]) (\s a -> s {ipv6CidrAssociations = a} :: GetAssociatedIPv6PoolCidrsResponse)
{-# DEPRECATED gaipcrsIPv6CidrAssociations "Use generic-lens or generic-optics with 'ipv6CidrAssociations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcrsNextToken :: Lens.Lens' GetAssociatedIPv6PoolCidrsResponse (Lude.Maybe Lude.Text)
gaipcrsNextToken = Lens.lens (nextToken :: GetAssociatedIPv6PoolCidrsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetAssociatedIPv6PoolCidrsResponse)
{-# DEPRECATED gaipcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaipcrsResponseStatus :: Lens.Lens' GetAssociatedIPv6PoolCidrsResponse Lude.Int
gaipcrsResponseStatus = Lens.lens (responseStatus :: GetAssociatedIPv6PoolCidrsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAssociatedIPv6PoolCidrsResponse)
{-# DEPRECATED gaipcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
