{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeMovingAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC platform, or that are being restored to the EC2-Classic platform. This request does not return information about any other Elastic IP addresses in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeMovingAddresses
  ( -- * Creating a request
    DescribeMovingAddresses (..),
    mkDescribeMovingAddresses,

    -- ** Request lenses
    dmaFilters,
    dmaPublicIPs,
    dmaNextToken,
    dmaDryRun,
    dmaMaxResults,

    -- * Destructuring the response
    DescribeMovingAddressesResponse (..),
    mkDescribeMovingAddressesResponse,

    -- ** Response lenses
    dmarsMovingAddressStatuses,
    dmarsNextToken,
    dmarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMovingAddresses' smart constructor.
data DescribeMovingAddresses = DescribeMovingAddresses'
  { filters ::
      Lude.Maybe [Filter],
    publicIPs :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMovingAddresses' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @moving-status@ - The status of the Elastic IP address (@MovingToVpc@ | @RestoringToClassic@ ).
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value outside of this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
-- * 'nextToken' - The token for the next page of results.
-- * 'publicIPs' - One or more Elastic IP addresses.
mkDescribeMovingAddresses ::
  DescribeMovingAddresses
mkDescribeMovingAddresses =
  DescribeMovingAddresses'
    { filters = Lude.Nothing,
      publicIPs = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @moving-status@ - The status of the Elastic IP address (@MovingToVpc@ | @RestoringToClassic@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaFilters :: Lens.Lens' DescribeMovingAddresses (Lude.Maybe [Filter])
dmaFilters = Lens.lens (filters :: DescribeMovingAddresses -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeMovingAddresses)
{-# DEPRECATED dmaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more Elastic IP addresses.
--
-- /Note:/ Consider using 'publicIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaPublicIPs :: Lens.Lens' DescribeMovingAddresses (Lude.Maybe [Lude.Text])
dmaPublicIPs = Lens.lens (publicIPs :: DescribeMovingAddresses -> Lude.Maybe [Lude.Text]) (\s a -> s {publicIPs = a} :: DescribeMovingAddresses)
{-# DEPRECATED dmaPublicIPs "Use generic-lens or generic-optics with 'publicIPs' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaNextToken :: Lens.Lens' DescribeMovingAddresses (Lude.Maybe Lude.Text)
dmaNextToken = Lens.lens (nextToken :: DescribeMovingAddresses -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMovingAddresses)
{-# DEPRECATED dmaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaDryRun :: Lens.Lens' DescribeMovingAddresses (Lude.Maybe Lude.Bool)
dmaDryRun = Lens.lens (dryRun :: DescribeMovingAddresses -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeMovingAddresses)
{-# DEPRECATED dmaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value outside of this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmaMaxResults :: Lens.Lens' DescribeMovingAddresses (Lude.Maybe Lude.Natural)
dmaMaxResults = Lens.lens (maxResults :: DescribeMovingAddresses -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMovingAddresses)
{-# DEPRECATED dmaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeMovingAddresses where
  page rq rs
    | Page.stop (rs Lens.^. dmarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmarsMovingAddressStatuses) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmaNextToken Lens..~ rs Lens.^. dmarsNextToken

instance Lude.AWSRequest DescribeMovingAddresses where
  type Rs DescribeMovingAddresses = DescribeMovingAddressesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeMovingAddressesResponse'
            Lude.<$> ( x Lude..@? "movingAddressStatusSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMovingAddresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeMovingAddresses where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMovingAddresses where
  toQuery DescribeMovingAddresses' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeMovingAddresses" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "PublicIp" Lude.<$> publicIPs),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeMovingAddressesResponse' smart constructor.
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
  { movingAddressStatuses ::
      Lude.Maybe
        [MovingAddressStatus],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeMovingAddressesResponse' with the minimum fields required to make a request.
--
-- * 'movingAddressStatuses' - The status for each Elastic IP address.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeMovingAddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMovingAddressesResponse
mkDescribeMovingAddressesResponse pResponseStatus_ =
  DescribeMovingAddressesResponse'
    { movingAddressStatuses =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status for each Elastic IP address.
--
-- /Note:/ Consider using 'movingAddressStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarsMovingAddressStatuses :: Lens.Lens' DescribeMovingAddressesResponse (Lude.Maybe [MovingAddressStatus])
dmarsMovingAddressStatuses = Lens.lens (movingAddressStatuses :: DescribeMovingAddressesResponse -> Lude.Maybe [MovingAddressStatus]) (\s a -> s {movingAddressStatuses = a} :: DescribeMovingAddressesResponse)
{-# DEPRECATED dmarsMovingAddressStatuses "Use generic-lens or generic-optics with 'movingAddressStatuses' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarsNextToken :: Lens.Lens' DescribeMovingAddressesResponse (Lude.Maybe Lude.Text)
dmarsNextToken = Lens.lens (nextToken :: DescribeMovingAddressesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMovingAddressesResponse)
{-# DEPRECATED dmarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarsResponseStatus :: Lens.Lens' DescribeMovingAddressesResponse Lude.Int
dmarsResponseStatus = Lens.lens (responseStatus :: DescribeMovingAddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMovingAddressesResponse)
{-# DEPRECATED dmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
