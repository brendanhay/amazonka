{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCClassicLinkDNSSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink DNS support status of one or more VPCs. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCClassicLinkDNSSupport
  ( -- * Creating a request
    DescribeVPCClassicLinkDNSSupport (..),
    mkDescribeVPCClassicLinkDNSSupport,

    -- ** Request lenses
    dvcldsNextToken,
    dvcldsVPCIds,
    dvcldsMaxResults,

    -- * Destructuring the response
    DescribeVPCClassicLinkDNSSupportResponse (..),
    mkDescribeVPCClassicLinkDNSSupportResponse,

    -- ** Response lenses
    dvpccldnssrsVPCs,
    dvpccldnssrsNextToken,
    dvpccldnssrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCClassicLinkDNSSupport' smart constructor.
data DescribeVPCClassicLinkDNSSupport = DescribeVPCClassicLinkDNSSupport'
  { nextToken ::
      Lude.Maybe Lude.Text,
    vpcIds ::
      Lude.Maybe [Lude.Text],
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'vpcIds' - One or more VPC IDs.
mkDescribeVPCClassicLinkDNSSupport ::
  DescribeVPCClassicLinkDNSSupport
mkDescribeVPCClassicLinkDNSSupport =
  DescribeVPCClassicLinkDNSSupport'
    { nextToken = Lude.Nothing,
      vpcIds = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsNextToken :: Lens.Lens' DescribeVPCClassicLinkDNSSupport (Lude.Maybe Lude.Text)
dvcldsNextToken = Lens.lens (nextToken :: DescribeVPCClassicLinkDNSSupport -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCClassicLinkDNSSupport)
{-# DEPRECATED dvcldsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more VPC IDs.
--
-- /Note:/ Consider using 'vpcIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsVPCIds :: Lens.Lens' DescribeVPCClassicLinkDNSSupport (Lude.Maybe [Lude.Text])
dvcldsVPCIds = Lens.lens (vpcIds :: DescribeVPCClassicLinkDNSSupport -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcIds = a} :: DescribeVPCClassicLinkDNSSupport)
{-# DEPRECATED dvcldsVPCIds "Use generic-lens or generic-optics with 'vpcIds' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsMaxResults :: Lens.Lens' DescribeVPCClassicLinkDNSSupport (Lude.Maybe Lude.Natural)
dvcldsMaxResults = Lens.lens (maxResults :: DescribeVPCClassicLinkDNSSupport -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeVPCClassicLinkDNSSupport)
{-# DEPRECATED dvcldsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCClassicLinkDNSSupport where
  page rq rs
    | Page.stop (rs Lens.^. dvpccldnssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvpccldnssrsVPCs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvcldsNextToken Lens..~ rs Lens.^. dvpccldnssrsNextToken

instance Lude.AWSRequest DescribeVPCClassicLinkDNSSupport where
  type
    Rs DescribeVPCClassicLinkDNSSupport =
      DescribeVPCClassicLinkDNSSupportResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCClassicLinkDNSSupportResponse'
            Lude.<$> ( x Lude..@? "vpcs" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCClassicLinkDNSSupport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCClassicLinkDNSSupport where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCClassicLinkDNSSupport where
  toQuery DescribeVPCClassicLinkDNSSupport' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcClassicLinkDnsSupport" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "VpcIds" Lude.<$> vpcIds),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCClassicLinkDNSSupportResponse' smart constructor.
data DescribeVPCClassicLinkDNSSupportResponse = DescribeVPCClassicLinkDNSSupportResponse'
  { vpcs ::
      Lude.Maybe
        [ClassicLinkDNSSupport],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeVPCClassicLinkDNSSupportResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'vpcs' - Information about the ClassicLink DNS support status of the VPCs.
mkDescribeVPCClassicLinkDNSSupportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCClassicLinkDNSSupportResponse
mkDescribeVPCClassicLinkDNSSupportResponse pResponseStatus_ =
  DescribeVPCClassicLinkDNSSupportResponse'
    { vpcs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the ClassicLink DNS support status of the VPCs.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpccldnssrsVPCs :: Lens.Lens' DescribeVPCClassicLinkDNSSupportResponse (Lude.Maybe [ClassicLinkDNSSupport])
dvpccldnssrsVPCs = Lens.lens (vpcs :: DescribeVPCClassicLinkDNSSupportResponse -> Lude.Maybe [ClassicLinkDNSSupport]) (\s a -> s {vpcs = a} :: DescribeVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED dvpccldnssrsVPCs "Use generic-lens or generic-optics with 'vpcs' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpccldnssrsNextToken :: Lens.Lens' DescribeVPCClassicLinkDNSSupportResponse (Lude.Maybe Lude.Text)
dvpccldnssrsNextToken = Lens.lens (nextToken :: DescribeVPCClassicLinkDNSSupportResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED dvpccldnssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpccldnssrsResponseStatus :: Lens.Lens' DescribeVPCClassicLinkDNSSupportResponse Lude.Int
dvpccldnssrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCClassicLinkDNSSupportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCClassicLinkDNSSupportResponse)
{-# DEPRECATED dvpccldnssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
