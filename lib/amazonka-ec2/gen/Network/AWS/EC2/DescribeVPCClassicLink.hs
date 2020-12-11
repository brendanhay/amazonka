{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink status of one or more VPCs.
module Network.AWS.EC2.DescribeVPCClassicLink
  ( -- * Creating a request
    DescribeVPCClassicLink (..),
    mkDescribeVPCClassicLink,

    -- ** Request lenses
    dvclFilters,
    dvclVPCIds,
    dvclDryRun,

    -- * Destructuring the response
    DescribeVPCClassicLinkResponse (..),
    mkDescribeVPCClassicLinkResponse,

    -- ** Response lenses
    dvclrsVPCs,
    dvclrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCClassicLink' smart constructor.
data DescribeVPCClassicLink = DescribeVPCClassicLink'
  { filters ::
      Lude.Maybe [Filter],
    vpcIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCClassicLink' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @is-classic-link-enabled@ - Whether the VPC is enabled for ClassicLink (@true@ | @false@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'vpcIds' - One or more VPCs for which you want to describe the ClassicLink status.
mkDescribeVPCClassicLink ::
  DescribeVPCClassicLink
mkDescribeVPCClassicLink =
  DescribeVPCClassicLink'
    { filters = Lude.Nothing,
      vpcIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @is-classic-link-enabled@ - Whether the VPC is enabled for ClassicLink (@true@ | @false@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclFilters :: Lens.Lens' DescribeVPCClassicLink (Lude.Maybe [Filter])
dvclFilters = Lens.lens (filters :: DescribeVPCClassicLink -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCClassicLink)
{-# DEPRECATED dvclFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more VPCs for which you want to describe the ClassicLink status.
--
-- /Note:/ Consider using 'vpcIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclVPCIds :: Lens.Lens' DescribeVPCClassicLink (Lude.Maybe [Lude.Text])
dvclVPCIds = Lens.lens (vpcIds :: DescribeVPCClassicLink -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcIds = a} :: DescribeVPCClassicLink)
{-# DEPRECATED dvclVPCIds "Use generic-lens or generic-optics with 'vpcIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclDryRun :: Lens.Lens' DescribeVPCClassicLink (Lude.Maybe Lude.Bool)
dvclDryRun = Lens.lens (dryRun :: DescribeVPCClassicLink -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCClassicLink)
{-# DEPRECATED dvclDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeVPCClassicLink where
  type Rs DescribeVPCClassicLink = DescribeVPCClassicLinkResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCClassicLinkResponse'
            Lude.<$> ( x Lude..@? "vpcSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCClassicLink where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCClassicLink where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCClassicLink where
  toQuery DescribeVPCClassicLink' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVpcClassicLink" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "VpcId" Lude.<$> vpcIds),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeVPCClassicLinkResponse' smart constructor.
data DescribeVPCClassicLinkResponse = DescribeVPCClassicLinkResponse'
  { vpcs ::
      Lude.Maybe [VPCClassicLink],
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

-- | Creates a value of 'DescribeVPCClassicLinkResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpcs' - The ClassicLink status of one or more VPCs.
mkDescribeVPCClassicLinkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCClassicLinkResponse
mkDescribeVPCClassicLinkResponse pResponseStatus_ =
  DescribeVPCClassicLinkResponse'
    { vpcs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ClassicLink status of one or more VPCs.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclrsVPCs :: Lens.Lens' DescribeVPCClassicLinkResponse (Lude.Maybe [VPCClassicLink])
dvclrsVPCs = Lens.lens (vpcs :: DescribeVPCClassicLinkResponse -> Lude.Maybe [VPCClassicLink]) (\s a -> s {vpcs = a} :: DescribeVPCClassicLinkResponse)
{-# DEPRECATED dvclrsVPCs "Use generic-lens or generic-optics with 'vpcs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclrsResponseStatus :: Lens.Lens' DescribeVPCClassicLinkResponse Lude.Int
dvclrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCClassicLinkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCClassicLinkResponse)
{-# DEPRECATED dvclrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
