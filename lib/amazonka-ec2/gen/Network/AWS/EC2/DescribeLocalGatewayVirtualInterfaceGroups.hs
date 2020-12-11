{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interface groups.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaceGroups
  ( -- * Creating a request
    DescribeLocalGatewayVirtualInterfaceGroups (..),
    mkDescribeLocalGatewayVirtualInterfaceGroups,

    -- ** Request lenses
    dlgvigFilters,
    dlgvigNextToken,
    dlgvigLocalGatewayVirtualInterfaceGroupIds,
    dlgvigDryRun,
    dlgvigMaxResults,

    -- * Destructuring the response
    DescribeLocalGatewayVirtualInterfaceGroupsResponse (..),
    mkDescribeLocalGatewayVirtualInterfaceGroupsResponse,

    -- ** Response lenses
    dlgvigrsNextToken,
    dlgvigrsLocalGatewayVirtualInterfaceGroups,
    dlgvigrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfaceGroups' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroups = DescribeLocalGatewayVirtualInterfaceGroups'
  { filters ::
      Lude.Maybe
        [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    localGatewayVirtualInterfaceGroupIds ::
      Lude.Maybe
        [Lude.Text],
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    maxResults ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocalGatewayVirtualInterfaceGroups' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-virtual-interface-id@ - The ID of the virtual interface.
--
--
--     * @local-gateway-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
-- * 'localGatewayVirtualInterfaceGroupIds' - The IDs of the virtual interface groups.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeLocalGatewayVirtualInterfaceGroups ::
  DescribeLocalGatewayVirtualInterfaceGroups
mkDescribeLocalGatewayVirtualInterfaceGroups =
  DescribeLocalGatewayVirtualInterfaceGroups'
    { filters =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      localGatewayVirtualInterfaceGroupIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-virtual-interface-id@ - The ID of the virtual interface.
--
--
--     * @local-gateway-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigFilters :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Lude.Maybe [Filter])
dlgvigFilters = Lens.lens (filters :: DescribeLocalGatewayVirtualInterfaceGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLocalGatewayVirtualInterfaceGroups)
{-# DEPRECATED dlgvigFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Lude.Maybe Lude.Text)
dlgvigNextToken = Lens.lens (nextToken :: DescribeLocalGatewayVirtualInterfaceGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroups)
{-# DEPRECATED dlgvigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the virtual interface groups.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigLocalGatewayVirtualInterfaceGroupIds :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Lude.Maybe [Lude.Text])
dlgvigLocalGatewayVirtualInterfaceGroupIds = Lens.lens (localGatewayVirtualInterfaceGroupIds :: DescribeLocalGatewayVirtualInterfaceGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayVirtualInterfaceGroupIds = a} :: DescribeLocalGatewayVirtualInterfaceGroups)
{-# DEPRECATED dlgvigLocalGatewayVirtualInterfaceGroupIds "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigDryRun :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Lude.Maybe Lude.Bool)
dlgvigDryRun = Lens.lens (dryRun :: DescribeLocalGatewayVirtualInterfaceGroups -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLocalGatewayVirtualInterfaceGroups)
{-# DEPRECATED dlgvigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigMaxResults :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroups (Lude.Maybe Lude.Natural)
dlgvigMaxResults = Lens.lens (maxResults :: DescribeLocalGatewayVirtualInterfaceGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLocalGatewayVirtualInterfaceGroups)
{-# DEPRECATED dlgvigMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeLocalGatewayVirtualInterfaceGroups where
  page rq rs
    | Page.stop (rs Lens.^. dlgvigrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlgvigrsLocalGatewayVirtualInterfaceGroups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgvigNextToken Lens..~ rs Lens.^. dlgvigrsNextToken

instance Lude.AWSRequest DescribeLocalGatewayVirtualInterfaceGroups where
  type
    Rs DescribeLocalGatewayVirtualInterfaceGroups =
      DescribeLocalGatewayVirtualInterfaceGroupsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfaceGroupsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "localGatewayVirtualInterfaceGroupSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLocalGatewayVirtualInterfaceGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLocalGatewayVirtualInterfaceGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLocalGatewayVirtualInterfaceGroups where
  toQuery DescribeLocalGatewayVirtualInterfaceGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLocalGatewayVirtualInterfaceGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "LocalGatewayVirtualInterfaceGroupId"
              Lude.<$> localGatewayVirtualInterfaceGroupIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeLocalGatewayVirtualInterfaceGroupsResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfaceGroupsResponse = DescribeLocalGatewayVirtualInterfaceGroupsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    localGatewayVirtualInterfaceGroups ::
      Lude.Maybe
        [LocalGatewayVirtualInterfaceGroup],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeLocalGatewayVirtualInterfaceGroupsResponse' with the minimum fields required to make a request.
--
-- * 'localGatewayVirtualInterfaceGroups' - The virtual interface groups.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLocalGatewayVirtualInterfaceGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocalGatewayVirtualInterfaceGroupsResponse
mkDescribeLocalGatewayVirtualInterfaceGroupsResponse
  pResponseStatus_ =
    DescribeLocalGatewayVirtualInterfaceGroupsResponse'
      { nextToken =
          Lude.Nothing,
        localGatewayVirtualInterfaceGroups =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigrsNextToken :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Lude.Maybe Lude.Text)
dlgvigrsNextToken = Lens.lens (nextToken :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)
{-# DEPRECATED dlgvigrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The virtual interface groups.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigrsLocalGatewayVirtualInterfaceGroups :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse (Lude.Maybe [LocalGatewayVirtualInterfaceGroup])
dlgvigrsLocalGatewayVirtualInterfaceGroups = Lens.lens (localGatewayVirtualInterfaceGroups :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> Lude.Maybe [LocalGatewayVirtualInterfaceGroup]) (\s a -> s {localGatewayVirtualInterfaceGroups = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)
{-# DEPRECATED dlgvigrsLocalGatewayVirtualInterfaceGroups "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgvigrsResponseStatus :: Lens.Lens' DescribeLocalGatewayVirtualInterfaceGroupsResponse Lude.Int
dlgvigrsResponseStatus = Lens.lens (responseStatus :: DescribeLocalGatewayVirtualInterfaceGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocalGatewayVirtualInterfaceGroupsResponse)
{-# DEPRECATED dlgvigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
