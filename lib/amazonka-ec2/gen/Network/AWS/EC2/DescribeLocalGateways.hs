{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateways. By default, all local gateways are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGateways
  ( -- * Creating a request
    DescribeLocalGateways (..),
    mkDescribeLocalGateways,

    -- ** Request lenses
    dlgFilters,
    dlgNextToken,
    dlgLocalGatewayIds,
    dlgDryRun,
    dlgMaxResults,

    -- * Destructuring the response
    DescribeLocalGatewaysResponse (..),
    mkDescribeLocalGatewaysResponse,

    -- ** Response lenses
    dlgrsLocalGateways,
    dlgrsNextToken,
    dlgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocalGateways' smart constructor.
data DescribeLocalGateways = DescribeLocalGateways'
  { -- | One or more filters.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more filters.
    --
    --
    --     * @local-gateway-id@ - The ID of a local gateway.
    --
    --
    --     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
    --
    --
    --     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
    --
    --
    --     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
    --
    --
    --     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
    --
    --
    --     * @state@ - The state of the association.
    localGatewayIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocalGateways' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
-- * 'nextToken' - The token for the next page of results.
-- * 'localGatewayIds' - One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
--
--     * @state@ - The state of the association.
--
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeLocalGateways ::
  DescribeLocalGateways
mkDescribeLocalGateways =
  DescribeLocalGateways'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      localGatewayIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgFilters :: Lens.Lens' DescribeLocalGateways (Lude.Maybe [Filter])
dlgFilters = Lens.lens (filters :: DescribeLocalGateways -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLocalGateways)
{-# DEPRECATED dlgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgNextToken :: Lens.Lens' DescribeLocalGateways (Lude.Maybe Lude.Text)
dlgNextToken = Lens.lens (nextToken :: DescribeLocalGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGateways)
{-# DEPRECATED dlgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
--
--     * @state@ - The state of the association.
--
--
--
-- /Note:/ Consider using 'localGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLocalGatewayIds :: Lens.Lens' DescribeLocalGateways (Lude.Maybe [Lude.Text])
dlgLocalGatewayIds = Lens.lens (localGatewayIds :: DescribeLocalGateways -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayIds = a} :: DescribeLocalGateways)
{-# DEPRECATED dlgLocalGatewayIds "Use generic-lens or generic-optics with 'localGatewayIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgDryRun :: Lens.Lens' DescribeLocalGateways (Lude.Maybe Lude.Bool)
dlgDryRun = Lens.lens (dryRun :: DescribeLocalGateways -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLocalGateways)
{-# DEPRECATED dlgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgMaxResults :: Lens.Lens' DescribeLocalGateways (Lude.Maybe Lude.Natural)
dlgMaxResults = Lens.lens (maxResults :: DescribeLocalGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLocalGateways)
{-# DEPRECATED dlgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeLocalGateways where
  page rq rs
    | Page.stop (rs Lens.^. dlgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlgrsLocalGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgNextToken Lens..~ rs Lens.^. dlgrsNextToken

instance Lude.AWSRequest DescribeLocalGateways where
  type Rs DescribeLocalGateways = DescribeLocalGatewaysResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLocalGatewaysResponse'
            Lude.<$> ( x Lude..@? "localGatewaySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLocalGateways where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLocalGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLocalGateways where
  toQuery DescribeLocalGateways' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeLocalGateways" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "LocalGatewayId" Lude.<$> localGatewayIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeLocalGatewaysResponse' smart constructor.
data DescribeLocalGatewaysResponse = DescribeLocalGatewaysResponse'
  { -- | Information about the local gateways.
    localGateways :: Lude.Maybe [LocalGateway],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocalGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'localGateways' - Information about the local gateways.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLocalGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocalGatewaysResponse
mkDescribeLocalGatewaysResponse pResponseStatus_ =
  DescribeLocalGatewaysResponse'
    { localGateways = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the local gateways.
--
-- /Note:/ Consider using 'localGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrsLocalGateways :: Lens.Lens' DescribeLocalGatewaysResponse (Lude.Maybe [LocalGateway])
dlgrsLocalGateways = Lens.lens (localGateways :: DescribeLocalGatewaysResponse -> Lude.Maybe [LocalGateway]) (\s a -> s {localGateways = a} :: DescribeLocalGatewaysResponse)
{-# DEPRECATED dlgrsLocalGateways "Use generic-lens or generic-optics with 'localGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrsNextToken :: Lens.Lens' DescribeLocalGatewaysResponse (Lude.Maybe Lude.Text)
dlgrsNextToken = Lens.lens (nextToken :: DescribeLocalGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewaysResponse)
{-# DEPRECATED dlgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrsResponseStatus :: Lens.Lens' DescribeLocalGatewaysResponse Lude.Int
dlgrsResponseStatus = Lens.lens (responseStatus :: DescribeLocalGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocalGatewaysResponse)
{-# DEPRECATED dlgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
