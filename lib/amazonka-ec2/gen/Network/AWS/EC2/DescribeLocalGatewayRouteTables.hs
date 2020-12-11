{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateway route tables. By default, all local gateway route tables are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTables
  ( -- * Creating a request
    DescribeLocalGatewayRouteTables (..),
    mkDescribeLocalGatewayRouteTables,

    -- ** Request lenses
    dlgrtFilters,
    dlgrtNextToken,
    dlgrtLocalGatewayRouteTableIds,
    dlgrtDryRun,
    dlgrtMaxResults,

    -- * Destructuring the response
    DescribeLocalGatewayRouteTablesResponse (..),
    mkDescribeLocalGatewayRouteTablesResponse,

    -- ** Response lenses
    dlgrtrsNextToken,
    dlgrtrsLocalGatewayRouteTables,
    dlgrtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocalGatewayRouteTables' smart constructor.
data DescribeLocalGatewayRouteTables = DescribeLocalGatewayRouteTables'
  { filters ::
      Lude.Maybe [Filter],
    nextToken ::
      Lude.Maybe Lude.Text,
    localGatewayRouteTableIds ::
      Lude.Maybe [Lude.Text],
    dryRun ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DescribeLocalGatewayRouteTables' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of a local gateway route table.
--
--
--     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
--
--     * @state@ - The state of the local gateway route table.
--
--
-- * 'localGatewayRouteTableIds' - The IDs of the local gateway route tables.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeLocalGatewayRouteTables ::
  DescribeLocalGatewayRouteTables
mkDescribeLocalGatewayRouteTables =
  DescribeLocalGatewayRouteTables'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      localGatewayRouteTableIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of a local gateway route table.
--
--
--     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
--
--     * @state@ - The state of the local gateway route table.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtFilters :: Lens.Lens' DescribeLocalGatewayRouteTables (Lude.Maybe [Filter])
dlgrtFilters = Lens.lens (filters :: DescribeLocalGatewayRouteTables -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLocalGatewayRouteTables)
{-# DEPRECATED dlgrtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtNextToken :: Lens.Lens' DescribeLocalGatewayRouteTables (Lude.Maybe Lude.Text)
dlgrtNextToken = Lens.lens (nextToken :: DescribeLocalGatewayRouteTables -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayRouteTables)
{-# DEPRECATED dlgrtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the local gateway route tables.
--
-- /Note:/ Consider using 'localGatewayRouteTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtLocalGatewayRouteTableIds :: Lens.Lens' DescribeLocalGatewayRouteTables (Lude.Maybe [Lude.Text])
dlgrtLocalGatewayRouteTableIds = Lens.lens (localGatewayRouteTableIds :: DescribeLocalGatewayRouteTables -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayRouteTableIds = a} :: DescribeLocalGatewayRouteTables)
{-# DEPRECATED dlgrtLocalGatewayRouteTableIds "Use generic-lens or generic-optics with 'localGatewayRouteTableIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtDryRun :: Lens.Lens' DescribeLocalGatewayRouteTables (Lude.Maybe Lude.Bool)
dlgrtDryRun = Lens.lens (dryRun :: DescribeLocalGatewayRouteTables -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLocalGatewayRouteTables)
{-# DEPRECATED dlgrtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtMaxResults :: Lens.Lens' DescribeLocalGatewayRouteTables (Lude.Maybe Lude.Natural)
dlgrtMaxResults = Lens.lens (maxResults :: DescribeLocalGatewayRouteTables -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLocalGatewayRouteTables)
{-# DEPRECATED dlgrtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeLocalGatewayRouteTables where
  page rq rs
    | Page.stop (rs Lens.^. dlgrtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlgrtrsLocalGatewayRouteTables) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgrtNextToken Lens..~ rs Lens.^. dlgrtrsNextToken

instance Lude.AWSRequest DescribeLocalGatewayRouteTables where
  type
    Rs DescribeLocalGatewayRouteTables =
      DescribeLocalGatewayRouteTablesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTablesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "localGatewayRouteTableSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLocalGatewayRouteTables where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLocalGatewayRouteTables where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLocalGatewayRouteTables where
  toQuery DescribeLocalGatewayRouteTables' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLocalGatewayRouteTables" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "LocalGatewayRouteTableId"
              Lude.<$> localGatewayRouteTableIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeLocalGatewayRouteTablesResponse' smart constructor.
data DescribeLocalGatewayRouteTablesResponse = DescribeLocalGatewayRouteTablesResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    localGatewayRouteTables ::
      Lude.Maybe
        [LocalGatewayRouteTable],
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

-- | Creates a value of 'DescribeLocalGatewayRouteTablesResponse' with the minimum fields required to make a request.
--
-- * 'localGatewayRouteTables' - Information about the local gateway route tables.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLocalGatewayRouteTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocalGatewayRouteTablesResponse
mkDescribeLocalGatewayRouteTablesResponse pResponseStatus_ =
  DescribeLocalGatewayRouteTablesResponse'
    { nextToken =
        Lude.Nothing,
      localGatewayRouteTables = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtrsNextToken :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse (Lude.Maybe Lude.Text)
dlgrtrsNextToken = Lens.lens (nextToken :: DescribeLocalGatewayRouteTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayRouteTablesResponse)
{-# DEPRECATED dlgrtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the local gateway route tables.
--
-- /Note:/ Consider using 'localGatewayRouteTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtrsLocalGatewayRouteTables :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse (Lude.Maybe [LocalGatewayRouteTable])
dlgrtrsLocalGatewayRouteTables = Lens.lens (localGatewayRouteTables :: DescribeLocalGatewayRouteTablesResponse -> Lude.Maybe [LocalGatewayRouteTable]) (\s a -> s {localGatewayRouteTables = a} :: DescribeLocalGatewayRouteTablesResponse)
{-# DEPRECATED dlgrtrsLocalGatewayRouteTables "Use generic-lens or generic-optics with 'localGatewayRouteTables' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtrsResponseStatus :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse Lude.Int
dlgrtrsResponseStatus = Lens.lens (responseStatus :: DescribeLocalGatewayRouteTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocalGatewayRouteTablesResponse)
{-# DEPRECATED dlgrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
