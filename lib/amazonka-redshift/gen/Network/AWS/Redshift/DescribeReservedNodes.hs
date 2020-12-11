{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of the reserved nodes.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeReservedNodes
  ( -- * Creating a request
    DescribeReservedNodes (..),
    mkDescribeReservedNodes,

    -- ** Request lenses
    drnReservedNodeId,
    drnMarker,
    drnMaxRecords,

    -- * Destructuring the response
    DescribeReservedNodesResponse (..),
    mkDescribeReservedNodesResponse,

    -- ** Response lenses
    drnrsReservedNodes,
    drnrsMarker,
    drnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReservedNodes' smart constructor.
data DescribeReservedNodes = DescribeReservedNodes'
  { reservedNodeId ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedNodes' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeReservedNodes' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'reservedNodeId' - Identifier for the node reservation.
mkDescribeReservedNodes ::
  DescribeReservedNodes
mkDescribeReservedNodes =
  DescribeReservedNodes'
    { reservedNodeId = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Identifier for the node reservation.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnReservedNodeId :: Lens.Lens' DescribeReservedNodes (Lude.Maybe Lude.Text)
drnReservedNodeId = Lens.lens (reservedNodeId :: DescribeReservedNodes -> Lude.Maybe Lude.Text) (\s a -> s {reservedNodeId = a} :: DescribeReservedNodes)
{-# DEPRECATED drnReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeReservedNodes' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnMarker :: Lens.Lens' DescribeReservedNodes (Lude.Maybe Lude.Text)
drnMarker = Lens.lens (marker :: DescribeReservedNodes -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedNodes)
{-# DEPRECATED drnMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnMaxRecords :: Lens.Lens' DescribeReservedNodes (Lude.Maybe Lude.Int)
drnMaxRecords = Lens.lens (maxRecords :: DescribeReservedNodes -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReservedNodes)
{-# DEPRECATED drnMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeReservedNodes where
  page rq rs
    | Page.stop (rs Lens.^. drnrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drnrsReservedNodes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& drnMarker Lens..~ rs Lens.^. drnrsMarker

instance Lude.AWSRequest DescribeReservedNodes where
  type Rs DescribeReservedNodes = DescribeReservedNodesResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeReservedNodesResult"
      ( \s h x ->
          DescribeReservedNodesResponse'
            Lude.<$> ( x Lude..@? "ReservedNodes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedNode")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedNodes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedNodes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedNodes where
  toQuery DescribeReservedNodes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeReservedNodes" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ReservedNodeId" Lude.=: reservedNodeId,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeReservedNodesResponse' smart constructor.
data DescribeReservedNodesResponse = DescribeReservedNodesResponse'
  { reservedNodes ::
      Lude.Maybe [ReservedNode],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeReservedNodesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'reservedNodes' - The list of @ReservedNode@ objects.
-- * 'responseStatus' - The response status code.
mkDescribeReservedNodesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedNodesResponse
mkDescribeReservedNodesResponse pResponseStatus_ =
  DescribeReservedNodesResponse'
    { reservedNodes = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of @ReservedNode@ objects.
--
-- /Note:/ Consider using 'reservedNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnrsReservedNodes :: Lens.Lens' DescribeReservedNodesResponse (Lude.Maybe [ReservedNode])
drnrsReservedNodes = Lens.lens (reservedNodes :: DescribeReservedNodesResponse -> Lude.Maybe [ReservedNode]) (\s a -> s {reservedNodes = a} :: DescribeReservedNodesResponse)
{-# DEPRECATED drnrsReservedNodes "Use generic-lens or generic-optics with 'reservedNodes' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnrsMarker :: Lens.Lens' DescribeReservedNodesResponse (Lude.Maybe Lude.Text)
drnrsMarker = Lens.lens (marker :: DescribeReservedNodesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedNodesResponse)
{-# DEPRECATED drnrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnrsResponseStatus :: Lens.Lens' DescribeReservedNodesResponse Lude.Int
drnrsResponseStatus = Lens.lens (responseStatus :: DescribeReservedNodesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedNodesResponse)
{-# DEPRECATED drnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
