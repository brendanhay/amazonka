{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the connections that have been made between the replication instance and an endpoint. Connections are created when you test an endpoint.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeConnections
  ( -- * Creating a request
    DescribeConnections (..),
    mkDescribeConnections,

    -- ** Request lenses
    dcFilters,
    dcMarker,
    dcMaxRecords,

    -- * Destructuring the response
    DescribeConnectionsResponse (..),
    mkDescribeConnectionsResponse,

    -- ** Response lenses
    dcsrsConnections,
    dcsrsMarker,
    dcsrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeConnections' smart constructor.
data DescribeConnections = DescribeConnections'
  { filters ::
      Lude.Maybe [Filter],
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

-- | Creates a value of 'DescribeConnections' with the minimum fields required to make a request.
--
-- * 'filters' - The filters applied to the connection.
--
-- Valid filter names: endpoint-arn | replication-instance-arn
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeConnections ::
  DescribeConnections
mkDescribeConnections =
  DescribeConnections'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The filters applied to the connection.
--
-- Valid filter names: endpoint-arn | replication-instance-arn
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcFilters :: Lens.Lens' DescribeConnections (Lude.Maybe [Filter])
dcFilters = Lens.lens (filters :: DescribeConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeConnections)
{-# DEPRECATED dcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeConnections (Lude.Maybe Lude.Text)
dcMarker = Lens.lens (marker :: DescribeConnections -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeConnections)
{-# DEPRECATED dcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxRecords :: Lens.Lens' DescribeConnections (Lude.Maybe Lude.Int)
dcMaxRecords = Lens.lens (maxRecords :: DescribeConnections -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeConnections)
{-# DEPRECATED dcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeConnections where
  page rq rs
    | Page.stop (rs Lens.^. dcsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcsrsConnections) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcMarker Lens..~ rs Lens.^. dcsrsMarker

instance Lude.AWSRequest DescribeConnections where
  type Rs DescribeConnections = DescribeConnectionsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeConnectionsResponse'
            Lude.<$> (x Lude..?> "Connections" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeConnections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeConnections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeConnections where
  toJSON DescribeConnections' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeConnections where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeConnectionsResponse' smart constructor.
data DescribeConnectionsResponse = DescribeConnectionsResponse'
  { connections ::
      Lude.Maybe [Connection],
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

-- | Creates a value of 'DescribeConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'connections' - A description of the connections.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeConnectionsResponse
mkDescribeConnectionsResponse pResponseStatus_ =
  DescribeConnectionsResponse'
    { connections = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the connections.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsConnections :: Lens.Lens' DescribeConnectionsResponse (Lude.Maybe [Connection])
dcsrsConnections = Lens.lens (connections :: DescribeConnectionsResponse -> Lude.Maybe [Connection]) (\s a -> s {connections = a} :: DescribeConnectionsResponse)
{-# DEPRECATED dcsrsConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsMarker :: Lens.Lens' DescribeConnectionsResponse (Lude.Maybe Lude.Text)
dcsrsMarker = Lens.lens (marker :: DescribeConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeConnectionsResponse)
{-# DEPRECATED dcsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DescribeConnectionsResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DescribeConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeConnectionsResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
