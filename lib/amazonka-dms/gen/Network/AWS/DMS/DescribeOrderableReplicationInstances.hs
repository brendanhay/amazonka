{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeOrderableReplicationInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication instance types that can be created in the specified region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeOrderableReplicationInstances
  ( -- * Creating a request
    DescribeOrderableReplicationInstances (..),
    mkDescribeOrderableReplicationInstances,

    -- ** Request lenses
    doriMarker,
    doriMaxRecords,

    -- * Destructuring the response
    DescribeOrderableReplicationInstancesResponse (..),
    mkDescribeOrderableReplicationInstancesResponse,

    -- ** Response lenses
    dorirsMarker,
    dorirsOrderableReplicationInstances,
    dorirsResponseStatus,
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
-- /See:/ 'mkDescribeOrderableReplicationInstances' smart constructor.
data DescribeOrderableReplicationInstances = DescribeOrderableReplicationInstances'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrderableReplicationInstances' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeOrderableReplicationInstances ::
  DescribeOrderableReplicationInstances
mkDescribeOrderableReplicationInstances =
  DescribeOrderableReplicationInstances'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doriMarker :: Lens.Lens' DescribeOrderableReplicationInstances (Lude.Maybe Lude.Text)
doriMarker = Lens.lens (marker :: DescribeOrderableReplicationInstances -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOrderableReplicationInstances)
{-# DEPRECATED doriMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doriMaxRecords :: Lens.Lens' DescribeOrderableReplicationInstances (Lude.Maybe Lude.Int)
doriMaxRecords = Lens.lens (maxRecords :: DescribeOrderableReplicationInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeOrderableReplicationInstances)
{-# DEPRECATED doriMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeOrderableReplicationInstances where
  page rq rs
    | Page.stop (rs Lens.^. dorirsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dorirsOrderableReplicationInstances) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& doriMarker Lens..~ rs Lens.^. dorirsMarker

instance Lude.AWSRequest DescribeOrderableReplicationInstances where
  type
    Rs DescribeOrderableReplicationInstances =
      DescribeOrderableReplicationInstancesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOrderableReplicationInstancesResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "OrderableReplicationInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrderableReplicationInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeOrderableReplicationInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeOrderableReplicationInstances where
  toJSON DescribeOrderableReplicationInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeOrderableReplicationInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrderableReplicationInstances where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeOrderableReplicationInstancesResponse' smart constructor.
data DescribeOrderableReplicationInstancesResponse = DescribeOrderableReplicationInstancesResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The order-able replication instances available.
    orderableReplicationInstances :: Lude.Maybe [OrderableReplicationInstance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrderableReplicationInstancesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'orderableReplicationInstances' - The order-able replication instances available.
-- * 'responseStatus' - The response status code.
mkDescribeOrderableReplicationInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrderableReplicationInstancesResponse
mkDescribeOrderableReplicationInstancesResponse pResponseStatus_ =
  DescribeOrderableReplicationInstancesResponse'
    { marker =
        Lude.Nothing,
      orderableReplicationInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorirsMarker :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Lude.Maybe Lude.Text)
dorirsMarker = Lens.lens (marker :: DescribeOrderableReplicationInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOrderableReplicationInstancesResponse)
{-# DEPRECATED dorirsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The order-able replication instances available.
--
-- /Note:/ Consider using 'orderableReplicationInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorirsOrderableReplicationInstances :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Lude.Maybe [OrderableReplicationInstance])
dorirsOrderableReplicationInstances = Lens.lens (orderableReplicationInstances :: DescribeOrderableReplicationInstancesResponse -> Lude.Maybe [OrderableReplicationInstance]) (\s a -> s {orderableReplicationInstances = a} :: DescribeOrderableReplicationInstancesResponse)
{-# DEPRECATED dorirsOrderableReplicationInstances "Use generic-lens or generic-optics with 'orderableReplicationInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorirsResponseStatus :: Lens.Lens' DescribeOrderableReplicationInstancesResponse Lude.Int
dorirsResponseStatus = Lens.lens (responseStatus :: DescribeOrderableReplicationInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrderableReplicationInstancesResponse)
{-# DEPRECATED dorirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
