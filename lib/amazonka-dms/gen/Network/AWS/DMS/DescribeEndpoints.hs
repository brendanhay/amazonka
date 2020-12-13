{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the endpoints for your account in the current region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpoints
  ( -- * Creating a request
    DescribeEndpoints (..),
    mkDescribeEndpoints,

    -- ** Request lenses
    deFilters,
    deMarker,
    deMaxRecords,

    -- * Destructuring the response
    DescribeEndpointsResponse (..),
    mkDescribeEndpointsResponse,

    -- ** Response lenses
    dersMarker,
    dersEndpoints,
    dersResponseStatus,
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
-- /See:/ 'mkDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { -- | Filters applied to the endpoints.
    --
    -- Valid filter names: endpoint-arn | endpoint-type | endpoint-id | engine-name
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to the endpoints.
--
-- Valid filter names: endpoint-arn | endpoint-type | endpoint-id | engine-name
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeEndpoints ::
  DescribeEndpoints
mkDescribeEndpoints =
  DescribeEndpoints'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to the endpoints.
--
-- Valid filter names: endpoint-arn | endpoint-type | endpoint-id | engine-name
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFilters :: Lens.Lens' DescribeEndpoints (Lude.Maybe [Filter])
deFilters = Lens.lens (filters :: DescribeEndpoints -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEndpoints)
{-# DEPRECATED deFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMarker :: Lens.Lens' DescribeEndpoints (Lude.Maybe Lude.Text)
deMarker = Lens.lens (marker :: DescribeEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEndpoints)
{-# DEPRECATED deMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEndpoints (Lude.Maybe Lude.Int)
deMaxRecords = Lens.lens (maxRecords :: DescribeEndpoints -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEndpoints)
{-# DEPRECATED deMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. dersMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dersEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& deMarker Lens..~ rs Lens.^. dersMarker

instance Lude.AWSRequest DescribeEndpoints where
  type Rs DescribeEndpoints = DescribeEndpointsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Endpoints" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeEndpoints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEndpoints where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | Endpoint description.
    endpoints :: Lude.Maybe [Endpoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'endpoints' - Endpoint description.
-- * 'responseStatus' - The response status code.
mkDescribeEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointsResponse
mkDescribeEndpointsResponse pResponseStatus_ =
  DescribeEndpointsResponse'
    { marker = Lude.Nothing,
      endpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersMarker :: Lens.Lens' DescribeEndpointsResponse (Lude.Maybe Lude.Text)
dersMarker = Lens.lens (marker :: DescribeEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Endpoint description.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpoints :: Lens.Lens' DescribeEndpointsResponse (Lude.Maybe [Endpoint])
dersEndpoints = Lens.lens (endpoints :: DescribeEndpointsResponse -> Lude.Maybe [Endpoint]) (\s a -> s {endpoints = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEndpointsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
