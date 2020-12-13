{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEndpointTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the type of endpoints available.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpointTypes
  ( -- * Creating a request
    DescribeEndpointTypes (..),
    mkDescribeEndpointTypes,

    -- ** Request lenses
    detFilters,
    detMarker,
    detMaxRecords,

    -- * Destructuring the response
    DescribeEndpointTypesResponse (..),
    mkDescribeEndpointTypesResponse,

    -- ** Response lenses
    detrsSupportedEndpointTypes,
    detrsMarker,
    detrsResponseStatus,
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
-- /See:/ 'mkDescribeEndpointTypes' smart constructor.
data DescribeEndpointTypes = DescribeEndpointTypes'
  { -- | Filters applied to the endpoint types.
    --
    -- Valid filter names: engine-name | endpoint-type
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

-- | Creates a value of 'DescribeEndpointTypes' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeEndpointTypes ::
  DescribeEndpointTypes
mkDescribeEndpointTypes =
  DescribeEndpointTypes'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeEndpointTypes (Lude.Maybe [Filter])
detFilters = Lens.lens (filters :: DescribeEndpointTypes -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEndpointTypes)
{-# DEPRECATED detFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMarker :: Lens.Lens' DescribeEndpointTypes (Lude.Maybe Lude.Text)
detMarker = Lens.lens (marker :: DescribeEndpointTypes -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEndpointTypes)
{-# DEPRECATED detMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxRecords :: Lens.Lens' DescribeEndpointTypes (Lude.Maybe Lude.Int)
detMaxRecords = Lens.lens (maxRecords :: DescribeEndpointTypes -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEndpointTypes)
{-# DEPRECATED detMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeEndpointTypes where
  page rq rs
    | Page.stop (rs Lens.^. detrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. detrsSupportedEndpointTypes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& detMarker Lens..~ rs Lens.^. detrsMarker

instance Lude.AWSRequest DescribeEndpointTypes where
  type Rs DescribeEndpointTypes = DescribeEndpointTypesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointTypesResponse'
            Lude.<$> (x Lude..?> "SupportedEndpointTypes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpointTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeEndpointTypes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEndpointTypes where
  toJSON DescribeEndpointTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeEndpointTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEndpointTypes where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeEndpointTypesResponse' smart constructor.
data DescribeEndpointTypesResponse = DescribeEndpointTypesResponse'
  { -- | The types of endpoints that are supported.
    supportedEndpointTypes :: Lude.Maybe [SupportedEndpointType],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointTypesResponse' with the minimum fields required to make a request.
--
-- * 'supportedEndpointTypes' - The types of endpoints that are supported.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeEndpointTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointTypesResponse
mkDescribeEndpointTypesResponse pResponseStatus_ =
  DescribeEndpointTypesResponse'
    { supportedEndpointTypes =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The types of endpoints that are supported.
--
-- /Note:/ Consider using 'supportedEndpointTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsSupportedEndpointTypes :: Lens.Lens' DescribeEndpointTypesResponse (Lude.Maybe [SupportedEndpointType])
detrsSupportedEndpointTypes = Lens.lens (supportedEndpointTypes :: DescribeEndpointTypesResponse -> Lude.Maybe [SupportedEndpointType]) (\s a -> s {supportedEndpointTypes = a} :: DescribeEndpointTypesResponse)
{-# DEPRECATED detrsSupportedEndpointTypes "Use generic-lens or generic-optics with 'supportedEndpointTypes' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsMarker :: Lens.Lens' DescribeEndpointTypesResponse (Lude.Maybe Lude.Text)
detrsMarker = Lens.lens (marker :: DescribeEndpointTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEndpointTypesResponse)
{-# DEPRECATED detrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeEndpointTypesResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeEndpointTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointTypesResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
