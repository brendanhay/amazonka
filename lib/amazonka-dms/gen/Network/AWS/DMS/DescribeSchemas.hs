{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the schema for the specified endpoint.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeSchemas
  ( -- * Creating a request
    DescribeSchemas (..),
    mkDescribeSchemas,

    -- ** Request lenses
    dsMarker,
    dsMaxRecords,
    dsEndpointARN,

    -- * Destructuring the response
    DescribeSchemasResponse (..),
    mkDescribeSchemasResponse,

    -- ** Response lenses
    dsrsSchemas,
    dsrsMarker,
    dsrsResponseStatus,
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
-- /See:/ 'mkDescribeSchemas' smart constructor.
data DescribeSchemas = DescribeSchemas'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSchemas' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
mkDescribeSchemas ::
  -- | 'endpointARN'
  Lude.Text ->
  DescribeSchemas
mkDescribeSchemas pEndpointARN_ =
  DescribeSchemas'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endpointARN = pEndpointARN_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMarker :: Lens.Lens' DescribeSchemas (Lude.Maybe Lude.Text)
dsMarker = Lens.lens (marker :: DescribeSchemas -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSchemas)
{-# DEPRECATED dsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxRecords :: Lens.Lens' DescribeSchemas (Lude.Maybe Lude.Int)
dsMaxRecords = Lens.lens (maxRecords :: DescribeSchemas -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeSchemas)
{-# DEPRECATED dsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEndpointARN :: Lens.Lens' DescribeSchemas Lude.Text
dsEndpointARN = Lens.lens (endpointARN :: DescribeSchemas -> Lude.Text) (\s a -> s {endpointARN = a} :: DescribeSchemas)
{-# DEPRECATED dsEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Page.AWSPager DescribeSchemas where
  page rq rs
    | Page.stop (rs Lens.^. dsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsSchemas) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dsMarker Lens..~ rs Lens.^. dsrsMarker

instance Lude.AWSRequest DescribeSchemas where
  type Rs DescribeSchemas = DescribeSchemasResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSchemasResponse'
            Lude.<$> (x Lude..?> "Schemas" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSchemas where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeSchemas" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSchemas where
  toJSON DescribeSchemas' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords,
            Lude.Just ("EndpointArn" Lude..= endpointARN)
          ]
      )

instance Lude.ToPath DescribeSchemas where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSchemas where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeSchemasResponse' smart constructor.
data DescribeSchemasResponse = DescribeSchemasResponse'
  { -- | The described schema.
    schemas :: Lude.Maybe [Lude.Text],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSchemasResponse' with the minimum fields required to make a request.
--
-- * 'schemas' - The described schema.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeSchemasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSchemasResponse
mkDescribeSchemasResponse pResponseStatus_ =
  DescribeSchemasResponse'
    { schemas = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The described schema.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSchemas :: Lens.Lens' DescribeSchemasResponse (Lude.Maybe [Lude.Text])
dsrsSchemas = Lens.lens (schemas :: DescribeSchemasResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {schemas = a} :: DescribeSchemasResponse)
{-# DEPRECATED dsrsSchemas "Use generic-lens or generic-optics with 'schemas' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsMarker :: Lens.Lens' DescribeSchemasResponse (Lude.Maybe Lude.Text)
dsrsMarker = Lens.lens (marker :: DescribeSchemasResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSchemasResponse)
{-# DEPRECATED dsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeSchemasResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeSchemasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSchemasResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
