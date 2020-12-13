{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeSourceRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the source AWS Regions where the current AWS Region can create a read replica or copy a DB snapshot from. This API action supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeSourceRegions
  ( -- * Creating a request
    DescribeSourceRegions (..),
    mkDescribeSourceRegions,

    -- ** Request lenses
    dsrRegionName,
    dsrFilters,
    dsrMarker,
    dsrMaxRecords,

    -- * Destructuring the response
    DescribeSourceRegionsResponse (..),
    mkDescribeSourceRegionsResponse,

    -- ** Response lenses
    dsrrsMarker,
    dsrrsSourceRegions,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeSourceRegions' smart constructor.
data DescribeSourceRegions = DescribeSourceRegions'
  { -- | The source AWS Region name. For example, @us-east-1@ .
    --
    -- Constraints:
    --
    --     * Must specify a valid AWS Region name.
    regionName :: Lude.Maybe Lude.Text,
    -- | This parameter isn't currently supported.
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSourceRegions' with the minimum fields required to make a request.
--
-- * 'regionName' - The source AWS Region name. For example, @us-east-1@ .
--
-- Constraints:
--
--     * Must specify a valid AWS Region name.
--
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeSourceRegions ::
  DescribeSourceRegions
mkDescribeSourceRegions =
  DescribeSourceRegions'
    { regionName = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The source AWS Region name. For example, @us-east-1@ .
--
-- Constraints:
--
--     * Must specify a valid AWS Region name.
--
--
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrRegionName :: Lens.Lens' DescribeSourceRegions (Lude.Maybe Lude.Text)
dsrRegionName = Lens.lens (regionName :: DescribeSourceRegions -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: DescribeSourceRegions)
{-# DEPRECATED dsrRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrFilters :: Lens.Lens' DescribeSourceRegions (Lude.Maybe [Filter])
dsrFilters = Lens.lens (filters :: DescribeSourceRegions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeSourceRegions)
{-# DEPRECATED dsrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeSourceRegions@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMarker :: Lens.Lens' DescribeSourceRegions (Lude.Maybe Lude.Text)
dsrMarker = Lens.lens (marker :: DescribeSourceRegions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSourceRegions)
{-# DEPRECATED dsrMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMaxRecords :: Lens.Lens' DescribeSourceRegions (Lude.Maybe Lude.Int)
dsrMaxRecords = Lens.lens (maxRecords :: DescribeSourceRegions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeSourceRegions)
{-# DEPRECATED dsrMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeSourceRegions where
  page rq rs
    | Page.stop (rs Lens.^. dsrrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrrsSourceRegions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dsrMarker Lens..~ rs Lens.^. dsrrsMarker

instance Lude.AWSRequest DescribeSourceRegions where
  type Rs DescribeSourceRegions = DescribeSourceRegionsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeSourceRegionsResult"
      ( \s h x ->
          DescribeSourceRegionsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "SourceRegions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "SourceRegion")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSourceRegions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSourceRegions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSourceRegions where
  toQuery DescribeSourceRegions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSourceRegions" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "RegionName" Lude.=: regionName,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the @DescribeSourceRegions@ action.
--
-- /See:/ 'mkDescribeSourceRegionsResponse' smart constructor.
data DescribeSourceRegionsResponse = DescribeSourceRegionsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
    sourceRegions :: Lude.Maybe [SourceRegion],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSourceRegionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'sourceRegions' - A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
-- * 'responseStatus' - The response status code.
mkDescribeSourceRegionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSourceRegionsResponse
mkDescribeSourceRegionsResponse pResponseStatus_ =
  DescribeSourceRegionsResponse'
    { marker = Lude.Nothing,
      sourceRegions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsMarker :: Lens.Lens' DescribeSourceRegionsResponse (Lude.Maybe Lude.Text)
dsrrsMarker = Lens.lens (marker :: DescribeSourceRegionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSourceRegionsResponse)
{-# DEPRECATED dsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a read replica or a DB snapshot from.
--
-- /Note:/ Consider using 'sourceRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSourceRegions :: Lens.Lens' DescribeSourceRegionsResponse (Lude.Maybe [SourceRegion])
dsrrsSourceRegions = Lens.lens (sourceRegions :: DescribeSourceRegionsResponse -> Lude.Maybe [SourceRegion]) (\s a -> s {sourceRegions = a} :: DescribeSourceRegionsResponse)
{-# DEPRECATED dsrrsSourceRegions "Use generic-lens or generic-optics with 'sourceRegions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSourceRegionsResponse Lude.Int
dsrrsResponseStatus = Lens.lens (responseStatus :: DescribeSourceRegionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSourceRegionsResponse)
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
