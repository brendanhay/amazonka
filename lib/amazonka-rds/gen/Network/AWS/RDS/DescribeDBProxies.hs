{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBProxies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxies.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxies
  ( -- * Creating a request
    DescribeDBProxies (..),
    mkDescribeDBProxies,

    -- ** Request lenses
    ddbpFilters,
    ddbpMarker,
    ddbpMaxRecords,
    ddbpDBProxyName,

    -- * Destructuring the response
    DescribeDBProxiesResponse (..),
    mkDescribeDBProxiesResponse,

    -- ** Response lenses
    ddpsrsDBProxies,
    ddpsrsMarker,
    ddpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDBProxies' smart constructor.
data DescribeDBProxies = DescribeDBProxies'
  { -- | This parameter is not currently supported.
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Natural,
    -- | The name of the DB proxy.
    dbProxyName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBProxies' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter is not currently supported.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'dbProxyName' - The name of the DB proxy.
mkDescribeDBProxies ::
  DescribeDBProxies
mkDescribeDBProxies =
  DescribeDBProxies'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbProxyName = Lude.Nothing
    }

-- | This parameter is not currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpFilters :: Lens.Lens' DescribeDBProxies (Lude.Maybe [Filter])
ddbpFilters = Lens.lens (filters :: DescribeDBProxies -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBProxies)
{-# DEPRECATED ddbpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpMarker :: Lens.Lens' DescribeDBProxies (Lude.Maybe Lude.Text)
ddbpMarker = Lens.lens (marker :: DescribeDBProxies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBProxies)
{-# DEPRECATED ddbpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpMaxRecords :: Lens.Lens' DescribeDBProxies (Lude.Maybe Lude.Natural)
ddbpMaxRecords = Lens.lens (maxRecords :: DescribeDBProxies -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeDBProxies)
{-# DEPRECATED ddbpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the DB proxy.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbpDBProxyName :: Lens.Lens' DescribeDBProxies (Lude.Maybe Lude.Text)
ddbpDBProxyName = Lens.lens (dbProxyName :: DescribeDBProxies -> Lude.Maybe Lude.Text) (\s a -> s {dbProxyName = a} :: DescribeDBProxies)
{-# DEPRECATED ddbpDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

instance Page.AWSPager DescribeDBProxies where
  page rq rs
    | Page.stop (rs Lens.^. ddpsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddpsrsDBProxies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddbpMarker Lens..~ rs Lens.^. ddpsrsMarker

instance Lude.AWSRequest DescribeDBProxies where
  type Rs DescribeDBProxies = DescribeDBProxiesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBProxiesResult"
      ( \s h x ->
          DescribeDBProxiesResponse'
            Lude.<$> ( x Lude..@? "DBProxies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBProxies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBProxies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBProxies where
  toQuery DescribeDBProxies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBProxies" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBProxyName" Lude.=: dbProxyName
      ]

-- | /See:/ 'mkDescribeDBProxiesResponse' smart constructor.
data DescribeDBProxiesResponse = DescribeDBProxiesResponse'
  { -- | A return value representing an arbitrary number of @DBProxy@ data structures.
    dbProxies :: Lude.Maybe [DBProxy],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBProxiesResponse' with the minimum fields required to make a request.
--
-- * 'dbProxies' - A return value representing an arbitrary number of @DBProxy@ data structures.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBProxiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBProxiesResponse
mkDescribeDBProxiesResponse pResponseStatus_ =
  DescribeDBProxiesResponse'
    { dbProxies = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A return value representing an arbitrary number of @DBProxy@ data structures.
--
-- /Note:/ Consider using 'dbProxies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpsrsDBProxies :: Lens.Lens' DescribeDBProxiesResponse (Lude.Maybe [DBProxy])
ddpsrsDBProxies = Lens.lens (dbProxies :: DescribeDBProxiesResponse -> Lude.Maybe [DBProxy]) (\s a -> s {dbProxies = a} :: DescribeDBProxiesResponse)
{-# DEPRECATED ddpsrsDBProxies "Use generic-lens or generic-optics with 'dbProxies' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpsrsMarker :: Lens.Lens' DescribeDBProxiesResponse (Lude.Maybe Lude.Text)
ddpsrsMarker = Lens.lens (marker :: DescribeDBProxiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBProxiesResponse)
{-# DEPRECATED ddpsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpsrsResponseStatus :: Lens.Lens' DescribeDBProxiesResponse Lude.Int
ddpsrsResponseStatus = Lens.lens (responseStatus :: DescribeDBProxiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBProxiesResponse)
{-# DEPRECATED ddpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
