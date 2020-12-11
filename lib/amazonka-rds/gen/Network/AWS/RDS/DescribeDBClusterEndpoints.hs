{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about endpoints for an Amazon Aurora DB cluster.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterEndpoints
  ( -- * Creating a request
    DescribeDBClusterEndpoints (..),
    mkDescribeDBClusterEndpoints,

    -- ** Request lenses
    ddbceDBClusterIdentifier,
    ddbceFilters,
    ddbceDBClusterEndpointIdentifier,
    ddbceMarker,
    ddbceMaxRecords,

    -- * Destructuring the response
    DescribeDBClusterEndpointsResponse (..),
    mkDescribeDBClusterEndpointsResponse,

    -- ** Response lenses
    ddcersDBClusterEndpoints,
    ddcersMarker,
    ddcersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDBClusterEndpoints' smart constructor.
data DescribeDBClusterEndpoints = DescribeDBClusterEndpoints'
  { dbClusterIdentifier ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    dbClusterEndpointIdentifier ::
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

-- | Creates a value of 'DescribeDBClusterEndpoints' with the minimum fields required to make a request.
--
-- * 'dbClusterEndpointIdentifier' - The identifier of the endpoint to describe. This parameter is stored as a lowercase string.
-- * 'dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
-- * 'filters' - A set of name-value pairs that define which endpoints to include in the output. The filters are specified as name-value pairs, in the format @Name=/endpoint_type/ ,Values=/endpoint_type1/ ,/endpoint_type2/ ,...@ . @Name@ can be one of: @db-cluster-endpoint-type@ , @db-cluster-endpoint-custom-type@ , @db-cluster-endpoint-id@ , @db-cluster-endpoint-status@ . @Values@ for the @db-cluster-endpoint-type@ filter can be one or more of: @reader@ , @writer@ , @custom@ . @Values@ for the @db-cluster-endpoint-custom-type@ filter can be one or more of: @reader@ , @any@ . @Values@ for the @db-cluster-endpoint-status@ filter can be one or more of: @available@ , @creating@ , @deleting@ , @inactive@ , @modifying@ .
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBClusterEndpoints ::
  DescribeDBClusterEndpoints
mkDescribeDBClusterEndpoints =
  DescribeDBClusterEndpoints'
    { dbClusterIdentifier = Lude.Nothing,
      filters = Lude.Nothing,
      dbClusterEndpointIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceDBClusterIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Lude.Maybe Lude.Text)
ddbceDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DescribeDBClusterEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DescribeDBClusterEndpoints)
{-# DEPRECATED ddbceDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | A set of name-value pairs that define which endpoints to include in the output. The filters are specified as name-value pairs, in the format @Name=/endpoint_type/ ,Values=/endpoint_type1/ ,/endpoint_type2/ ,...@ . @Name@ can be one of: @db-cluster-endpoint-type@ , @db-cluster-endpoint-custom-type@ , @db-cluster-endpoint-id@ , @db-cluster-endpoint-status@ . @Values@ for the @db-cluster-endpoint-type@ filter can be one or more of: @reader@ , @writer@ , @custom@ . @Values@ for the @db-cluster-endpoint-custom-type@ filter can be one or more of: @reader@ , @any@ . @Values@ for the @db-cluster-endpoint-status@ filter can be one or more of: @available@ , @creating@ , @deleting@ , @inactive@ , @modifying@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceFilters :: Lens.Lens' DescribeDBClusterEndpoints (Lude.Maybe [Filter])
ddbceFilters = Lens.lens (filters :: DescribeDBClusterEndpoints -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBClusterEndpoints)
{-# DEPRECATED ddbceFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The identifier of the endpoint to describe. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceDBClusterEndpointIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Lude.Maybe Lude.Text)
ddbceDBClusterEndpointIdentifier = Lens.lens (dbClusterEndpointIdentifier :: DescribeDBClusterEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterEndpointIdentifier = a} :: DescribeDBClusterEndpoints)
{-# DEPRECATED ddbceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointIdentifier' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceMarker :: Lens.Lens' DescribeDBClusterEndpoints (Lude.Maybe Lude.Text)
ddbceMarker = Lens.lens (marker :: DescribeDBClusterEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterEndpoints)
{-# DEPRECATED ddbceMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceMaxRecords :: Lens.Lens' DescribeDBClusterEndpoints (Lude.Maybe Lude.Int)
ddbceMaxRecords = Lens.lens (maxRecords :: DescribeDBClusterEndpoints -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBClusterEndpoints)
{-# DEPRECATED ddbceMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeDBClusterEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. ddcersMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcersDBClusterEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddbceMarker Lens..~ rs Lens.^. ddcersMarker

instance Lude.AWSRequest DescribeDBClusterEndpoints where
  type
    Rs DescribeDBClusterEndpoints =
      DescribeDBClusterEndpointsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClusterEndpointsResult"
      ( \s h x ->
          DescribeDBClusterEndpointsResponse'
            Lude.<$> ( x Lude..@? "DBClusterEndpoints" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBClusterEndpointList")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusterEndpoints where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusterEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusterEndpoints where
  toQuery DescribeDBClusterEndpoints' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBClusterEndpoints" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBClusterEndpointIdentifier" Lude.=: dbClusterEndpointIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeDBClusterEndpointsResponse' smart constructor.
data DescribeDBClusterEndpointsResponse = DescribeDBClusterEndpointsResponse'
  { dbClusterEndpoints ::
      Lude.Maybe
        [DBClusterEndpoint],
    marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeDBClusterEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterEndpoints' - Contains the details of the endpoints associated with the cluster and matching any filter conditions.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBClusterEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClusterEndpointsResponse
mkDescribeDBClusterEndpointsResponse pResponseStatus_ =
  DescribeDBClusterEndpointsResponse'
    { dbClusterEndpoints =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the details of the endpoints associated with the cluster and matching any filter conditions.
--
-- /Note:/ Consider using 'dbClusterEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcersDBClusterEndpoints :: Lens.Lens' DescribeDBClusterEndpointsResponse (Lude.Maybe [DBClusterEndpoint])
ddcersDBClusterEndpoints = Lens.lens (dbClusterEndpoints :: DescribeDBClusterEndpointsResponse -> Lude.Maybe [DBClusterEndpoint]) (\s a -> s {dbClusterEndpoints = a} :: DescribeDBClusterEndpointsResponse)
{-# DEPRECATED ddcersDBClusterEndpoints "Use generic-lens or generic-optics with 'dbClusterEndpoints' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcersMarker :: Lens.Lens' DescribeDBClusterEndpointsResponse (Lude.Maybe Lude.Text)
ddcersMarker = Lens.lens (marker :: DescribeDBClusterEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterEndpointsResponse)
{-# DEPRECATED ddcersMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcersResponseStatus :: Lens.Lens' DescribeDBClusterEndpointsResponse Lude.Int
ddcersResponseStatus = Lens.lens (responseStatus :: DescribeDBClusterEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClusterEndpointsResponse)
{-# DEPRECATED ddcersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
