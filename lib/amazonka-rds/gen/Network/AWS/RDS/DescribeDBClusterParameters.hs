{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster parameter group.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameters
  ( -- * Creating a request
    DescribeDBClusterParameters (..),
    mkDescribeDBClusterParameters,

    -- ** Request lenses
    ddcpFilters,
    ddcpMarker,
    ddcpMaxRecords,
    ddcpSource,
    ddcpDBClusterParameterGroupName,

    -- * Destructuring the response
    DescribeDBClusterParametersResponse (..),
    mkDescribeDBClusterParametersResponse,

    -- ** Response lenses
    ddcprsMarker,
    ddcprsParameters,
    ddcprsResponseStatus,
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
-- /See:/ 'mkDescribeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { filters ::
      Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    source :: Lude.Maybe Lude.Text,
    dbClusterParameterGroupName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterParameters' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupName' - The name of a specific DB cluster parameter group to return parameter details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'source' - A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ .
mkDescribeDBClusterParameters ::
  -- | 'dbClusterParameterGroupName'
  Lude.Text ->
  DescribeDBClusterParameters
mkDescribeDBClusterParameters pDBClusterParameterGroupName_ =
  DescribeDBClusterParameters'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      source = Lude.Nothing,
      dbClusterParameterGroupName = pDBClusterParameterGroupName_
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpFilters :: Lens.Lens' DescribeDBClusterParameters (Lude.Maybe [Filter])
ddcpFilters = Lens.lens (filters :: DescribeDBClusterParameters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBClusterParameters)
{-# DEPRECATED ddcpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpMarker :: Lens.Lens' DescribeDBClusterParameters (Lude.Maybe Lude.Text)
ddcpMarker = Lens.lens (marker :: DescribeDBClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterParameters)
{-# DEPRECATED ddcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpMaxRecords :: Lens.Lens' DescribeDBClusterParameters (Lude.Maybe Lude.Int)
ddcpMaxRecords = Lens.lens (maxRecords :: DescribeDBClusterParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBClusterParameters)
{-# DEPRECATED ddcpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ .
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpSource :: Lens.Lens' DescribeDBClusterParameters (Lude.Maybe Lude.Text)
ddcpSource = Lens.lens (source :: DescribeDBClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: DescribeDBClusterParameters)
{-# DEPRECATED ddcpSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of a specific DB cluster parameter group to return parameter details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpDBClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameters Lude.Text
ddcpDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: DescribeDBClusterParameters -> Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameters)
{-# DEPRECATED ddcpDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

instance Page.AWSPager DescribeDBClusterParameters where
  page rq rs
    | Page.stop (rs Lens.^. ddcprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcpMarker Lens..~ rs Lens.^. ddcprsMarker

instance Lude.AWSRequest DescribeDBClusterParameters where
  type
    Rs DescribeDBClusterParameters =
      DescribeDBClusterParametersResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClusterParametersResult"
      ( \s h x ->
          DescribeDBClusterParametersResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusterParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusterParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusterParameters where
  toQuery DescribeDBClusterParameters' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBClusterParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "Source" Lude.=: source,
        "DBClusterParameterGroupName" Lude.=: dbClusterParameterGroupName
      ]

-- | Provides details about a DB cluster parameter group including the parameters in the DB cluster parameter group.
--
-- /See:/ 'mkDescribeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    parameters ::
      Lude.Maybe
        [Parameter],
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

-- | Creates a value of 'DescribeDBClusterParametersResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'parameters' - Provides a list of parameters for the DB cluster parameter group.
-- * 'responseStatus' - The response status code.
mkDescribeDBClusterParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClusterParametersResponse
mkDescribeDBClusterParametersResponse pResponseStatus_ =
  DescribeDBClusterParametersResponse'
    { marker = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprsMarker :: Lens.Lens' DescribeDBClusterParametersResponse (Lude.Maybe Lude.Text)
ddcprsMarker = Lens.lens (marker :: DescribeDBClusterParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterParametersResponse)
{-# DEPRECATED ddcprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Provides a list of parameters for the DB cluster parameter group.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprsParameters :: Lens.Lens' DescribeDBClusterParametersResponse (Lude.Maybe [Parameter])
ddcprsParameters = Lens.lens (parameters :: DescribeDBClusterParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeDBClusterParametersResponse)
{-# DEPRECATED ddcprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprsResponseStatus :: Lens.Lens' DescribeDBClusterParametersResponse Lude.Int
ddcprsResponseStatus = Lens.lens (responseStatus :: DescribeDBClusterParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClusterParametersResponse)
{-# DEPRECATED ddcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
