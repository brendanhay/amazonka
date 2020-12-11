{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB parameter group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameters
  ( -- * Creating a request
    DescribeDBParameters (..),
    mkDescribeDBParameters,

    -- ** Request lenses
    ddpFilters,
    ddpMarker,
    ddpMaxRecords,
    ddpSource,
    ddpDBParameterGroupName,

    -- * Destructuring the response
    DescribeDBParametersResponse (..),
    mkDescribeDBParametersResponse,

    -- ** Response lenses
    ddprsMarker,
    ddprsParameters,
    ddprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDBParameters' smart constructor.
data DescribeDBParameters = DescribeDBParameters'
  { filters ::
      Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    source :: Lude.Maybe Lude.Text,
    dbParameterGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBParameters' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBParameterGroup.
--
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'source' - The parameter types to return.
--
-- Default: All parameter types returned
-- Valid Values: @user | system | engine-default@
mkDescribeDBParameters ::
  -- | 'dbParameterGroupName'
  Lude.Text ->
  DescribeDBParameters
mkDescribeDBParameters pDBParameterGroupName_ =
  DescribeDBParameters'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      source = Lude.Nothing,
      dbParameterGroupName = pDBParameterGroupName_
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpFilters :: Lens.Lens' DescribeDBParameters (Lude.Maybe [Filter])
ddpFilters = Lens.lens (filters :: DescribeDBParameters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBParameters)
{-# DEPRECATED ddpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpMarker :: Lens.Lens' DescribeDBParameters (Lude.Maybe Lude.Text)
ddpMarker = Lens.lens (marker :: DescribeDBParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBParameters)
{-# DEPRECATED ddpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpMaxRecords :: Lens.Lens' DescribeDBParameters (Lude.Maybe Lude.Int)
ddpMaxRecords = Lens.lens (maxRecords :: DescribeDBParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBParameters)
{-# DEPRECATED ddpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The parameter types to return.
--
-- Default: All parameter types returned
-- Valid Values: @user | system | engine-default@
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpSource :: Lens.Lens' DescribeDBParameters (Lude.Maybe Lude.Text)
ddpSource = Lens.lens (source :: DescribeDBParameters -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: DescribeDBParameters)
{-# DEPRECATED ddpSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
--     * If supplied, must match the name of an existing DBParameterGroup.
--
--
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpDBParameterGroupName :: Lens.Lens' DescribeDBParameters Lude.Text
ddpDBParameterGroupName = Lens.lens (dbParameterGroupName :: DescribeDBParameters -> Lude.Text) (\s a -> s {dbParameterGroupName = a} :: DescribeDBParameters)
{-# DEPRECATED ddpDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

instance Page.AWSPager DescribeDBParameters where
  page rq rs
    | Page.stop (rs Lens.^. ddprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ddpMarker Lens..~ rs Lens.^. ddprsMarker

instance Lude.AWSRequest DescribeDBParameters where
  type Rs DescribeDBParameters = DescribeDBParametersResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBParametersResult"
      ( \s h x ->
          DescribeDBParametersResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBParameters where
  toQuery DescribeDBParameters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "Source" Lude.=: source,
        "DBParameterGroupName" Lude.=: dbParameterGroupName
      ]

-- | Contains the result of a successful invocation of the @DescribeDBParameters@ action.
--
-- /See:/ 'mkDescribeDBParametersResponse' smart constructor.
data DescribeDBParametersResponse = DescribeDBParametersResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    parameters ::
      Lude.Maybe [Parameter],
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

-- | Creates a value of 'DescribeDBParametersResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'parameters' - A list of @Parameter@ values.
-- * 'responseStatus' - The response status code.
mkDescribeDBParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBParametersResponse
mkDescribeDBParametersResponse pResponseStatus_ =
  DescribeDBParametersResponse'
    { marker = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsMarker :: Lens.Lens' DescribeDBParametersResponse (Lude.Maybe Lude.Text)
ddprsMarker = Lens.lens (marker :: DescribeDBParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBParametersResponse)
{-# DEPRECATED ddprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @Parameter@ values.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsParameters :: Lens.Lens' DescribeDBParametersResponse (Lude.Maybe [Parameter])
ddprsParameters = Lens.lens (parameters :: DescribeDBParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeDBParametersResponse)
{-# DEPRECATED ddprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsResponseStatus :: Lens.Lens' DescribeDBParametersResponse Lude.Int
ddprsResponseStatus = Lens.lens (responseStatus :: DescribeDBParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBParametersResponse)
{-# DEPRECATED ddprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
