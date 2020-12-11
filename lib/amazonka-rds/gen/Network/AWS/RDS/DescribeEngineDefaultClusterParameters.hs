{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEngineDefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the cluster database engine.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultClusterParameters
  ( -- * Creating a request
    DescribeEngineDefaultClusterParameters (..),
    mkDescribeEngineDefaultClusterParameters,

    -- ** Request lenses
    dedcpFilters,
    dedcpMarker,
    dedcpMaxRecords,
    dedcpDBParameterGroupFamily,

    -- * Destructuring the response
    DescribeEngineDefaultClusterParametersResponse (..),
    mkDescribeEngineDefaultClusterParametersResponse,

    -- ** Response lenses
    dedcprsEngineDefaults,
    dedcprsResponseStatus,
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
-- /See:/ 'mkDescribeEngineDefaultClusterParameters' smart constructor.
data DescribeEngineDefaultClusterParameters = DescribeEngineDefaultClusterParameters'
  { filters ::
      Lude.Maybe
        [Filter],
    marker ::
      Lude.Maybe
        Lude.Text,
    maxRecords ::
      Lude.Maybe
        Lude.Int,
    dbParameterGroupFamily ::
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

-- | Creates a value of 'DescribeEngineDefaultClusterParameters' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupFamily' - The name of the DB cluster parameter group family to return engine parameter information for.
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous @DescribeEngineDefaultClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeEngineDefaultClusterParameters ::
  -- | 'dbParameterGroupFamily'
  Lude.Text ->
  DescribeEngineDefaultClusterParameters
mkDescribeEngineDefaultClusterParameters pDBParameterGroupFamily_ =
  DescribeEngineDefaultClusterParameters'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      dbParameterGroupFamily = pDBParameterGroupFamily_
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpFilters :: Lens.Lens' DescribeEngineDefaultClusterParameters (Lude.Maybe [Filter])
dedcpFilters = Lens.lens (filters :: DescribeEngineDefaultClusterParameters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEngineDefaultClusterParameters)
{-# DEPRECATED dedcpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeEngineDefaultClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpMarker :: Lens.Lens' DescribeEngineDefaultClusterParameters (Lude.Maybe Lude.Text)
dedcpMarker = Lens.lens (marker :: DescribeEngineDefaultClusterParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEngineDefaultClusterParameters)
{-# DEPRECATED dedcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpMaxRecords :: Lens.Lens' DescribeEngineDefaultClusterParameters (Lude.Maybe Lude.Int)
dedcpMaxRecords = Lens.lens (maxRecords :: DescribeEngineDefaultClusterParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEngineDefaultClusterParameters)
{-# DEPRECATED dedcpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the DB cluster parameter group family to return engine parameter information for.
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcpDBParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultClusterParameters Lude.Text
dedcpDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: DescribeEngineDefaultClusterParameters -> Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: DescribeEngineDefaultClusterParameters)
{-# DEPRECATED dedcpDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

instance Page.AWSPager DescribeEngineDefaultClusterParameters where
  page rq rs
    | Page.stop
        ( rs
            Lens.^? dedcprsEngineDefaults Lude.. Lens._Just
              Lude.. edMarker
              Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Page.stop
        ( rs
            Lens.^? dedcprsEngineDefaults Lude.. Lens._Just
              Lude.. edParameters
              Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dedcpMarker
          Lens..~ rs
          Lens.^? dedcprsEngineDefaults Lude.. Lens._Just
            Lude.. edMarker
            Lude.. Lens._Just

instance Lude.AWSRequest DescribeEngineDefaultClusterParameters where
  type
    Rs DescribeEngineDefaultClusterParameters =
      DescribeEngineDefaultClusterParametersResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeEngineDefaultClusterParametersResult"
      ( \s h x ->
          DescribeEngineDefaultClusterParametersResponse'
            Lude.<$> (x Lude..@? "EngineDefaults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEngineDefaultClusterParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEngineDefaultClusterParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEngineDefaultClusterParameters where
  toQuery DescribeEngineDefaultClusterParameters' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEngineDefaultClusterParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "DBParameterGroupFamily" Lude.=: dbParameterGroupFamily
      ]

-- | /See:/ 'mkDescribeEngineDefaultClusterParametersResponse' smart constructor.
data DescribeEngineDefaultClusterParametersResponse = DescribeEngineDefaultClusterParametersResponse'
  { engineDefaults ::
      Lude.Maybe
        EngineDefaults,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeEngineDefaultClusterParametersResponse' with the minimum fields required to make a request.
--
-- * 'engineDefaults' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeEngineDefaultClusterParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEngineDefaultClusterParametersResponse
mkDescribeEngineDefaultClusterParametersResponse pResponseStatus_ =
  DescribeEngineDefaultClusterParametersResponse'
    { engineDefaults =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'engineDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcprsEngineDefaults :: Lens.Lens' DescribeEngineDefaultClusterParametersResponse (Lude.Maybe EngineDefaults)
dedcprsEngineDefaults = Lens.lens (engineDefaults :: DescribeEngineDefaultClusterParametersResponse -> Lude.Maybe EngineDefaults) (\s a -> s {engineDefaults = a} :: DescribeEngineDefaultClusterParametersResponse)
{-# DEPRECATED dedcprsEngineDefaults "Use generic-lens or generic-optics with 'engineDefaults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedcprsResponseStatus :: Lens.Lens' DescribeEngineDefaultClusterParametersResponse Lude.Int
dedcprsResponseStatus = Lens.lens (responseStatus :: DescribeEngineDefaultClusterParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEngineDefaultClusterParametersResponse)
{-# DEPRECATED dedcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
