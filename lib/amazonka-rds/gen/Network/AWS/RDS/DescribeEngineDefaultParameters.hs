{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the specified database engine.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultParameters
  ( -- * Creating a request
    DescribeEngineDefaultParameters (..),
    mkDescribeEngineDefaultParameters,

    -- ** Request lenses
    dedpFilters,
    dedpDBParameterGroupFamily,
    dedpMarker,
    dedpMaxRecords,

    -- * Destructuring the response
    DescribeEngineDefaultParametersResponse (..),
    mkDescribeEngineDefaultParametersResponse,

    -- ** Response lenses
    dedprsEngineDefaults,
    dedprsResponseStatus,
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
-- /See:/ 'mkDescribeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { -- | This parameter isn't currently supported.
    filters :: Lude.Maybe [Filter],
    -- | The name of the DB parameter group family.
    dbParameterGroupFamily :: Lude.Text,
    -- | An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEngineDefaultParameters' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'dbParameterGroupFamily' - The name of the DB parameter group family.
-- * 'marker' - An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeEngineDefaultParameters ::
  -- | 'dbParameterGroupFamily'
  Lude.Text ->
  DescribeEngineDefaultParameters
mkDescribeEngineDefaultParameters pDBParameterGroupFamily_ =
  DescribeEngineDefaultParameters'
    { filters = Lude.Nothing,
      dbParameterGroupFamily = pDBParameterGroupFamily_,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpFilters :: Lens.Lens' DescribeEngineDefaultParameters (Lude.Maybe [Filter])
dedpFilters = Lens.lens (filters :: DescribeEngineDefaultParameters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEngineDefaultParameters)
{-# DEPRECATED dedpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The name of the DB parameter group family.
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpDBParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultParameters Lude.Text
dedpDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: DescribeEngineDefaultParameters -> Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: DescribeEngineDefaultParameters)
{-# DEPRECATED dedpDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpMarker :: Lens.Lens' DescribeEngineDefaultParameters (Lude.Maybe Lude.Text)
dedpMarker = Lens.lens (marker :: DescribeEngineDefaultParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEngineDefaultParameters)
{-# DEPRECATED dedpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpMaxRecords :: Lens.Lens' DescribeEngineDefaultParameters (Lude.Maybe Lude.Int)
dedpMaxRecords = Lens.lens (maxRecords :: DescribeEngineDefaultParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEngineDefaultParameters)
{-# DEPRECATED dedpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeEngineDefaultParameters where
  page rq rs
    | Page.stop
        (rs Lens.^? dedprsEngineDefaults Lude.. edMarker Lude.. Lens._Just) =
      Lude.Nothing
    | Page.stop
        ( rs
            Lens.^? dedprsEngineDefaults Lude.. edParameters Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dedpMarker
          Lens..~ rs Lens.^? dedprsEngineDefaults Lude.. edMarker Lude.. Lens._Just

instance Lude.AWSRequest DescribeEngineDefaultParameters where
  type
    Rs DescribeEngineDefaultParameters =
      DescribeEngineDefaultParametersResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeEngineDefaultParametersResult"
      ( \s h x ->
          DescribeEngineDefaultParametersResponse'
            Lude.<$> (x Lude..@ "EngineDefaults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEngineDefaultParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEngineDefaultParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEngineDefaultParameters where
  toQuery DescribeEngineDefaultParameters' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEngineDefaultParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBParameterGroupFamily" Lude.=: dbParameterGroupFamily,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { engineDefaults :: EngineDefaults,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEngineDefaultParametersResponse' with the minimum fields required to make a request.
--
-- * 'engineDefaults' -
-- * 'responseStatus' - The response status code.
mkDescribeEngineDefaultParametersResponse ::
  -- | 'engineDefaults'
  EngineDefaults ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEngineDefaultParametersResponse
mkDescribeEngineDefaultParametersResponse
  pEngineDefaults_
  pResponseStatus_ =
    DescribeEngineDefaultParametersResponse'
      { engineDefaults =
          pEngineDefaults_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'engineDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedprsEngineDefaults :: Lens.Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprsEngineDefaults = Lens.lens (engineDefaults :: DescribeEngineDefaultParametersResponse -> EngineDefaults) (\s a -> s {engineDefaults = a} :: DescribeEngineDefaultParametersResponse)
{-# DEPRECATED dedprsEngineDefaults "Use generic-lens or generic-optics with 'engineDefaults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedprsResponseStatus :: Lens.Lens' DescribeEngineDefaultParametersResponse Lude.Int
dedprsResponseStatus = Lens.lens (responseStatus :: DescribeEngineDefaultParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEngineDefaultParametersResponse)
{-# DEPRECATED dedprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
