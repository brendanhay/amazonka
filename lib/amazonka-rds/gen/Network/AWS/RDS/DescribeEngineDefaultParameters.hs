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
    dedpDBParameterGroupFamily,
    dedpFilters,
    dedpMarker,
    dedpMaxRecords,

    -- * Destructuring the response
    DescribeEngineDefaultParametersResponse (..),
    mkDescribeEngineDefaultParametersResponse,

    -- ** Response lenses
    dedprrsEngineDefaults,
    dedprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { -- | The name of the DB parameter group family.
    dBParameterGroupFamily :: Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEngineDefaultParameters' value with any optional fields omitted.
mkDescribeEngineDefaultParameters ::
  -- | 'dBParameterGroupFamily'
  Types.String ->
  DescribeEngineDefaultParameters
mkDescribeEngineDefaultParameters dBParameterGroupFamily =
  DescribeEngineDefaultParameters'
    { dBParameterGroupFamily,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the DB parameter group family.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpDBParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultParameters Types.String
dedpDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# DEPRECATED dedpDBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpFilters :: Lens.Lens' DescribeEngineDefaultParameters (Core.Maybe [Types.Filter])
dedpFilters = Lens.field @"filters"
{-# DEPRECATED dedpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeEngineDefaultParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpMarker :: Lens.Lens' DescribeEngineDefaultParameters (Core.Maybe Types.String)
dedpMarker = Lens.field @"marker"
{-# DEPRECATED dedpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedpMaxRecords :: Lens.Lens' DescribeEngineDefaultParameters (Core.Maybe Core.Int)
dedpMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dedpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeEngineDefaultParameters where
  type
    Rs DescribeEngineDefaultParameters =
      DescribeEngineDefaultParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeEngineDefaultParameters")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBParameterGroupFamily" dBParameterGroupFamily)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEngineDefaultParametersResult"
      ( \s h x ->
          DescribeEngineDefaultParametersResponse'
            Core.<$> (x Core..@ "EngineDefaults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEngineDefaultParameters where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^. Lens.field @"engineDefaults" Core.. Lens.field @"marker"
        ) =
      Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"engineDefaults"
              Core.. Lens.field @"parameters"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs
            Lens.^. Lens.field @"engineDefaults" Core.. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { engineDefaults :: Types.EngineDefaults,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEngineDefaultParametersResponse' value with any optional fields omitted.
mkDescribeEngineDefaultParametersResponse ::
  -- | 'engineDefaults'
  Types.EngineDefaults ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeEngineDefaultParametersResponse
mkDescribeEngineDefaultParametersResponse
  engineDefaults
  responseStatus =
    DescribeEngineDefaultParametersResponse'
      { engineDefaults,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'engineDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedprrsEngineDefaults :: Lens.Lens' DescribeEngineDefaultParametersResponse Types.EngineDefaults
dedprrsEngineDefaults = Lens.field @"engineDefaults"
{-# DEPRECATED dedprrsEngineDefaults "Use generic-lens or generic-optics with 'engineDefaults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedprrsResponseStatus :: Lens.Lens' DescribeEngineDefaultParametersResponse Core.Int
dedprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dedprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
