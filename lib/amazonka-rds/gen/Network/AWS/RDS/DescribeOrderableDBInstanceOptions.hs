{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable DB instance options for the specified engine.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
  ( -- * Creating a request
    DescribeOrderableDBInstanceOptions (..),
    mkDescribeOrderableDBInstanceOptions,

    -- ** Request lenses
    dodbioEngine,
    dodbioAvailabilityZoneGroup,
    dodbioDBInstanceClass,
    dodbioEngineVersion,
    dodbioFilters,
    dodbioLicenseModel,
    dodbioMarker,
    dodbioMaxRecords,
    dodbioVpc,

    -- * Destructuring the response
    DescribeOrderableDBInstanceOptionsResponse (..),
    mkDescribeOrderableDBInstanceOptionsResponse,

    -- ** Response lenses
    dodbiorrsMarker,
    dodbiorrsOrderableDBInstanceOptions,
    dodbiorrsResponseStatus,
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
-- /See:/ 'mkDescribeOrderableDBInstanceOptions' smart constructor.
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions'
  { -- | The name of the engine to retrieve DB instance options for.
    engine :: Types.Engine,
    -- | The Availability Zone group associated with a Local Zone. Specify this parameter to retrieve available offerings for the Local Zones in the group.
    --
    -- Omit this parameter to show the available offerings in the specified AWS Region.
    availabilityZoneGroup :: Core.Maybe Types.AvailabilityZoneGroup,
    -- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
    dBInstanceClass :: Core.Maybe Types.DBInstanceClass,
    -- | The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
    licenseModel :: Core.Maybe Types.LicenseModel,
    -- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | A value that indicates whether to show only VPC or non-VPC offerings.
    vpc :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableDBInstanceOptions' value with any optional fields omitted.
mkDescribeOrderableDBInstanceOptions ::
  -- | 'engine'
  Types.Engine ->
  DescribeOrderableDBInstanceOptions
mkDescribeOrderableDBInstanceOptions engine =
  DescribeOrderableDBInstanceOptions'
    { engine,
      availabilityZoneGroup = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      engineVersion = Core.Nothing,
      filters = Core.Nothing,
      licenseModel = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      vpc = Core.Nothing
    }

-- | The name of the engine to retrieve DB instance options for.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioEngine :: Lens.Lens' DescribeOrderableDBInstanceOptions Types.Engine
dodbioEngine = Lens.field @"engine"
{-# DEPRECATED dodbioEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The Availability Zone group associated with a Local Zone. Specify this parameter to retrieve available offerings for the Local Zones in the group.
--
-- Omit this parameter to show the available offerings in the specified AWS Region.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioAvailabilityZoneGroup :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Types.AvailabilityZoneGroup)
dodbioAvailabilityZoneGroup = Lens.field @"availabilityZoneGroup"
{-# DEPRECATED dodbioAvailabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead." #-}

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioDBInstanceClass :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Types.DBInstanceClass)
dodbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED dodbioDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioEngineVersion :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Types.EngineVersion)
dodbioEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dodbioEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioFilters :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe [Types.Filter])
dodbioFilters = Lens.field @"filters"
{-# DEPRECATED dodbioFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioLicenseModel :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Types.LicenseModel)
dodbioLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED dodbioLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioMarker :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Types.Marker)
dodbioMarker = Lens.field @"marker"
{-# DEPRECATED dodbioMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioMaxRecords :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Int)
dodbioMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dodbioMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to show only VPC or non-VPC offerings.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioVpc :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Bool)
dodbioVpc = Lens.field @"vpc"
{-# DEPRECATED dodbioVpc "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.AWSRequest DescribeOrderableDBInstanceOptions where
  type
    Rs DescribeOrderableDBInstanceOptions =
      DescribeOrderableDBInstanceOptionsResponse
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
            ( Core.pure ("Action", "DescribeOrderableDBInstanceOptions")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> ( Core.toQueryValue "AvailabilityZoneGroup"
                            Core.<$> availabilityZoneGroup
                        )
                Core.<> (Core.toQueryValue "DBInstanceClass" Core.<$> dBInstanceClass)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "LicenseModel" Core.<$> licenseModel)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "Vpc" Core.<$> vpc)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeOrderableDBInstanceOptionsResult"
      ( \s h x ->
          DescribeOrderableDBInstanceOptionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "OrderableDBInstanceOptions"
                         Core..<@> Core.parseXMLList "OrderableDBInstanceOption"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeOrderableDBInstanceOptions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"orderableDBInstanceOptions" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'mkDescribeOrderableDBInstanceOptionsResponse' smart constructor.
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse'
  { -- | An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | An @OrderableDBInstanceOption@ structure containing information about orderable options for the DB instance.
    orderableDBInstanceOptions :: Core.Maybe [Types.OrderableDBInstanceOption],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableDBInstanceOptionsResponse' value with any optional fields omitted.
mkDescribeOrderableDBInstanceOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeOrderableDBInstanceOptionsResponse
mkDescribeOrderableDBInstanceOptionsResponse responseStatus =
  DescribeOrderableDBInstanceOptionsResponse'
    { marker =
        Core.Nothing,
      orderableDBInstanceOptions = Core.Nothing,
      responseStatus
    }

-- | An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbiorrsMarker :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Core.Maybe Types.String)
dodbiorrsMarker = Lens.field @"marker"
{-# DEPRECATED dodbiorrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An @OrderableDBInstanceOption@ structure containing information about orderable options for the DB instance.
--
-- /Note:/ Consider using 'orderableDBInstanceOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbiorrsOrderableDBInstanceOptions :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Core.Maybe [Types.OrderableDBInstanceOption])
dodbiorrsOrderableDBInstanceOptions = Lens.field @"orderableDBInstanceOptions"
{-# DEPRECATED dodbiorrsOrderableDBInstanceOptions "Use generic-lens or generic-optics with 'orderableDBInstanceOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbiorrsResponseStatus :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse Core.Int
dodbiorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dodbiorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
