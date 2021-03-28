{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeOrderableDBInstanceOptions (..)
    , mkDescribeOrderableDBInstanceOptions
    -- ** Request lenses
    , dodbioEngine
    , dodbioAvailabilityZoneGroup
    , dodbioDBInstanceClass
    , dodbioEngineVersion
    , dodbioFilters
    , dodbioLicenseModel
    , dodbioMarker
    , dodbioMaxRecords
    , dodbioVpc

    -- * Destructuring the response
    , DescribeOrderableDBInstanceOptionsResponse (..)
    , mkDescribeOrderableDBInstanceOptionsResponse
    -- ** Response lenses
    , dodbiorrsMarker
    , dodbiorrsOrderableDBInstanceOptions
    , dodbiorrsResponseStatus
    ) where

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
  { engine :: Core.Text
    -- ^ The name of the engine to retrieve DB instance options for.
  , availabilityZoneGroup :: Core.Maybe Core.Text
    -- ^ The Availability Zone group associated with a Local Zone. Specify this parameter to retrieve available offerings for the Local Zones in the group.
--
-- Omit this parameter to show the available offerings in the specified AWS Region.
  , dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , licenseModel :: Core.Maybe Core.Text
    -- ^ The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , vpc :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to show only VPC or non-VPC offerings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableDBInstanceOptions' value with any optional fields omitted.
mkDescribeOrderableDBInstanceOptions
    :: Core.Text -- ^ 'engine'
    -> DescribeOrderableDBInstanceOptions
mkDescribeOrderableDBInstanceOptions engine
  = DescribeOrderableDBInstanceOptions'{engine,
                                        availabilityZoneGroup = Core.Nothing,
                                        dBInstanceClass = Core.Nothing,
                                        engineVersion = Core.Nothing, filters = Core.Nothing,
                                        licenseModel = Core.Nothing, marker = Core.Nothing,
                                        maxRecords = Core.Nothing, vpc = Core.Nothing}

-- | The name of the engine to retrieve DB instance options for.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioEngine :: Lens.Lens' DescribeOrderableDBInstanceOptions Core.Text
dodbioEngine = Lens.field @"engine"
{-# INLINEABLE dodbioEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The Availability Zone group associated with a Local Zone. Specify this parameter to retrieve available offerings for the Local Zones in the group.
--
-- Omit this parameter to show the available offerings in the specified AWS Region.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioAvailabilityZoneGroup :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
dodbioAvailabilityZoneGroup = Lens.field @"availabilityZoneGroup"
{-# INLINEABLE dodbioAvailabilityZoneGroup #-}
{-# DEPRECATED availabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead"  #-}

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioDBInstanceClass :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
dodbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE dodbioDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioEngineVersion :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
dodbioEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE dodbioEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioFilters :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe [Types.Filter])
dodbioFilters = Lens.field @"filters"
{-# INLINEABLE dodbioFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioLicenseModel :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
dodbioLicenseModel = Lens.field @"licenseModel"
{-# INLINEABLE dodbioLicenseModel #-}
{-# DEPRECATED licenseModel "Use generic-lens or generic-optics with 'licenseModel' instead"  #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioMarker :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
dodbioMarker = Lens.field @"marker"
{-# INLINEABLE dodbioMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioMaxRecords :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Int)
dodbioMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dodbioMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | A value that indicates whether to show only VPC or non-VPC offerings.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbioVpc :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Bool)
dodbioVpc = Lens.field @"vpc"
{-# INLINEABLE dodbioVpc #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

instance Core.ToQuery DescribeOrderableDBInstanceOptions where
        toQuery DescribeOrderableDBInstanceOptions{..}
          = Core.toQueryPair "Action"
              ("DescribeOrderableDBInstanceOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "Engine" engine
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZoneGroup")
                availabilityZoneGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceClass")
                dBInstanceClass
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LicenseModel")
                licenseModel
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Vpc") vpc

instance Core.ToHeaders DescribeOrderableDBInstanceOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeOrderableDBInstanceOptions where
        type Rs DescribeOrderableDBInstanceOptions =
             DescribeOrderableDBInstanceOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "DescribeOrderableDBInstanceOptionsResult"
              (\ s h x ->
                 DescribeOrderableDBInstanceOptionsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "OrderableDBInstanceOptions" Core..<@>
                       Core.parseXMLList "OrderableDBInstanceOption"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeOrderableDBInstanceOptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"orderableDBInstanceOptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeOrderableDBInstanceOptions@ action. 
--
-- /See:/ 'mkDescribeOrderableDBInstanceOptionsResponse' smart constructor.
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , orderableDBInstanceOptions :: Core.Maybe [Types.OrderableDBInstanceOption]
    -- ^ An @OrderableDBInstanceOption@ structure containing information about orderable options for the DB instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrderableDBInstanceOptionsResponse' value with any optional fields omitted.
mkDescribeOrderableDBInstanceOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrderableDBInstanceOptionsResponse
mkDescribeOrderableDBInstanceOptionsResponse responseStatus
  = DescribeOrderableDBInstanceOptionsResponse'{marker =
                                                  Core.Nothing,
                                                orderableDBInstanceOptions = Core.Nothing,
                                                responseStatus}

-- | An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbiorrsMarker :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Core.Maybe Core.Text)
dodbiorrsMarker = Lens.field @"marker"
{-# INLINEABLE dodbiorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | An @OrderableDBInstanceOption@ structure containing information about orderable options for the DB instance.
--
-- /Note:/ Consider using 'orderableDBInstanceOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbiorrsOrderableDBInstanceOptions :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Core.Maybe [Types.OrderableDBInstanceOption])
dodbiorrsOrderableDBInstanceOptions = Lens.field @"orderableDBInstanceOptions"
{-# INLINEABLE dodbiorrsOrderableDBInstanceOptions #-}
{-# DEPRECATED orderableDBInstanceOptions "Use generic-lens or generic-optics with 'orderableDBInstanceOptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodbiorrsResponseStatus :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse Core.Int
dodbiorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dodbiorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
