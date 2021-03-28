{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeTableRestoreStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of one or more table restore requests made using the 'RestoreTableFromClusterSnapshot' API action. If you don't specify a value for the @TableRestoreRequestId@ parameter, then @DescribeTableRestoreStatus@ returns the status of all table restore requests ordered by the date and time of the request in ascending order. Otherwise @DescribeTableRestoreStatus@ returns the status of the table specified by @TableRestoreRequestId@ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeTableRestoreStatus
    (
    -- * Creating a request
      DescribeTableRestoreStatus (..)
    , mkDescribeTableRestoreStatus
    -- ** Request lenses
    , dtrsClusterIdentifier
    , dtrsMarker
    , dtrsMaxRecords
    , dtrsTableRestoreRequestId

    -- * Destructuring the response
    , DescribeTableRestoreStatusResponse (..)
    , mkDescribeTableRestoreStatusResponse
    -- ** Response lenses
    , dtrsrrsMarker
    , dtrsrrsTableRestoreStatusDetails
    , dtrsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeTableRestoreStatus' smart constructor.
data DescribeTableRestoreStatus = DescribeTableRestoreStatus'
  { clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The Amazon Redshift cluster that the table is being restored to.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
  , tableRestoreRequestId :: Core.Maybe Core.Text
    -- ^ The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTableRestoreStatus' value with any optional fields omitted.
mkDescribeTableRestoreStatus
    :: DescribeTableRestoreStatus
mkDescribeTableRestoreStatus
  = DescribeTableRestoreStatus'{clusterIdentifier = Core.Nothing,
                                marker = Core.Nothing, maxRecords = Core.Nothing,
                                tableRestoreRequestId = Core.Nothing}

-- | The Amazon Redshift cluster that the table is being restored to.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsClusterIdentifier :: Lens.Lens' DescribeTableRestoreStatus (Core.Maybe Core.Text)
dtrsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE dtrsClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeTableRestoreStatus@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsMarker :: Lens.Lens' DescribeTableRestoreStatus (Core.Maybe Core.Text)
dtrsMarker = Lens.field @"marker"
{-# INLINEABLE dtrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsMaxRecords :: Lens.Lens' DescribeTableRestoreStatus (Core.Maybe Core.Int)
dtrsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dtrsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier of the table restore request to return status for. If you don't specify a @TableRestoreRequestId@ value, then @DescribeTableRestoreStatus@ returns the status of all in-progress table restore requests.
--
-- /Note:/ Consider using 'tableRestoreRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTableRestoreRequestId :: Lens.Lens' DescribeTableRestoreStatus (Core.Maybe Core.Text)
dtrsTableRestoreRequestId = Lens.field @"tableRestoreRequestId"
{-# INLINEABLE dtrsTableRestoreRequestId #-}
{-# DEPRECATED tableRestoreRequestId "Use generic-lens or generic-optics with 'tableRestoreRequestId' instead"  #-}

instance Core.ToQuery DescribeTableRestoreStatus where
        toQuery DescribeTableRestoreStatus{..}
          = Core.toQueryPair "Action"
              ("DescribeTableRestoreStatus" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterIdentifier")
                clusterIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TableRestoreRequestId")
                tableRestoreRequestId

instance Core.ToHeaders DescribeTableRestoreStatus where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTableRestoreStatus where
        type Rs DescribeTableRestoreStatus =
             DescribeTableRestoreStatusResponse
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
          = Response.receiveXMLWrapper "DescribeTableRestoreStatusResult"
              (\ s h x ->
                 DescribeTableRestoreStatusResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "TableRestoreStatusDetails" Core..<@>
                       Core.parseXMLList "TableRestoreStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTableRestoreStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"tableRestoreStatusDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeTableRestoreStatusResponse' smart constructor.
data DescribeTableRestoreStatusResponse = DescribeTableRestoreStatusResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
  , tableRestoreStatusDetails :: Core.Maybe [Types.TableRestoreStatus]
    -- ^ A list of status details for one or more table restore requests.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTableRestoreStatusResponse' value with any optional fields omitted.
mkDescribeTableRestoreStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTableRestoreStatusResponse
mkDescribeTableRestoreStatusResponse responseStatus
  = DescribeTableRestoreStatusResponse'{marker = Core.Nothing,
                                        tableRestoreStatusDetails = Core.Nothing, responseStatus}

-- | A pagination token that can be used in a subsequent 'DescribeTableRestoreStatus' request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsrrsMarker :: Lens.Lens' DescribeTableRestoreStatusResponse (Core.Maybe Core.Text)
dtrsrrsMarker = Lens.field @"marker"
{-# INLINEABLE dtrsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of status details for one or more table restore requests.
--
-- /Note:/ Consider using 'tableRestoreStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsrrsTableRestoreStatusDetails :: Lens.Lens' DescribeTableRestoreStatusResponse (Core.Maybe [Types.TableRestoreStatus])
dtrsrrsTableRestoreStatusDetails = Lens.field @"tableRestoreStatusDetails"
{-# INLINEABLE dtrsrrsTableRestoreStatusDetails #-}
{-# DEPRECATED tableRestoreStatusDetails "Use generic-lens or generic-optics with 'tableRestoreStatusDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsrrsResponseStatus :: Lens.Lens' DescribeTableRestoreStatusResponse Core.Int
dtrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
