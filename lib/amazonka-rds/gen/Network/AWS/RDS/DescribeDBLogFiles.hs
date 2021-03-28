{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBLogFiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB log files for the DB instance.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBLogFiles
    (
    -- * Creating a request
      DescribeDBLogFiles (..)
    , mkDescribeDBLogFiles
    -- ** Request lenses
    , ddblfDBInstanceIdentifier
    , ddblfFileLastWritten
    , ddblfFileSize
    , ddblfFilenameContains
    , ddblfFilters
    , ddblfMarker
    , ddblfMaxRecords

    -- * Destructuring the response
    , DescribeDBLogFilesResponse (..)
    , mkDescribeDBLogFilesResponse
    -- ** Response lenses
    , ddblfrrsDescribeDBLogFiles
    , ddblfrrsMarker
    , ddblfrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBLogFiles' smart constructor.
data DescribeDBLogFiles = DescribeDBLogFiles'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
  , fileLastWritten :: Core.Maybe Core.Integer
    -- ^ Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
  , fileSize :: Core.Maybe Core.Integer
    -- ^ Filters the available log files for files larger than the specified size.
  , filenameContains :: Core.Maybe Core.Text
    -- ^ Filters the available log files for log file names that contain the specified string.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBLogFiles' value with any optional fields omitted.
mkDescribeDBLogFiles
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> DescribeDBLogFiles
mkDescribeDBLogFiles dBInstanceIdentifier
  = DescribeDBLogFiles'{dBInstanceIdentifier,
                        fileLastWritten = Core.Nothing, fileSize = Core.Nothing,
                        filenameContains = Core.Nothing, filters = Core.Nothing,
                        marker = Core.Nothing, maxRecords = Core.Nothing}

-- | The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfDBInstanceIdentifier :: Lens.Lens' DescribeDBLogFiles Core.Text
ddblfDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE ddblfDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
--
-- /Note:/ Consider using 'fileLastWritten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFileLastWritten :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Integer)
ddblfFileLastWritten = Lens.field @"fileLastWritten"
{-# INLINEABLE ddblfFileLastWritten #-}
{-# DEPRECATED fileLastWritten "Use generic-lens or generic-optics with 'fileLastWritten' instead"  #-}

-- | Filters the available log files for files larger than the specified size.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFileSize :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Integer)
ddblfFileSize = Lens.field @"fileSize"
{-# INLINEABLE ddblfFileSize #-}
{-# DEPRECATED fileSize "Use generic-lens or generic-optics with 'fileSize' instead"  #-}

-- | Filters the available log files for log file names that contain the specified string.
--
-- /Note:/ Consider using 'filenameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFilenameContains :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Text)
ddblfFilenameContains = Lens.field @"filenameContains"
{-# INLINEABLE ddblfFilenameContains #-}
{-# DEPRECATED filenameContains "Use generic-lens or generic-optics with 'filenameContains' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFilters :: Lens.Lens' DescribeDBLogFiles (Core.Maybe [Types.Filter])
ddblfFilters = Lens.field @"filters"
{-# INLINEABLE ddblfFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfMarker :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Text)
ddblfMarker = Lens.field @"marker"
{-# INLINEABLE ddblfMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfMaxRecords :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Int)
ddblfMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddblfMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBLogFiles where
        toQuery DescribeDBLogFiles{..}
          = Core.toQueryPair "Action" ("DescribeDBLogFiles" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FileLastWritten")
                fileLastWritten
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FileSize") fileSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FilenameContains")
                filenameContains
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBLogFiles where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBLogFiles where
        type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse
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
          = Response.receiveXMLWrapper "DescribeDBLogFilesResult"
              (\ s h x ->
                 DescribeDBLogFilesResponse' Core.<$>
                   (x Core..@? "DescribeDBLogFiles" Core..<@>
                      Core.parseXMLList "DescribeDBLogFilesDetails")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBLogFiles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"describeDBLogFiles" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | The response from a call to @DescribeDBLogFiles@ . 
--
-- /See:/ 'mkDescribeDBLogFilesResponse' smart constructor.
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse'
  { describeDBLogFiles :: Core.Maybe [Types.DescribeDBLogFilesDetails]
    -- ^ The DB log files returned.
  , marker :: Core.Maybe Core.Text
    -- ^ A pagination token that can be used in a later DescribeDBLogFiles request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBLogFilesResponse' value with any optional fields omitted.
mkDescribeDBLogFilesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBLogFilesResponse
mkDescribeDBLogFilesResponse responseStatus
  = DescribeDBLogFilesResponse'{describeDBLogFiles = Core.Nothing,
                                marker = Core.Nothing, responseStatus}

-- | The DB log files returned.
--
-- /Note:/ Consider using 'describeDBLogFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfrrsDescribeDBLogFiles :: Lens.Lens' DescribeDBLogFilesResponse (Core.Maybe [Types.DescribeDBLogFilesDetails])
ddblfrrsDescribeDBLogFiles = Lens.field @"describeDBLogFiles"
{-# INLINEABLE ddblfrrsDescribeDBLogFiles #-}
{-# DEPRECATED describeDBLogFiles "Use generic-lens or generic-optics with 'describeDBLogFiles' instead"  #-}

-- | A pagination token that can be used in a later DescribeDBLogFiles request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfrrsMarker :: Lens.Lens' DescribeDBLogFilesResponse (Core.Maybe Core.Text)
ddblfrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddblfrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfrrsResponseStatus :: Lens.Lens' DescribeDBLogFilesResponse Core.Int
ddblfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddblfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
