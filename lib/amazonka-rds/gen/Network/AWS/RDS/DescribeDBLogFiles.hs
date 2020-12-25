{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeDBLogFiles (..),
    mkDescribeDBLogFiles,

    -- ** Request lenses
    ddblfDBInstanceIdentifier,
    ddblfFileLastWritten,
    ddblfFileSize,
    ddblfFilenameContains,
    ddblfFilters,
    ddblfMarker,
    ddblfMaxRecords,

    -- * Destructuring the response
    DescribeDBLogFilesResponse (..),
    mkDescribeDBLogFilesResponse,

    -- ** Response lenses
    ddblfrrsDescribeDBLogFiles,
    ddblfrrsMarker,
    ddblfrrsResponseStatus,
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
-- /See:/ 'mkDescribeDBLogFiles' smart constructor.
data DescribeDBLogFiles = DescribeDBLogFiles'
  { -- | The customer-assigned name of the DB instance that contains the log files you want to list.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DBInstance.
    dBInstanceIdentifier :: Types.String,
    -- | Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
    fileLastWritten :: Core.Maybe Core.Integer,
    -- | Filters the available log files for files larger than the specified size.
    fileSize :: Core.Maybe Core.Integer,
    -- | Filters the available log files for log file names that contain the specified string.
    filenameContains :: Core.Maybe Types.String,
    -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBLogFiles' value with any optional fields omitted.
mkDescribeDBLogFiles ::
  -- | 'dBInstanceIdentifier'
  Types.String ->
  DescribeDBLogFiles
mkDescribeDBLogFiles dBInstanceIdentifier =
  DescribeDBLogFiles'
    { dBInstanceIdentifier,
      fileLastWritten = Core.Nothing,
      fileSize = Core.Nothing,
      filenameContains = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfDBInstanceIdentifier :: Lens.Lens' DescribeDBLogFiles Types.String
ddblfDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED ddblfDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

-- | Filters the available log files for files written since the specified date, in POSIX timestamp format with milliseconds.
--
-- /Note:/ Consider using 'fileLastWritten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFileLastWritten :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Integer)
ddblfFileLastWritten = Lens.field @"fileLastWritten"
{-# DEPRECATED ddblfFileLastWritten "Use generic-lens or generic-optics with 'fileLastWritten' instead." #-}

-- | Filters the available log files for files larger than the specified size.
--
-- /Note:/ Consider using 'fileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFileSize :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Integer)
ddblfFileSize = Lens.field @"fileSize"
{-# DEPRECATED ddblfFileSize "Use generic-lens or generic-optics with 'fileSize' instead." #-}

-- | Filters the available log files for log file names that contain the specified string.
--
-- /Note:/ Consider using 'filenameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFilenameContains :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Types.String)
ddblfFilenameContains = Lens.field @"filenameContains"
{-# DEPRECATED ddblfFilenameContains "Use generic-lens or generic-optics with 'filenameContains' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfFilters :: Lens.Lens' DescribeDBLogFiles (Core.Maybe [Types.Filter])
ddblfFilters = Lens.field @"filters"
{-# DEPRECATED ddblfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to MaxRecords.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfMarker :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Types.String)
ddblfMarker = Lens.field @"marker"
{-# DEPRECATED ddblfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfMaxRecords :: Lens.Lens' DescribeDBLogFiles (Core.Maybe Core.Int)
ddblfMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddblfMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBLogFiles where
  type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse
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
            ( Core.pure ("Action", "DescribeDBLogFiles")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBInstanceIdentifier" dBInstanceIdentifier)
                Core.<> (Core.toQueryValue "FileLastWritten" Core.<$> fileLastWritten)
                Core.<> (Core.toQueryValue "FileSize" Core.<$> fileSize)
                Core.<> (Core.toQueryValue "FilenameContains" Core.<$> filenameContains)
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
      "DescribeDBLogFilesResult"
      ( \s h x ->
          DescribeDBLogFilesResponse'
            Core.<$> ( x Core..@? "DescribeDBLogFiles"
                         Core..<@> Core.parseXMLList "DescribeDBLogFilesDetails"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBLogFiles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"describeDBLogFiles" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | The response from a call to @DescribeDBLogFiles@ .
--
-- /See:/ 'mkDescribeDBLogFilesResponse' smart constructor.
data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse'
  { -- | The DB log files returned.
    describeDBLogFiles :: Core.Maybe [Types.DescribeDBLogFilesDetails],
    -- | A pagination token that can be used in a later DescribeDBLogFiles request.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBLogFilesResponse' value with any optional fields omitted.
mkDescribeDBLogFilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBLogFilesResponse
mkDescribeDBLogFilesResponse responseStatus =
  DescribeDBLogFilesResponse'
    { describeDBLogFiles = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The DB log files returned.
--
-- /Note:/ Consider using 'describeDBLogFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfrrsDescribeDBLogFiles :: Lens.Lens' DescribeDBLogFilesResponse (Core.Maybe [Types.DescribeDBLogFilesDetails])
ddblfrrsDescribeDBLogFiles = Lens.field @"describeDBLogFiles"
{-# DEPRECATED ddblfrrsDescribeDBLogFiles "Use generic-lens or generic-optics with 'describeDBLogFiles' instead." #-}

-- | A pagination token that can be used in a later DescribeDBLogFiles request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfrrsMarker :: Lens.Lens' DescribeDBLogFilesResponse (Core.Maybe Types.String)
ddblfrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddblfrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfrrsResponseStatus :: Lens.Lens' DescribeDBLogFilesResponse Core.Int
ddblfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddblfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
