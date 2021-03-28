{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads all or a portion of the specified log file, up to 1 MB in size.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DownloadDBLogFilePortion
    (
    -- * Creating a request
      DownloadDBLogFilePortion (..)
    , mkDownloadDBLogFilePortion
    -- ** Request lenses
    , ddblfpDBInstanceIdentifier
    , ddblfpLogFileName
    , ddblfpMarker
    , ddblfpNumberOfLines

    -- * Destructuring the response
    , DownloadDBLogFilePortionResponse (..)
    , mkDownloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddblfprrsAdditionalDataPending
    , ddblfprrsLogFileData
    , ddblfprrsMarker
    , ddblfprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDownloadDBLogFilePortion' smart constructor.
data DownloadDBLogFilePortion = DownloadDBLogFilePortion'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
  , logFileName :: Core.Text
    -- ^ The name of the log file to be downloaded.
  , marker :: Core.Maybe Core.Text
    -- ^ The pagination token provided in the previous request or "0". If the Marker parameter is specified the response includes only records beyond the marker until the end of the file or up to NumberOfLines.
  , numberOfLines :: Core.Maybe Core.Int
    -- ^ The number of lines to download. If the number of lines specified results in a file over 1 MB in size, the file is truncated at 1 MB in size.
--
-- If the NumberOfLines parameter is specified, then the block of lines returned can be from the beginning or the end of the log file, depending on the value of the Marker parameter.
--
--     * If neither Marker or NumberOfLines are specified, the entire log file is returned up to a maximum of 10000 lines, starting with the most recent log entries first.
--
--
--     * If NumberOfLines is specified and Marker isn't specified, then the most recent lines from the end of the log file are returned.
--
--
--     * If Marker is specified as "0", then the specified number of lines from the beginning of the log file are returned.
--
--
--     * You can download the log file in blocks of lines by specifying the size of the block using the NumberOfLines parameter, and by specifying a value of "0" for the Marker parameter in your first request. Include the Marker value returned in the response as the Marker value for the next request, continuing until the AdditionalDataPending response element returns false.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DownloadDBLogFilePortion' value with any optional fields omitted.
mkDownloadDBLogFilePortion
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> Core.Text -- ^ 'logFileName'
    -> DownloadDBLogFilePortion
mkDownloadDBLogFilePortion dBInstanceIdentifier logFileName
  = DownloadDBLogFilePortion'{dBInstanceIdentifier, logFileName,
                              marker = Core.Nothing, numberOfLines = Core.Nothing}

-- | The customer-assigned name of the DB instance that contains the log files you want to list.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfpDBInstanceIdentifier :: Lens.Lens' DownloadDBLogFilePortion Core.Text
ddblfpDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE ddblfpDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The name of the log file to be downloaded.
--
-- /Note:/ Consider using 'logFileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfpLogFileName :: Lens.Lens' DownloadDBLogFilePortion Core.Text
ddblfpLogFileName = Lens.field @"logFileName"
{-# INLINEABLE ddblfpLogFileName #-}
{-# DEPRECATED logFileName "Use generic-lens or generic-optics with 'logFileName' instead"  #-}

-- | The pagination token provided in the previous request or "0". If the Marker parameter is specified the response includes only records beyond the marker until the end of the file or up to NumberOfLines.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfpMarker :: Lens.Lens' DownloadDBLogFilePortion (Core.Maybe Core.Text)
ddblfpMarker = Lens.field @"marker"
{-# INLINEABLE ddblfpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The number of lines to download. If the number of lines specified results in a file over 1 MB in size, the file is truncated at 1 MB in size.
--
-- If the NumberOfLines parameter is specified, then the block of lines returned can be from the beginning or the end of the log file, depending on the value of the Marker parameter.
--
--     * If neither Marker or NumberOfLines are specified, the entire log file is returned up to a maximum of 10000 lines, starting with the most recent log entries first.
--
--
--     * If NumberOfLines is specified and Marker isn't specified, then the most recent lines from the end of the log file are returned.
--
--
--     * If Marker is specified as "0", then the specified number of lines from the beginning of the log file are returned.
--
--
--     * You can download the log file in blocks of lines by specifying the size of the block using the NumberOfLines parameter, and by specifying a value of "0" for the Marker parameter in your first request. Include the Marker value returned in the response as the Marker value for the next request, continuing until the AdditionalDataPending response element returns false.
--
--
--
-- /Note:/ Consider using 'numberOfLines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfpNumberOfLines :: Lens.Lens' DownloadDBLogFilePortion (Core.Maybe Core.Int)
ddblfpNumberOfLines = Lens.field @"numberOfLines"
{-# INLINEABLE ddblfpNumberOfLines #-}
{-# DEPRECATED numberOfLines "Use generic-lens or generic-optics with 'numberOfLines' instead"  #-}

instance Core.ToQuery DownloadDBLogFilePortion where
        toQuery DownloadDBLogFilePortion{..}
          = Core.toQueryPair "Action"
              ("DownloadDBLogFilePortion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<> Core.toQueryPair "LogFileName" logFileName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumberOfLines")
                numberOfLines

instance Core.ToHeaders DownloadDBLogFilePortion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DownloadDBLogFilePortion where
        type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse
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
          = Response.receiveXMLWrapper "DownloadDBLogFilePortionResult"
              (\ s h x ->
                 DownloadDBLogFilePortionResponse' Core.<$>
                   (x Core..@? "AdditionalDataPending") Core.<*>
                     x Core..@? "LogFileData"
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DownloadDBLogFilePortion where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"additionalDataPending") =
            Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | This data type is used as a response element to @DownloadDBLogFilePortion@ .
--
-- /See:/ 'mkDownloadDBLogFilePortionResponse' smart constructor.
data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse'
  { additionalDataPending :: Core.Maybe Core.Bool
    -- ^ Boolean value that if true, indicates there is more data to be downloaded.
  , logFileData :: Core.Maybe Core.Text
    -- ^ Entries from the specified log file.
  , marker :: Core.Maybe Core.Text
    -- ^ A pagination token that can be used in a later DownloadDBLogFilePortion request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DownloadDBLogFilePortionResponse' value with any optional fields omitted.
mkDownloadDBLogFilePortionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DownloadDBLogFilePortionResponse
mkDownloadDBLogFilePortionResponse responseStatus
  = DownloadDBLogFilePortionResponse'{additionalDataPending =
                                        Core.Nothing,
                                      logFileData = Core.Nothing, marker = Core.Nothing,
                                      responseStatus}

-- | Boolean value that if true, indicates there is more data to be downloaded.
--
-- /Note:/ Consider using 'additionalDataPending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfprrsAdditionalDataPending :: Lens.Lens' DownloadDBLogFilePortionResponse (Core.Maybe Core.Bool)
ddblfprrsAdditionalDataPending = Lens.field @"additionalDataPending"
{-# INLINEABLE ddblfprrsAdditionalDataPending #-}
{-# DEPRECATED additionalDataPending "Use generic-lens or generic-optics with 'additionalDataPending' instead"  #-}

-- | Entries from the specified log file.
--
-- /Note:/ Consider using 'logFileData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfprrsLogFileData :: Lens.Lens' DownloadDBLogFilePortionResponse (Core.Maybe Core.Text)
ddblfprrsLogFileData = Lens.field @"logFileData"
{-# INLINEABLE ddblfprrsLogFileData #-}
{-# DEPRECATED logFileData "Use generic-lens or generic-optics with 'logFileData' instead"  #-}

-- | A pagination token that can be used in a later DownloadDBLogFilePortion request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfprrsMarker :: Lens.Lens' DownloadDBLogFilePortionResponse (Core.Maybe Core.Text)
ddblfprrsMarker = Lens.field @"marker"
{-# INLINEABLE ddblfprrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddblfprrsResponseStatus :: Lens.Lens' DownloadDBLogFilePortionResponse Core.Int
ddblfprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddblfprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
