{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.GetDatasetContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of a data set as presigned URIs.
module Network.AWS.IoTAnalytics.GetDatasetContent
    (
    -- * Creating a request
      GetDatasetContent (..)
    , mkGetDatasetContent
    -- ** Request lenses
    , gdcDatasetName
    , gdcVersionId

    -- * Destructuring the response
    , GetDatasetContentResponse (..)
    , mkGetDatasetContentResponse
    -- ** Response lenses
    , gdcrrsEntries
    , gdcrrsStatus
    , gdcrrsTimestamp
    , gdcrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDatasetContent' smart constructor.
data GetDatasetContent = GetDatasetContent'
  { datasetName :: Types.DatasetName
    -- ^ The name of the data set whose contents are retrieved.
  , versionId :: Core.Maybe Types.DatasetContentVersion
    -- ^ The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatasetContent' value with any optional fields omitted.
mkGetDatasetContent
    :: Types.DatasetName -- ^ 'datasetName'
    -> GetDatasetContent
mkGetDatasetContent datasetName
  = GetDatasetContent'{datasetName, versionId = Core.Nothing}

-- | The name of the data set whose contents are retrieved.
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcDatasetName :: Lens.Lens' GetDatasetContent Types.DatasetName
gdcDatasetName = Lens.field @"datasetName"
{-# INLINEABLE gdcDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

-- | The version of the data set whose contents are retrieved. You can also use the strings "$LATEST" or "$LATEST_SUCCEEDED" to retrieve the contents of the latest or latest successfully completed data set. If not specified, "$LATEST_SUCCEEDED" is the default.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcVersionId :: Lens.Lens' GetDatasetContent (Core.Maybe Types.DatasetContentVersion)
gdcVersionId = Lens.field @"versionId"
{-# INLINEABLE gdcVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery GetDatasetContent where
        toQuery GetDatasetContent{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId

instance Core.ToHeaders GetDatasetContent where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetDatasetContent where
        type Rs GetDatasetContent = GetDatasetContentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/datasets/" Core.<> Core.toText datasetName Core.<> "/content",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDatasetContentResponse' Core.<$>
                   (x Core..:? "entries") Core.<*> x Core..:? "status" Core.<*>
                     x Core..:? "timestamp"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDatasetContentResponse' smart constructor.
data GetDatasetContentResponse = GetDatasetContentResponse'
  { entries :: Core.Maybe [Types.DatasetEntry]
    -- ^ A list of @DatasetEntry@ objects.
  , status :: Core.Maybe Types.DatasetContentStatus
    -- ^ The status of the data set content.
  , timestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the request was made.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDatasetContentResponse' value with any optional fields omitted.
mkGetDatasetContentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDatasetContentResponse
mkGetDatasetContentResponse responseStatus
  = GetDatasetContentResponse'{entries = Core.Nothing,
                               status = Core.Nothing, timestamp = Core.Nothing, responseStatus}

-- | A list of @DatasetEntry@ objects.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsEntries :: Lens.Lens' GetDatasetContentResponse (Core.Maybe [Types.DatasetEntry])
gdcrrsEntries = Lens.field @"entries"
{-# INLINEABLE gdcrrsEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

-- | The status of the data set content.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsStatus :: Lens.Lens' GetDatasetContentResponse (Core.Maybe Types.DatasetContentStatus)
gdcrrsStatus = Lens.field @"status"
{-# INLINEABLE gdcrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time when the request was made.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsTimestamp :: Lens.Lens' GetDatasetContentResponse (Core.Maybe Core.NominalDiffTime)
gdcrrsTimestamp = Lens.field @"timestamp"
{-# INLINEABLE gdcrrsTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsResponseStatus :: Lens.Lens' GetDatasetContentResponse Core.Int
gdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
