{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the export jobs for an application.
module Network.AWS.Pinpoint.GetExportJobs
    (
    -- * Creating a request
      GetExportJobs (..)
    , mkGetExportJobs
    -- ** Request lenses
    , gejsApplicationId
    , gejsPageSize
    , gejsToken

    -- * Destructuring the response
    , GetExportJobsResponse (..)
    , mkGetExportJobsResponse
    -- ** Response lenses
    , gejrrsExportJobsResponse
    , gejrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExportJobs' smart constructor.
data GetExportJobs = GetExportJobs'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , token :: Core.Maybe Core.Text
    -- ^ The NextToken string that specifies which page of results to return in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportJobs' value with any optional fields omitted.
mkGetExportJobs
    :: Core.Text -- ^ 'applicationId'
    -> GetExportJobs
mkGetExportJobs applicationId
  = GetExportJobs'{applicationId, pageSize = Core.Nothing,
                   token = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsApplicationId :: Lens.Lens' GetExportJobs Core.Text
gejsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gejsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsPageSize :: Lens.Lens' GetExportJobs (Core.Maybe Core.Text)
gejsPageSize = Lens.field @"pageSize"
{-# INLINEABLE gejsPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsToken :: Lens.Lens' GetExportJobs (Core.Maybe Core.Text)
gejsToken = Lens.field @"token"
{-# INLINEABLE gejsToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery GetExportJobs where
        toQuery GetExportJobs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "token") token

instance Core.ToHeaders GetExportJobs where
        toHeaders GetExportJobs{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetExportJobs where
        type Rs GetExportJobs = GetExportJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/jobs/export",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetExportJobsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetExportJobsResponse' smart constructor.
data GetExportJobsResponse = GetExportJobsResponse'
  { exportJobsResponse :: Types.ExportJobsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportJobsResponse' value with any optional fields omitted.
mkGetExportJobsResponse
    :: Types.ExportJobsResponse -- ^ 'exportJobsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetExportJobsResponse
mkGetExportJobsResponse exportJobsResponse responseStatus
  = GetExportJobsResponse'{exportJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrrsExportJobsResponse :: Lens.Lens' GetExportJobsResponse Types.ExportJobsResponse
gejrrsExportJobsResponse = Lens.field @"exportJobsResponse"
{-# INLINEABLE gejrrsExportJobsResponse #-}
{-# DEPRECATED exportJobsResponse "Use generic-lens or generic-optics with 'exportJobsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrrsResponseStatus :: Lens.Lens' GetExportJobsResponse Core.Int
gejrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gejrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
