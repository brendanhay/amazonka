{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the import jobs for a segment.
module Network.AWS.Pinpoint.GetSegmentImportJobs
    (
    -- * Creating a request
      GetSegmentImportJobs (..)
    , mkGetSegmentImportJobs
    -- ** Request lenses
    , gsijSegmentId
    , gsijApplicationId
    , gsijPageSize
    , gsijToken

    -- * Destructuring the response
    , GetSegmentImportJobsResponse (..)
    , mkGetSegmentImportJobsResponse
    -- ** Response lenses
    , gsijrrsImportJobsResponse
    , gsijrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegmentImportJobs' smart constructor.
data GetSegmentImportJobs = GetSegmentImportJobs'
  { segmentId :: Core.Text
    -- ^ The unique identifier for the segment.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , token :: Core.Maybe Core.Text
    -- ^ The NextToken string that specifies which page of results to return in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentImportJobs' value with any optional fields omitted.
mkGetSegmentImportJobs
    :: Core.Text -- ^ 'segmentId'
    -> Core.Text -- ^ 'applicationId'
    -> GetSegmentImportJobs
mkGetSegmentImportJobs segmentId applicationId
  = GetSegmentImportJobs'{segmentId, applicationId,
                          pageSize = Core.Nothing, token = Core.Nothing}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijSegmentId :: Lens.Lens' GetSegmentImportJobs Core.Text
gsijSegmentId = Lens.field @"segmentId"
{-# INLINEABLE gsijSegmentId #-}
{-# DEPRECATED segmentId "Use generic-lens or generic-optics with 'segmentId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijApplicationId :: Lens.Lens' GetSegmentImportJobs Core.Text
gsijApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gsijApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijPageSize :: Lens.Lens' GetSegmentImportJobs (Core.Maybe Core.Text)
gsijPageSize = Lens.field @"pageSize"
{-# INLINEABLE gsijPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijToken :: Lens.Lens' GetSegmentImportJobs (Core.Maybe Core.Text)
gsijToken = Lens.field @"token"
{-# INLINEABLE gsijToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery GetSegmentImportJobs where
        toQuery GetSegmentImportJobs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "token") token

instance Core.ToHeaders GetSegmentImportJobs where
        toHeaders GetSegmentImportJobs{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSegmentImportJobs where
        type Rs GetSegmentImportJobs = GetSegmentImportJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/segments/"
                             Core.<> Core.toText segmentId
                             Core.<> "/jobs/import",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSegmentImportJobsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSegmentImportJobsResponse' smart constructor.
data GetSegmentImportJobsResponse = GetSegmentImportJobsResponse'
  { importJobsResponse :: Types.ImportJobsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentImportJobsResponse' value with any optional fields omitted.
mkGetSegmentImportJobsResponse
    :: Types.ImportJobsResponse -- ^ 'importJobsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetSegmentImportJobsResponse
mkGetSegmentImportJobsResponse importJobsResponse responseStatus
  = GetSegmentImportJobsResponse'{importJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijrrsImportJobsResponse :: Lens.Lens' GetSegmentImportJobsResponse Types.ImportJobsResponse
gsijrrsImportJobsResponse = Lens.field @"importJobsResponse"
{-# INLINEABLE gsijrrsImportJobsResponse #-}
{-# DEPRECATED importJobsResponse "Use generic-lens or generic-optics with 'importJobsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijrrsResponseStatus :: Lens.Lens' GetSegmentImportJobsResponse Core.Int
gsijrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsijrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
