{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the import jobs for an application.
module Network.AWS.Pinpoint.GetImportJobs
    (
    -- * Creating a request
      GetImportJobs (..)
    , mkGetImportJobs
    -- ** Request lenses
    , gijsApplicationId
    , gijsPageSize
    , gijsToken

    -- * Destructuring the response
    , GetImportJobsResponse (..)
    , mkGetImportJobsResponse
    -- ** Response lenses
    , gijrfrsImportJobsResponse
    , gijrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , token :: Core.Maybe Core.Text
    -- ^ The NextToken string that specifies which page of results to return in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetImportJobs' value with any optional fields omitted.
mkGetImportJobs
    :: Core.Text -- ^ 'applicationId'
    -> GetImportJobs
mkGetImportJobs applicationId
  = GetImportJobs'{applicationId, pageSize = Core.Nothing,
                   token = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsApplicationId :: Lens.Lens' GetImportJobs Core.Text
gijsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gijsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsPageSize :: Lens.Lens' GetImportJobs (Core.Maybe Core.Text)
gijsPageSize = Lens.field @"pageSize"
{-# INLINEABLE gijsPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsToken :: Lens.Lens' GetImportJobs (Core.Maybe Core.Text)
gijsToken = Lens.field @"token"
{-# INLINEABLE gijsToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery GetImportJobs where
        toQuery GetImportJobs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "token") token

instance Core.ToHeaders GetImportJobs where
        toHeaders GetImportJobs{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetImportJobs where
        type Rs GetImportJobs = GetImportJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/jobs/import",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetImportJobsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { importJobsResponse :: Types.ImportJobsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetImportJobsResponse' value with any optional fields omitted.
mkGetImportJobsResponse
    :: Types.ImportJobsResponse -- ^ 'importJobsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetImportJobsResponse
mkGetImportJobsResponse importJobsResponse responseStatus
  = GetImportJobsResponse'{importJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrfrsImportJobsResponse :: Lens.Lens' GetImportJobsResponse Types.ImportJobsResponse
gijrfrsImportJobsResponse = Lens.field @"importJobsResponse"
{-# INLINEABLE gijrfrsImportJobsResponse #-}
{-# DEPRECATED importJobsResponse "Use generic-lens or generic-optics with 'importJobsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrfrsResponseStatus :: Lens.Lens' GetImportJobsResponse Core.Int
gijrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gijrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
