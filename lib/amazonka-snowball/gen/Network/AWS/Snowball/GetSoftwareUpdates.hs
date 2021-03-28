{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetSoftwareUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon S3 presigned URL for an update file associated with a specified @JobId@ .
module Network.AWS.Snowball.GetSoftwareUpdates
    (
    -- * Creating a request
      GetSoftwareUpdates (..)
    , mkGetSoftwareUpdates
    -- ** Request lenses
    , gsuJobId

    -- * Destructuring the response
    , GetSoftwareUpdatesResponse (..)
    , mkGetSoftwareUpdatesResponse
    -- ** Response lenses
    , grsUpdatesURI
    , grsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkGetSoftwareUpdates' smart constructor.
newtype GetSoftwareUpdates = GetSoftwareUpdates'
  { jobId :: Types.JobId
    -- ^ The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSoftwareUpdates' value with any optional fields omitted.
mkGetSoftwareUpdates
    :: Types.JobId -- ^ 'jobId'
    -> GetSoftwareUpdates
mkGetSoftwareUpdates jobId = GetSoftwareUpdates'{jobId}

-- | The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsuJobId :: Lens.Lens' GetSoftwareUpdates Types.JobId
gsuJobId = Lens.field @"jobId"
{-# INLINEABLE gsuJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery GetSoftwareUpdates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSoftwareUpdates where
        toHeaders GetSoftwareUpdates{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.GetSoftwareUpdates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSoftwareUpdates where
        toJSON GetSoftwareUpdates{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest GetSoftwareUpdates where
        type Rs GetSoftwareUpdates = GetSoftwareUpdatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSoftwareUpdatesResponse' Core.<$>
                   (x Core..:? "UpdatesURI") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSoftwareUpdatesResponse' smart constructor.
data GetSoftwareUpdatesResponse = GetSoftwareUpdatesResponse'
  { updatesURI :: Core.Maybe Core.Text
    -- ^ The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSoftwareUpdatesResponse' value with any optional fields omitted.
mkGetSoftwareUpdatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSoftwareUpdatesResponse
mkGetSoftwareUpdatesResponse responseStatus
  = GetSoftwareUpdatesResponse'{updatesURI = Core.Nothing,
                                responseStatus}

-- | The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
--
-- /Note:/ Consider using 'updatesURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsUpdatesURI :: Lens.Lens' GetSoftwareUpdatesResponse (Core.Maybe Core.Text)
grsUpdatesURI = Lens.field @"updatesURI"
{-# INLINEABLE grsUpdatesURI #-}
{-# DEPRECATED updatesURI "Use generic-lens or generic-optics with 'updatesURI' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSoftwareUpdatesResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
