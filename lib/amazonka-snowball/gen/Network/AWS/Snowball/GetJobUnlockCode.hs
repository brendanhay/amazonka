{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetJobUnlockCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @UnlockCode@ code value for the specified job. A particular @UnlockCode@ value can be accessed for up to 90 days after the associated job has been created.
--
-- The @UnlockCode@ value is a 29-character code with 25 alphanumeric characters and 4 hyphens. This code is used to decrypt the manifest file when it is passed along with the manifest to the Snow device through the Snowball client when the client is started for the first time.
-- As a best practice, we recommend that you don't save a copy of the @UnlockCode@ in the same location as the manifest file for that job. Saving these separately helps prevent unauthorized parties from gaining access to the Snow device associated with that job.
module Network.AWS.Snowball.GetJobUnlockCode
    (
    -- * Creating a request
      GetJobUnlockCode (..)
    , mkGetJobUnlockCode
    -- ** Request lenses
    , gjucJobId

    -- * Destructuring the response
    , GetJobUnlockCodeResponse (..)
    , mkGetJobUnlockCodeResponse
    -- ** Response lenses
    , gjucrrsUnlockCode
    , gjucrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkGetJobUnlockCode' smart constructor.
newtype GetJobUnlockCode = GetJobUnlockCode'
  { jobId :: Types.JobId
    -- ^ The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobUnlockCode' value with any optional fields omitted.
mkGetJobUnlockCode
    :: Types.JobId -- ^ 'jobId'
    -> GetJobUnlockCode
mkGetJobUnlockCode jobId = GetJobUnlockCode'{jobId}

-- | The ID for the job that you want to get the @UnlockCode@ value for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucJobId :: Lens.Lens' GetJobUnlockCode Types.JobId
gjucJobId = Lens.field @"jobId"
{-# INLINEABLE gjucJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

instance Core.ToQuery GetJobUnlockCode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetJobUnlockCode where
        toHeaders GetJobUnlockCode{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.GetJobUnlockCode")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetJobUnlockCode where
        toJSON GetJobUnlockCode{..}
          = Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest GetJobUnlockCode where
        type Rs GetJobUnlockCode = GetJobUnlockCodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJobUnlockCodeResponse' Core.<$>
                   (x Core..:? "UnlockCode") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetJobUnlockCodeResponse' smart constructor.
data GetJobUnlockCodeResponse = GetJobUnlockCodeResponse'
  { unlockCode :: Core.Maybe Core.Text
    -- ^ The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobUnlockCodeResponse' value with any optional fields omitted.
mkGetJobUnlockCodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetJobUnlockCodeResponse
mkGetJobUnlockCodeResponse responseStatus
  = GetJobUnlockCodeResponse'{unlockCode = Core.Nothing,
                              responseStatus}

-- | The @UnlockCode@ value for the specified job. The @UnlockCode@ value can be accessed for up to 90 days after the job has been created.
--
-- /Note:/ Consider using 'unlockCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucrrsUnlockCode :: Lens.Lens' GetJobUnlockCodeResponse (Core.Maybe Core.Text)
gjucrrsUnlockCode = Lens.field @"unlockCode"
{-# INLINEABLE gjucrrsUnlockCode #-}
{-# DEPRECATED unlockCode "Use generic-lens or generic-optics with 'unlockCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjucrrsResponseStatus :: Lens.Lens' GetJobUnlockCodeResponse Core.Int
gjucrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjucrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
