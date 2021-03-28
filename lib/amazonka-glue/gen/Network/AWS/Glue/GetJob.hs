{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an existing job definition.
module Network.AWS.Glue.GetJob
    (
    -- * Creating a request
      GetJob (..)
    , mkGetJob
    -- ** Request lenses
    , gjJobName

    -- * Destructuring the response
    , GetJobResponse (..)
    , mkGetJobResponse
    -- ** Response lenses
    , gjrrsJob
    , gjrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJob' smart constructor.
newtype GetJob = GetJob'
  { jobName :: Types.JobName
    -- ^ The name of the job definition to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetJob' value with any optional fields omitted.
mkGetJob
    :: Types.JobName -- ^ 'jobName'
    -> GetJob
mkGetJob jobName = GetJob'{jobName}

-- | The name of the job definition to retrieve.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjJobName :: Lens.Lens' GetJob Types.JobName
gjJobName = Lens.field @"jobName"
{-# INLINEABLE gjJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

instance Core.ToQuery GetJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetJob where
        toHeaders GetJob{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetJob where
        toJSON GetJob{..}
          = Core.object
              (Core.catMaybes [Core.Just ("JobName" Core..= jobName)])

instance Core.AWSRequest GetJob where
        type Rs GetJob = GetJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetJobResponse' Core.<$>
                   (x Core..:? "Job") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { job :: Core.Maybe Types.Job
    -- ^ The requested job definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetJobResponse' value with any optional fields omitted.
mkGetJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetJobResponse
mkGetJobResponse responseStatus
  = GetJobResponse'{job = Core.Nothing, responseStatus}

-- | The requested job definition.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrsJob :: Lens.Lens' GetJobResponse (Core.Maybe Types.Job)
gjrrsJob = Lens.field @"job"
{-# INLINEABLE gjrrsJob #-}
{-# DEPRECATED job "Use generic-lens or generic-optics with 'job' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrsResponseStatus :: Lens.Lens' GetJobResponse Core.Int
gjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
