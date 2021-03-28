{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing job definition.
module Network.AWS.Glue.UpdateJob
    (
    -- * Creating a request
      UpdateJob (..)
    , mkUpdateJob
    -- ** Request lenses
    , ujJobName
    , ujJobUpdate

    -- * Destructuring the response
    , UpdateJobResponse (..)
    , mkUpdateJobResponse
    -- ** Response lenses
    , ujrrsJobName
    , ujrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { jobName :: Types.JobName
    -- ^ The name of the job definition to update.
  , jobUpdate :: Types.JobUpdate
    -- ^ Specifies the values with which to update the job definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJob' value with any optional fields omitted.
mkUpdateJob
    :: Types.JobName -- ^ 'jobName'
    -> Types.JobUpdate -- ^ 'jobUpdate'
    -> UpdateJob
mkUpdateJob jobName jobUpdate = UpdateJob'{jobName, jobUpdate}

-- | The name of the job definition to update.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobName :: Lens.Lens' UpdateJob Types.JobName
ujJobName = Lens.field @"jobName"
{-# INLINEABLE ujJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Specifies the values with which to update the job definition.
--
-- /Note:/ Consider using 'jobUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobUpdate :: Lens.Lens' UpdateJob Types.JobUpdate
ujJobUpdate = Lens.field @"jobUpdate"
{-# INLINEABLE ujJobUpdate #-}
{-# DEPRECATED jobUpdate "Use generic-lens or generic-optics with 'jobUpdate' instead"  #-}

instance Core.ToQuery UpdateJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateJob where
        toHeaders UpdateJob{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateJob where
        toJSON UpdateJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobName" Core..= jobName),
                  Core.Just ("JobUpdate" Core..= jobUpdate)])

instance Core.AWSRequest UpdateJob where
        type Rs UpdateJob = UpdateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateJobResponse' Core.<$>
                   (x Core..:? "JobName") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { jobName :: Core.Maybe Types.NameString
    -- ^ Returns the name of the updated job definition.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobResponse' value with any optional fields omitted.
mkUpdateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateJobResponse
mkUpdateJobResponse responseStatus
  = UpdateJobResponse'{jobName = Core.Nothing, responseStatus}

-- | Returns the name of the updated job definition.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsJobName :: Lens.Lens' UpdateJobResponse (Core.Maybe Types.NameString)
ujrrsJobName = Lens.field @"jobName"
{-# INLINEABLE ujrrsJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsResponseStatus :: Lens.Lens' UpdateJobResponse Core.Int
ujrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ujrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
