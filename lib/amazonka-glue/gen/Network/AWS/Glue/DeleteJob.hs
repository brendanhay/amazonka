{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified job definition. If the job definition is not found, no exception is thrown.
module Network.AWS.Glue.DeleteJob
    (
    -- * Creating a request
      DeleteJob (..)
    , mkDeleteJob
    -- ** Request lenses
    , djJobName

    -- * Destructuring the response
    , DeleteJobResponse (..)
    , mkDeleteJobResponse
    -- ** Response lenses
    , djrrsJobName
    , djrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteJob' smart constructor.
newtype DeleteJob = DeleteJob'
  { jobName :: Types.JobName
    -- ^ The name of the job definition to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJob' value with any optional fields omitted.
mkDeleteJob
    :: Types.JobName -- ^ 'jobName'
    -> DeleteJob
mkDeleteJob jobName = DeleteJob'{jobName}

-- | The name of the job definition to delete.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobName :: Lens.Lens' DeleteJob Types.JobName
djJobName = Lens.field @"jobName"
{-# INLINEABLE djJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

instance Core.ToQuery DeleteJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteJob where
        toHeaders DeleteJob{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteJob where
        toJSON DeleteJob{..}
          = Core.object
              (Core.catMaybes [Core.Just ("JobName" Core..= jobName)])

instance Core.AWSRequest DeleteJob where
        type Rs DeleteJob = DeleteJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteJobResponse' Core.<$>
                   (x Core..:? "JobName") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { jobName :: Core.Maybe Types.NameString
    -- ^ The name of the job definition that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobResponse' value with any optional fields omitted.
mkDeleteJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteJobResponse
mkDeleteJobResponse responseStatus
  = DeleteJobResponse'{jobName = Core.Nothing, responseStatus}

-- | The name of the job definition that was deleted.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsJobName :: Lens.Lens' DeleteJobResponse (Core.Maybe Types.NameString)
djrrsJobName = Lens.field @"jobName"
{-# INLINEABLE djrrsJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsResponseStatus :: Lens.Lens' DeleteJobResponse Core.Int
djrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
