{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of the specified workflow run.
module Network.AWS.Glue.StopWorkflowRun
    (
    -- * Creating a request
      StopWorkflowRun (..)
    , mkStopWorkflowRun
    -- ** Request lenses
    , swrfName
    , swrfRunId

    -- * Destructuring the response
    , StopWorkflowRunResponse (..)
    , mkStopWorkflowRunResponse
    -- ** Response lenses
    , swrrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { name :: Types.Name
    -- ^ The name of the workflow to stop.
  , runId :: Types.RunId
    -- ^ The ID of the workflow run to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopWorkflowRun' value with any optional fields omitted.
mkStopWorkflowRun
    :: Types.Name -- ^ 'name'
    -> Types.RunId -- ^ 'runId'
    -> StopWorkflowRun
mkStopWorkflowRun name runId = StopWorkflowRun'{name, runId}

-- | The name of the workflow to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrfName :: Lens.Lens' StopWorkflowRun Types.Name
swrfName = Lens.field @"name"
{-# INLINEABLE swrfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the workflow run to stop.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrfRunId :: Lens.Lens' StopWorkflowRun Types.RunId
swrfRunId = Lens.field @"runId"
{-# INLINEABLE swrfRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.ToQuery StopWorkflowRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopWorkflowRun where
        toHeaders StopWorkflowRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StopWorkflowRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopWorkflowRun where
        toJSON StopWorkflowRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("RunId" Core..= runId)])

instance Core.AWSRequest StopWorkflowRun where
        type Rs StopWorkflowRun = StopWorkflowRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopWorkflowRunResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopWorkflowRunResponse' smart constructor.
newtype StopWorkflowRunResponse = StopWorkflowRunResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopWorkflowRunResponse' value with any optional fields omitted.
mkStopWorkflowRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopWorkflowRunResponse
mkStopWorkflowRunResponse responseStatus
  = StopWorkflowRunResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrfrsResponseStatus :: Lens.Lens' StopWorkflowRunResponse Core.Int
swrrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE swrrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
