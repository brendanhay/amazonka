{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new run of the specified workflow.
module Network.AWS.Glue.StartWorkflowRun
    (
    -- * Creating a request
      StartWorkflowRun (..)
    , mkStartWorkflowRun
    -- ** Request lenses
    , swrName

    -- * Destructuring the response
    , StartWorkflowRunResponse (..)
    , mkStartWorkflowRunResponse
    -- ** Response lenses
    , swrrrsRunId
    , swrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartWorkflowRun' smart constructor.
newtype StartWorkflowRun = StartWorkflowRun'
  { name :: Types.NameString
    -- ^ The name of the workflow to start.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkflowRun' value with any optional fields omitted.
mkStartWorkflowRun
    :: Types.NameString -- ^ 'name'
    -> StartWorkflowRun
mkStartWorkflowRun name = StartWorkflowRun'{name}

-- | The name of the workflow to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrName :: Lens.Lens' StartWorkflowRun Types.NameString
swrName = Lens.field @"name"
{-# INLINEABLE swrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StartWorkflowRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartWorkflowRun where
        toHeaders StartWorkflowRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StartWorkflowRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartWorkflowRun where
        toJSON StartWorkflowRun{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartWorkflowRun where
        type Rs StartWorkflowRun = StartWorkflowRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartWorkflowRunResponse' Core.<$>
                   (x Core..:? "RunId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartWorkflowRunResponse' smart constructor.
data StartWorkflowRunResponse = StartWorkflowRunResponse'
  { runId :: Core.Maybe Types.RunId
    -- ^ An Id for the new run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkflowRunResponse' value with any optional fields omitted.
mkStartWorkflowRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartWorkflowRunResponse
mkStartWorkflowRunResponse responseStatus
  = StartWorkflowRunResponse'{runId = Core.Nothing, responseStatus}

-- | An Id for the new run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrrsRunId :: Lens.Lens' StartWorkflowRunResponse (Core.Maybe Types.RunId)
swrrrsRunId = Lens.field @"runId"
{-# INLINEABLE swrrrsRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrrsResponseStatus :: Lens.Lens' StartWorkflowRunResponse Core.Int
swrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE swrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
