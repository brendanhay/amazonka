{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run. 
module Network.AWS.Glue.GetWorkflowRun
    (
    -- * Creating a request
      GetWorkflowRun (..)
    , mkGetWorkflowRun
    -- ** Request lenses
    , gwrfName
    , gwrfRunId
    , gwrfIncludeGraph

    -- * Destructuring the response
    , GetWorkflowRunResponse (..)
    , mkGetWorkflowRunResponse
    -- ** Response lenses
    , gwrrfrsRun
    , gwrrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'
  { name :: Types.Name
    -- ^ Name of the workflow being run.
  , runId :: Types.RunId
    -- ^ The ID of the workflow run.
  , includeGraph :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include the workflow graph in response or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflowRun' value with any optional fields omitted.
mkGetWorkflowRun
    :: Types.Name -- ^ 'name'
    -> Types.RunId -- ^ 'runId'
    -> GetWorkflowRun
mkGetWorkflowRun name runId
  = GetWorkflowRun'{name, runId, includeGraph = Core.Nothing}

-- | Name of the workflow being run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrfName :: Lens.Lens' GetWorkflowRun Types.Name
gwrfName = Lens.field @"name"
{-# INLINEABLE gwrfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the workflow run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrfRunId :: Lens.Lens' GetWorkflowRun Types.RunId
gwrfRunId = Lens.field @"runId"
{-# INLINEABLE gwrfRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

-- | Specifies whether to include the workflow graph in response or not.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrfIncludeGraph :: Lens.Lens' GetWorkflowRun (Core.Maybe Core.Bool)
gwrfIncludeGraph = Lens.field @"includeGraph"
{-# INLINEABLE gwrfIncludeGraph #-}
{-# DEPRECATED includeGraph "Use generic-lens or generic-optics with 'includeGraph' instead"  #-}

instance Core.ToQuery GetWorkflowRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetWorkflowRun where
        toHeaders GetWorkflowRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetWorkflowRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetWorkflowRun where
        toJSON GetWorkflowRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("RunId" Core..= runId),
                  ("IncludeGraph" Core..=) Core.<$> includeGraph])

instance Core.AWSRequest GetWorkflowRun where
        type Rs GetWorkflowRun = GetWorkflowRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetWorkflowRunResponse' Core.<$>
                   (x Core..:? "Run") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'
  { run :: Core.Maybe Types.WorkflowRun
    -- ^ The requested workflow run metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetWorkflowRunResponse' value with any optional fields omitted.
mkGetWorkflowRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetWorkflowRunResponse
mkGetWorkflowRunResponse responseStatus
  = GetWorkflowRunResponse'{run = Core.Nothing, responseStatus}

-- | The requested workflow run metadata.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrfrsRun :: Lens.Lens' GetWorkflowRunResponse (Core.Maybe Types.WorkflowRun)
gwrrfrsRun = Lens.field @"run"
{-# INLINEABLE gwrrfrsRun #-}
{-# DEPRECATED run "Use generic-lens or generic-optics with 'run' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrfrsResponseStatus :: Lens.Lens' GetWorkflowRunResponse Core.Int
gwrrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gwrrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
