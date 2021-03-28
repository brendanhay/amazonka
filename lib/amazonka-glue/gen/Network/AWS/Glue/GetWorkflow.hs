{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves resource metadata for a workflow.
module Network.AWS.Glue.GetWorkflow
    (
    -- * Creating a request
      GetWorkflow (..)
    , mkGetWorkflow
    -- ** Request lenses
    , gwName
    , gwIncludeGraph

    -- * Destructuring the response
    , GetWorkflowResponse (..)
    , mkGetWorkflowResponse
    -- ** Response lenses
    , gwrrsWorkflow
    , gwrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { name :: Types.Name
    -- ^ The name of the workflow to retrieve.
  , includeGraph :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include a graph when returning the workflow resource metadata.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflow' value with any optional fields omitted.
mkGetWorkflow
    :: Types.Name -- ^ 'name'
    -> GetWorkflow
mkGetWorkflow name
  = GetWorkflow'{name, includeGraph = Core.Nothing}

-- | The name of the workflow to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwName :: Lens.Lens' GetWorkflow Types.Name
gwName = Lens.field @"name"
{-# INLINEABLE gwName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies whether to include a graph when returning the workflow resource metadata.
--
-- /Note:/ Consider using 'includeGraph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwIncludeGraph :: Lens.Lens' GetWorkflow (Core.Maybe Core.Bool)
gwIncludeGraph = Lens.field @"includeGraph"
{-# INLINEABLE gwIncludeGraph #-}
{-# DEPRECATED includeGraph "Use generic-lens or generic-optics with 'includeGraph' instead"  #-}

instance Core.ToQuery GetWorkflow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetWorkflow where
        toHeaders GetWorkflow{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetWorkflow") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetWorkflow where
        toJSON GetWorkflow{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("IncludeGraph" Core..=) Core.<$> includeGraph])

instance Core.AWSRequest GetWorkflow where
        type Rs GetWorkflow = GetWorkflowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetWorkflowResponse' Core.<$>
                   (x Core..:? "Workflow") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { workflow :: Core.Maybe Types.Workflow
    -- ^ The resource metadata for the workflow.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetWorkflowResponse' value with any optional fields omitted.
mkGetWorkflowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetWorkflowResponse
mkGetWorkflowResponse responseStatus
  = GetWorkflowResponse'{workflow = Core.Nothing, responseStatus}

-- | The resource metadata for the workflow.
--
-- /Note:/ Consider using 'workflow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsWorkflow :: Lens.Lens' GetWorkflowResponse (Core.Maybe Types.Workflow)
gwrrsWorkflow = Lens.field @"workflow"
{-# INLINEABLE gwrrsWorkflow #-}
{-# DEPRECATED workflow "Use generic-lens or generic-optics with 'workflow' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrrsResponseStatus :: Lens.Lens' GetWorkflowResponse Core.Int
gwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
