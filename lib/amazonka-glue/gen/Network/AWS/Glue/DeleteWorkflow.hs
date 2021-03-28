{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow.
module Network.AWS.Glue.DeleteWorkflow
    (
    -- * Creating a request
      DeleteWorkflow (..)
    , mkDeleteWorkflow
    -- ** Request lenses
    , dwName

    -- * Destructuring the response
    , DeleteWorkflowResponse (..)
    , mkDeleteWorkflowResponse
    -- ** Response lenses
    , dwrrsName
    , dwrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWorkflow' smart constructor.
newtype DeleteWorkflow = DeleteWorkflow'
  { name :: Types.NameString
    -- ^ Name of the workflow to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkflow' value with any optional fields omitted.
mkDeleteWorkflow
    :: Types.NameString -- ^ 'name'
    -> DeleteWorkflow
mkDeleteWorkflow name = DeleteWorkflow'{name}

-- | Name of the workflow to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwName :: Lens.Lens' DeleteWorkflow Types.NameString
dwName = Lens.field @"name"
{-# INLINEABLE dwName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteWorkflow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteWorkflow where
        toHeaders DeleteWorkflow{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteWorkflow") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteWorkflow where
        toJSON DeleteWorkflow{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteWorkflow where
        type Rs DeleteWorkflow = DeleteWorkflowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteWorkflowResponse' Core.<$>
                   (x Core..:? "Name") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { name :: Core.Maybe Types.Name
    -- ^ Name of the workflow specified in input.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkflowResponse' value with any optional fields omitted.
mkDeleteWorkflowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteWorkflowResponse
mkDeleteWorkflowResponse responseStatus
  = DeleteWorkflowResponse'{name = Core.Nothing, responseStatus}

-- | Name of the workflow specified in input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsName :: Lens.Lens' DeleteWorkflowResponse (Core.Maybe Types.Name)
dwrrsName = Lens.field @"name"
{-# INLINEABLE dwrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsResponseStatus :: Lens.Lens' DeleteWorkflowResponse Core.Int
dwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
