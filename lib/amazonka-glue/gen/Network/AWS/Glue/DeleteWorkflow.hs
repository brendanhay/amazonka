{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteWorkflow (..),
    mkDeleteWorkflow,

    -- ** Request lenses
    dwName,

    -- * Destructuring the response
    DeleteWorkflowResponse (..),
    mkDeleteWorkflowResponse,

    -- ** Response lenses
    dwrrsName,
    dwrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWorkflow' smart constructor.
newtype DeleteWorkflow = DeleteWorkflow'
  { -- | Name of the workflow to be deleted.
    name :: Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkflow' value with any optional fields omitted.
mkDeleteWorkflow ::
  -- | 'name'
  Types.NameString ->
  DeleteWorkflow
mkDeleteWorkflow name = DeleteWorkflow' {name}

-- | Name of the workflow to be deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwName :: Lens.Lens' DeleteWorkflow Types.NameString
dwName = Lens.field @"name"
{-# DEPRECATED dwName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteWorkflow where
  toJSON DeleteWorkflow {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteWorkflow where
  type Rs DeleteWorkflow = DeleteWorkflowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteWorkflow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkflowResponse'
            Core.<$> (x Core..:? "Name") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { -- | Name of the workflow specified in input.
    name :: Core.Maybe Types.Name,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkflowResponse' value with any optional fields omitted.
mkDeleteWorkflowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteWorkflowResponse
mkDeleteWorkflowResponse responseStatus =
  DeleteWorkflowResponse' {name = Core.Nothing, responseStatus}

-- | Name of the workflow specified in input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsName :: Lens.Lens' DeleteWorkflowResponse (Core.Maybe Types.Name)
dwrrsName = Lens.field @"name"
{-# DEPRECATED dwrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsResponseStatus :: Lens.Lens' DeleteWorkflowResponse Core.Int
dwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
