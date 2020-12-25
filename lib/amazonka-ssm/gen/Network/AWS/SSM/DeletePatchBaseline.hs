{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeletePatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a patch baseline.
module Network.AWS.SSM.DeletePatchBaseline
  ( -- * Creating a request
    DeletePatchBaseline (..),
    mkDeletePatchBaseline,

    -- ** Request lenses
    dpbBaselineId,

    -- * Destructuring the response
    DeletePatchBaselineResponse (..),
    mkDeletePatchBaselineResponse,

    -- ** Response lenses
    dpbrrsBaselineId,
    dpbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeletePatchBaseline' smart constructor.
newtype DeletePatchBaseline = DeletePatchBaseline'
  { -- | The ID of the patch baseline to delete.
    baselineId :: Types.BaselineId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePatchBaseline' value with any optional fields omitted.
mkDeletePatchBaseline ::
  -- | 'baselineId'
  Types.BaselineId ->
  DeletePatchBaseline
mkDeletePatchBaseline baselineId = DeletePatchBaseline' {baselineId}

-- | The ID of the patch baseline to delete.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbBaselineId :: Lens.Lens' DeletePatchBaseline Types.BaselineId
dpbBaselineId = Lens.field @"baselineId"
{-# DEPRECATED dpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Core.FromJSON DeletePatchBaseline where
  toJSON DeletePatchBaseline {..} =
    Core.object
      (Core.catMaybes [Core.Just ("BaselineId" Core..= baselineId)])

instance Core.AWSRequest DeletePatchBaseline where
  type Rs DeletePatchBaseline = DeletePatchBaselineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DeletePatchBaseline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePatchBaselineResponse'
            Core.<$> (x Core..:? "BaselineId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePatchBaselineResponse' smart constructor.
data DeletePatchBaselineResponse = DeletePatchBaselineResponse'
  { -- | The ID of the deleted patch baseline.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePatchBaselineResponse' value with any optional fields omitted.
mkDeletePatchBaselineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePatchBaselineResponse
mkDeletePatchBaselineResponse responseStatus =
  DeletePatchBaselineResponse'
    { baselineId = Core.Nothing,
      responseStatus
    }

-- | The ID of the deleted patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrrsBaselineId :: Lens.Lens' DeletePatchBaselineResponse (Core.Maybe Types.BaselineId)
dpbrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED dpbrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrrsResponseStatus :: Lens.Lens' DeletePatchBaselineResponse Core.Int
dpbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
