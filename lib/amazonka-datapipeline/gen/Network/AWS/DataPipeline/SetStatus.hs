{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests that the status of the specified physical or logical pipeline objects be updated in the specified pipeline. This update might not occur immediately, but is eventually consistent. The status that can be set depends on the type of object (for example, DataNode or Activity). You cannot perform this operation on @FINISHED@ pipelines and attempting to do so returns @InvalidRequestException@ .
module Network.AWS.DataPipeline.SetStatus
  ( -- * Creating a request
    SetStatus (..),
    mkSetStatus,

    -- ** Request lenses
    ssPipelineId,
    ssObjectIds,
    ssStatus,

    -- * Destructuring the response
    SetStatusResponse (..),
    mkSetStatusResponse,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetStatus.
--
-- /See:/ 'mkSetStatus' smart constructor.
data SetStatus = SetStatus'
  { -- | The ID of the pipeline that contains the objects.
    pipelineId :: Types.Id,
    -- | The IDs of the objects. The corresponding objects can be either physical or components, but not a mix of both types.
    objectIds :: [Types.Id],
    -- | The status to be set on all the objects specified in @objectIds@ . For components, use @PAUSE@ or @RESUME@ . For instances, use @TRY_CANCEL@ , @RERUN@ , or @MARK_FINISHED@ .
    status :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetStatus' value with any optional fields omitted.
mkSetStatus ::
  -- | 'pipelineId'
  Types.Id ->
  -- | 'status'
  Types.String ->
  SetStatus
mkSetStatus pipelineId status =
  SetStatus' {pipelineId, objectIds = Core.mempty, status}

-- | The ID of the pipeline that contains the objects.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPipelineId :: Lens.Lens' SetStatus Types.Id
ssPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED ssPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The IDs of the objects. The corresponding objects can be either physical or components, but not a mix of both types.
--
-- /Note:/ Consider using 'objectIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssObjectIds :: Lens.Lens' SetStatus [Types.Id]
ssObjectIds = Lens.field @"objectIds"
{-# DEPRECATED ssObjectIds "Use generic-lens or generic-optics with 'objectIds' instead." #-}

-- | The status to be set on all the objects specified in @objectIds@ . For components, use @PAUSE@ or @RESUME@ . For instances, use @TRY_CANCEL@ , @RERUN@ , or @MARK_FINISHED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' SetStatus Types.String
ssStatus = Lens.field @"status"
{-# DEPRECATED ssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON SetStatus where
  toJSON SetStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("objectIds" Core..= objectIds),
            Core.Just ("status" Core..= status)
          ]
      )

instance Core.AWSRequest SetStatus where
  type Rs SetStatus = SetStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.SetStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SetStatusResponse'

-- | /See:/ 'mkSetStatusResponse' smart constructor.
data SetStatusResponse = SetStatusResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetStatusResponse' value with any optional fields omitted.
mkSetStatusResponse ::
  SetStatusResponse
mkSetStatusResponse = SetStatusResponse'
