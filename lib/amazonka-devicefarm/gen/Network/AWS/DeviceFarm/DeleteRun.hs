{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the run, given the run ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteRun
  ( -- * Creating a request
    DeleteRun (..),
    mkDeleteRun,

    -- ** Request lenses
    drArn,

    -- * Destructuring the response
    DeleteRunResponse (..),
    mkDeleteRunResponse,

    -- ** Response lenses
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete run operation.
--
-- /See:/ 'mkDeleteRun' smart constructor.
newtype DeleteRun = DeleteRun'
  { -- | The Amazon Resource Name (ARN) for the run to delete.
    arn :: Types.AmazonResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRun' value with any optional fields omitted.
mkDeleteRun ::
  -- | 'arn'
  Types.AmazonResourceName ->
  DeleteRun
mkDeleteRun arn = DeleteRun' {arn}

-- | The Amazon Resource Name (ARN) for the run to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drArn :: Lens.Lens' DeleteRun Types.AmazonResourceName
drArn = Lens.field @"arn"
{-# DEPRECATED drArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON DeleteRun where
  toJSON DeleteRun {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteRun where
  type Rs DeleteRun = DeleteRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.DeleteRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRunResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a delete run request.
--
-- /See:/ 'mkDeleteRunResponse' smart constructor.
newtype DeleteRunResponse = DeleteRunResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRunResponse' value with any optional fields omitted.
mkDeleteRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRunResponse
mkDeleteRunResponse responseStatus =
  DeleteRunResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteRunResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
