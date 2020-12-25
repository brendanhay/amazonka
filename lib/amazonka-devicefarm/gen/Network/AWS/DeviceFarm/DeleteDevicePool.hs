{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.
module Network.AWS.DeviceFarm.DeleteDevicePool
  ( -- * Creating a request
    DeleteDevicePool (..),
    mkDeleteDevicePool,

    -- ** Request lenses
    ddpArn,

    -- * Destructuring the response
    DeleteDevicePoolResponse (..),
    mkDeleteDevicePoolResponse,

    -- ** Response lenses
    ddprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete device pool operation.
--
-- /See:/ 'mkDeleteDevicePool' smart constructor.
newtype DeleteDevicePool = DeleteDevicePool'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevicePool' value with any optional fields omitted.
mkDeleteDevicePool ::
  -- | 'arn'
  Types.Arn ->
  DeleteDevicePool
mkDeleteDevicePool arn = DeleteDevicePool' {arn}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpArn :: Lens.Lens' DeleteDevicePool Types.Arn
ddpArn = Lens.field @"arn"
{-# DEPRECATED ddpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON DeleteDevicePool where
  toJSON DeleteDevicePool {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteDevicePool where
  type Rs DeleteDevicePool = DeleteDevicePoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.DeleteDevicePool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDevicePoolResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a delete device pool request.
--
-- /See:/ 'mkDeleteDevicePoolResponse' smart constructor.
newtype DeleteDevicePoolResponse = DeleteDevicePoolResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevicePoolResponse' value with any optional fields omitted.
mkDeleteDevicePoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDevicePoolResponse
mkDeleteDevicePoolResponse responseStatus =
  DeleteDevicePoolResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprrsResponseStatus :: Lens.Lens' DeleteDevicePoolResponse Core.Int
ddprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
