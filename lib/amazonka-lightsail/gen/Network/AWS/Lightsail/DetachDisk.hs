{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.
--
-- The @detach disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DetachDisk
  ( -- * Creating a request
    DetachDisk (..),
    mkDetachDisk,

    -- ** Request lenses
    ddDiskName,

    -- * Destructuring the response
    DetachDiskResponse (..),
    mkDetachDiskResponse,

    -- ** Response lenses
    ddrrsOperations,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachDisk' smart constructor.
newtype DetachDisk = DetachDisk'
  { -- | The unique name of the disk you want to detach from your instance (e.g., @my-disk@ ).
    diskName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachDisk' value with any optional fields omitted.
mkDetachDisk ::
  -- | 'diskName'
  Types.ResourceName ->
  DetachDisk
mkDetachDisk diskName = DetachDisk' {diskName}

-- | The unique name of the disk you want to detach from your instance (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDiskName :: Lens.Lens' DetachDisk Types.ResourceName
ddDiskName = Lens.field @"diskName"
{-# DEPRECATED ddDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

instance Core.FromJSON DetachDisk where
  toJSON DetachDisk {..} =
    Core.object
      (Core.catMaybes [Core.Just ("diskName" Core..= diskName)])

instance Core.AWSRequest DetachDisk where
  type Rs DetachDisk = DetachDiskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DetachDisk")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachDiskResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachDiskResponse' smart constructor.
data DetachDiskResponse = DetachDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DetachDiskResponse' value with any optional fields omitted.
mkDetachDiskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachDiskResponse
mkDetachDiskResponse responseStatus =
  DetachDiskResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsOperations :: Lens.Lens' DetachDiskResponse (Core.Maybe [Types.Operation])
ddrrsOperations = Lens.field @"operations"
{-# DEPRECATED ddrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DetachDiskResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
