{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified block storage disk. The disk must be in the @available@ state (not attached to a Lightsail instance).
--
-- The @delete disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDisk
  ( -- * Creating a request
    DeleteDisk (..),
    mkDeleteDisk,

    -- ** Request lenses
    dDiskName,
    dForceDeleteAddOns,

    -- * Destructuring the response
    DeleteDiskResponse (..),
    mkDeleteDiskResponse,

    -- ** Response lenses
    drsOperations,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDisk' smart constructor.
data DeleteDisk = DeleteDisk'
  { -- | The unique name of the disk you want to delete (e.g., @my-disk@ ).
    diskName :: Types.ResourceName,
    -- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
    forceDeleteAddOns :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDisk' value with any optional fields omitted.
mkDeleteDisk ::
  -- | 'diskName'
  Types.ResourceName ->
  DeleteDisk
mkDeleteDisk diskName =
  DeleteDisk' {diskName, forceDeleteAddOns = Core.Nothing}

-- | The unique name of the disk you want to delete (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskName :: Lens.Lens' DeleteDisk Types.ResourceName
dDiskName = Lens.field @"diskName"
{-# DEPRECATED dDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- /Note:/ Consider using 'forceDeleteAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dForceDeleteAddOns :: Lens.Lens' DeleteDisk (Core.Maybe Core.Bool)
dForceDeleteAddOns = Lens.field @"forceDeleteAddOns"
{-# DEPRECATED dForceDeleteAddOns "Use generic-lens or generic-optics with 'forceDeleteAddOns' instead." #-}

instance Core.FromJSON DeleteDisk where
  toJSON DeleteDisk {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("diskName" Core..= diskName),
            ("forceDeleteAddOns" Core..=) Core.<$> forceDeleteAddOns
          ]
      )

instance Core.AWSRequest DeleteDisk where
  type Rs DeleteDisk = DeleteDiskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteDisk")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDiskResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDiskResponse' smart constructor.
data DeleteDiskResponse = DeleteDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteDiskResponse' value with any optional fields omitted.
mkDeleteDiskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDiskResponse
mkDeleteDiskResponse responseStatus =
  DeleteDiskResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsOperations :: Lens.Lens' DeleteDiskResponse (Core.Maybe [Types.Operation])
drsOperations = Lens.field @"operations"
{-# DEPRECATED drsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteDiskResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
