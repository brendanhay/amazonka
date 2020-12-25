{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the entire registry including schema and all of its versions. To get the status of the delete operation, you can call the @GetRegistry@ API after the asynchronous call. Deleting a registry will disable all online operations for the registry such as the @UpdateRegistry@ , @CreateSchema@ , @UpdateSchema@ , and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteRegistry
  ( -- * Creating a request
    DeleteRegistry (..),
    mkDeleteRegistry,

    -- ** Request lenses
    drRegistryId,

    -- * Destructuring the response
    DeleteRegistryResponse (..),
    mkDeleteRegistryResponse,

    -- ** Response lenses
    drrrsRegistryArn,
    drrrsRegistryName,
    drrrsStatus,
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRegistry' smart constructor.
newtype DeleteRegistry = DeleteRegistry'
  { -- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
    registryId :: Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegistry' value with any optional fields omitted.
mkDeleteRegistry ::
  -- | 'registryId'
  Types.RegistryId ->
  DeleteRegistry
mkDeleteRegistry registryId = DeleteRegistry' {registryId}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRegistryId :: Lens.Lens' DeleteRegistry Types.RegistryId
drRegistryId = Lens.field @"registryId"
{-# DEPRECATED drRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON DeleteRegistry where
  toJSON DeleteRegistry {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RegistryId" Core..= registryId)])

instance Core.AWSRequest DeleteRegistry where
  type Rs DeleteRegistry = DeleteRegistryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteRegistry")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegistryResponse'
            Core.<$> (x Core..:? "RegistryArn")
            Core.<*> (x Core..:? "RegistryName")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRegistryResponse' smart constructor.
data DeleteRegistryResponse = DeleteRegistryResponse'
  { -- | The Amazon Resource Name (ARN) of the registry being deleted.
    registryArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the registry being deleted.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The status of the registry. A successful operation will return the @Deleting@ status.
    status :: Core.Maybe Types.RegistryStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRegistryResponse' value with any optional fields omitted.
mkDeleteRegistryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRegistryResponse
mkDeleteRegistryResponse responseStatus =
  DeleteRegistryResponse'
    { registryArn = Core.Nothing,
      registryName = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the registry being deleted.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRegistryArn :: Lens.Lens' DeleteRegistryResponse (Core.Maybe Types.GlueResourceArn)
drrrsRegistryArn = Lens.field @"registryArn"
{-# DEPRECATED drrrsRegistryArn "Use generic-lens or generic-optics with 'registryArn' instead." #-}

-- | The name of the registry being deleted.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRegistryName :: Lens.Lens' DeleteRegistryResponse (Core.Maybe Types.RegistryName)
drrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED drrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The status of the registry. A successful operation will return the @Deleting@ status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsStatus :: Lens.Lens' DeleteRegistryResponse (Core.Maybe Types.RegistryStatus)
drrrsStatus = Lens.field @"status"
{-# DEPRECATED drrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteRegistryResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
