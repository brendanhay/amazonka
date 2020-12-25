{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified container. Before you make a @DeleteContainer@ request, delete any objects in the container or in any folders in the container. You can delete only empty containers.
module Network.AWS.MediaStore.DeleteContainer
  ( -- * Creating a request
    DeleteContainer (..),
    mkDeleteContainer,

    -- ** Request lenses
    dcContainerName,

    -- * Destructuring the response
    DeleteContainerResponse (..),
    mkDeleteContainerResponse,

    -- ** Response lenses
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainer' smart constructor.
newtype DeleteContainer = DeleteContainer'
  { -- | The name of the container to delete.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainer' value with any optional fields omitted.
mkDeleteContainer ::
  -- | 'containerName'
  Types.ContainerName ->
  DeleteContainer
mkDeleteContainer containerName = DeleteContainer' {containerName}

-- | The name of the container to delete.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcContainerName :: Lens.Lens' DeleteContainer Types.ContainerName
dcContainerName = Lens.field @"containerName"
{-# DEPRECATED dcContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON DeleteContainer where
  toJSON DeleteContainer {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest DeleteContainer where
  type Rs DeleteContainer = DeleteContainerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "MediaStore_20170901.DeleteContainer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteContainerResponse' smart constructor.
newtype DeleteContainerResponse = DeleteContainerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerResponse' value with any optional fields omitted.
mkDeleteContainerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteContainerResponse
mkDeleteContainerResponse responseStatus =
  DeleteContainerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteContainerResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
