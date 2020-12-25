{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an object lifecycle policy from a container. It takes up to 20 minutes for the change to take effect.
module Network.AWS.MediaStore.DeleteLifecyclePolicy
  ( -- * Creating a request
    DeleteLifecyclePolicy (..),
    mkDeleteLifecyclePolicy,

    -- ** Request lenses
    dlpContainerName,

    -- * Destructuring the response
    DeleteLifecyclePolicyResponse (..),
    mkDeleteLifecyclePolicyResponse,

    -- ** Response lenses
    dlprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLifecyclePolicy' smart constructor.
newtype DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { -- | The name of the container that holds the object lifecycle policy.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLifecyclePolicy' value with any optional fields omitted.
mkDeleteLifecyclePolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  DeleteLifecyclePolicy
mkDeleteLifecyclePolicy containerName =
  DeleteLifecyclePolicy' {containerName}

-- | The name of the container that holds the object lifecycle policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpContainerName :: Lens.Lens' DeleteLifecyclePolicy Types.ContainerName
dlpContainerName = Lens.field @"containerName"
{-# DEPRECATED dlpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON DeleteLifecyclePolicy where
  toJSON DeleteLifecyclePolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest DeleteLifecyclePolicy where
  type Rs DeleteLifecyclePolicy = DeleteLifecyclePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MediaStore_20170901.DeleteLifecyclePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLifecyclePolicyResponse' smart constructor.
newtype DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLifecyclePolicyResponse' value with any optional fields omitted.
mkDeleteLifecyclePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLifecyclePolicyResponse
mkDeleteLifecyclePolicyResponse responseStatus =
  DeleteLifecyclePolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlprrsResponseStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Core.Int
dlprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
