{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DeleteContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access policy that is associated with the specified container.
module Network.AWS.MediaStore.DeleteContainerPolicy
  ( -- * Creating a request
    DeleteContainerPolicy (..),
    mkDeleteContainerPolicy,

    -- ** Request lenses
    dcpfContainerName,

    -- * Destructuring the response
    DeleteContainerPolicyResponse (..),
    mkDeleteContainerPolicyResponse,

    -- ** Response lenses
    dcprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainerPolicy' smart constructor.
newtype DeleteContainerPolicy = DeleteContainerPolicy'
  { -- | The name of the container that holds the policy.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerPolicy' value with any optional fields omitted.
mkDeleteContainerPolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  DeleteContainerPolicy
mkDeleteContainerPolicy containerName =
  DeleteContainerPolicy' {containerName}

-- | The name of the container that holds the policy.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpfContainerName :: Lens.Lens' DeleteContainerPolicy Types.ContainerName
dcpfContainerName = Lens.field @"containerName"
{-# DEPRECATED dcpfContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON DeleteContainerPolicy where
  toJSON DeleteContainerPolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest DeleteContainerPolicy where
  type Rs DeleteContainerPolicy = DeleteContainerPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MediaStore_20170901.DeleteContainerPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteContainerPolicyResponse' smart constructor.
newtype DeleteContainerPolicyResponse = DeleteContainerPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerPolicyResponse' value with any optional fields omitted.
mkDeleteContainerPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteContainerPolicyResponse
mkDeleteContainerPolicyResponse responseStatus =
  DeleteContainerPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprfrsResponseStatus :: Lens.Lens' DeleteContainerPolicyResponse Core.Int
dcprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
