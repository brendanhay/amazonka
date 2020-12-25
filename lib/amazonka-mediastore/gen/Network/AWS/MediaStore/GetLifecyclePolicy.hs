{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetLifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the object lifecycle policy that is assigned to a container.
module Network.AWS.MediaStore.GetLifecyclePolicy
  ( -- * Creating a request
    GetLifecyclePolicy (..),
    mkGetLifecyclePolicy,

    -- ** Request lenses
    glpContainerName,

    -- * Destructuring the response
    GetLifecyclePolicyResponse (..),
    mkGetLifecyclePolicyResponse,

    -- ** Response lenses
    glprrsLifecyclePolicy,
    glprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLifecyclePolicy' smart constructor.
newtype GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The name of the container that the object lifecycle policy is assigned to.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicy' value with any optional fields omitted.
mkGetLifecyclePolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  GetLifecyclePolicy
mkGetLifecyclePolicy containerName =
  GetLifecyclePolicy' {containerName}

-- | The name of the container that the object lifecycle policy is assigned to.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpContainerName :: Lens.Lens' GetLifecyclePolicy Types.ContainerName
glpContainerName = Lens.field @"containerName"
{-# DEPRECATED glpContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest GetLifecyclePolicy where
  type Rs GetLifecyclePolicy = GetLifecyclePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MediaStore_20170901.GetLifecyclePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Core.<$> (x Core..: "LifecyclePolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The object lifecycle policy that is assigned to the container.
    lifecyclePolicy :: Types.LifecyclePolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLifecyclePolicyResponse' value with any optional fields omitted.
mkGetLifecyclePolicyResponse ::
  -- | 'lifecyclePolicy'
  Types.LifecyclePolicy ->
  -- | 'responseStatus'
  Core.Int ->
  GetLifecyclePolicyResponse
mkGetLifecyclePolicyResponse lifecyclePolicy responseStatus =
  GetLifecyclePolicyResponse' {lifecyclePolicy, responseStatus}

-- | The object lifecycle policy that is assigned to the container.
--
-- /Note:/ Consider using 'lifecyclePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsLifecyclePolicy :: Lens.Lens' GetLifecyclePolicyResponse Types.LifecyclePolicy
glprrsLifecyclePolicy = Lens.field @"lifecyclePolicy"
{-# DEPRECATED glprrsLifecyclePolicy "Use generic-lens or generic-optics with 'lifecyclePolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprrsResponseStatus :: Lens.Lens' GetLifecyclePolicyResponse Core.Int
glprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
