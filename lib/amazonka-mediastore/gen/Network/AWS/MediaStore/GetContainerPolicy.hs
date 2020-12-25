{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.GetContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the access policy for the specified container. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
module Network.AWS.MediaStore.GetContainerPolicy
  ( -- * Creating a request
    GetContainerPolicy (..),
    mkGetContainerPolicy,

    -- ** Request lenses
    gContainerName,

    -- * Destructuring the response
    GetContainerPolicyResponse (..),
    mkGetContainerPolicyResponse,

    -- ** Response lenses
    grsPolicy,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerPolicy' smart constructor.
newtype GetContainerPolicy = GetContainerPolicy'
  { -- | The name of the container.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerPolicy' value with any optional fields omitted.
mkGetContainerPolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  GetContainerPolicy
mkGetContainerPolicy containerName =
  GetContainerPolicy' {containerName}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gContainerName :: Lens.Lens' GetContainerPolicy Types.ContainerName
gContainerName = Lens.field @"containerName"
{-# DEPRECATED gContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON GetContainerPolicy where
  toJSON GetContainerPolicy {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest GetContainerPolicy where
  type Rs GetContainerPolicy = GetContainerPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MediaStore_20170901.GetContainerPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerPolicyResponse'
            Core.<$> (x Core..: "Policy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContainerPolicyResponse' smart constructor.
data GetContainerPolicyResponse = GetContainerPolicyResponse'
  { -- | The contents of the access policy.
    policy :: Types.ContainerPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerPolicyResponse' value with any optional fields omitted.
mkGetContainerPolicyResponse ::
  -- | 'policy'
  Types.ContainerPolicy ->
  -- | 'responseStatus'
  Core.Int ->
  GetContainerPolicyResponse
mkGetContainerPolicyResponse policy responseStatus =
  GetContainerPolicyResponse' {policy, responseStatus}

-- | The contents of the access policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsPolicy :: Lens.Lens' GetContainerPolicyResponse Types.ContainerPolicy
grsPolicy = Lens.field @"policy"
{-# DEPRECATED grsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetContainerPolicyResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
