{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.PutContainerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access policy for the specified container to restrict the users and clients that can access it. For information about the data that is included in an access policy, see the <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide> .
--
-- For this release of the REST API, you can create only one policy for a container. If you enter @PutContainerPolicy@ twice, the second command modifies the existing policy.
module Network.AWS.MediaStore.PutContainerPolicy
  ( -- * Creating a request
    PutContainerPolicy (..),
    mkPutContainerPolicy,

    -- ** Request lenses
    pContainerName,
    pPolicy,

    -- * Destructuring the response
    PutContainerPolicyResponse (..),
    mkPutContainerPolicyResponse,

    -- ** Response lenses
    prsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutContainerPolicy' smart constructor.
data PutContainerPolicy = PutContainerPolicy'
  { -- | The name of the container.
    containerName :: Types.ContainerName,
    -- | The contents of the policy, which includes the following:
    --
    --
    --     * One @Version@ tag
    --
    --
    --     * One @Statement@ tag that contains the standard tags for the policy.
    policy :: Types.ContainerPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutContainerPolicy' value with any optional fields omitted.
mkPutContainerPolicy ::
  -- | 'containerName'
  Types.ContainerName ->
  -- | 'policy'
  Types.ContainerPolicy ->
  PutContainerPolicy
mkPutContainerPolicy containerName policy =
  PutContainerPolicy' {containerName, policy}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pContainerName :: Lens.Lens' PutContainerPolicy Types.ContainerName
pContainerName = Lens.field @"containerName"
{-# DEPRECATED pContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The contents of the policy, which includes the following:
--
--
--     * One @Version@ tag
--
--
--     * One @Statement@ tag that contains the standard tags for the policy.
--
--
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicy :: Lens.Lens' PutContainerPolicy Types.ContainerPolicy
pPolicy = Lens.field @"policy"
{-# DEPRECATED pPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Core.FromJSON PutContainerPolicy where
  toJSON PutContainerPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            Core.Just ("Policy" Core..= policy)
          ]
      )

instance Core.AWSRequest PutContainerPolicy where
  type Rs PutContainerPolicy = PutContainerPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MediaStore_20170901.PutContainerPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutContainerPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutContainerPolicyResponse' smart constructor.
newtype PutContainerPolicyResponse = PutContainerPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutContainerPolicyResponse' value with any optional fields omitted.
mkPutContainerPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutContainerPolicyResponse
mkPutContainerPolicyResponse responseStatus =
  PutContainerPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PutContainerPolicyResponse Core.Int
prsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
