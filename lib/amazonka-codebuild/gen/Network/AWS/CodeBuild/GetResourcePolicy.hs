{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that is identified by its resource ARN.
module Network.AWS.CodeBuild.GetResourcePolicy
  ( -- * Creating a request
    GetResourcePolicy (..),
    mkGetResourcePolicy,

    -- ** Request lenses
    grpResourceArn,

    -- * Destructuring the response
    GetResourcePolicyResponse (..),
    mkGetResourcePolicyResponse,

    -- ** Response lenses
    grprrsPolicy,
    grprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN of the resource that is associated with the resource policy.
    resourceArn :: Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicy' value with any optional fields omitted.
mkGetResourcePolicy ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  GetResourcePolicy
mkGetResourcePolicy resourceArn = GetResourcePolicy' {resourceArn}

-- | The ARN of the resource that is associated with the resource policy.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpResourceArn :: Lens.Lens' GetResourcePolicy Types.ResourceArn
grpResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED grpResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON GetResourcePolicy where
  toJSON GetResourcePolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("resourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.GetResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Core.<$> (x Core..:? "policy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The resource policy for the resource identified by the input ARN parameter.
    policy :: Core.Maybe Types.Policy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicyResponse' value with any optional fields omitted.
mkGetResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourcePolicyResponse
mkGetResourcePolicyResponse responseStatus =
  GetResourcePolicyResponse' {policy = Core.Nothing, responseStatus}

-- | The resource policy for the resource identified by the input ARN parameter.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicy :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.Policy)
grprrsPolicy = Lens.field @"policy"
{-# DEPRECATED grprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
