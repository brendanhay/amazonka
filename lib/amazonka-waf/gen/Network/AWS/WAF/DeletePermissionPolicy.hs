{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeletePermissionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an IAM policy from the specified RuleGroup.
--
-- The user making the request must be the owner of the RuleGroup.
module Network.AWS.WAF.DeletePermissionPolicy
  ( -- * Creating a request
    DeletePermissionPolicy (..),
    mkDeletePermissionPolicy,

    -- ** Request lenses
    dppResourceArn,

    -- * Destructuring the response
    DeletePermissionPolicyResponse (..),
    mkDeletePermissionPolicyResponse,

    -- ** Response lenses
    dpprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeletePermissionPolicy' smart constructor.
newtype DeletePermissionPolicy = DeletePermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup from which you want to delete the policy.
    --
    -- The user making the request must be the owner of the RuleGroup.
    resourceArn :: Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePermissionPolicy' value with any optional fields omitted.
mkDeletePermissionPolicy ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  DeletePermissionPolicy
mkDeletePermissionPolicy resourceArn =
  DeletePermissionPolicy' {resourceArn}

-- | The Amazon Resource Name (ARN) of the RuleGroup from which you want to delete the policy.
--
-- The user making the request must be the owner of the RuleGroup.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppResourceArn :: Lens.Lens' DeletePermissionPolicy Types.ResourceArn
dppResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED dppResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON DeletePermissionPolicy where
  toJSON DeletePermissionPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest DeletePermissionPolicy where
  type Rs DeletePermissionPolicy = DeletePermissionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_20150824.DeletePermissionPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePermissionPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePermissionPolicyResponse' smart constructor.
newtype DeletePermissionPolicyResponse = DeletePermissionPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePermissionPolicyResponse' value with any optional fields omitted.
mkDeletePermissionPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePermissionPolicyResponse
mkDeletePermissionPolicyResponse responseStatus =
  DeletePermissionPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsResponseStatus :: Lens.Lens' DeletePermissionPolicyResponse Core.Int
dpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
