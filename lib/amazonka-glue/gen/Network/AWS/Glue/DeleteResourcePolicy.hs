{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified policy.
module Network.AWS.Glue.DeleteResourcePolicy
  ( -- * Creating a request
    DeleteResourcePolicy (..),
    mkDeleteResourcePolicy,

    -- ** Request lenses
    drpPolicyHashCondition,
    drpResourceArn,

    -- * Destructuring the response
    DeleteResourcePolicyResponse (..),
    mkDeleteResourcePolicyResponse,

    -- ** Response lenses
    drprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The hash value returned when this policy was set.
    policyHashCondition :: Core.Maybe Types.HashString,
    -- | The ARN of the AWS Glue resource for the resource policy to be deleted.
    resourceArn :: Core.Maybe Types.GlueResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcePolicy' value with any optional fields omitted.
mkDeleteResourcePolicy ::
  DeleteResourcePolicy
mkDeleteResourcePolicy =
  DeleteResourcePolicy'
    { policyHashCondition = Core.Nothing,
      resourceArn = Core.Nothing
    }

-- | The hash value returned when this policy was set.
--
-- /Note:/ Consider using 'policyHashCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPolicyHashCondition :: Lens.Lens' DeleteResourcePolicy (Core.Maybe Types.HashString)
drpPolicyHashCondition = Lens.field @"policyHashCondition"
{-# DEPRECATED drpPolicyHashCondition "Use generic-lens or generic-optics with 'policyHashCondition' instead." #-}

-- | The ARN of the AWS Glue resource for the resource policy to be deleted.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpResourceArn :: Lens.Lens' DeleteResourcePolicy (Core.Maybe Types.GlueResourceArn)
drpResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED drpResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ ("PolicyHashCondition" Core..=) Core.<$> policyHashCondition,
            ("ResourceArn" Core..=) Core.<$> resourceArn
          ]
      )

instance Core.AWSRequest DeleteResourcePolicy where
  type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteResourcePolicyResponse' smart constructor.
newtype DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcePolicyResponse' value with any optional fields omitted.
mkDeleteResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteResourcePolicyResponse
mkDeleteResourcePolicyResponse responseStatus =
  DeleteResourcePolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DeleteResourcePolicyResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
