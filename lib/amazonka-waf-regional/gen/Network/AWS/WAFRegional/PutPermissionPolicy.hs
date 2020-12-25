{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.PutPermissionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an IAM policy to the specified resource. The only supported use for this action is to share a RuleGroup across accounts.
--
-- The @PutPermissionPolicy@ is subject to the following restrictions:
--
--     * You can attach only one policy with each @PutPermissionPolicy@ request.
--
--
--     * The policy must include an @Effect@ , @Action@ and @Principal@ .
--
--
--     * @Effect@ must specify @Allow@ .
--
--
--     * The @Action@ in the policy must be @waf:UpdateWebACL@ , @waf-regional:UpdateWebACL@ , @waf:GetRuleGroup@ and @waf-regional:GetRuleGroup@ . Any extra or wildcard actions in the policy will be rejected.
--
--
--     * The policy cannot include a @Resource@ parameter.
--
--
--     * The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.
--
--
--     * The user making the request must be the owner of the RuleGroup.
--
--
--     * Your policy must be composed using IAM Policy version 2012-10-17.
--
--
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies> .
-- An example of a valid policy parameter is shown in the Examples section below.
module Network.AWS.WAFRegional.PutPermissionPolicy
  ( -- * Creating a request
    PutPermissionPolicy (..),
    mkPutPermissionPolicy,

    -- ** Request lenses
    pppResourceArn,
    pppPolicy,

    -- * Destructuring the response
    PutPermissionPolicyResponse (..),
    mkPutPermissionPolicyResponse,

    -- ** Response lenses
    ppprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkPutPermissionPolicy' smart constructor.
data PutPermissionPolicy = PutPermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to attach the policy.
    resourceArn :: Types.ResourceArn,
    -- | The policy to attach to the specified RuleGroup.
    policy :: Types.Policy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPermissionPolicy' value with any optional fields omitted.
mkPutPermissionPolicy ::
  -- | 'resourceArn'
  Types.ResourceArn ->
  -- | 'policy'
  Types.Policy ->
  PutPermissionPolicy
mkPutPermissionPolicy resourceArn policy =
  PutPermissionPolicy' {resourceArn, policy}

-- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to attach the policy.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppResourceArn :: Lens.Lens' PutPermissionPolicy Types.ResourceArn
pppResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED pppResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | The policy to attach to the specified RuleGroup.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppPolicy :: Lens.Lens' PutPermissionPolicy Types.Policy
pppPolicy = Lens.field @"policy"
{-# DEPRECATED pppPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Core.FromJSON PutPermissionPolicy where
  toJSON PutPermissionPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceArn" Core..= resourceArn),
            Core.Just ("Policy" Core..= policy)
          ]
      )

instance Core.AWSRequest PutPermissionPolicy where
  type Rs PutPermissionPolicy = PutPermissionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_Regional_20161128.PutPermissionPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutPermissionPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutPermissionPolicyResponse' smart constructor.
newtype PutPermissionPolicyResponse = PutPermissionPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutPermissionPolicyResponse' value with any optional fields omitted.
mkPutPermissionPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutPermissionPolicyResponse
mkPutPermissionPolicyResponse responseStatus =
  PutPermissionPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppprrsResponseStatus :: Lens.Lens' PutPermissionPolicyResponse Core.Int
ppprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ppprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
