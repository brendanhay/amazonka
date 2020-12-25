{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager policy.
module Network.AWS.FMS.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpPolicyId,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprrsPolicy,
    gprrsPolicyArn,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { -- | The ID of the AWS Firewall Manager policy that you want the details for.
    policyId :: Types.PolicyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicy' value with any optional fields omitted.
mkGetPolicy ::
  -- | 'policyId'
  Types.PolicyId ->
  GetPolicy
mkGetPolicy policyId = GetPolicy' {policyId}

-- | The ID of the AWS Firewall Manager policy that you want the details for.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyId :: Lens.Lens' GetPolicy Types.PolicyId
gpPolicyId = Lens.field @"policyId"
{-# DEPRECATED gpPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

instance Core.FromJSON GetPolicy where
  toJSON GetPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PolicyId" Core..= policyId)])

instance Core.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Core.<$> (x Core..:? "Policy")
            Core.<*> (x Core..:? "PolicyArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | Information about the specified AWS Firewall Manager policy.
    policy :: Core.Maybe Types.Policy,
    -- | The Amazon Resource Name (ARN) of the specified policy.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPolicyResponse' value with any optional fields omitted.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPolicyResponse
mkGetPolicyResponse responseStatus =
  GetPolicyResponse'
    { policy = Core.Nothing,
      policyArn = Core.Nothing,
      responseStatus
    }

-- | Information about the specified AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicy :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.Policy)
gprrsPolicy = Lens.field @"policy"
{-# DEPRECATED gprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The Amazon Resource Name (ARN) of the specified policy.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPolicyArn :: Lens.Lens' GetPolicyResponse (Core.Maybe Types.PolicyArn)
gprrsPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED gprrsPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPolicyResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
