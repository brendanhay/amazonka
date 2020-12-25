{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to the specified target.
module Network.AWS.IoT.AttachPolicy
  ( -- * Creating a request
    AttachPolicy (..),
    mkAttachPolicy,

    -- ** Request lenses
    apPolicyName,
    apTarget,

    -- * Destructuring the response
    AttachPolicyResponse (..),
    mkAttachPolicyResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The name of the policy to attach.
    policyName :: Types.PolicyName,
    -- | The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
    target :: Types.PolicyTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicy' value with any optional fields omitted.
mkAttachPolicy ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'target'
  Types.PolicyTarget ->
  AttachPolicy
mkAttachPolicy policyName target =
  AttachPolicy' {policyName, target}

-- | The name of the policy to attach.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyName :: Lens.Lens' AttachPolicy Types.PolicyName
apPolicyName = Lens.field @"policyName"
{-# DEPRECATED apPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTarget :: Lens.Lens' AttachPolicy Types.PolicyTarget
apTarget = Lens.field @"target"
{-# DEPRECATED apTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Core.FromJSON AttachPolicy where
  toJSON AttachPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("target" Core..= target)])

instance Core.AWSRequest AttachPolicy where
  type Rs AttachPolicy = AttachPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/target-policies/" Core.<> (Core.toText policyName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AttachPolicyResponse'

-- | /See:/ 'mkAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicyResponse' value with any optional fields omitted.
mkAttachPolicyResponse ::
  AttachPolicyResponse
mkAttachPolicyResponse = AttachPolicyResponse'
