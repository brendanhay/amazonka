{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from the specified target.
module Network.AWS.IoT.DetachPolicy
  ( -- * Creating a request
    DetachPolicy (..),
    mkDetachPolicy,

    -- ** Request lenses
    dPolicyName,
    dTarget,

    -- * Destructuring the response
    DetachPolicyResponse (..),
    mkDetachPolicyResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The policy to detach.
    policyName :: Types.PolicyName,
    -- | The target from which the policy will be detached.
    target :: Types.Target
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachPolicy' value with any optional fields omitted.
mkDetachPolicy ::
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'target'
  Types.Target ->
  DetachPolicy
mkDetachPolicy policyName target =
  DetachPolicy' {policyName, target}

-- | The policy to detach.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyName :: Lens.Lens' DetachPolicy Types.PolicyName
dPolicyName = Lens.field @"policyName"
{-# DEPRECATED dPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The target from which the policy will be detached.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTarget :: Lens.Lens' DetachPolicy Types.Target
dTarget = Lens.field @"target"
{-# DEPRECATED dTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Core.FromJSON DetachPolicy where
  toJSON DetachPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("target" Core..= target)])

instance Core.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/target-policies/" Core.<> (Core.toText policyName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DetachPolicyResponse'

-- | /See:/ 'mkDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachPolicyResponse' value with any optional fields omitted.
mkDetachPolicyResponse ::
  DetachPolicyResponse
mkDetachPolicyResponse = DetachPolicyResponse'
