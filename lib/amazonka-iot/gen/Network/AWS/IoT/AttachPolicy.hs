{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AttachPolicy (..)
    , mkAttachPolicy
    -- ** Request lenses
    , apPolicyName
    , apTarget

    -- * Destructuring the response
    , AttachPolicyResponse (..)
    , mkAttachPolicyResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { policyName :: Types.PolicyName
    -- ^ The name of the policy to attach.
  , target :: Types.PolicyTarget
    -- ^ The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicy' value with any optional fields omitted.
mkAttachPolicy
    :: Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyTarget -- ^ 'target'
    -> AttachPolicy
mkAttachPolicy policyName target
  = AttachPolicy'{policyName, target}

-- | The name of the policy to attach.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyName :: Lens.Lens' AttachPolicy Types.PolicyName
apPolicyName = Lens.field @"policyName"
{-# INLINEABLE apPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTarget :: Lens.Lens' AttachPolicy Types.PolicyTarget
apTarget = Lens.field @"target"
{-# INLINEABLE apTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

instance Core.ToQuery AttachPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON AttachPolicy where
        toJSON AttachPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("target" Core..= target)])

instance Core.AWSRequest AttachPolicy where
        type Rs AttachPolicy = AttachPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/target-policies/" Core.<> Core.toText policyName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AttachPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicyResponse' value with any optional fields omitted.
mkAttachPolicyResponse
    :: AttachPolicyResponse
mkAttachPolicyResponse = AttachPolicyResponse'
