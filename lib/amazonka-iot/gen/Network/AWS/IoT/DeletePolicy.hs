{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy.
--
-- A policy cannot be deleted if it has non-default versions or it is attached to any certificate.
-- To delete a policy, use the DeletePolicyVersion API to delete all non-default versions of the policy; use the DetachPrincipalPolicy API to detach the policy from any certificate; and then use the DeletePolicy API to delete the policy.
-- When a policy is deleted using DeletePolicy, its default version is deleted with it.
module Network.AWS.IoT.DeletePolicy
    (
    -- * Creating a request
      DeletePolicy (..)
    , mkDeletePolicy
    -- ** Request lenses
    , dpPolicyName

    -- * Destructuring the response
    , DeletePolicyResponse (..)
    , mkDeletePolicyResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeletePolicy operation.
--
-- /See:/ 'mkDeletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy'
  { policyName :: Types.PolicyName
    -- ^ The name of the policy to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicy' value with any optional fields omitted.
mkDeletePolicy
    :: Types.PolicyName -- ^ 'policyName'
    -> DeletePolicy
mkDeletePolicy policyName = DeletePolicy'{policyName}

-- | The name of the policy to delete.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyName :: Lens.Lens' DeletePolicy Types.PolicyName
dpPolicyName = Lens.field @"policyName"
{-# INLINEABLE dpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery DeletePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeletePolicy where
        type Rs DeletePolicy = DeletePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/policies/" Core.<> Core.toText policyName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeletePolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicyResponse' value with any optional fields omitted.
mkDeletePolicyResponse
    :: DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
