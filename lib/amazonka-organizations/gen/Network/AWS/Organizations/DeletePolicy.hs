{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from your organization. Before you perform this operation, you must first detach the policy from all organizational units (OUs), roots, and accounts.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DeletePolicy
    (
    -- * Creating a request
      DeletePolicy (..)
    , mkDeletePolicy
    -- ** Request lenses
    , dPolicyId

    -- * Destructuring the response
    , DeletePolicyResponse (..)
    , mkDeletePolicyResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy'
  { policyId :: Types.PolicyId
    -- ^ The unique identifier (ID) of the policy that you want to delete. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicy' value with any optional fields omitted.
mkDeletePolicy
    :: Types.PolicyId -- ^ 'policyId'
    -> DeletePolicy
mkDeletePolicy policyId = DeletePolicy'{policyId}

-- | The unique identifier (ID) of the policy that you want to delete. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyId :: Lens.Lens' DeletePolicy Types.PolicyId
dPolicyId = Lens.field @"policyId"
{-# INLINEABLE dPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

instance Core.ToQuery DeletePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePolicy where
        toHeaders DeletePolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.DeletePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePolicy where
        toJSON DeletePolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("PolicyId" Core..= policyId)])

instance Core.AWSRequest DeletePolicy where
        type Rs DeletePolicy = DeletePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
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
