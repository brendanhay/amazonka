{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager policy. 
module Network.AWS.FMS.DeletePolicy
    (
    -- * Creating a request
      DeletePolicy (..)
    , mkDeletePolicy
    -- ** Request lenses
    , dpPolicyId
    , dpDeleteAllPolicyResources

    -- * Destructuring the response
    , DeletePolicyResponse (..)
    , mkDeletePolicyResponse
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { policyId :: Types.PolicyId
    -- ^ The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
  , deleteAllPolicyResources :: Core.Maybe Core.Bool
    -- ^ If @True@ , the request performs cleanup according to the policy type. 
--
-- For AWS WAF and Shield Advanced policies, the cleanup does the following:
--
--     * Deletes rule groups created by AWS Firewall Manager
--
--
--     * Removes web ACLs from in-scope resources
--
--
--     * Deletes web ACLs that contain no rules or rule groups
--
--
-- For security group policies, the cleanup does the following for each security group in the policy:
--
--     * Disassociates the security group from in-scope resources 
--
--
--     * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy
--
--
-- After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePolicy' value with any optional fields omitted.
mkDeletePolicy
    :: Types.PolicyId -- ^ 'policyId'
    -> DeletePolicy
mkDeletePolicy policyId
  = DeletePolicy'{policyId, deleteAllPolicyResources = Core.Nothing}

-- | The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyId :: Lens.Lens' DeletePolicy Types.PolicyId
dpPolicyId = Lens.field @"policyId"
{-# INLINEABLE dpPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | If @True@ , the request performs cleanup according to the policy type. 
--
-- For AWS WAF and Shield Advanced policies, the cleanup does the following:
--
--     * Deletes rule groups created by AWS Firewall Manager
--
--
--     * Removes web ACLs from in-scope resources
--
--
--     * Deletes web ACLs that contain no rules or rule groups
--
--
-- For security group policies, the cleanup does the following for each security group in the policy:
--
--     * Disassociates the security group from in-scope resources 
--
--
--     * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy
--
--
-- After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope. 
--
-- /Note:/ Consider using 'deleteAllPolicyResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDeleteAllPolicyResources :: Lens.Lens' DeletePolicy (Core.Maybe Core.Bool)
dpDeleteAllPolicyResources = Lens.field @"deleteAllPolicyResources"
{-# INLINEABLE dpDeleteAllPolicyResources #-}
{-# DEPRECATED deleteAllPolicyResources "Use generic-lens or generic-optics with 'deleteAllPolicyResources' instead"  #-}

instance Core.ToQuery DeletePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePolicy where
        toHeaders DeletePolicy{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.DeletePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePolicy where
        toJSON DeletePolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyId" Core..= policyId),
                  ("DeleteAllPolicyResources" Core..=) Core.<$>
                    deleteAllPolicyResources])

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
