{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.EnablePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a policy type in a root. After you enable a policy type in a root, you can attach policies of that type to the root, any organizational unit (OU), or account in that root. You can undo this by using the 'DisablePolicyType' operation.
--
-- This is an asynchronous request that AWS performs in the background. AWS recommends that you first use 'ListRoots' to see the status of policy types for a specified root, and then use this operation.
-- This operation can be called only from the organization's management account.
-- You can enable a policy type in a root only if that policy type is available in the organization. To view the status of available policy types in the organization, use 'DescribeOrganization' .
module Network.AWS.Organizations.EnablePolicyType
    (
    -- * Creating a request
      EnablePolicyType (..)
    , mkEnablePolicyType
    -- ** Request lenses
    , eptRootId
    , eptPolicyType

    -- * Destructuring the response
    , EnablePolicyTypeResponse (..)
    , mkEnablePolicyTypeResponse
    -- ** Response lenses
    , eptrrsRoot
    , eptrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnablePolicyType' smart constructor.
data EnablePolicyType = EnablePolicyType'
  { rootId :: Types.RootId
    -- ^ The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
  , policyType :: Types.PolicyType
    -- ^ The policy type that you want to enable. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY> 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnablePolicyType' value with any optional fields omitted.
mkEnablePolicyType
    :: Types.RootId -- ^ 'rootId'
    -> Types.PolicyType -- ^ 'policyType'
    -> EnablePolicyType
mkEnablePolicyType rootId policyType
  = EnablePolicyType'{rootId, policyType}

-- | The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptRootId :: Lens.Lens' EnablePolicyType Types.RootId
eptRootId = Lens.field @"rootId"
{-# INLINEABLE eptRootId #-}
{-# DEPRECATED rootId "Use generic-lens or generic-optics with 'rootId' instead"  #-}

-- | The policy type that you want to enable. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY> 
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY> 
--
--
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptPolicyType :: Lens.Lens' EnablePolicyType Types.PolicyType
eptPolicyType = Lens.field @"policyType"
{-# INLINEABLE eptPolicyType #-}
{-# DEPRECATED policyType "Use generic-lens or generic-optics with 'policyType' instead"  #-}

instance Core.ToQuery EnablePolicyType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnablePolicyType where
        toHeaders EnablePolicyType{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.EnablePolicyType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnablePolicyType where
        toJSON EnablePolicyType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RootId" Core..= rootId),
                  Core.Just ("PolicyType" Core..= policyType)])

instance Core.AWSRequest EnablePolicyType where
        type Rs EnablePolicyType = EnablePolicyTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 EnablePolicyTypeResponse' Core.<$>
                   (x Core..:? "Root") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnablePolicyTypeResponse' smart constructor.
data EnablePolicyTypeResponse = EnablePolicyTypeResponse'
  { root :: Core.Maybe Types.Root
    -- ^ A structure that shows the root with the updated list of enabled policy types.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnablePolicyTypeResponse' value with any optional fields omitted.
mkEnablePolicyTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnablePolicyTypeResponse
mkEnablePolicyTypeResponse responseStatus
  = EnablePolicyTypeResponse'{root = Core.Nothing, responseStatus}

-- | A structure that shows the root with the updated list of enabled policy types.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptrrsRoot :: Lens.Lens' EnablePolicyTypeResponse (Core.Maybe Types.Root)
eptrrsRoot = Lens.field @"root"
{-# INLINEABLE eptrrsRoot #-}
{-# DEPRECATED root "Use generic-lens or generic-optics with 'root' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptrrsResponseStatus :: Lens.Lens' EnablePolicyTypeResponse Core.Int
eptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
