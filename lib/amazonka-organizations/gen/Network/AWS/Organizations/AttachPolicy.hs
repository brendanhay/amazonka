{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.AttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to a root, an organizational unit (OU), or an individual account. How the policy affects accounts depends on the type of policy. Refer to the /AWS Organizations User Guide/ for information about each policy type:
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
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.AttachPolicy
    (
    -- * Creating a request
      AttachPolicy (..)
    , mkAttachPolicy
    -- ** Request lenses
    , apPolicyId
    , apTargetId

    -- * Destructuring the response
    , AttachPolicyResponse (..)
    , mkAttachPolicyResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { policyId :: Types.PolicyId
    -- ^ The unique identifier (ID) of the policy that you want to attach to the target. You can get the ID for the policy by calling the 'ListPolicies' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
  , targetId :: Types.PolicyTargetId
    -- ^ The unique identifier (ID) of the root, OU, or account that you want to attach the policy to. You can get the ID by calling the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachPolicy' value with any optional fields omitted.
mkAttachPolicy
    :: Types.PolicyId -- ^ 'policyId'
    -> Types.PolicyTargetId -- ^ 'targetId'
    -> AttachPolicy
mkAttachPolicy policyId targetId
  = AttachPolicy'{policyId, targetId}

-- | The unique identifier (ID) of the policy that you want to attach to the target. You can get the ID for the policy by calling the 'ListPolicies' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyId :: Lens.Lens' AttachPolicy Types.PolicyId
apPolicyId = Lens.field @"policyId"
{-# INLINEABLE apPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | The unique identifier (ID) of the root, OU, or account that you want to attach the policy to. You can get the ID by calling the 'ListRoots' , 'ListOrganizationalUnitsForParent' , or 'ListAccounts' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTargetId :: Lens.Lens' AttachPolicy Types.PolicyTargetId
apTargetId = Lens.field @"targetId"
{-# INLINEABLE apTargetId #-}
{-# DEPRECATED targetId "Use generic-lens or generic-optics with 'targetId' instead"  #-}

instance Core.ToQuery AttachPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachPolicy where
        toHeaders AttachPolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSOrganizationsV20161128.AttachPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AttachPolicy where
        toJSON AttachPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyId" Core..= policyId),
                  Core.Just ("TargetId" Core..= targetId)])

instance Core.AWSRequest AttachPolicy where
        type Rs AttachPolicy = AttachPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
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
