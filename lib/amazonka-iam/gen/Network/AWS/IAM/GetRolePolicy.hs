{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded with the specified IAM role.
--
-- An IAM role can also have managed policies attached to it. To retrieve a managed policy document that is attached to a role, use 'GetPolicy' to determine the policy's default version, then use 'GetPolicyVersion' to retrieve the policy document.
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For more information about roles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities> .
module Network.AWS.IAM.GetRolePolicy
    (
    -- * Creating a request
      GetRolePolicy (..)
    , mkGetRolePolicy
    -- ** Request lenses
    , grpRoleName
    , grpPolicyName

    -- * Destructuring the response
    , GetRolePolicyResponse (..)
    , mkGetRolePolicyResponse
    -- ** Response lenses
    , grprrsRoleName
    , grprrsPolicyName
    , grprrsPolicyDocument
    , grprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRolePolicy' smart constructor.
data GetRolePolicy = GetRolePolicy'
  { roleName :: Types.RoleName
    -- ^ The name of the role associated with the policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyName
    -- ^ The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRolePolicy' value with any optional fields omitted.
mkGetRolePolicy
    :: Types.RoleName -- ^ 'roleName'
    -> Types.PolicyName -- ^ 'policyName'
    -> GetRolePolicy
mkGetRolePolicy roleName policyName
  = GetRolePolicy'{roleName, policyName}

-- | The name of the role associated with the policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRoleName :: Lens.Lens' GetRolePolicy Types.RoleName
grpRoleName = Lens.field @"roleName"
{-# INLINEABLE grpRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpPolicyName :: Lens.Lens' GetRolePolicy Types.PolicyName
grpPolicyName = Lens.field @"policyName"
{-# INLINEABLE grpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery GetRolePolicy where
        toQuery GetRolePolicy{..}
          = Core.toQueryPair "Action" ("GetRolePolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders GetRolePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetRolePolicy where
        type Rs GetRolePolicy = GetRolePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetRolePolicyResult"
              (\ s h x ->
                 GetRolePolicyResponse' Core.<$>
                   (x Core..@ "RoleName") Core.<*> x Core..@ "PolicyName" Core.<*>
                     x Core..@ "PolicyDocument"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetRolePolicy' request. 
--
-- /See:/ 'mkGetRolePolicyResponse' smart constructor.
data GetRolePolicyResponse = GetRolePolicyResponse'
  { roleName :: Types.RoleNameType
    -- ^ The role the policy is associated with.
  , policyName :: Types.PolicyNameType
    -- ^ The name of the policy.
  , policyDocument :: Types.PolicyDocumentType
    -- ^ The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRolePolicyResponse' value with any optional fields omitted.
mkGetRolePolicyResponse
    :: Types.RoleNameType -- ^ 'roleName'
    -> Types.PolicyNameType -- ^ 'policyName'
    -> Types.PolicyDocumentType -- ^ 'policyDocument'
    -> Core.Int -- ^ 'responseStatus'
    -> GetRolePolicyResponse
mkGetRolePolicyResponse roleName policyName policyDocument
  responseStatus
  = GetRolePolicyResponse'{roleName, policyName, policyDocument,
                           responseStatus}

-- | The role the policy is associated with.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRoleName :: Lens.Lens' GetRolePolicyResponse Types.RoleNameType
grprrsRoleName = Lens.field @"roleName"
{-# INLINEABLE grprrsRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyName :: Lens.Lens' GetRolePolicyResponse Types.PolicyNameType
grprrsPolicyName = Lens.field @"policyName"
{-# INLINEABLE grprrsPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyDocument :: Lens.Lens' GetRolePolicyResponse Types.PolicyDocumentType
grprrsPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE grprrsPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetRolePolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
