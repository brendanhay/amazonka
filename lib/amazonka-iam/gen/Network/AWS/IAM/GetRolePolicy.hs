{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetRolePolicy (..),
    mkGetRolePolicy,

    -- ** Request lenses
    grpRoleName,
    grpPolicyName,

    -- * Destructuring the response
    GetRolePolicyResponse (..),
    mkGetRolePolicyResponse,

    -- ** Response lenses
    grprrsRoleName,
    grprrsPolicyName,
    grprrsPolicyDocument,
    grprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRolePolicy' smart constructor.
data GetRolePolicy = GetRolePolicy'
  { -- | The name of the role associated with the policy.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Types.RoleName,
    -- | The name of the policy document to get.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRolePolicy' value with any optional fields omitted.
mkGetRolePolicy ::
  -- | 'roleName'
  Types.RoleName ->
  -- | 'policyName'
  Types.PolicyName ->
  GetRolePolicy
mkGetRolePolicy roleName policyName =
  GetRolePolicy' {roleName, policyName}

-- | The name of the role associated with the policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRoleName :: Lens.Lens' GetRolePolicy Types.RoleName
grpRoleName = Lens.field @"roleName"
{-# DEPRECATED grpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpPolicyName :: Lens.Lens' GetRolePolicy Types.PolicyName
grpPolicyName = Lens.field @"policyName"
{-# DEPRECATED grpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.AWSRequest GetRolePolicy where
  type Rs GetRolePolicy = GetRolePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetRolePolicy")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
                Core.<> (Core.toQueryValue "PolicyName" policyName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetRolePolicyResult"
      ( \s h x ->
          GetRolePolicyResponse'
            Core.<$> (x Core..@ "RoleName")
            Core.<*> (x Core..@ "PolicyName")
            Core.<*> (x Core..@ "PolicyDocument")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetRolePolicy' request.
--
-- /See:/ 'mkGetRolePolicyResponse' smart constructor.
data GetRolePolicyResponse = GetRolePolicyResponse'
  { -- | The role the policy is associated with.
    roleName :: Types.RoleNameType,
    -- | The name of the policy.
    policyName :: Types.PolicyNameType,
    -- | The policy document.
    --
    -- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
    policyDocument :: Types.PolicyDocumentType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRolePolicyResponse' value with any optional fields omitted.
mkGetRolePolicyResponse ::
  -- | 'roleName'
  Types.RoleNameType ->
  -- | 'policyName'
  Types.PolicyNameType ->
  -- | 'policyDocument'
  Types.PolicyDocumentType ->
  -- | 'responseStatus'
  Core.Int ->
  GetRolePolicyResponse
mkGetRolePolicyResponse
  roleName
  policyName
  policyDocument
  responseStatus =
    GetRolePolicyResponse'
      { roleName,
        policyName,
        policyDocument,
        responseStatus
      }

-- | The role the policy is associated with.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsRoleName :: Lens.Lens' GetRolePolicyResponse Types.RoleNameType
grprrsRoleName = Lens.field @"roleName"
{-# DEPRECATED grprrsRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyName :: Lens.Lens' GetRolePolicyResponse Types.PolicyNameType
grprrsPolicyName = Lens.field @"policyName"
{-# DEPRECATED grprrsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyDocument :: Lens.Lens' GetRolePolicyResponse Types.PolicyDocumentType
grprrsPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED grprrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetRolePolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
