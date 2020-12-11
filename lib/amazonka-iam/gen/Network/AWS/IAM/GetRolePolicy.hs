{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    grprsResponseStatus,
    grprsRoleName,
    grprsPolicyName,
    grprsPolicyDocument,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRolePolicy' smart constructor.
data GetRolePolicy = GetRolePolicy'
  { roleName :: Lude.Text,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRolePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'roleName' - The name of the role associated with the policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetRolePolicy ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  GetRolePolicy
mkGetRolePolicy pRoleName_ pPolicyName_ =
  GetRolePolicy' {roleName = pRoleName_, policyName = pPolicyName_}

-- | The name of the role associated with the policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRoleName :: Lens.Lens' GetRolePolicy Lude.Text
grpRoleName = Lens.lens (roleName :: GetRolePolicy -> Lude.Text) (\s a -> s {roleName = a} :: GetRolePolicy)
{-# DEPRECATED grpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpPolicyName :: Lens.Lens' GetRolePolicy Lude.Text
grpPolicyName = Lens.lens (policyName :: GetRolePolicy -> Lude.Text) (\s a -> s {policyName = a} :: GetRolePolicy)
{-# DEPRECATED grpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest GetRolePolicy where
  type Rs GetRolePolicy = GetRolePolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetRolePolicyResult"
      ( \s h x ->
          GetRolePolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "RoleName")
            Lude.<*> (x Lude..@ "PolicyName")
            Lude.<*> (x Lude..@ "PolicyDocument")
      )

instance Lude.ToHeaders GetRolePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetRolePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRolePolicy where
  toQuery GetRolePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetRolePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "PolicyName" Lude.=: policyName
      ]

-- | Contains the response to a successful 'GetRolePolicy' request.
--
-- /See:/ 'mkGetRolePolicyResponse' smart constructor.
data GetRolePolicyResponse = GetRolePolicyResponse'
  { responseStatus ::
      Lude.Int,
    roleName :: Lude.Text,
    policyName :: Lude.Text,
    policyDocument :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRolePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- * 'policyName' - The name of the policy.
-- * 'responseStatus' - The response status code.
-- * 'roleName' - The role the policy is associated with.
mkGetRolePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'roleName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  GetRolePolicyResponse
mkGetRolePolicyResponse
  pResponseStatus_
  pRoleName_
  pPolicyName_
  pPolicyDocument_ =
    GetRolePolicyResponse'
      { responseStatus = pResponseStatus_,
        roleName = pRoleName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsResponseStatus :: Lens.Lens' GetRolePolicyResponse Lude.Int
grprsResponseStatus = Lens.lens (responseStatus :: GetRolePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRolePolicyResponse)
{-# DEPRECATED grprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The role the policy is associated with.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsRoleName :: Lens.Lens' GetRolePolicyResponse Lude.Text
grprsRoleName = Lens.lens (roleName :: GetRolePolicyResponse -> Lude.Text) (\s a -> s {roleName = a} :: GetRolePolicyResponse)
{-# DEPRECATED grprsRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsPolicyName :: Lens.Lens' GetRolePolicyResponse Lude.Text
grprsPolicyName = Lens.lens (policyName :: GetRolePolicyResponse -> Lude.Text) (\s a -> s {policyName = a} :: GetRolePolicyResponse)
{-# DEPRECATED grprsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsPolicyDocument :: Lens.Lens' GetRolePolicyResponse Lude.Text
grprsPolicyDocument = Lens.lens (policyDocument :: GetRolePolicyResponse -> Lude.Text) (\s a -> s {policyDocument = a} :: GetRolePolicyResponse)
{-# DEPRECATED grprsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}
