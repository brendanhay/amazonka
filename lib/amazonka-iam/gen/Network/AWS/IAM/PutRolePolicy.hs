{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM role.
--
-- When you embed an inline policy in a role, the inline policy is used as part of the role's access (permissions) policy. The role's trust policy is created at the same time as the role, using 'CreateRole' . You can update a role's trust policy using 'UpdateAssumeRolePolicy' . For more information about IAM roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities> .
-- A role can also have a managed policy attached to it. To attach a managed policy to a role, use 'AttachRolePolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For information about limits on the number of inline policies that you can embed with a role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
module Network.AWS.IAM.PutRolePolicy
  ( -- * Creating a request
    PutRolePolicy (..),
    mkPutRolePolicy,

    -- ** Request lenses
    prpPolicyDocument,
    prpPolicyName,
    prpRoleName,

    -- * Destructuring the response
    PutRolePolicyResponse (..),
    mkPutRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRolePolicy' smart constructor.
data PutRolePolicy = PutRolePolicy'
  { -- | The policy document.
    --
    -- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
    --
    --     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
    --
    --
    --     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
    --
    --
    --     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
    policyDocument :: Lude.Text,
    -- | The name of the policy document.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Lude.Text,
    -- | The name of the role to associate the policy with.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRolePolicy' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- * 'policyName' - The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'roleName' - The name of the role to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkPutRolePolicy ::
  -- | 'policyDocument'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'roleName'
  Lude.Text ->
  PutRolePolicy
mkPutRolePolicy pPolicyDocument_ pPolicyName_ pRoleName_ =
  PutRolePolicy'
    { policyDocument = pPolicyDocument_,
      policyName = pPolicyName_,
      roleName = pRoleName_
    }

-- | The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyDocument :: Lens.Lens' PutRolePolicy Lude.Text
prpPolicyDocument = Lens.lens (policyDocument :: PutRolePolicy -> Lude.Text) (\s a -> s {policyDocument = a} :: PutRolePolicy)
{-# DEPRECATED prpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyName :: Lens.Lens' PutRolePolicy Lude.Text
prpPolicyName = Lens.lens (policyName :: PutRolePolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutRolePolicy)
{-# DEPRECATED prpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the role to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpRoleName :: Lens.Lens' PutRolePolicy Lude.Text
prpRoleName = Lens.lens (roleName :: PutRolePolicy -> Lude.Text) (\s a -> s {roleName = a} :: PutRolePolicy)
{-# DEPRECATED prpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest PutRolePolicy where
  type Rs PutRolePolicy = PutRolePolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull PutRolePolicyResponse'

instance Lude.ToHeaders PutRolePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutRolePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRolePolicy where
  toQuery PutRolePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutRolePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyDocument" Lude.=: policyDocument,
        "PolicyName" Lude.=: policyName,
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkPutRolePolicyResponse' smart constructor.
data PutRolePolicyResponse = PutRolePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRolePolicyResponse' with the minimum fields required to make a request.
mkPutRolePolicyResponse ::
  PutRolePolicyResponse
mkPutRolePolicyResponse = PutRolePolicyResponse'
