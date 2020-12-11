{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the policy that grants an IAM entity permission to assume a role. This is typically referred to as the "role trust policy". For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities> .
module Network.AWS.IAM.UpdateAssumeRolePolicy
  ( -- * Creating a request
    UpdateAssumeRolePolicy (..),
    mkUpdateAssumeRolePolicy,

    -- ** Request lenses
    uarpRoleName,
    uarpPolicyDocument,

    -- * Destructuring the response
    UpdateAssumeRolePolicyResponse (..),
    mkUpdateAssumeRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAssumeRolePolicy' smart constructor.
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy'
  { roleName ::
      Lude.Text,
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

-- | Creates a value of 'UpdateAssumeRolePolicy' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy that grants an entity permission to assume the role.
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
-- * 'roleName' - The name of the role to update with the new policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkUpdateAssumeRolePolicy ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  UpdateAssumeRolePolicy
mkUpdateAssumeRolePolicy pRoleName_ pPolicyDocument_ =
  UpdateAssumeRolePolicy'
    { roleName = pRoleName_,
      policyDocument = pPolicyDocument_
    }

-- | The name of the role to update with the new policy.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarpRoleName :: Lens.Lens' UpdateAssumeRolePolicy Lude.Text
uarpRoleName = Lens.lens (roleName :: UpdateAssumeRolePolicy -> Lude.Text) (\s a -> s {roleName = a} :: UpdateAssumeRolePolicy)
{-# DEPRECATED uarpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The policy that grants an entity permission to assume the role.
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
uarpPolicyDocument :: Lens.Lens' UpdateAssumeRolePolicy Lude.Text
uarpPolicyDocument = Lens.lens (policyDocument :: UpdateAssumeRolePolicy -> Lude.Text) (\s a -> s {policyDocument = a} :: UpdateAssumeRolePolicy)
{-# DEPRECATED uarpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest UpdateAssumeRolePolicy where
  type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateAssumeRolePolicyResponse'

instance Lude.ToHeaders UpdateAssumeRolePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateAssumeRolePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAssumeRolePolicy where
  toQuery UpdateAssumeRolePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateAssumeRolePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "PolicyDocument" Lude.=: policyDocument
      ]

-- | /See:/ 'mkUpdateAssumeRolePolicyResponse' smart constructor.
data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAssumeRolePolicyResponse' with the minimum fields required to make a request.
mkUpdateAssumeRolePolicyResponse ::
  UpdateAssumeRolePolicyResponse
mkUpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'
