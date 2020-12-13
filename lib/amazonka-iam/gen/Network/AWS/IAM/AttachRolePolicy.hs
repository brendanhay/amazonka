{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM role. When you attach a managed policy to a role, the managed policy becomes part of the role's permission (access) policy.
--
-- Use this API to attach a /managed/ policy to a role. To embed an inline policy in a role, use 'PutRolePolicy' . For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.AttachRolePolicy
  ( -- * Creating a request
    AttachRolePolicy (..),
    mkAttachRolePolicy,

    -- ** Request lenses
    arpRoleName,
    arpPolicyARN,

    -- * Destructuring the response
    AttachRolePolicyResponse (..),
    mkAttachRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachRolePolicy' smart constructor.
data AttachRolePolicy = AttachRolePolicy'
  { -- | The name (friendly name, not ARN) of the role to attach the policy to.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachRolePolicy' with the minimum fields required to make a request.
--
-- * 'roleName' - The name (friendly name, not ARN) of the role to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkAttachRolePolicy ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  AttachRolePolicy
mkAttachRolePolicy pRoleName_ pPolicyARN_ =
  AttachRolePolicy' {roleName = pRoleName_, policyARN = pPolicyARN_}

-- | The name (friendly name, not ARN) of the role to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpRoleName :: Lens.Lens' AttachRolePolicy Lude.Text
arpRoleName = Lens.lens (roleName :: AttachRolePolicy -> Lude.Text) (\s a -> s {roleName = a} :: AttachRolePolicy)
{-# DEPRECATED arpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arpPolicyARN :: Lens.Lens' AttachRolePolicy Lude.Text
arpPolicyARN = Lens.lens (policyARN :: AttachRolePolicy -> Lude.Text) (\s a -> s {policyARN = a} :: AttachRolePolicy)
{-# DEPRECATED arpPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest AttachRolePolicy where
  type Rs AttachRolePolicy = AttachRolePolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull AttachRolePolicyResponse'

instance Lude.ToHeaders AttachRolePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachRolePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachRolePolicy where
  toQuery AttachRolePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachRolePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkAttachRolePolicyResponse' smart constructor.
data AttachRolePolicyResponse = AttachRolePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachRolePolicyResponse' with the minimum fields required to make a request.
mkAttachRolePolicyResponse ::
  AttachRolePolicyResponse
mkAttachRolePolicyResponse = AttachRolePolicyResponse'
