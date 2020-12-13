{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteRolePolicy' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DetachRolePolicy
  ( -- * Creating a request
    DetachRolePolicy (..),
    mkDetachRolePolicy,

    -- ** Request lenses
    drpRoleName,
    drpPolicyARN,

    -- * Destructuring the response
    DetachRolePolicyResponse (..),
    mkDetachRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachRolePolicy' smart constructor.
data DetachRolePolicy = DetachRolePolicy'
  { -- | The name (friendly name, not ARN) of the IAM role to detach the policy from.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachRolePolicy' with the minimum fields required to make a request.
--
-- * 'roleName' - The name (friendly name, not ARN) of the IAM role to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkDetachRolePolicy ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  DetachRolePolicy
mkDetachRolePolicy pRoleName_ pPolicyARN_ =
  DetachRolePolicy' {roleName = pRoleName_, policyARN = pPolicyARN_}

-- | The name (friendly name, not ARN) of the IAM role to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRoleName :: Lens.Lens' DetachRolePolicy Lude.Text
drpRoleName = Lens.lens (roleName :: DetachRolePolicy -> Lude.Text) (\s a -> s {roleName = a} :: DetachRolePolicy)
{-# DEPRECATED drpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPolicyARN :: Lens.Lens' DetachRolePolicy Lude.Text
drpPolicyARN = Lens.lens (policyARN :: DetachRolePolicy -> Lude.Text) (\s a -> s {policyARN = a} :: DetachRolePolicy)
{-# DEPRECATED drpPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest DetachRolePolicy where
  type Rs DetachRolePolicy = DetachRolePolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DetachRolePolicyResponse'

instance Lude.ToHeaders DetachRolePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachRolePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachRolePolicy where
  toQuery DetachRolePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachRolePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkDetachRolePolicyResponse' smart constructor.
data DetachRolePolicyResponse = DetachRolePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachRolePolicyResponse' with the minimum fields required to make a request.
mkDetachRolePolicyResponse ::
  DetachRolePolicyResponse
mkDetachRolePolicyResponse = DetachRolePolicyResponse'
