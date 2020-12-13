{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM role.
--
-- A role can also have managed policies attached to it. To detach a managed policy from a role, use 'DetachRolePolicy' . For more information about policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteRolePolicy
  ( -- * Creating a request
    DeleteRolePolicy (..),
    mkDeleteRolePolicy,

    -- ** Request lenses
    dPolicyName,
    dRoleName,

    -- * Destructuring the response
    DeleteRolePolicyResponse (..),
    mkDeleteRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRolePolicy' smart constructor.
data DeleteRolePolicy = DeleteRolePolicy'
  { -- | The name of the inline policy to delete from the specified IAM role.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Lude.Text,
    -- | The name (friendly name, not ARN) identifying the role that the policy is embedded in.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRolePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the inline policy to delete from the specified IAM role.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'roleName' - The name (friendly name, not ARN) identifying the role that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteRolePolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'roleName'
  Lude.Text ->
  DeleteRolePolicy
mkDeleteRolePolicy pPolicyName_ pRoleName_ =
  DeleteRolePolicy'
    { policyName = pPolicyName_,
      roleName = pRoleName_
    }

-- | The name of the inline policy to delete from the specified IAM role.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyName :: Lens.Lens' DeleteRolePolicy Lude.Text
dPolicyName = Lens.lens (policyName :: DeleteRolePolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeleteRolePolicy)
{-# DEPRECATED dPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name (friendly name, not ARN) identifying the role that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoleName :: Lens.Lens' DeleteRolePolicy Lude.Text
dRoleName = Lens.lens (roleName :: DeleteRolePolicy -> Lude.Text) (\s a -> s {roleName = a} :: DeleteRolePolicy)
{-# DEPRECATED dRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest DeleteRolePolicy where
  type Rs DeleteRolePolicy = DeleteRolePolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteRolePolicyResponse'

instance Lude.ToHeaders DeleteRolePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRolePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRolePolicy where
  toQuery DeleteRolePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteRolePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkDeleteRolePolicyResponse' smart constructor.
data DeleteRolePolicyResponse = DeleteRolePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRolePolicyResponse' with the minimum fields required to make a request.
mkDeleteRolePolicyResponse ::
  DeleteRolePolicyResponse
mkDeleteRolePolicyResponse = DeleteRolePolicyResponse'
