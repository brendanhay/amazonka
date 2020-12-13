{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM user.
--
-- A user can also have managed policies attached to it. To detach a managed policy from a user, use 'DetachUserPolicy' . For more information about policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteUserPolicy
  ( -- * Creating a request
    DeleteUserPolicy (..),
    mkDeleteUserPolicy,

    -- ** Request lenses
    dupfPolicyName,
    dupfUserName,

    -- * Destructuring the response
    DeleteUserPolicyResponse (..),
    mkDeleteUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserPolicy' smart constructor.
data DeleteUserPolicy = DeleteUserPolicy'
  { -- | The name identifying the policy document to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Lude.Text,
    -- | The name (friendly name, not ARN) identifying the user that the policy is embedded in.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'userName' - The name (friendly name, not ARN) identifying the user that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteUserPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  DeleteUserPolicy
mkDeleteUserPolicy pPolicyName_ pUserName_ =
  DeleteUserPolicy'
    { policyName = pPolicyName_,
      userName = pUserName_
    }

-- | The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupfPolicyName :: Lens.Lens' DeleteUserPolicy Lude.Text
dupfPolicyName = Lens.lens (policyName :: DeleteUserPolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeleteUserPolicy)
{-# DEPRECATED dupfPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name (friendly name, not ARN) identifying the user that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupfUserName :: Lens.Lens' DeleteUserPolicy Lude.Text
dupfUserName = Lens.lens (userName :: DeleteUserPolicy -> Lude.Text) (\s a -> s {userName = a} :: DeleteUserPolicy)
{-# DEPRECATED dupfUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest DeleteUserPolicy where
  type Rs DeleteUserPolicy = DeleteUserPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteUserPolicyResponse'

instance Lude.ToHeaders DeleteUserPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteUserPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserPolicy where
  toQuery DeleteUserPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteUserPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkDeleteUserPolicyResponse' smart constructor.
data DeleteUserPolicyResponse = DeleteUserPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPolicyResponse' with the minimum fields required to make a request.
mkDeleteUserPolicyResponse ::
  DeleteUserPolicyResponse
mkDeleteUserPolicyResponse = DeleteUserPolicyResponse'
