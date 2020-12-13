{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM user. Unlike the AWS Management Console, when you delete a user programmatically, you must delete the items attached to the user manually, or the deletion fails. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_deleting_cli Deleting an IAM User> . Before attempting to delete a user, remove the following items:
--
--
--     * Password ('DeleteLoginProfile' )
--
--
--     * Access keys ('DeleteAccessKey' )
--
--
--     * Signing certificate ('DeleteSigningCertificate' )
--
--
--     * SSH public key ('DeleteSSHPublicKey' )
--
--
--     * Git credentials ('DeleteServiceSpecificCredential' )
--
--
--     * Multi-factor authentication (MFA) device ('DeactivateMFADevice' , 'DeleteVirtualMFADevice' )
--
--
--     * Inline policies ('DeleteUserPolicy' )
--
--
--     * Attached managed policies ('DetachUserPolicy' )
--
--
--     * Group memberships ('RemoveUserFromGroup' )
module Network.AWS.IAM.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    duUserName,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUser' smart constructor.
newtype DeleteUser = DeleteUser'
  { -- | The name of the user to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteUser ::
  -- | 'userName'
  Lude.Text ->
  DeleteUser
mkDeleteUser pUserName_ = DeleteUser' {userName = pUserName_}

-- | The name of the user to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserName :: Lens.Lens' DeleteUser Lude.Text
duUserName = Lens.lens (userName :: DeleteUser -> Lude.Text) (\s a -> s {userName = a} :: DeleteUser)
{-# DEPRECATED duUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteUserResponse'

instance Lude.ToHeaders DeleteUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUser where
  toQuery DeleteUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
mkDeleteUserResponse ::
  DeleteUserResponse
mkDeleteUserResponse = DeleteUserResponse'
