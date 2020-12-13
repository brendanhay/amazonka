{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified SSH public key.
--
-- The SSH public key deleted by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.DeleteSSHPublicKey
  ( -- * Creating a request
    DeleteSSHPublicKey (..),
    mkDeleteSSHPublicKey,

    -- ** Request lenses
    dspkSSHPublicKeyId,
    dspkUserName,

    -- * Destructuring the response
    DeleteSSHPublicKeyResponse (..),
    mkDeleteSSHPublicKeyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSSHPublicKey' smart constructor.
data DeleteSSHPublicKey = DeleteSSHPublicKey'
  { -- | The unique identifier for the SSH public key.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    sshPublicKeyId :: Lude.Text,
    -- | The name of the IAM user associated with the SSH public key.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSSHPublicKey' with the minimum fields required to make a request.
--
-- * 'sshPublicKeyId' - The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
-- * 'userName' - The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteSSHPublicKey ::
  -- | 'sshPublicKeyId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  DeleteSSHPublicKey
mkDeleteSSHPublicKey pSSHPublicKeyId_ pUserName_ =
  DeleteSSHPublicKey'
    { sshPublicKeyId = pSSHPublicKeyId_,
      userName = pUserName_
    }

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'sshPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspkSSHPublicKeyId :: Lens.Lens' DeleteSSHPublicKey Lude.Text
dspkSSHPublicKeyId = Lens.lens (sshPublicKeyId :: DeleteSSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyId = a} :: DeleteSSHPublicKey)
{-# DEPRECATED dspkSSHPublicKeyId "Use generic-lens or generic-optics with 'sshPublicKeyId' instead." #-}

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspkUserName :: Lens.Lens' DeleteSSHPublicKey Lude.Text
dspkUserName = Lens.lens (userName :: DeleteSSHPublicKey -> Lude.Text) (\s a -> s {userName = a} :: DeleteSSHPublicKey)
{-# DEPRECATED dspkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest DeleteSSHPublicKey where
  type Rs DeleteSSHPublicKey = DeleteSSHPublicKeyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteSSHPublicKeyResponse'

instance Lude.ToHeaders DeleteSSHPublicKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSSHPublicKey where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSSHPublicKey where
  toQuery DeleteSSHPublicKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSSHPublicKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "SSHPublicKeyId" Lude.=: sshPublicKeyId,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkDeleteSSHPublicKeyResponse' smart constructor.
data DeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSSHPublicKeyResponse' with the minimum fields required to make a request.
mkDeleteSSHPublicKeyResponse ::
  DeleteSSHPublicKeyResponse
mkDeleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'
