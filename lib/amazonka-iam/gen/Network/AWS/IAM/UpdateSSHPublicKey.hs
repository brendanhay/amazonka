{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of an IAM user's SSH public key to active or inactive. SSH public keys that are inactive cannot be used for authentication. This operation can be used to disable a user's SSH public key as part of a key rotation work flow.
--
-- The SSH public key affected by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.UpdateSSHPublicKey
  ( -- * Creating a request
    UpdateSSHPublicKey (..),
    mkUpdateSSHPublicKey,

    -- ** Request lenses
    usshpkStatus,
    usshpkSSHPublicKeyId,
    usshpkUserName,

    -- * Destructuring the response
    UpdateSSHPublicKeyResponse (..),
    mkUpdateSSHPublicKeyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSSHPublicKey' smart constructor.
data UpdateSSHPublicKey = UpdateSSHPublicKey'
  { -- | The status to assign to the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
    status :: StatusType,
    -- | The unique identifier for the SSH public key.
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

-- | Creates a value of 'UpdateSSHPublicKey' with the minimum fields required to make a request.
--
-- * 'status' - The status to assign to the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
-- * 'sshPublicKeyId' - The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
-- * 'userName' - The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkUpdateSSHPublicKey ::
  -- | 'status'
  StatusType ->
  -- | 'sshPublicKeyId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  UpdateSSHPublicKey
mkUpdateSSHPublicKey pStatus_ pSSHPublicKeyId_ pUserName_ =
  UpdateSSHPublicKey'
    { status = pStatus_,
      sshPublicKeyId = pSSHPublicKeyId_,
      userName = pUserName_
    }

-- | The status to assign to the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkStatus :: Lens.Lens' UpdateSSHPublicKey StatusType
usshpkStatus = Lens.lens (status :: UpdateSSHPublicKey -> StatusType) (\s a -> s {status = a} :: UpdateSSHPublicKey)
{-# DEPRECATED usshpkStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'sshPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkSSHPublicKeyId :: Lens.Lens' UpdateSSHPublicKey Lude.Text
usshpkSSHPublicKeyId = Lens.lens (sshPublicKeyId :: UpdateSSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyId = a} :: UpdateSSHPublicKey)
{-# DEPRECATED usshpkSSHPublicKeyId "Use generic-lens or generic-optics with 'sshPublicKeyId' instead." #-}

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usshpkUserName :: Lens.Lens' UpdateSSHPublicKey Lude.Text
usshpkUserName = Lens.lens (userName :: UpdateSSHPublicKey -> Lude.Text) (\s a -> s {userName = a} :: UpdateSSHPublicKey)
{-# DEPRECATED usshpkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest UpdateSSHPublicKey where
  type Rs UpdateSSHPublicKey = UpdateSSHPublicKeyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateSSHPublicKeyResponse'

instance Lude.ToHeaders UpdateSSHPublicKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateSSHPublicKey where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSSHPublicKey where
  toQuery UpdateSSHPublicKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateSSHPublicKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Status" Lude.=: status,
        "SSHPublicKeyId" Lude.=: sshPublicKeyId,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkUpdateSSHPublicKeyResponse' smart constructor.
data UpdateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSSHPublicKeyResponse' with the minimum fields required to make a request.
mkUpdateSSHPublicKeyResponse ::
  UpdateSSHPublicKeyResponse
mkUpdateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'
