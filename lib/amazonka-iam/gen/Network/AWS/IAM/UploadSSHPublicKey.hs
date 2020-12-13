{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an SSH public key and associates it with the specified IAM user.
--
-- The SSH public key uploaded by this operation can be used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.UploadSSHPublicKey
  ( -- * Creating a request
    UploadSSHPublicKey (..),
    mkUploadSSHPublicKey,

    -- ** Request lenses
    uspkUserName,
    uspkSSHPublicKeyBody,

    -- * Destructuring the response
    UploadSSHPublicKeyResponse (..),
    mkUploadSSHPublicKeyResponse,

    -- ** Response lenses
    uspkrsSSHPublicKey,
    uspkrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUploadSSHPublicKey' smart constructor.
data UploadSSHPublicKey = UploadSSHPublicKey'
  { -- | The name of the IAM user to associate the SSH public key with.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | The SSH public key. The public key must be encoded in ssh-rsa format or PEM format. The minimum bit-length of the public key is 2048 bits. For example, you can generate a 2048-bit key, and the resulting PEM file is 1679 bytes long.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
    --
    --     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
    --
    --
    --     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
    --
    --
    --     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
    sshPublicKeyBody :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadSSHPublicKey' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the IAM user to associate the SSH public key with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'sshPublicKeyBody' - The SSH public key. The public key must be encoded in ssh-rsa format or PEM format. The minimum bit-length of the public key is 2048 bits. For example, you can generate a 2048-bit key, and the resulting PEM file is 1679 bytes long.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
mkUploadSSHPublicKey ::
  -- | 'userName'
  Lude.Text ->
  -- | 'sshPublicKeyBody'
  Lude.Text ->
  UploadSSHPublicKey
mkUploadSSHPublicKey pUserName_ pSSHPublicKeyBody_ =
  UploadSSHPublicKey'
    { userName = pUserName_,
      sshPublicKeyBody = pSSHPublicKeyBody_
    }

-- | The name of the IAM user to associate the SSH public key with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspkUserName :: Lens.Lens' UploadSSHPublicKey Lude.Text
uspkUserName = Lens.lens (userName :: UploadSSHPublicKey -> Lude.Text) (\s a -> s {userName = a} :: UploadSSHPublicKey)
{-# DEPRECATED uspkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The SSH public key. The public key must be encoded in ssh-rsa format or PEM format. The minimum bit-length of the public key is 2048 bits. For example, you can generate a 2048-bit key, and the resulting PEM file is 1679 bytes long.
--
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
-- /Note:/ Consider using 'sshPublicKeyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspkSSHPublicKeyBody :: Lens.Lens' UploadSSHPublicKey Lude.Text
uspkSSHPublicKeyBody = Lens.lens (sshPublicKeyBody :: UploadSSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyBody = a} :: UploadSSHPublicKey)
{-# DEPRECATED uspkSSHPublicKeyBody "Use generic-lens or generic-optics with 'sshPublicKeyBody' instead." #-}

instance Lude.AWSRequest UploadSSHPublicKey where
  type Rs UploadSSHPublicKey = UploadSSHPublicKeyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "UploadSSHPublicKeyResult"
      ( \s h x ->
          UploadSSHPublicKeyResponse'
            Lude.<$> (x Lude..@? "SSHPublicKey") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UploadSSHPublicKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UploadSSHPublicKey where
  toPath = Lude.const "/"

instance Lude.ToQuery UploadSSHPublicKey where
  toQuery UploadSSHPublicKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UploadSSHPublicKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "SSHPublicKeyBody" Lude.=: sshPublicKeyBody
      ]

-- | Contains the response to a successful 'UploadSSHPublicKey' request.
--
-- /See:/ 'mkUploadSSHPublicKeyResponse' smart constructor.
data UploadSSHPublicKeyResponse = UploadSSHPublicKeyResponse'
  { -- | Contains information about the SSH public key.
    sshPublicKey :: Lude.Maybe SSHPublicKey,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadSSHPublicKeyResponse' with the minimum fields required to make a request.
--
-- * 'sshPublicKey' - Contains information about the SSH public key.
-- * 'responseStatus' - The response status code.
mkUploadSSHPublicKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadSSHPublicKeyResponse
mkUploadSSHPublicKeyResponse pResponseStatus_ =
  UploadSSHPublicKeyResponse'
    { sshPublicKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains information about the SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspkrsSSHPublicKey :: Lens.Lens' UploadSSHPublicKeyResponse (Lude.Maybe SSHPublicKey)
uspkrsSSHPublicKey = Lens.lens (sshPublicKey :: UploadSSHPublicKeyResponse -> Lude.Maybe SSHPublicKey) (\s a -> s {sshPublicKey = a} :: UploadSSHPublicKeyResponse)
{-# DEPRECATED uspkrsSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspkrsResponseStatus :: Lens.Lens' UploadSSHPublicKeyResponse Lude.Int
uspkrsResponseStatus = Lens.lens (responseStatus :: UploadSSHPublicKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadSSHPublicKeyResponse)
{-# DEPRECATED uspkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
