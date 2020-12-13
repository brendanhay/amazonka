{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetSSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified SSH public key, including metadata about the key.
--
-- The SSH public key retrieved by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <https://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
module Network.AWS.IAM.GetSSHPublicKey
  ( -- * Creating a request
    GetSSHPublicKey (..),
    mkGetSSHPublicKey,

    -- ** Request lenses
    gspkSSHPublicKeyId,
    gspkUserName,
    gspkEncoding,

    -- * Destructuring the response
    GetSSHPublicKeyResponse (..),
    mkGetSSHPublicKeyResponse,

    -- ** Response lenses
    gspkrsSSHPublicKey,
    gspkrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSSHPublicKey' smart constructor.
data GetSSHPublicKey = GetSSHPublicKey'
  { -- | The unique identifier for the SSH public key.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    sshPublicKeyId :: Lude.Text,
    -- | The name of the IAM user associated with the SSH public key.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
    encoding :: EncodingType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSSHPublicKey' with the minimum fields required to make a request.
--
-- * 'sshPublicKeyId' - The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
-- * 'userName' - The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'encoding' - Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
mkGetSSHPublicKey ::
  -- | 'sshPublicKeyId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'encoding'
  EncodingType ->
  GetSSHPublicKey
mkGetSSHPublicKey pSSHPublicKeyId_ pUserName_ pEncoding_ =
  GetSSHPublicKey'
    { sshPublicKeyId = pSSHPublicKeyId_,
      userName = pUserName_,
      encoding = pEncoding_
    }

-- | The unique identifier for the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'sshPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspkSSHPublicKeyId :: Lens.Lens' GetSSHPublicKey Lude.Text
gspkSSHPublicKeyId = Lens.lens (sshPublicKeyId :: GetSSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyId = a} :: GetSSHPublicKey)
{-# DEPRECATED gspkSSHPublicKeyId "Use generic-lens or generic-optics with 'sshPublicKeyId' instead." #-}

-- | The name of the IAM user associated with the SSH public key.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspkUserName :: Lens.Lens' GetSSHPublicKey Lude.Text
gspkUserName = Lens.lens (userName :: GetSSHPublicKey -> Lude.Text) (\s a -> s {userName = a} :: GetSSHPublicKey)
{-# DEPRECATED gspkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
--
-- /Note:/ Consider using 'encoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspkEncoding :: Lens.Lens' GetSSHPublicKey EncodingType
gspkEncoding = Lens.lens (encoding :: GetSSHPublicKey -> EncodingType) (\s a -> s {encoding = a} :: GetSSHPublicKey)
{-# DEPRECATED gspkEncoding "Use generic-lens or generic-optics with 'encoding' instead." #-}

instance Lude.AWSRequest GetSSHPublicKey where
  type Rs GetSSHPublicKey = GetSSHPublicKeyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetSSHPublicKeyResult"
      ( \s h x ->
          GetSSHPublicKeyResponse'
            Lude.<$> (x Lude..@? "SSHPublicKey") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSSHPublicKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetSSHPublicKey where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSSHPublicKey where
  toQuery GetSSHPublicKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetSSHPublicKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "SSHPublicKeyId" Lude.=: sshPublicKeyId,
        "UserName" Lude.=: userName,
        "Encoding" Lude.=: encoding
      ]

-- | Contains the response to a successful 'GetSSHPublicKey' request.
--
-- /See:/ 'mkGetSSHPublicKeyResponse' smart constructor.
data GetSSHPublicKeyResponse = GetSSHPublicKeyResponse'
  { -- | A structure containing details about the SSH public key.
    sshPublicKey :: Lude.Maybe SSHPublicKey,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSSHPublicKeyResponse' with the minimum fields required to make a request.
--
-- * 'sshPublicKey' - A structure containing details about the SSH public key.
-- * 'responseStatus' - The response status code.
mkGetSSHPublicKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSSHPublicKeyResponse
mkGetSSHPublicKeyResponse pResponseStatus_ =
  GetSSHPublicKeyResponse'
    { sshPublicKey = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspkrsSSHPublicKey :: Lens.Lens' GetSSHPublicKeyResponse (Lude.Maybe SSHPublicKey)
gspkrsSSHPublicKey = Lens.lens (sshPublicKey :: GetSSHPublicKeyResponse -> Lude.Maybe SSHPublicKey) (\s a -> s {sshPublicKey = a} :: GetSSHPublicKeyResponse)
{-# DEPRECATED gspkrsSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspkrsResponseStatus :: Lens.Lens' GetSSHPublicKeyResponse Lude.Int
gspkrsResponseStatus = Lens.lens (responseStatus :: GetSSHPublicKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSSHPublicKeyResponse)
{-# DEPRECATED gspkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
