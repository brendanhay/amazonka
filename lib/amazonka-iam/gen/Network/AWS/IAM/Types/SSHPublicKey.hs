{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SSHPublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SSHPublicKey
  ( SSHPublicKey (..),

    -- * Smart constructor
    mkSSHPublicKey,

    -- * Lenses
    spkUploadDate,
    spkUserName,
    spkSSHPublicKeyId,
    spkFingerprint,
    spkSSHPublicKeyBody,
    spkStatus,
  )
where

import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an SSH public key.
--
-- This data type is used as a response element in the 'GetSSHPublicKey' and 'UploadSSHPublicKey' operations.
--
-- /See:/ 'mkSSHPublicKey' smart constructor.
data SSHPublicKey = SSHPublicKey'
  { uploadDate ::
      Lude.Maybe Lude.DateTime,
    userName :: Lude.Text,
    sshPublicKeyId :: Lude.Text,
    fingerprint :: Lude.Text,
    sshPublicKeyBody :: Lude.Text,
    status :: StatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSHPublicKey' with the minimum fields required to make a request.
--
-- * 'fingerprint' - The MD5 message digest of the SSH public key.
-- * 'sshPublicKeyBody' - The SSH public key.
-- * 'sshPublicKeyId' - The unique identifier for the SSH public key.
-- * 'status' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
-- * 'uploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
-- * 'userName' - The name of the IAM user associated with the SSH public key.
mkSSHPublicKey ::
  -- | 'userName'
  Lude.Text ->
  -- | 'sshPublicKeyId'
  Lude.Text ->
  -- | 'fingerprint'
  Lude.Text ->
  -- | 'sshPublicKeyBody'
  Lude.Text ->
  -- | 'status'
  StatusType ->
  SSHPublicKey
mkSSHPublicKey
  pUserName_
  pSSHPublicKeyId_
  pFingerprint_
  pSSHPublicKeyBody_
  pStatus_ =
    SSHPublicKey'
      { uploadDate = Lude.Nothing,
        userName = pUserName_,
        sshPublicKeyId = pSSHPublicKeyId_,
        fingerprint = pFingerprint_,
        sshPublicKeyBody = pSSHPublicKeyBody_,
        status = pStatus_
      }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkUploadDate :: Lens.Lens' SSHPublicKey (Lude.Maybe Lude.DateTime)
spkUploadDate = Lens.lens (uploadDate :: SSHPublicKey -> Lude.Maybe Lude.DateTime) (\s a -> s {uploadDate = a} :: SSHPublicKey)
{-# DEPRECATED spkUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

-- | The name of the IAM user associated with the SSH public key.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkUserName :: Lens.Lens' SSHPublicKey Lude.Text
spkUserName = Lens.lens (userName :: SSHPublicKey -> Lude.Text) (\s a -> s {userName = a} :: SSHPublicKey)
{-# DEPRECATED spkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- /Note:/ Consider using 'sshPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkSSHPublicKeyId :: Lens.Lens' SSHPublicKey Lude.Text
spkSSHPublicKeyId = Lens.lens (sshPublicKeyId :: SSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyId = a} :: SSHPublicKey)
{-# DEPRECATED spkSSHPublicKeyId "Use generic-lens or generic-optics with 'sshPublicKeyId' instead." #-}

-- | The MD5 message digest of the SSH public key.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkFingerprint :: Lens.Lens' SSHPublicKey Lude.Text
spkFingerprint = Lens.lens (fingerprint :: SSHPublicKey -> Lude.Text) (\s a -> s {fingerprint = a} :: SSHPublicKey)
{-# DEPRECATED spkFingerprint "Use generic-lens or generic-optics with 'fingerprint' instead." #-}

-- | The SSH public key.
--
-- /Note:/ Consider using 'sshPublicKeyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkSSHPublicKeyBody :: Lens.Lens' SSHPublicKey Lude.Text
spkSSHPublicKeyBody = Lens.lens (sshPublicKeyBody :: SSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyBody = a} :: SSHPublicKey)
{-# DEPRECATED spkSSHPublicKeyBody "Use generic-lens or generic-optics with 'sshPublicKeyBody' instead." #-}

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkStatus :: Lens.Lens' SSHPublicKey StatusType
spkStatus = Lens.lens (status :: SSHPublicKey -> StatusType) (\s a -> s {status = a} :: SSHPublicKey)
{-# DEPRECATED spkStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML SSHPublicKey where
  parseXML x =
    SSHPublicKey'
      Lude.<$> (x Lude..@? "UploadDate")
      Lude.<*> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "SSHPublicKeyId")
      Lude.<*> (x Lude..@ "Fingerprint")
      Lude.<*> (x Lude..@ "SSHPublicKeyBody")
      Lude.<*> (x Lude..@ "Status")
