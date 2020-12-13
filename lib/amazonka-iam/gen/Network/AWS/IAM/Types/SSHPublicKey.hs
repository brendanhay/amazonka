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
    spkStatus,
    spkUploadDate,
    spkFingerprint,
    spkSSHPublicKeyId,
    spkUserName,
    spkSSHPublicKeyBody,
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
  { -- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
    status :: StatusType,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
    uploadDate :: Lude.Maybe Lude.DateTime,
    -- | The MD5 message digest of the SSH public key.
    fingerprint :: Lude.Text,
    -- | The unique identifier for the SSH public key.
    sshPublicKeyId :: Lude.Text,
    -- | The name of the IAM user associated with the SSH public key.
    userName :: Lude.Text,
    -- | The SSH public key.
    sshPublicKeyBody :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSHPublicKey' with the minimum fields required to make a request.
--
-- * 'status' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
-- * 'uploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
-- * 'fingerprint' - The MD5 message digest of the SSH public key.
-- * 'sshPublicKeyId' - The unique identifier for the SSH public key.
-- * 'userName' - The name of the IAM user associated with the SSH public key.
-- * 'sshPublicKeyBody' - The SSH public key.
mkSSHPublicKey ::
  -- | 'status'
  StatusType ->
  -- | 'fingerprint'
  Lude.Text ->
  -- | 'sshPublicKeyId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'sshPublicKeyBody'
  Lude.Text ->
  SSHPublicKey
mkSSHPublicKey
  pStatus_
  pFingerprint_
  pSSHPublicKeyId_
  pUserName_
  pSSHPublicKeyBody_ =
    SSHPublicKey'
      { status = pStatus_,
        uploadDate = Lude.Nothing,
        fingerprint = pFingerprint_,
        sshPublicKeyId = pSSHPublicKeyId_,
        userName = pUserName_,
        sshPublicKeyBody = pSSHPublicKeyBody_
      }

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkStatus :: Lens.Lens' SSHPublicKey StatusType
spkStatus = Lens.lens (status :: SSHPublicKey -> StatusType) (\s a -> s {status = a} :: SSHPublicKey)
{-# DEPRECATED spkStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkUploadDate :: Lens.Lens' SSHPublicKey (Lude.Maybe Lude.DateTime)
spkUploadDate = Lens.lens (uploadDate :: SSHPublicKey -> Lude.Maybe Lude.DateTime) (\s a -> s {uploadDate = a} :: SSHPublicKey)
{-# DEPRECATED spkUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

-- | The MD5 message digest of the SSH public key.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkFingerprint :: Lens.Lens' SSHPublicKey Lude.Text
spkFingerprint = Lens.lens (fingerprint :: SSHPublicKey -> Lude.Text) (\s a -> s {fingerprint = a} :: SSHPublicKey)
{-# DEPRECATED spkFingerprint "Use generic-lens or generic-optics with 'fingerprint' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- /Note:/ Consider using 'sshPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkSSHPublicKeyId :: Lens.Lens' SSHPublicKey Lude.Text
spkSSHPublicKeyId = Lens.lens (sshPublicKeyId :: SSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyId = a} :: SSHPublicKey)
{-# DEPRECATED spkSSHPublicKeyId "Use generic-lens or generic-optics with 'sshPublicKeyId' instead." #-}

-- | The name of the IAM user associated with the SSH public key.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkUserName :: Lens.Lens' SSHPublicKey Lude.Text
spkUserName = Lens.lens (userName :: SSHPublicKey -> Lude.Text) (\s a -> s {userName = a} :: SSHPublicKey)
{-# DEPRECATED spkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The SSH public key.
--
-- /Note:/ Consider using 'sshPublicKeyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkSSHPublicKeyBody :: Lens.Lens' SSHPublicKey Lude.Text
spkSSHPublicKeyBody = Lens.lens (sshPublicKeyBody :: SSHPublicKey -> Lude.Text) (\s a -> s {sshPublicKeyBody = a} :: SSHPublicKey)
{-# DEPRECATED spkSSHPublicKeyBody "Use generic-lens or generic-optics with 'sshPublicKeyBody' instead." #-}

instance Lude.FromXML SSHPublicKey where
  parseXML x =
    SSHPublicKey'
      Lude.<$> (x Lude..@ "Status")
      Lude.<*> (x Lude..@? "UploadDate")
      Lude.<*> (x Lude..@ "Fingerprint")
      Lude.<*> (x Lude..@ "SSHPublicKeyId")
      Lude.<*> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "SSHPublicKeyBody")
