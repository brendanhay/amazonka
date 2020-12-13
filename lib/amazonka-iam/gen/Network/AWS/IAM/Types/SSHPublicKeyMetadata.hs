{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SSHPublicKeyMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SSHPublicKeyMetadata
  ( SSHPublicKeyMetadata (..),

    -- * Smart constructor
    mkSSHPublicKeyMetadata,

    -- * Lenses
    spkmStatus,
    spkmUploadDate,
    spkmSSHPublicKeyId,
    spkmUserName,
  )
where

import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an SSH public key, without the key's body or fingerprint.
--
-- This data type is used as a response element in the 'ListSSHPublicKeys' operation.
--
-- /See:/ 'mkSSHPublicKeyMetadata' smart constructor.
data SSHPublicKeyMetadata = SSHPublicKeyMetadata'
  { -- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
    status :: StatusType,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
    uploadDate :: Lude.DateTime,
    -- | The unique identifier for the SSH public key.
    sshPublicKeyId :: Lude.Text,
    -- | The name of the IAM user associated with the SSH public key.
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSHPublicKeyMetadata' with the minimum fields required to make a request.
--
-- * 'status' - The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
-- * 'uploadDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
-- * 'sshPublicKeyId' - The unique identifier for the SSH public key.
-- * 'userName' - The name of the IAM user associated with the SSH public key.
mkSSHPublicKeyMetadata ::
  -- | 'status'
  StatusType ->
  -- | 'uploadDate'
  Lude.DateTime ->
  -- | 'sshPublicKeyId'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  SSHPublicKeyMetadata
mkSSHPublicKeyMetadata
  pStatus_
  pUploadDate_
  pSSHPublicKeyId_
  pUserName_ =
    SSHPublicKeyMetadata'
      { status = pStatus_,
        uploadDate = pUploadDate_,
        sshPublicKeyId = pSSHPublicKeyId_,
        userName = pUserName_
      }

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkmStatus :: Lens.Lens' SSHPublicKeyMetadata StatusType
spkmStatus = Lens.lens (status :: SSHPublicKeyMetadata -> StatusType) (\s a -> s {status = a} :: SSHPublicKeyMetadata)
{-# DEPRECATED spkmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkmUploadDate :: Lens.Lens' SSHPublicKeyMetadata Lude.DateTime
spkmUploadDate = Lens.lens (uploadDate :: SSHPublicKeyMetadata -> Lude.DateTime) (\s a -> s {uploadDate = a} :: SSHPublicKeyMetadata)
{-# DEPRECATED spkmUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- /Note:/ Consider using 'sshPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkmSSHPublicKeyId :: Lens.Lens' SSHPublicKeyMetadata Lude.Text
spkmSSHPublicKeyId = Lens.lens (sshPublicKeyId :: SSHPublicKeyMetadata -> Lude.Text) (\s a -> s {sshPublicKeyId = a} :: SSHPublicKeyMetadata)
{-# DEPRECATED spkmSSHPublicKeyId "Use generic-lens or generic-optics with 'sshPublicKeyId' instead." #-}

-- | The name of the IAM user associated with the SSH public key.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spkmUserName :: Lens.Lens' SSHPublicKeyMetadata Lude.Text
spkmUserName = Lens.lens (userName :: SSHPublicKeyMetadata -> Lude.Text) (\s a -> s {userName = a} :: SSHPublicKeyMetadata)
{-# DEPRECATED spkmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.FromXML SSHPublicKeyMetadata where
  parseXML x =
    SSHPublicKeyMetadata'
      Lude.<$> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "UploadDate")
      Lude.<*> (x Lude..@ "SSHPublicKeyId")
      Lude.<*> (x Lude..@ "UserName")
