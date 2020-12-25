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
    sshpkUserName,
    sshpkSSHPublicKeyId,
    sshpkFingerprint,
    sshpkSSHPublicKeyBody,
    sshpkStatus,
    sshpkUploadDate,
  )
where

import qualified Network.AWS.IAM.Types.Fingerprint as Types
import qualified Network.AWS.IAM.Types.PublicKeyMaterialType as Types
import qualified Network.AWS.IAM.Types.SSHPublicKeyId as Types
import qualified Network.AWS.IAM.Types.StatusType as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an SSH public key.
--
-- This data type is used as a response element in the 'GetSSHPublicKey' and 'UploadSSHPublicKey' operations.
--
-- /See:/ 'mkSSHPublicKey' smart constructor.
data SSHPublicKey = SSHPublicKey'
  { -- | The name of the IAM user associated with the SSH public key.
    userName :: Types.UserName,
    -- | The unique identifier for the SSH public key.
    sSHPublicKeyId :: Types.SSHPublicKeyId,
    -- | The MD5 message digest of the SSH public key.
    fingerprint :: Types.Fingerprint,
    -- | The SSH public key.
    sSHPublicKeyBody :: Types.PublicKeyMaterialType,
    -- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
    status :: Types.StatusType,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
    uploadDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SSHPublicKey' value with any optional fields omitted.
mkSSHPublicKey ::
  -- | 'userName'
  Types.UserName ->
  -- | 'sSHPublicKeyId'
  Types.SSHPublicKeyId ->
  -- | 'fingerprint'
  Types.Fingerprint ->
  -- | 'sSHPublicKeyBody'
  Types.PublicKeyMaterialType ->
  -- | 'status'
  Types.StatusType ->
  SSHPublicKey
mkSSHPublicKey
  userName
  sSHPublicKeyId
  fingerprint
  sSHPublicKeyBody
  status =
    SSHPublicKey'
      { userName,
        sSHPublicKeyId,
        fingerprint,
        sSHPublicKeyBody,
        status,
        uploadDate = Core.Nothing
      }

-- | The name of the IAM user associated with the SSH public key.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkUserName :: Lens.Lens' SSHPublicKey Types.UserName
sshpkUserName = Lens.field @"userName"
{-# DEPRECATED sshpkUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- /Note:/ Consider using 'sSHPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkSSHPublicKeyId :: Lens.Lens' SSHPublicKey Types.SSHPublicKeyId
sshpkSSHPublicKeyId = Lens.field @"sSHPublicKeyId"
{-# DEPRECATED sshpkSSHPublicKeyId "Use generic-lens or generic-optics with 'sSHPublicKeyId' instead." #-}

-- | The MD5 message digest of the SSH public key.
--
-- /Note:/ Consider using 'fingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkFingerprint :: Lens.Lens' SSHPublicKey Types.Fingerprint
sshpkFingerprint = Lens.field @"fingerprint"
{-# DEPRECATED sshpkFingerprint "Use generic-lens or generic-optics with 'fingerprint' instead." #-}

-- | The SSH public key.
--
-- /Note:/ Consider using 'sSHPublicKeyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkSSHPublicKeyBody :: Lens.Lens' SSHPublicKey Types.PublicKeyMaterialType
sshpkSSHPublicKeyBody = Lens.field @"sSHPublicKeyBody"
{-# DEPRECATED sshpkSSHPublicKeyBody "Use generic-lens or generic-optics with 'sSHPublicKeyBody' instead." #-}

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkStatus :: Lens.Lens' SSHPublicKey Types.StatusType
sshpkStatus = Lens.field @"status"
{-# DEPRECATED sshpkStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkUploadDate :: Lens.Lens' SSHPublicKey (Core.Maybe Core.UTCTime)
sshpkUploadDate = Lens.field @"uploadDate"
{-# DEPRECATED sshpkUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

instance Core.FromXML SSHPublicKey where
  parseXML x =
    SSHPublicKey'
      Core.<$> (x Core..@ "UserName")
      Core.<*> (x Core..@ "SSHPublicKeyId")
      Core.<*> (x Core..@ "Fingerprint")
      Core.<*> (x Core..@ "SSHPublicKeyBody")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@? "UploadDate")
