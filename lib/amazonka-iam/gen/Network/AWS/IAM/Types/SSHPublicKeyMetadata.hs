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
    sshpkmUserName,
    sshpkmSSHPublicKeyId,
    sshpkmStatus,
    sshpkmUploadDate,
  )
where

import qualified Network.AWS.IAM.Types.SSHPublicKeyId as Types
import qualified Network.AWS.IAM.Types.StatusType as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an SSH public key, without the key's body or fingerprint.
--
-- This data type is used as a response element in the 'ListSSHPublicKeys' operation.
--
-- /See:/ 'mkSSHPublicKeyMetadata' smart constructor.
data SSHPublicKeyMetadata = SSHPublicKeyMetadata'
  { -- | The name of the IAM user associated with the SSH public key.
    userName :: Types.UserName,
    -- | The unique identifier for the SSH public key.
    sSHPublicKeyId :: Types.SSHPublicKeyId,
    -- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
    status :: Types.StatusType,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
    uploadDate :: Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SSHPublicKeyMetadata' value with any optional fields omitted.
mkSSHPublicKeyMetadata ::
  -- | 'userName'
  Types.UserName ->
  -- | 'sSHPublicKeyId'
  Types.SSHPublicKeyId ->
  -- | 'status'
  Types.StatusType ->
  -- | 'uploadDate'
  Core.UTCTime ->
  SSHPublicKeyMetadata
mkSSHPublicKeyMetadata userName sSHPublicKeyId status uploadDate =
  SSHPublicKeyMetadata'
    { userName,
      sSHPublicKeyId,
      status,
      uploadDate
    }

-- | The name of the IAM user associated with the SSH public key.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkmUserName :: Lens.Lens' SSHPublicKeyMetadata Types.UserName
sshpkmUserName = Lens.field @"userName"
{-# DEPRECATED sshpkmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier for the SSH public key.
--
-- /Note:/ Consider using 'sSHPublicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkmSSHPublicKeyId :: Lens.Lens' SSHPublicKeyMetadata Types.SSHPublicKeyId
sshpkmSSHPublicKeyId = Lens.field @"sSHPublicKeyId"
{-# DEPRECATED sshpkmSSHPublicKeyId "Use generic-lens or generic-optics with 'sSHPublicKeyId' instead." #-}

-- | The status of the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkmStatus :: Lens.Lens' SSHPublicKeyMetadata Types.StatusType
sshpkmStatus = Lens.field @"status"
{-# DEPRECATED sshpkmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the SSH public key was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sshpkmUploadDate :: Lens.Lens' SSHPublicKeyMetadata Core.UTCTime
sshpkmUploadDate = Lens.field @"uploadDate"
{-# DEPRECATED sshpkmUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

instance Core.FromXML SSHPublicKeyMetadata where
  parseXML x =
    SSHPublicKeyMetadata'
      Core.<$> (x Core..@ "UserName")
      Core.<*> (x Core..@ "SSHPublicKeyId")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "UploadDate")
