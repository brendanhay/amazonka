{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEDescription
  ( SSEDescription (..),

    -- * Smart constructor
    mkSSEDescription,

    -- * Lenses
    ssedInaccessibleEncryptionDateTime,
    ssedKMSMasterKeyArn,
    ssedSSEType,
    ssedStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types.KMSMasterKeyArn as Types
import qualified Network.AWS.DynamoDB.Types.SSEStatus as Types
import qualified Network.AWS.DynamoDB.Types.SSEType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of the server-side encryption status on the specified table.
--
-- /See:/ 'mkSSEDescription' smart constructor.
data SSEDescription = SSEDescription'
  { -- | Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
    inaccessibleEncryptionDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
    kMSMasterKeyArn :: Core.Maybe Types.KMSMasterKeyArn,
    -- | Server-side encryption type. The only supported value is:
    --
    --
    --     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
    sSEType :: Core.Maybe Types.SSEType,
    -- | Represents the current state of server-side encryption. The only supported values are:
    --
    --
    --     * @ENABLED@ - Server-side encryption is enabled.
    --
    --
    --     * @UPDATING@ - Server-side encryption is being updated.
    status :: Core.Maybe Types.SSEStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SSEDescription' value with any optional fields omitted.
mkSSEDescription ::
  SSEDescription
mkSSEDescription =
  SSEDescription'
    { inaccessibleEncryptionDateTime = Core.Nothing,
      kMSMasterKeyArn = Core.Nothing,
      sSEType = Core.Nothing,
      status = Core.Nothing
    }

-- | Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
--
-- /Note:/ Consider using 'inaccessibleEncryptionDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedInaccessibleEncryptionDateTime :: Lens.Lens' SSEDescription (Core.Maybe Core.NominalDiffTime)
ssedInaccessibleEncryptionDateTime = Lens.field @"inaccessibleEncryptionDateTime"
{-# DEPRECATED ssedInaccessibleEncryptionDateTime "Use generic-lens or generic-optics with 'inaccessibleEncryptionDateTime' instead." #-}

-- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
--
-- /Note:/ Consider using 'kMSMasterKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedKMSMasterKeyArn :: Lens.Lens' SSEDescription (Core.Maybe Types.KMSMasterKeyArn)
ssedKMSMasterKeyArn = Lens.field @"kMSMasterKeyArn"
{-# DEPRECATED ssedKMSMasterKeyArn "Use generic-lens or generic-optics with 'kMSMasterKeyArn' instead." #-}

-- | Server-side encryption type. The only supported value is:
--
--
--     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
--
--
--
-- /Note:/ Consider using 'sSEType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedSSEType :: Lens.Lens' SSEDescription (Core.Maybe Types.SSEType)
ssedSSEType = Lens.field @"sSEType"
{-# DEPRECATED ssedSSEType "Use generic-lens or generic-optics with 'sSEType' instead." #-}

-- | Represents the current state of server-side encryption. The only supported values are:
--
--
--     * @ENABLED@ - Server-side encryption is enabled.
--
--
--     * @UPDATING@ - Server-side encryption is being updated.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedStatus :: Lens.Lens' SSEDescription (Core.Maybe Types.SSEStatus)
ssedStatus = Lens.field @"status"
{-# DEPRECATED ssedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON SSEDescription where
  parseJSON =
    Core.withObject "SSEDescription" Core.$
      \x ->
        SSEDescription'
          Core.<$> (x Core..:? "InaccessibleEncryptionDateTime")
          Core.<*> (x Core..:? "KMSMasterKeyArn")
          Core.<*> (x Core..:? "SSEType")
          Core.<*> (x Core..:? "Status")
