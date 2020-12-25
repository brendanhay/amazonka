{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Encryption
  ( Encryption (..),

    -- * Smart constructor
    mkEncryption,

    -- * Lenses
    eEncryptionType,
    eKMSContext,
    eKMSKeyId,
  )
where

import qualified Network.AWS.Glacier.Types.EncryptionType as Types
import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the encryption used to store the job results in Amazon S3.
--
-- /See:/ 'mkEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
    encryptionType :: Core.Maybe Types.EncryptionType,
    -- | Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
    kMSContext :: Core.Maybe Types.String,
    -- | The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
    kMSKeyId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Encryption' value with any optional fields omitted.
mkEncryption ::
  Encryption
mkEncryption =
  Encryption'
    { encryptionType = Core.Nothing,
      kMSContext = Core.Nothing,
      kMSKeyId = Core.Nothing
    }

-- | The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionType :: Lens.Lens' Encryption (Core.Maybe Types.EncryptionType)
eEncryptionType = Lens.field @"encryptionType"
{-# DEPRECATED eEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
--
-- /Note:/ Consider using 'kMSContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSContext :: Lens.Lens' Encryption (Core.Maybe Types.String)
eKMSContext = Lens.field @"kMSContext"
{-# DEPRECATED eKMSContext "Use generic-lens or generic-optics with 'kMSContext' instead." #-}

-- | The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
--
-- /Note:/ Consider using 'kMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSKeyId :: Lens.Lens' Encryption (Core.Maybe Types.String)
eKMSKeyId = Lens.field @"kMSKeyId"
{-# DEPRECATED eKMSKeyId "Use generic-lens or generic-optics with 'kMSKeyId' instead." #-}

instance Core.FromJSON Encryption where
  toJSON Encryption {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncryptionType" Core..=) Core.<$> encryptionType,
            ("KMSContext" Core..=) Core.<$> kMSContext,
            ("KMSKeyId" Core..=) Core.<$> kMSKeyId
          ]
      )

instance Core.FromJSON Encryption where
  parseJSON =
    Core.withObject "Encryption" Core.$
      \x ->
        Encryption'
          Core.<$> (x Core..:? "EncryptionType")
          Core.<*> (x Core..:? "KMSContext")
          Core.<*> (x Core..:? "KMSKeyId")
