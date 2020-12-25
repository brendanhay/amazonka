{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Encryption
  ( Encryption (..),

    -- * Smart constructor
    mkEncryption,

    -- * Lenses
    eEncryptionType,
    eKMSContext,
    eKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.KMSContext as Types
import qualified Network.AWS.S3.Types.KMSKeyId as Types
import qualified Network.AWS.S3.Types.ServerSideEncryption as Types

-- | Contains the type of server-side encryption used.
--
-- /See:/ 'mkEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
    encryptionType :: Types.ServerSideEncryption,
    -- | If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
    kMSContext :: Core.Maybe Types.KMSContext,
    -- | If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
    kMSKeyId :: Core.Maybe Types.KMSKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Encryption' value with any optional fields omitted.
mkEncryption ::
  -- | 'encryptionType'
  Types.ServerSideEncryption ->
  Encryption
mkEncryption encryptionType =
  Encryption'
    { encryptionType,
      kMSContext = Core.Nothing,
      kMSKeyId = Core.Nothing
    }

-- | The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionType :: Lens.Lens' Encryption Types.ServerSideEncryption
eEncryptionType = Lens.field @"encryptionType"
{-# DEPRECATED eEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
--
-- /Note:/ Consider using 'kMSContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSContext :: Lens.Lens' Encryption (Core.Maybe Types.KMSContext)
eKMSContext = Lens.field @"kMSContext"
{-# DEPRECATED eKMSContext "Use generic-lens or generic-optics with 'kMSContext' instead." #-}

-- | If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSKeyId :: Lens.Lens' Encryption (Core.Maybe Types.KMSKeyId)
eKMSKeyId = Lens.field @"kMSKeyId"
{-# DEPRECATED eKMSKeyId "Use generic-lens or generic-optics with 'kMSKeyId' instead." #-}

instance Core.ToXML Encryption where
  toXML Encryption {..} =
    Core.toXMLNode "EncryptionType" encryptionType
      Core.<> Core.toXMLNode "KMSContext" Core.<$> kMSContext
      Core.<> Core.toXMLNode "KMSKeyId" Core.<$> kMSKeyId
