{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..)
  -- * Smart constructor
  , mkEncryptionConfiguration
  -- * Lenses
  , ecEncryptionType
  , ecKmsKey
  ) where

import qualified Network.AWS.ECR.Types.EncryptionType as Types
import qualified Network.AWS.ECR.Types.KmsKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
-- By default, when no encryption configuration is set or the @AES256@ encryption type is used, Amazon ECR uses server-side encryption with Amazon S3-managed encryption keys which encrypts your data at rest using an AES-256 encryption algorithm. This does not require any action on your part.
-- For more control over the encryption of the contents of your repository, you can use server-side encryption with customer master keys (CMKs) stored in AWS Key Management Service (AWS KMS) to encrypt your images. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/encryption-at-rest.html Amazon ECR encryption at rest> in the /Amazon Elastic Container Registry User Guide/ .
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { encryptionType :: Types.EncryptionType
    -- ^ The encryption type to use.
--
-- If you use the @KMS@ encryption type, the contents of the repository will be encrypted using server-side encryption with customer master keys (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you can either use the default AWS managed CMK for Amazon ECR, or specify your own CMK, which you already created. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)> in the /Amazon Simple Storage Service Console Developer Guide./ .
-- If you use the @AES256@ encryption type, Amazon ECR uses server-side encryption with Amazon S3-managed encryption keys which encrypts the images in the repository using an AES-256 encryption algorithm. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)> in the /Amazon Simple Storage Service Console Developer Guide./ .
  , kmsKey :: Core.Maybe Types.KmsKey
    -- ^ If you use the @KMS@ encryption type, specify the CMK to use for encryption. The alias, key ID, or full ARN of the CMK can be specified. The key must exist in the same Region as the repository. If no key is specified, the default AWS managed CMK for Amazon ECR will be used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfiguration' value with any optional fields omitted.
mkEncryptionConfiguration
    :: Types.EncryptionType -- ^ 'encryptionType'
    -> EncryptionConfiguration
mkEncryptionConfiguration encryptionType
  = EncryptionConfiguration'{encryptionType, kmsKey = Core.Nothing}

-- | The encryption type to use.
--
-- If you use the @KMS@ encryption type, the contents of the repository will be encrypted using server-side encryption with customer master keys (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you can either use the default AWS managed CMK for Amazon ECR, or specify your own CMK, which you already created. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)> in the /Amazon Simple Storage Service Console Developer Guide./ .
-- If you use the @AES256@ encryption type, Amazon ECR uses server-side encryption with Amazon S3-managed encryption keys which encrypts the images in the repository using an AES-256 encryption algorithm. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)> in the /Amazon Simple Storage Service Console Developer Guide./ .
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEncryptionType :: Lens.Lens' EncryptionConfiguration Types.EncryptionType
ecEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE ecEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | If you use the @KMS@ encryption type, specify the CMK to use for encryption. The alias, key ID, or full ARN of the CMK can be specified. The key must exist in the same Region as the repository. If no key is specified, the default AWS managed CMK for Amazon ECR will be used.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKmsKey :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.KmsKey)
ecKmsKey = Lens.field @"kmsKey"
{-# INLINEABLE ecKmsKey #-}
{-# DEPRECATED kmsKey "Use generic-lens or generic-optics with 'kmsKey' instead"  #-}

instance Core.FromJSON EncryptionConfiguration where
        toJSON EncryptionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("encryptionType" Core..= encryptionType),
                  ("kmsKey" Core..=) Core.<$> kmsKey])

instance Core.FromJSON EncryptionConfiguration where
        parseJSON
          = Core.withObject "EncryptionConfiguration" Core.$
              \ x ->
                EncryptionConfiguration' Core.<$>
                  (x Core..: "encryptionType") Core.<*> x Core..:? "kmsKey"
