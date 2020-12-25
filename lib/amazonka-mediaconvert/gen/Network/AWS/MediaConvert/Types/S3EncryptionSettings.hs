{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3EncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3EncryptionSettings
  ( S3EncryptionSettings (..),

    -- * Smart constructor
    mkS3EncryptionSettings,

    -- * Lenses
    sesEncryptionType,
    sesKmsKeyArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
--
-- /See:/ 'mkS3EncryptionSettings' smart constructor.
data S3EncryptionSettings = S3EncryptionSettings'
  { -- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
    encryptionType :: Core.Maybe Types.S3ServerSideEncryptionType,
    -- | Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
    kmsKeyArn :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3EncryptionSettings' value with any optional fields omitted.
mkS3EncryptionSettings ::
  S3EncryptionSettings
mkS3EncryptionSettings =
  S3EncryptionSettings'
    { encryptionType = Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesEncryptionType :: Lens.Lens' S3EncryptionSettings (Core.Maybe Types.S3ServerSideEncryptionType)
sesEncryptionType = Lens.field @"encryptionType"
{-# DEPRECATED sesEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesKmsKeyArn :: Lens.Lens' S3EncryptionSettings (Core.Maybe Core.Text)
sesKmsKeyArn = Lens.field @"kmsKeyArn"
{-# DEPRECATED sesKmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead." #-}

instance Core.FromJSON S3EncryptionSettings where
  toJSON S3EncryptionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("encryptionType" Core..=) Core.<$> encryptionType,
            ("kmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )

instance Core.FromJSON S3EncryptionSettings where
  parseJSON =
    Core.withObject "S3EncryptionSettings" Core.$
      \x ->
        S3EncryptionSettings'
          Core.<$> (x Core..:? "encryptionType") Core.<*> (x Core..:? "kmsKeyArn")
