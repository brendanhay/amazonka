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
    sesKMSKeyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
import qualified Network.AWS.Prelude as Lude

-- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
--
-- /See:/ 'mkS3EncryptionSettings' smart constructor.
data S3EncryptionSettings = S3EncryptionSettings'
  { -- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
    encryptionType :: Lude.Maybe S3ServerSideEncryptionType,
    -- | Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
    kmsKeyARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3EncryptionSettings' with the minimum fields required to make a request.
--
-- * 'encryptionType' - Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
-- * 'kmsKeyARN' - Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
mkS3EncryptionSettings ::
  S3EncryptionSettings
mkS3EncryptionSettings =
  S3EncryptionSettings'
    { encryptionType = Lude.Nothing,
      kmsKeyARN = Lude.Nothing
    }

-- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesEncryptionType :: Lens.Lens' S3EncryptionSettings (Lude.Maybe S3ServerSideEncryptionType)
sesEncryptionType = Lens.lens (encryptionType :: S3EncryptionSettings -> Lude.Maybe S3ServerSideEncryptionType) (\s a -> s {encryptionType = a} :: S3EncryptionSettings)
{-# DEPRECATED sesEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesKMSKeyARN :: Lens.Lens' S3EncryptionSettings (Lude.Maybe Lude.Text)
sesKMSKeyARN = Lens.lens (kmsKeyARN :: S3EncryptionSettings -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: S3EncryptionSettings)
{-# DEPRECATED sesKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

instance Lude.FromJSON S3EncryptionSettings where
  parseJSON =
    Lude.withObject
      "S3EncryptionSettings"
      ( \x ->
          S3EncryptionSettings'
            Lude.<$> (x Lude..:? "encryptionType") Lude.<*> (x Lude..:? "kmsKeyArn")
      )

instance Lude.ToJSON S3EncryptionSettings where
  toJSON S3EncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptionType" Lude..=) Lude.<$> encryptionType,
            ("kmsKeyArn" Lude..=) Lude.<$> kmsKeyARN
          ]
      )
