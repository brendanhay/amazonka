{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3EncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3EncryptionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
import qualified Network.AWS.Prelude as Prelude

-- | Settings for how your job outputs are encrypted as they are uploaded to
-- Amazon S3.
--
-- /See:/ 'newS3EncryptionSettings' smart constructor.
data S3EncryptionSettings = S3EncryptionSettings'
  { -- | Specify how you want your data keys managed. AWS uses data keys to
    -- encrypt your content. AWS also encrypts the data keys themselves, using
    -- a customer master key (CMK), and then stores the encrypted data keys
    -- alongside your encrypted content. Use this setting to specify which AWS
    -- service manages the CMK. For simplest set up, choose Amazon S3
    -- (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed
    -- by AWS Key Management Service (KMS), choose AWS KMS
    -- (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS
    -- uses the AWS managed customer master key (CMK) associated with Amazon S3
    -- to encrypt your data keys. You can optionally choose to specify a
    -- different, customer managed CMK. Do so by specifying the Amazon Resource
    -- Name (ARN) of the key for the setting KMS ARN (kmsKeyArn).
    encryptionType :: Prelude.Maybe S3ServerSideEncryptionType,
    -- | Optionally, specify the customer master key (CMK) that you want to use
    -- to encrypt the data key that AWS uses to encrypt your output content.
    -- Enter the Amazon Resource Name (ARN) of the CMK. To use this setting,
    -- you must also set Server-side encryption (S3ServerSideEncryptionType) to
    -- AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption
    -- to AWS KMS but don\'t specify a CMK here, AWS uses the AWS managed CMK
    -- associated with Amazon S3.
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3EncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 's3EncryptionSettings_encryptionType' - Specify how you want your data keys managed. AWS uses data keys to
-- encrypt your content. AWS also encrypts the data keys themselves, using
-- a customer master key (CMK), and then stores the encrypted data keys
-- alongside your encrypted content. Use this setting to specify which AWS
-- service manages the CMK. For simplest set up, choose Amazon S3
-- (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed
-- by AWS Key Management Service (KMS), choose AWS KMS
-- (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS
-- uses the AWS managed customer master key (CMK) associated with Amazon S3
-- to encrypt your data keys. You can optionally choose to specify a
-- different, customer managed CMK. Do so by specifying the Amazon Resource
-- Name (ARN) of the key for the setting KMS ARN (kmsKeyArn).
--
-- 'kmsKeyArn', 's3EncryptionSettings_kmsKeyArn' - Optionally, specify the customer master key (CMK) that you want to use
-- to encrypt the data key that AWS uses to encrypt your output content.
-- Enter the Amazon Resource Name (ARN) of the CMK. To use this setting,
-- you must also set Server-side encryption (S3ServerSideEncryptionType) to
-- AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption
-- to AWS KMS but don\'t specify a CMK here, AWS uses the AWS managed CMK
-- associated with Amazon S3.
newS3EncryptionSettings ::
  S3EncryptionSettings
newS3EncryptionSettings =
  S3EncryptionSettings'
    { encryptionType =
        Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | Specify how you want your data keys managed. AWS uses data keys to
-- encrypt your content. AWS also encrypts the data keys themselves, using
-- a customer master key (CMK), and then stores the encrypted data keys
-- alongside your encrypted content. Use this setting to specify which AWS
-- service manages the CMK. For simplest set up, choose Amazon S3
-- (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed
-- by AWS Key Management Service (KMS), choose AWS KMS
-- (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS
-- uses the AWS managed customer master key (CMK) associated with Amazon S3
-- to encrypt your data keys. You can optionally choose to specify a
-- different, customer managed CMK. Do so by specifying the Amazon Resource
-- Name (ARN) of the key for the setting KMS ARN (kmsKeyArn).
s3EncryptionSettings_encryptionType :: Lens.Lens' S3EncryptionSettings (Prelude.Maybe S3ServerSideEncryptionType)
s3EncryptionSettings_encryptionType = Lens.lens (\S3EncryptionSettings' {encryptionType} -> encryptionType) (\s@S3EncryptionSettings' {} a -> s {encryptionType = a} :: S3EncryptionSettings)

-- | Optionally, specify the customer master key (CMK) that you want to use
-- to encrypt the data key that AWS uses to encrypt your output content.
-- Enter the Amazon Resource Name (ARN) of the CMK. To use this setting,
-- you must also set Server-side encryption (S3ServerSideEncryptionType) to
-- AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption
-- to AWS KMS but don\'t specify a CMK here, AWS uses the AWS managed CMK
-- associated with Amazon S3.
s3EncryptionSettings_kmsKeyArn :: Lens.Lens' S3EncryptionSettings (Prelude.Maybe Prelude.Text)
s3EncryptionSettings_kmsKeyArn = Lens.lens (\S3EncryptionSettings' {kmsKeyArn} -> kmsKeyArn) (\s@S3EncryptionSettings' {} a -> s {kmsKeyArn = a} :: S3EncryptionSettings)

instance Prelude.FromJSON S3EncryptionSettings where
  parseJSON =
    Prelude.withObject
      "S3EncryptionSettings"
      ( \x ->
          S3EncryptionSettings'
            Prelude.<$> (x Prelude..:? "encryptionType")
            Prelude.<*> (x Prelude..:? "kmsKeyArn")
      )

instance Prelude.Hashable S3EncryptionSettings

instance Prelude.NFData S3EncryptionSettings

instance Prelude.ToJSON S3EncryptionSettings where
  toJSON S3EncryptionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("encryptionType" Prelude..=)
              Prelude.<$> encryptionType,
            ("kmsKeyArn" Prelude..=) Prelude.<$> kmsKeyArn
          ]
      )
