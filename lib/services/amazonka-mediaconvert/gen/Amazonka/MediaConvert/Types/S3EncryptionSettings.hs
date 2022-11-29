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
-- Module      : Amazonka.MediaConvert.Types.S3EncryptionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.S3EncryptionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.S3ServerSideEncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Settings for how your job outputs are encrypted as they are uploaded to
-- Amazon S3.
--
-- /See:/ 'newS3EncryptionSettings' smart constructor.
data S3EncryptionSettings = S3EncryptionSettings'
  { -- | Optionally, specify the encryption context that you want to use
    -- alongside your KMS key. AWS KMS uses this encryption context as
    -- additional authenticated data (AAD) to support authenticated encryption.
    -- This value must be a base64-encoded UTF-8 string holding JSON which
    -- represents a string-string map. To use this setting, you must also set
    -- Server-side encryption (S3ServerSideEncryptionType) to AWS KMS
    -- (SERVER_SIDE_ENCRYPTION_KMS). For more information about encryption
    -- context, see:
    -- https:\/\/docs.aws.amazon.com\/kms\/latest\/developerguide\/concepts.html#encrypt_context.
    kmsEncryptionContext :: Prelude.Maybe Prelude.Text,
    -- | Optionally, specify the customer master key (CMK) that you want to use
    -- to encrypt the data key that AWS uses to encrypt your output content.
    -- Enter the Amazon Resource Name (ARN) of the CMK. To use this setting,
    -- you must also set Server-side encryption (S3ServerSideEncryptionType) to
    -- AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption
    -- to AWS KMS but don\'t specify a CMK here, AWS uses the AWS managed CMK
    -- associated with Amazon S3.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
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
    encryptionType :: Prelude.Maybe S3ServerSideEncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3EncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsEncryptionContext', 's3EncryptionSettings_kmsEncryptionContext' - Optionally, specify the encryption context that you want to use
-- alongside your KMS key. AWS KMS uses this encryption context as
-- additional authenticated data (AAD) to support authenticated encryption.
-- This value must be a base64-encoded UTF-8 string holding JSON which
-- represents a string-string map. To use this setting, you must also set
-- Server-side encryption (S3ServerSideEncryptionType) to AWS KMS
-- (SERVER_SIDE_ENCRYPTION_KMS). For more information about encryption
-- context, see:
-- https:\/\/docs.aws.amazon.com\/kms\/latest\/developerguide\/concepts.html#encrypt_context.
--
-- 'kmsKeyArn', 's3EncryptionSettings_kmsKeyArn' - Optionally, specify the customer master key (CMK) that you want to use
-- to encrypt the data key that AWS uses to encrypt your output content.
-- Enter the Amazon Resource Name (ARN) of the CMK. To use this setting,
-- you must also set Server-side encryption (S3ServerSideEncryptionType) to
-- AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption
-- to AWS KMS but don\'t specify a CMK here, AWS uses the AWS managed CMK
-- associated with Amazon S3.
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
newS3EncryptionSettings ::
  S3EncryptionSettings
newS3EncryptionSettings =
  S3EncryptionSettings'
    { kmsEncryptionContext =
        Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      encryptionType = Prelude.Nothing
    }

-- | Optionally, specify the encryption context that you want to use
-- alongside your KMS key. AWS KMS uses this encryption context as
-- additional authenticated data (AAD) to support authenticated encryption.
-- This value must be a base64-encoded UTF-8 string holding JSON which
-- represents a string-string map. To use this setting, you must also set
-- Server-side encryption (S3ServerSideEncryptionType) to AWS KMS
-- (SERVER_SIDE_ENCRYPTION_KMS). For more information about encryption
-- context, see:
-- https:\/\/docs.aws.amazon.com\/kms\/latest\/developerguide\/concepts.html#encrypt_context.
s3EncryptionSettings_kmsEncryptionContext :: Lens.Lens' S3EncryptionSettings (Prelude.Maybe Prelude.Text)
s3EncryptionSettings_kmsEncryptionContext = Lens.lens (\S3EncryptionSettings' {kmsEncryptionContext} -> kmsEncryptionContext) (\s@S3EncryptionSettings' {} a -> s {kmsEncryptionContext = a} :: S3EncryptionSettings)

-- | Optionally, specify the customer master key (CMK) that you want to use
-- to encrypt the data key that AWS uses to encrypt your output content.
-- Enter the Amazon Resource Name (ARN) of the CMK. To use this setting,
-- you must also set Server-side encryption (S3ServerSideEncryptionType) to
-- AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption
-- to AWS KMS but don\'t specify a CMK here, AWS uses the AWS managed CMK
-- associated with Amazon S3.
s3EncryptionSettings_kmsKeyArn :: Lens.Lens' S3EncryptionSettings (Prelude.Maybe Prelude.Text)
s3EncryptionSettings_kmsKeyArn = Lens.lens (\S3EncryptionSettings' {kmsKeyArn} -> kmsKeyArn) (\s@S3EncryptionSettings' {} a -> s {kmsKeyArn = a} :: S3EncryptionSettings)

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

instance Core.FromJSON S3EncryptionSettings where
  parseJSON =
    Core.withObject
      "S3EncryptionSettings"
      ( \x ->
          S3EncryptionSettings'
            Prelude.<$> (x Core..:? "kmsEncryptionContext")
            Prelude.<*> (x Core..:? "kmsKeyArn")
            Prelude.<*> (x Core..:? "encryptionType")
      )

instance Prelude.Hashable S3EncryptionSettings where
  hashWithSalt _salt S3EncryptionSettings' {..} =
    _salt `Prelude.hashWithSalt` kmsEncryptionContext
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` encryptionType

instance Prelude.NFData S3EncryptionSettings where
  rnf S3EncryptionSettings' {..} =
    Prelude.rnf kmsEncryptionContext
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf encryptionType

instance Core.ToJSON S3EncryptionSettings where
  toJSON S3EncryptionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsEncryptionContext" Core..=)
              Prelude.<$> kmsEncryptionContext,
            ("kmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("encryptionType" Core..=)
              Prelude.<$> encryptionType
          ]
      )
