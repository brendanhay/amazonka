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
-- Module      : Network.AWS.ECR.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.EncryptionConfiguration where

import Network.AWS.ECR.Types.EncryptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
--
-- By default, when no encryption configuration is set or the @AES256@
-- encryption type is used, Amazon ECR uses server-side encryption with
-- Amazon S3-managed encryption keys which encrypts your data at rest using
-- an AES-256 encryption algorithm. This does not require any action on
-- your part.
--
-- For more control over the encryption of the contents of your repository,
-- you can use server-side encryption with customer master keys (CMKs)
-- stored in AWS Key Management Service (AWS KMS) to encrypt your images.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/encryption-at-rest.html Amazon ECR encryption at rest>
-- in the /Amazon Elastic Container Registry User Guide/.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | If you use the @KMS@ encryption type, specify the CMK to use for
    -- encryption. The alias, key ID, or full ARN of the CMK can be specified.
    -- The key must exist in the same Region as the repository. If no key is
    -- specified, the default AWS managed CMK for Amazon ECR will be used.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The encryption type to use.
    --
    -- If you use the @KMS@ encryption type, the contents of the repository
    -- will be encrypted using server-side encryption with customer master keys
    -- (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you
    -- can either use the default AWS managed CMK for Amazon ECR, or specify
    -- your own CMK, which you already created. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)>
    -- in the /Amazon Simple Storage Service Console Developer Guide./.
    --
    -- If you use the @AES256@ encryption type, Amazon ECR uses server-side
    -- encryption with Amazon S3-managed encryption keys which encrypts the
    -- images in the repository using an AES-256 encryption algorithm. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)>
    -- in the /Amazon Simple Storage Service Console Developer Guide./.
    encryptionType :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'encryptionConfiguration_kmsKey' - If you use the @KMS@ encryption type, specify the CMK to use for
-- encryption. The alias, key ID, or full ARN of the CMK can be specified.
-- The key must exist in the same Region as the repository. If no key is
-- specified, the default AWS managed CMK for Amazon ECR will be used.
--
-- 'encryptionType', 'encryptionConfiguration_encryptionType' - The encryption type to use.
--
-- If you use the @KMS@ encryption type, the contents of the repository
-- will be encrypted using server-side encryption with customer master keys
-- (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you
-- can either use the default AWS managed CMK for Amazon ECR, or specify
-- your own CMK, which you already created. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)>
-- in the /Amazon Simple Storage Service Console Developer Guide./.
--
-- If you use the @AES256@ encryption type, Amazon ECR uses server-side
-- encryption with Amazon S3-managed encryption keys which encrypts the
-- images in the repository using an AES-256 encryption algorithm. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)>
-- in the /Amazon Simple Storage Service Console Developer Guide./.
newEncryptionConfiguration ::
  -- | 'encryptionType'
  EncryptionType ->
  EncryptionConfiguration
newEncryptionConfiguration pEncryptionType_ =
  EncryptionConfiguration'
    { kmsKey = Prelude.Nothing,
      encryptionType = pEncryptionType_
    }

-- | If you use the @KMS@ encryption type, specify the CMK to use for
-- encryption. The alias, key ID, or full ARN of the CMK can be specified.
-- The key must exist in the same Region as the repository. If no key is
-- specified, the default AWS managed CMK for Amazon ECR will be used.
encryptionConfiguration_kmsKey :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe Prelude.Text)
encryptionConfiguration_kmsKey = Lens.lens (\EncryptionConfiguration' {kmsKey} -> kmsKey) (\s@EncryptionConfiguration' {} a -> s {kmsKey = a} :: EncryptionConfiguration)

-- | The encryption type to use.
--
-- If you use the @KMS@ encryption type, the contents of the repository
-- will be encrypted using server-side encryption with customer master keys
-- (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you
-- can either use the default AWS managed CMK for Amazon ECR, or specify
-- your own CMK, which you already created. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)>
-- in the /Amazon Simple Storage Service Console Developer Guide./.
--
-- If you use the @AES256@ encryption type, Amazon ECR uses server-side
-- encryption with Amazon S3-managed encryption keys which encrypts the
-- images in the repository using an AES-256 encryption algorithm. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)>
-- in the /Amazon Simple Storage Service Console Developer Guide./.
encryptionConfiguration_encryptionType :: Lens.Lens' EncryptionConfiguration EncryptionType
encryptionConfiguration_encryptionType = Lens.lens (\EncryptionConfiguration' {encryptionType} -> encryptionType) (\s@EncryptionConfiguration' {} a -> s {encryptionType = a} :: EncryptionConfiguration)

instance Prelude.FromJSON EncryptionConfiguration where
  parseJSON =
    Prelude.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Prelude..:? "kmsKey")
            Prelude.<*> (x Prelude..: "encryptionType")
      )

instance Prelude.Hashable EncryptionConfiguration

instance Prelude.NFData EncryptionConfiguration

instance Prelude.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("kmsKey" Prelude..=) Prelude.<$> kmsKey,
            Prelude.Just
              ("encryptionType" Prelude..= encryptionType)
          ]
      )
