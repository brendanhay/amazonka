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
-- Module      : Amazonka.ECR.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

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
-- you can use server-side encryption with Key Management Service key
-- stored in Key Management Service (KMS) to encrypt your images. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/encryption-at-rest.html Amazon ECR encryption at rest>
-- in the /Amazon Elastic Container Registry User Guide/.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | If you use the @KMS@ encryption type, specify the KMS key to use for
    -- encryption. The alias, key ID, or full ARN of the KMS key can be
    -- specified. The key must exist in the same Region as the repository. If
    -- no key is specified, the default Amazon Web Services managed KMS key for
    -- Amazon ECR will be used.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The encryption type to use.
    --
    -- If you use the @KMS@ encryption type, the contents of the repository
    -- will be encrypted using server-side encryption with Key Management
    -- Service key stored in KMS. When you use KMS to encrypt your data, you
    -- can either use the default Amazon Web Services managed KMS key for
    -- Amazon ECR, or specify your own KMS key, which you already created. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting data using server-side encryption with an KMS key stored in Key Management Service (SSE-KMS)>
    -- in the /Amazon Simple Storage Service Console Developer Guide/.
    --
    -- If you use the @AES256@ encryption type, Amazon ECR uses server-side
    -- encryption with Amazon S3-managed encryption keys which encrypts the
    -- images in the repository using an AES-256 encryption algorithm. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting data using server-side encryption with Amazon S3-managed encryption keys (SSE-S3)>
    -- in the /Amazon Simple Storage Service Console Developer Guide/.
    encryptionType :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'encryptionConfiguration_kmsKey' - If you use the @KMS@ encryption type, specify the KMS key to use for
-- encryption. The alias, key ID, or full ARN of the KMS key can be
-- specified. The key must exist in the same Region as the repository. If
-- no key is specified, the default Amazon Web Services managed KMS key for
-- Amazon ECR will be used.
--
-- 'encryptionType', 'encryptionConfiguration_encryptionType' - The encryption type to use.
--
-- If you use the @KMS@ encryption type, the contents of the repository
-- will be encrypted using server-side encryption with Key Management
-- Service key stored in KMS. When you use KMS to encrypt your data, you
-- can either use the default Amazon Web Services managed KMS key for
-- Amazon ECR, or specify your own KMS key, which you already created. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting data using server-side encryption with an KMS key stored in Key Management Service (SSE-KMS)>
-- in the /Amazon Simple Storage Service Console Developer Guide/.
--
-- If you use the @AES256@ encryption type, Amazon ECR uses server-side
-- encryption with Amazon S3-managed encryption keys which encrypts the
-- images in the repository using an AES-256 encryption algorithm. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting data using server-side encryption with Amazon S3-managed encryption keys (SSE-S3)>
-- in the /Amazon Simple Storage Service Console Developer Guide/.
newEncryptionConfiguration ::
  -- | 'encryptionType'
  EncryptionType ->
  EncryptionConfiguration
newEncryptionConfiguration pEncryptionType_ =
  EncryptionConfiguration'
    { kmsKey = Prelude.Nothing,
      encryptionType = pEncryptionType_
    }

-- | If you use the @KMS@ encryption type, specify the KMS key to use for
-- encryption. The alias, key ID, or full ARN of the KMS key can be
-- specified. The key must exist in the same Region as the repository. If
-- no key is specified, the default Amazon Web Services managed KMS key for
-- Amazon ECR will be used.
encryptionConfiguration_kmsKey :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe Prelude.Text)
encryptionConfiguration_kmsKey = Lens.lens (\EncryptionConfiguration' {kmsKey} -> kmsKey) (\s@EncryptionConfiguration' {} a -> s {kmsKey = a} :: EncryptionConfiguration)

-- | The encryption type to use.
--
-- If you use the @KMS@ encryption type, the contents of the repository
-- will be encrypted using server-side encryption with Key Management
-- Service key stored in KMS. When you use KMS to encrypt your data, you
-- can either use the default Amazon Web Services managed KMS key for
-- Amazon ECR, or specify your own KMS key, which you already created. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting data using server-side encryption with an KMS key stored in Key Management Service (SSE-KMS)>
-- in the /Amazon Simple Storage Service Console Developer Guide/.
--
-- If you use the @AES256@ encryption type, Amazon ECR uses server-side
-- encryption with Amazon S3-managed encryption keys which encrypts the
-- images in the repository using an AES-256 encryption algorithm. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting data using server-side encryption with Amazon S3-managed encryption keys (SSE-S3)>
-- in the /Amazon Simple Storage Service Console Developer Guide/.
encryptionConfiguration_encryptionType :: Lens.Lens' EncryptionConfiguration EncryptionType
encryptionConfiguration_encryptionType = Lens.lens (\EncryptionConfiguration' {encryptionType} -> encryptionType) (\s@EncryptionConfiguration' {} a -> s {encryptionType = a} :: EncryptionConfiguration)

instance Data.FromJSON EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Data..:? "kmsKey")
            Prelude.<*> (x Data..: "encryptionType")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt _salt EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` encryptionType

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf encryptionType

instance Data.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKey" Data..=) Prelude.<$> kmsKey,
            Prelude.Just
              ("encryptionType" Data..= encryptionType)
          ]
      )
