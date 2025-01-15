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
-- Module      : Amazonka.Synthetics.Types.S3EncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.S3EncryptionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.EncryptionMode

-- | A structure that contains the configuration of encryption-at-rest
-- settings for canary artifacts that the canary uploads to Amazon S3.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_artifact_encryption.html Encrypting canary artifacts>
--
-- /See:/ 'newS3EncryptionConfig' smart constructor.
data S3EncryptionConfig = S3EncryptionConfig'
  { -- | The encryption method to use for artifacts created by this canary.
    -- Specify @SSE_S3@ to use server-side encryption (SSE) with an Amazon
    -- S3-managed key. Specify @SSE-KMS@ to use server-side encryption with a
    -- customer-managed KMS key.
    --
    -- If you omit this parameter, an Amazon Web Services-managed KMS key is
    -- used.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | The ARN of the customer-managed KMS key to use, if you specify @SSE-KMS@
    -- for @EncryptionMode@
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3EncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionMode', 's3EncryptionConfig_encryptionMode' - The encryption method to use for artifacts created by this canary.
-- Specify @SSE_S3@ to use server-side encryption (SSE) with an Amazon
-- S3-managed key. Specify @SSE-KMS@ to use server-side encryption with a
-- customer-managed KMS key.
--
-- If you omit this parameter, an Amazon Web Services-managed KMS key is
-- used.
--
-- 'kmsKeyArn', 's3EncryptionConfig_kmsKeyArn' - The ARN of the customer-managed KMS key to use, if you specify @SSE-KMS@
-- for @EncryptionMode@
newS3EncryptionConfig ::
  S3EncryptionConfig
newS3EncryptionConfig =
  S3EncryptionConfig'
    { encryptionMode =
        Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | The encryption method to use for artifacts created by this canary.
-- Specify @SSE_S3@ to use server-side encryption (SSE) with an Amazon
-- S3-managed key. Specify @SSE-KMS@ to use server-side encryption with a
-- customer-managed KMS key.
--
-- If you omit this parameter, an Amazon Web Services-managed KMS key is
-- used.
s3EncryptionConfig_encryptionMode :: Lens.Lens' S3EncryptionConfig (Prelude.Maybe EncryptionMode)
s3EncryptionConfig_encryptionMode = Lens.lens (\S3EncryptionConfig' {encryptionMode} -> encryptionMode) (\s@S3EncryptionConfig' {} a -> s {encryptionMode = a} :: S3EncryptionConfig)

-- | The ARN of the customer-managed KMS key to use, if you specify @SSE-KMS@
-- for @EncryptionMode@
s3EncryptionConfig_kmsKeyArn :: Lens.Lens' S3EncryptionConfig (Prelude.Maybe Prelude.Text)
s3EncryptionConfig_kmsKeyArn = Lens.lens (\S3EncryptionConfig' {kmsKeyArn} -> kmsKeyArn) (\s@S3EncryptionConfig' {} a -> s {kmsKeyArn = a} :: S3EncryptionConfig)

instance Data.FromJSON S3EncryptionConfig where
  parseJSON =
    Data.withObject
      "S3EncryptionConfig"
      ( \x ->
          S3EncryptionConfig'
            Prelude.<$> (x Data..:? "EncryptionMode")
            Prelude.<*> (x Data..:? "KmsKeyArn")
      )

instance Prelude.Hashable S3EncryptionConfig where
  hashWithSalt _salt S3EncryptionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData S3EncryptionConfig where
  rnf S3EncryptionConfig' {..} =
    Prelude.rnf encryptionMode `Prelude.seq`
      Prelude.rnf kmsKeyArn

instance Data.ToJSON S3EncryptionConfig where
  toJSON S3EncryptionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn
          ]
      )
