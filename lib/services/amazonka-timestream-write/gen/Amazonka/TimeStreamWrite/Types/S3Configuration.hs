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
-- Module      : Amazonka.TimeStreamWrite.Types.S3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.S3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.S3EncryptionOption

-- | Configuration specifing an S3 location.
--
-- /See:/ 'newS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | >Bucket name of the customer S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | Encryption option for the customer s3 location. Options are S3 server
    -- side encryption with an S3-managed key or KMS managed key.
    encryptionOption :: Prelude.Maybe S3EncryptionOption,
    -- | KMS key id for the customer s3 location when encrypting with a KMS
    -- managed key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Object key preview for the customer S3 location.
    objectKeyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3Configuration_bucketName' - >Bucket name of the customer S3 bucket.
--
-- 'encryptionOption', 's3Configuration_encryptionOption' - Encryption option for the customer s3 location. Options are S3 server
-- side encryption with an S3-managed key or KMS managed key.
--
-- 'kmsKeyId', 's3Configuration_kmsKeyId' - KMS key id for the customer s3 location when encrypting with a KMS
-- managed key.
--
-- 'objectKeyPrefix', 's3Configuration_objectKeyPrefix' - Object key preview for the customer S3 location.
newS3Configuration ::
  S3Configuration
newS3Configuration =
  S3Configuration'
    { bucketName = Prelude.Nothing,
      encryptionOption = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      objectKeyPrefix = Prelude.Nothing
    }

-- | >Bucket name of the customer S3 bucket.
s3Configuration_bucketName :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_bucketName = Lens.lens (\S3Configuration' {bucketName} -> bucketName) (\s@S3Configuration' {} a -> s {bucketName = a} :: S3Configuration)

-- | Encryption option for the customer s3 location. Options are S3 server
-- side encryption with an S3-managed key or KMS managed key.
s3Configuration_encryptionOption :: Lens.Lens' S3Configuration (Prelude.Maybe S3EncryptionOption)
s3Configuration_encryptionOption = Lens.lens (\S3Configuration' {encryptionOption} -> encryptionOption) (\s@S3Configuration' {} a -> s {encryptionOption = a} :: S3Configuration)

-- | KMS key id for the customer s3 location when encrypting with a KMS
-- managed key.
s3Configuration_kmsKeyId :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_kmsKeyId = Lens.lens (\S3Configuration' {kmsKeyId} -> kmsKeyId) (\s@S3Configuration' {} a -> s {kmsKeyId = a} :: S3Configuration)

-- | Object key preview for the customer S3 location.
s3Configuration_objectKeyPrefix :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_objectKeyPrefix = Lens.lens (\S3Configuration' {objectKeyPrefix} -> objectKeyPrefix) (\s@S3Configuration' {} a -> s {objectKeyPrefix = a} :: S3Configuration)

instance Data.FromJSON S3Configuration where
  parseJSON =
    Data.withObject
      "S3Configuration"
      ( \x ->
          S3Configuration'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "EncryptionOption")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "ObjectKeyPrefix")
      )

instance Prelude.Hashable S3Configuration where
  hashWithSalt _salt S3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` encryptionOption
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` objectKeyPrefix

instance Prelude.NFData S3Configuration where
  rnf S3Configuration' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf encryptionOption
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf objectKeyPrefix

instance Data.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("EncryptionOption" Data..=)
              Prelude.<$> encryptionOption,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("ObjectKeyPrefix" Data..=)
              Prelude.<$> objectKeyPrefix
          ]
      )
