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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | Object key preview for the customer S3 location.
    objectKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Encryption option for the customer s3 location. Options are S3 server
    -- side encryption with an S3-managed key or KMS managed key.
    encryptionOption :: Prelude.Maybe S3EncryptionOption,
    -- | >Bucket name of the customer S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | KMS key id for the customer s3 location when encrypting with a KMS
    -- managed key.
    kmsKeyId :: Prelude.Maybe Prelude.Text
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
-- 'objectKeyPrefix', 's3Configuration_objectKeyPrefix' - Object key preview for the customer S3 location.
--
-- 'encryptionOption', 's3Configuration_encryptionOption' - Encryption option for the customer s3 location. Options are S3 server
-- side encryption with an S3-managed key or KMS managed key.
--
-- 'bucketName', 's3Configuration_bucketName' - >Bucket name of the customer S3 bucket.
--
-- 'kmsKeyId', 's3Configuration_kmsKeyId' - KMS key id for the customer s3 location when encrypting with a KMS
-- managed key.
newS3Configuration ::
  S3Configuration
newS3Configuration =
  S3Configuration'
    { objectKeyPrefix = Prelude.Nothing,
      encryptionOption = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | Object key preview for the customer S3 location.
s3Configuration_objectKeyPrefix :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_objectKeyPrefix = Lens.lens (\S3Configuration' {objectKeyPrefix} -> objectKeyPrefix) (\s@S3Configuration' {} a -> s {objectKeyPrefix = a} :: S3Configuration)

-- | Encryption option for the customer s3 location. Options are S3 server
-- side encryption with an S3-managed key or KMS managed key.
s3Configuration_encryptionOption :: Lens.Lens' S3Configuration (Prelude.Maybe S3EncryptionOption)
s3Configuration_encryptionOption = Lens.lens (\S3Configuration' {encryptionOption} -> encryptionOption) (\s@S3Configuration' {} a -> s {encryptionOption = a} :: S3Configuration)

-- | >Bucket name of the customer S3 bucket.
s3Configuration_bucketName :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_bucketName = Lens.lens (\S3Configuration' {bucketName} -> bucketName) (\s@S3Configuration' {} a -> s {bucketName = a} :: S3Configuration)

-- | KMS key id for the customer s3 location when encrypting with a KMS
-- managed key.
s3Configuration_kmsKeyId :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_kmsKeyId = Lens.lens (\S3Configuration' {kmsKeyId} -> kmsKeyId) (\s@S3Configuration' {} a -> s {kmsKeyId = a} :: S3Configuration)

instance Data.FromJSON S3Configuration where
  parseJSON =
    Data.withObject
      "S3Configuration"
      ( \x ->
          S3Configuration'
            Prelude.<$> (x Data..:? "ObjectKeyPrefix")
            Prelude.<*> (x Data..:? "EncryptionOption")
            Prelude.<*> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "KmsKeyId")
      )

instance Prelude.Hashable S3Configuration where
  hashWithSalt _salt S3Configuration' {..} =
    _salt `Prelude.hashWithSalt` objectKeyPrefix
      `Prelude.hashWithSalt` encryptionOption
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData S3Configuration where
  rnf S3Configuration' {..} =
    Prelude.rnf objectKeyPrefix
      `Prelude.seq` Prelude.rnf encryptionOption
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectKeyPrefix" Data..=)
              Prelude.<$> objectKeyPrefix,
            ("EncryptionOption" Data..=)
              Prelude.<$> encryptionOption,
            ("BucketName" Data..=) Prelude.<$> bucketName,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId
          ]
      )
