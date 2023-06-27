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
-- Module      : Amazonka.TimeStreamQuery.Types.S3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.S3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.S3EncryptionOption

-- | Details on S3 location for error reports that result from running a
-- query.
--
-- /See:/ 'newS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | Encryption at rest options for the error reports. If no encryption
    -- option is specified, Timestream will choose SSE_S3 as default.
    encryptionOption :: Prelude.Maybe S3EncryptionOption,
    -- | Prefix for the error report key. Timestream by default adds the
    -- following prefix to the error report path.
    objectKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Name of the S3 bucket under which error reports will be created.
    bucketName :: Prelude.Text
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
-- 'encryptionOption', 's3Configuration_encryptionOption' - Encryption at rest options for the error reports. If no encryption
-- option is specified, Timestream will choose SSE_S3 as default.
--
-- 'objectKeyPrefix', 's3Configuration_objectKeyPrefix' - Prefix for the error report key. Timestream by default adds the
-- following prefix to the error report path.
--
-- 'bucketName', 's3Configuration_bucketName' - Name of the S3 bucket under which error reports will be created.
newS3Configuration ::
  -- | 'bucketName'
  Prelude.Text ->
  S3Configuration
newS3Configuration pBucketName_ =
  S3Configuration'
    { encryptionOption =
        Prelude.Nothing,
      objectKeyPrefix = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | Encryption at rest options for the error reports. If no encryption
-- option is specified, Timestream will choose SSE_S3 as default.
s3Configuration_encryptionOption :: Lens.Lens' S3Configuration (Prelude.Maybe S3EncryptionOption)
s3Configuration_encryptionOption = Lens.lens (\S3Configuration' {encryptionOption} -> encryptionOption) (\s@S3Configuration' {} a -> s {encryptionOption = a} :: S3Configuration)

-- | Prefix for the error report key. Timestream by default adds the
-- following prefix to the error report path.
s3Configuration_objectKeyPrefix :: Lens.Lens' S3Configuration (Prelude.Maybe Prelude.Text)
s3Configuration_objectKeyPrefix = Lens.lens (\S3Configuration' {objectKeyPrefix} -> objectKeyPrefix) (\s@S3Configuration' {} a -> s {objectKeyPrefix = a} :: S3Configuration)

-- | Name of the S3 bucket under which error reports will be created.
s3Configuration_bucketName :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_bucketName = Lens.lens (\S3Configuration' {bucketName} -> bucketName) (\s@S3Configuration' {} a -> s {bucketName = a} :: S3Configuration)

instance Data.FromJSON S3Configuration where
  parseJSON =
    Data.withObject
      "S3Configuration"
      ( \x ->
          S3Configuration'
            Prelude.<$> (x Data..:? "EncryptionOption")
            Prelude.<*> (x Data..:? "ObjectKeyPrefix")
            Prelude.<*> (x Data..: "BucketName")
      )

instance Prelude.Hashable S3Configuration where
  hashWithSalt _salt S3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionOption
      `Prelude.hashWithSalt` objectKeyPrefix
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData S3Configuration where
  rnf S3Configuration' {..} =
    Prelude.rnf encryptionOption
      `Prelude.seq` Prelude.rnf objectKeyPrefix
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionOption" Data..=)
              Prelude.<$> encryptionOption,
            ("ObjectKeyPrefix" Data..=)
              Prelude.<$> objectKeyPrefix,
            Prelude.Just ("BucketName" Data..= bucketName)
          ]
      )
