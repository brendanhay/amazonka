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
-- Module      : Amazonka.Connect.Types.S3Config
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.S3Config where

import Amazonka.Connect.Types.EncryptionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Amazon Simple Storage Service (Amazon S3) storage
-- type.
--
-- /See:/ 'newS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The Amazon S3 encryption configuration.
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | The S3 bucket name.
    bucketName :: Prelude.Text,
    -- | The S3 bucket prefix.
    bucketPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Config' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfig', 's3Config_encryptionConfig' - The Amazon S3 encryption configuration.
--
-- 'bucketName', 's3Config_bucketName' - The S3 bucket name.
--
-- 'bucketPrefix', 's3Config_bucketPrefix' - The S3 bucket prefix.
newS3Config ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'bucketPrefix'
  Prelude.Text ->
  S3Config
newS3Config pBucketName_ pBucketPrefix_ =
  S3Config'
    { encryptionConfig = Prelude.Nothing,
      bucketName = pBucketName_,
      bucketPrefix = pBucketPrefix_
    }

-- | The Amazon S3 encryption configuration.
s3Config_encryptionConfig :: Lens.Lens' S3Config (Prelude.Maybe EncryptionConfig)
s3Config_encryptionConfig = Lens.lens (\S3Config' {encryptionConfig} -> encryptionConfig) (\s@S3Config' {} a -> s {encryptionConfig = a} :: S3Config)

-- | The S3 bucket name.
s3Config_bucketName :: Lens.Lens' S3Config Prelude.Text
s3Config_bucketName = Lens.lens (\S3Config' {bucketName} -> bucketName) (\s@S3Config' {} a -> s {bucketName = a} :: S3Config)

-- | The S3 bucket prefix.
s3Config_bucketPrefix :: Lens.Lens' S3Config Prelude.Text
s3Config_bucketPrefix = Lens.lens (\S3Config' {bucketPrefix} -> bucketPrefix) (\s@S3Config' {} a -> s {bucketPrefix = a} :: S3Config)

instance Data.FromJSON S3Config where
  parseJSON =
    Data.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Prelude.<$> (x Data..:? "EncryptionConfig")
            Prelude.<*> (x Data..: "BucketName")
            Prelude.<*> (x Data..: "BucketPrefix")
      )

instance Prelude.Hashable S3Config where
  hashWithSalt _salt S3Config' {..} =
    _salt `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` bucketPrefix

instance Prelude.NFData S3Config where
  rnf S3Config' {..} =
    Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bucketPrefix

instance Data.ToJSON S3Config where
  toJSON S3Config' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionConfig" Data..=)
              Prelude.<$> encryptionConfig,
            Prelude.Just ("BucketName" Data..= bucketName),
            Prelude.Just ("BucketPrefix" Data..= bucketPrefix)
          ]
      )
