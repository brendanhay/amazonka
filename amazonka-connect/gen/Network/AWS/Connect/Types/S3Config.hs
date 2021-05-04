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
-- Module      : Network.AWS.Connect.Types.S3Config
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.S3Config where

import Network.AWS.Connect.Types.EncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON S3Config where
  parseJSON =
    Prelude.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Prelude.<$> (x Prelude..:? "EncryptionConfig")
            Prelude.<*> (x Prelude..: "BucketName")
            Prelude.<*> (x Prelude..: "BucketPrefix")
      )

instance Prelude.Hashable S3Config

instance Prelude.NFData S3Config

instance Prelude.ToJSON S3Config where
  toJSON S3Config' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EncryptionConfig" Prelude..=)
              Prelude.<$> encryptionConfig,
            Prelude.Just ("BucketName" Prelude..= bucketName),
            Prelude.Just
              ("BucketPrefix" Prelude..= bucketPrefix)
          ]
      )
