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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the Amazon Simple Storage Service (Amazon S3) storage
-- type.
--
-- /See:/ 'newS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The Amazon S3 encryption configuration.
    encryptionConfig :: Core.Maybe EncryptionConfig,
    -- | The S3 bucket name.
    bucketName :: Core.Text,
    -- | The S3 bucket prefix.
    bucketPrefix :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'bucketPrefix'
  Core.Text ->
  S3Config
newS3Config pBucketName_ pBucketPrefix_ =
  S3Config'
    { encryptionConfig = Core.Nothing,
      bucketName = pBucketName_,
      bucketPrefix = pBucketPrefix_
    }

-- | The Amazon S3 encryption configuration.
s3Config_encryptionConfig :: Lens.Lens' S3Config (Core.Maybe EncryptionConfig)
s3Config_encryptionConfig = Lens.lens (\S3Config' {encryptionConfig} -> encryptionConfig) (\s@S3Config' {} a -> s {encryptionConfig = a} :: S3Config)

-- | The S3 bucket name.
s3Config_bucketName :: Lens.Lens' S3Config Core.Text
s3Config_bucketName = Lens.lens (\S3Config' {bucketName} -> bucketName) (\s@S3Config' {} a -> s {bucketName = a} :: S3Config)

-- | The S3 bucket prefix.
s3Config_bucketPrefix :: Lens.Lens' S3Config Core.Text
s3Config_bucketPrefix = Lens.lens (\S3Config' {bucketPrefix} -> bucketPrefix) (\s@S3Config' {} a -> s {bucketPrefix = a} :: S3Config)

instance Core.FromJSON S3Config where
  parseJSON =
    Core.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Core.<$> (x Core..:? "EncryptionConfig")
            Core.<*> (x Core..: "BucketName")
            Core.<*> (x Core..: "BucketPrefix")
      )

instance Core.Hashable S3Config

instance Core.NFData S3Config

instance Core.ToJSON S3Config where
  toJSON S3Config' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EncryptionConfig" Core..=)
              Core.<$> encryptionConfig,
            Core.Just ("BucketName" Core..= bucketName),
            Core.Just ("BucketPrefix" Core..= bucketPrefix)
          ]
      )
