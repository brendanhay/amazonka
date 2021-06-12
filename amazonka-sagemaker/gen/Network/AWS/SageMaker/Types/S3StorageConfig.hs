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
-- Module      : Network.AWS.SageMaker.Types.S3StorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3StorageConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon Simple Storage (Amazon S3) location and and security
-- configuration for @OfflineStore@.
--
-- /See:/ 'newS3StorageConfig' smart constructor.
data S3StorageConfig = S3StorageConfig'
  { -- | The AWS Key Management Service (KMS) key ID of the key used to encrypt
    -- any objects written into the @OfflineStore@ S3 location.
    --
    -- The IAM @roleARN@ that is passed as a parameter to @CreateFeatureGroup@
    -- must have below permissions to the @KmsKeyId@:
    --
    -- -   @\"kms:GenerateDataKey\"@
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The S3 path where offline records are written.
    resolvedOutputS3Uri :: Core.Maybe Core.Text,
    -- | The S3 URI, or location in Amazon S3, of @OfflineStore@.
    --
    -- S3 URIs have a format similar to the following:
    -- @s3:\/\/example-bucket\/prefix\/@.
    s3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3StorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 's3StorageConfig_kmsKeyId' - The AWS Key Management Service (KMS) key ID of the key used to encrypt
-- any objects written into the @OfflineStore@ S3 location.
--
-- The IAM @roleARN@ that is passed as a parameter to @CreateFeatureGroup@
-- must have below permissions to the @KmsKeyId@:
--
-- -   @\"kms:GenerateDataKey\"@
--
-- 'resolvedOutputS3Uri', 's3StorageConfig_resolvedOutputS3Uri' - The S3 path where offline records are written.
--
-- 's3Uri', 's3StorageConfig_s3Uri' - The S3 URI, or location in Amazon S3, of @OfflineStore@.
--
-- S3 URIs have a format similar to the following:
-- @s3:\/\/example-bucket\/prefix\/@.
newS3StorageConfig ::
  -- | 's3Uri'
  Core.Text ->
  S3StorageConfig
newS3StorageConfig pS3Uri_ =
  S3StorageConfig'
    { kmsKeyId = Core.Nothing,
      resolvedOutputS3Uri = Core.Nothing,
      s3Uri = pS3Uri_
    }

-- | The AWS Key Management Service (KMS) key ID of the key used to encrypt
-- any objects written into the @OfflineStore@ S3 location.
--
-- The IAM @roleARN@ that is passed as a parameter to @CreateFeatureGroup@
-- must have below permissions to the @KmsKeyId@:
--
-- -   @\"kms:GenerateDataKey\"@
s3StorageConfig_kmsKeyId :: Lens.Lens' S3StorageConfig (Core.Maybe Core.Text)
s3StorageConfig_kmsKeyId = Lens.lens (\S3StorageConfig' {kmsKeyId} -> kmsKeyId) (\s@S3StorageConfig' {} a -> s {kmsKeyId = a} :: S3StorageConfig)

-- | The S3 path where offline records are written.
s3StorageConfig_resolvedOutputS3Uri :: Lens.Lens' S3StorageConfig (Core.Maybe Core.Text)
s3StorageConfig_resolvedOutputS3Uri = Lens.lens (\S3StorageConfig' {resolvedOutputS3Uri} -> resolvedOutputS3Uri) (\s@S3StorageConfig' {} a -> s {resolvedOutputS3Uri = a} :: S3StorageConfig)

-- | The S3 URI, or location in Amazon S3, of @OfflineStore@.
--
-- S3 URIs have a format similar to the following:
-- @s3:\/\/example-bucket\/prefix\/@.
s3StorageConfig_s3Uri :: Lens.Lens' S3StorageConfig Core.Text
s3StorageConfig_s3Uri = Lens.lens (\S3StorageConfig' {s3Uri} -> s3Uri) (\s@S3StorageConfig' {} a -> s {s3Uri = a} :: S3StorageConfig)

instance Core.FromJSON S3StorageConfig where
  parseJSON =
    Core.withObject
      "S3StorageConfig"
      ( \x ->
          S3StorageConfig'
            Core.<$> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "ResolvedOutputS3Uri")
            Core.<*> (x Core..: "S3Uri")
      )

instance Core.Hashable S3StorageConfig

instance Core.NFData S3StorageConfig

instance Core.ToJSON S3StorageConfig where
  toJSON S3StorageConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("ResolvedOutputS3Uri" Core..=)
              Core.<$> resolvedOutputS3Uri,
            Core.Just ("S3Uri" Core..= s3Uri)
          ]
      )
