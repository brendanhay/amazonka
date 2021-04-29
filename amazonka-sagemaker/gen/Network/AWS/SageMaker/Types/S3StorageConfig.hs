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
-- Module      : Network.AWS.SageMaker.Types.S3StorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3StorageConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The S3 path where offline records are written.
    resolvedOutputS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The S3 URI, or location in Amazon S3, of @OfflineStore@.
    --
    -- S3 URIs have a format similar to the following:
    -- @s3:\/\/example-bucket\/prefix\/@.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  S3StorageConfig
newS3StorageConfig pS3Uri_ =
  S3StorageConfig'
    { kmsKeyId = Prelude.Nothing,
      resolvedOutputS3Uri = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | The AWS Key Management Service (KMS) key ID of the key used to encrypt
-- any objects written into the @OfflineStore@ S3 location.
--
-- The IAM @roleARN@ that is passed as a parameter to @CreateFeatureGroup@
-- must have below permissions to the @KmsKeyId@:
--
-- -   @\"kms:GenerateDataKey\"@
s3StorageConfig_kmsKeyId :: Lens.Lens' S3StorageConfig (Prelude.Maybe Prelude.Text)
s3StorageConfig_kmsKeyId = Lens.lens (\S3StorageConfig' {kmsKeyId} -> kmsKeyId) (\s@S3StorageConfig' {} a -> s {kmsKeyId = a} :: S3StorageConfig)

-- | The S3 path where offline records are written.
s3StorageConfig_resolvedOutputS3Uri :: Lens.Lens' S3StorageConfig (Prelude.Maybe Prelude.Text)
s3StorageConfig_resolvedOutputS3Uri = Lens.lens (\S3StorageConfig' {resolvedOutputS3Uri} -> resolvedOutputS3Uri) (\s@S3StorageConfig' {} a -> s {resolvedOutputS3Uri = a} :: S3StorageConfig)

-- | The S3 URI, or location in Amazon S3, of @OfflineStore@.
--
-- S3 URIs have a format similar to the following:
-- @s3:\/\/example-bucket\/prefix\/@.
s3StorageConfig_s3Uri :: Lens.Lens' S3StorageConfig Prelude.Text
s3StorageConfig_s3Uri = Lens.lens (\S3StorageConfig' {s3Uri} -> s3Uri) (\s@S3StorageConfig' {} a -> s {s3Uri = a} :: S3StorageConfig)

instance Prelude.FromJSON S3StorageConfig where
  parseJSON =
    Prelude.withObject
      "S3StorageConfig"
      ( \x ->
          S3StorageConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "ResolvedOutputS3Uri")
            Prelude.<*> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable S3StorageConfig

instance Prelude.NFData S3StorageConfig

instance Prelude.ToJSON S3StorageConfig where
  toJSON S3StorageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("ResolvedOutputS3Uri" Prelude..=)
              Prelude.<$> resolvedOutputS3Uri,
            Prelude.Just ("S3Uri" Prelude..= s3Uri)
          ]
      )
