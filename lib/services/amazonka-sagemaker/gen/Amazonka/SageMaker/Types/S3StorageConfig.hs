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
-- Module      : Amazonka.SageMaker.Types.S3StorageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.S3StorageConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Simple Storage (Amazon S3) location and and security
-- configuration for @OfflineStore@.
--
-- /See:/ 'newS3StorageConfig' smart constructor.
data S3StorageConfig = S3StorageConfig'
  { -- | The Amazon Web Services Key Management Service (KMS) key ARN of the key
    -- used to encrypt any objects written into the @OfflineStore@ S3 location.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3StorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 's3StorageConfig_kmsKeyId' - The Amazon Web Services Key Management Service (KMS) key ARN of the key
-- used to encrypt any objects written into the @OfflineStore@ S3 location.
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

-- | The Amazon Web Services Key Management Service (KMS) key ARN of the key
-- used to encrypt any objects written into the @OfflineStore@ S3 location.
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

instance Data.FromJSON S3StorageConfig where
  parseJSON =
    Data.withObject
      "S3StorageConfig"
      ( \x ->
          S3StorageConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "ResolvedOutputS3Uri")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable S3StorageConfig where
  hashWithSalt _salt S3StorageConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` resolvedOutputS3Uri
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData S3StorageConfig where
  rnf S3StorageConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf resolvedOutputS3Uri
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON S3StorageConfig where
  toJSON S3StorageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("ResolvedOutputS3Uri" Data..=)
              Prelude.<$> resolvedOutputS3Uri,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
