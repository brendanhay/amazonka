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
-- Module      : Amazonka.MacieV2.Types.BucketCountByEncryptionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketCountByEncryptionType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the number of S3 buckets that use certain
-- types of server-side encryption by default or don\'t encrypt new objects
-- by default. For detailed information about these settings, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucket-encryption.html Setting default server-side encryption behavior for Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- /See:/ 'newBucketCountByEncryptionType' smart constructor.
data BucketCountByEncryptionType = BucketCountByEncryptionType'
  { -- | The total number of buckets that use an KMS key to encrypt new objects
    -- by default, either an Amazon Web Services managed key or a customer
    -- managed key. These buckets use KMS encryption (SSE-KMS) by default.
    kmsManaged :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that use an Amazon S3 managed key to encrypt
    -- new objects by default. These buckets use Amazon S3 managed encryption
    -- (SSE-S3) by default.
    s3Managed :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that don\'t encrypt new objects by default.
    -- Default encryption is disabled for these buckets.
    unencrypted :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that Amazon Macie doesn\'t have current
    -- encryption metadata for. Macie can\'t provide current data about the
    -- default encryption settings for these buckets.
    unknown :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketCountByEncryptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsManaged', 'bucketCountByEncryptionType_kmsManaged' - The total number of buckets that use an KMS key to encrypt new objects
-- by default, either an Amazon Web Services managed key or a customer
-- managed key. These buckets use KMS encryption (SSE-KMS) by default.
--
-- 's3Managed', 'bucketCountByEncryptionType_s3Managed' - The total number of buckets that use an Amazon S3 managed key to encrypt
-- new objects by default. These buckets use Amazon S3 managed encryption
-- (SSE-S3) by default.
--
-- 'unencrypted', 'bucketCountByEncryptionType_unencrypted' - The total number of buckets that don\'t encrypt new objects by default.
-- Default encryption is disabled for these buckets.
--
-- 'unknown', 'bucketCountByEncryptionType_unknown' - The total number of buckets that Amazon Macie doesn\'t have current
-- encryption metadata for. Macie can\'t provide current data about the
-- default encryption settings for these buckets.
newBucketCountByEncryptionType ::
  BucketCountByEncryptionType
newBucketCountByEncryptionType =
  BucketCountByEncryptionType'
    { kmsManaged =
        Prelude.Nothing,
      s3Managed = Prelude.Nothing,
      unencrypted = Prelude.Nothing,
      unknown = Prelude.Nothing
    }

-- | The total number of buckets that use an KMS key to encrypt new objects
-- by default, either an Amazon Web Services managed key or a customer
-- managed key. These buckets use KMS encryption (SSE-KMS) by default.
bucketCountByEncryptionType_kmsManaged :: Lens.Lens' BucketCountByEncryptionType (Prelude.Maybe Prelude.Integer)
bucketCountByEncryptionType_kmsManaged = Lens.lens (\BucketCountByEncryptionType' {kmsManaged} -> kmsManaged) (\s@BucketCountByEncryptionType' {} a -> s {kmsManaged = a} :: BucketCountByEncryptionType)

-- | The total number of buckets that use an Amazon S3 managed key to encrypt
-- new objects by default. These buckets use Amazon S3 managed encryption
-- (SSE-S3) by default.
bucketCountByEncryptionType_s3Managed :: Lens.Lens' BucketCountByEncryptionType (Prelude.Maybe Prelude.Integer)
bucketCountByEncryptionType_s3Managed = Lens.lens (\BucketCountByEncryptionType' {s3Managed} -> s3Managed) (\s@BucketCountByEncryptionType' {} a -> s {s3Managed = a} :: BucketCountByEncryptionType)

-- | The total number of buckets that don\'t encrypt new objects by default.
-- Default encryption is disabled for these buckets.
bucketCountByEncryptionType_unencrypted :: Lens.Lens' BucketCountByEncryptionType (Prelude.Maybe Prelude.Integer)
bucketCountByEncryptionType_unencrypted = Lens.lens (\BucketCountByEncryptionType' {unencrypted} -> unencrypted) (\s@BucketCountByEncryptionType' {} a -> s {unencrypted = a} :: BucketCountByEncryptionType)

-- | The total number of buckets that Amazon Macie doesn\'t have current
-- encryption metadata for. Macie can\'t provide current data about the
-- default encryption settings for these buckets.
bucketCountByEncryptionType_unknown :: Lens.Lens' BucketCountByEncryptionType (Prelude.Maybe Prelude.Integer)
bucketCountByEncryptionType_unknown = Lens.lens (\BucketCountByEncryptionType' {unknown} -> unknown) (\s@BucketCountByEncryptionType' {} a -> s {unknown = a} :: BucketCountByEncryptionType)

instance Data.FromJSON BucketCountByEncryptionType where
  parseJSON =
    Data.withObject
      "BucketCountByEncryptionType"
      ( \x ->
          BucketCountByEncryptionType'
            Prelude.<$> (x Data..:? "kmsManaged")
            Prelude.<*> (x Data..:? "s3Managed")
            Prelude.<*> (x Data..:? "unencrypted")
            Prelude.<*> (x Data..:? "unknown")
      )

instance Prelude.Hashable BucketCountByEncryptionType where
  hashWithSalt _salt BucketCountByEncryptionType' {..} =
    _salt
      `Prelude.hashWithSalt` kmsManaged
      `Prelude.hashWithSalt` s3Managed
      `Prelude.hashWithSalt` unencrypted
      `Prelude.hashWithSalt` unknown

instance Prelude.NFData BucketCountByEncryptionType where
  rnf BucketCountByEncryptionType' {..} =
    Prelude.rnf kmsManaged `Prelude.seq`
      Prelude.rnf s3Managed `Prelude.seq`
        Prelude.rnf unencrypted `Prelude.seq`
          Prelude.rnf unknown
