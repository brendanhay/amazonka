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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSourceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, provides the bucket
-- name and object key name that stores the reference data.
--
-- /See:/ 'newS3ReferenceDataSourceDescription' smart constructor.
data S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription'
  { -- | The ARN of the IAM role that Kinesis Data Analytics can assume to read
    -- the Amazon S3 object on your behalf to populate the in-application
    -- reference table.
    --
    -- Provided for backward compatibility. Applications that are created with
    -- the current API version have an application-level service execution role
    -- rather than a resource-level role.
    referenceRoleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Prelude.Text,
    -- | Amazon S3 object key name.
    fileKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ReferenceDataSourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceRoleARN', 's3ReferenceDataSourceDescription_referenceRoleARN' - The ARN of the IAM role that Kinesis Data Analytics can assume to read
-- the Amazon S3 object on your behalf to populate the in-application
-- reference table.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
--
-- 'bucketARN', 's3ReferenceDataSourceDescription_bucketARN' - The Amazon Resource Name (ARN) of the S3 bucket.
--
-- 'fileKey', 's3ReferenceDataSourceDescription_fileKey' - Amazon S3 object key name.
newS3ReferenceDataSourceDescription ::
  -- | 'bucketARN'
  Prelude.Text ->
  -- | 'fileKey'
  Prelude.Text ->
  S3ReferenceDataSourceDescription
newS3ReferenceDataSourceDescription
  pBucketARN_
  pFileKey_ =
    S3ReferenceDataSourceDescription'
      { referenceRoleARN =
          Prelude.Nothing,
        bucketARN = pBucketARN_,
        fileKey = pFileKey_
      }

-- | The ARN of the IAM role that Kinesis Data Analytics can assume to read
-- the Amazon S3 object on your behalf to populate the in-application
-- reference table.
--
-- Provided for backward compatibility. Applications that are created with
-- the current API version have an application-level service execution role
-- rather than a resource-level role.
s3ReferenceDataSourceDescription_referenceRoleARN :: Lens.Lens' S3ReferenceDataSourceDescription (Prelude.Maybe Prelude.Text)
s3ReferenceDataSourceDescription_referenceRoleARN = Lens.lens (\S3ReferenceDataSourceDescription' {referenceRoleARN} -> referenceRoleARN) (\s@S3ReferenceDataSourceDescription' {} a -> s {referenceRoleARN = a} :: S3ReferenceDataSourceDescription)

-- | The Amazon Resource Name (ARN) of the S3 bucket.
s3ReferenceDataSourceDescription_bucketARN :: Lens.Lens' S3ReferenceDataSourceDescription Prelude.Text
s3ReferenceDataSourceDescription_bucketARN = Lens.lens (\S3ReferenceDataSourceDescription' {bucketARN} -> bucketARN) (\s@S3ReferenceDataSourceDescription' {} a -> s {bucketARN = a} :: S3ReferenceDataSourceDescription)

-- | Amazon S3 object key name.
s3ReferenceDataSourceDescription_fileKey :: Lens.Lens' S3ReferenceDataSourceDescription Prelude.Text
s3ReferenceDataSourceDescription_fileKey = Lens.lens (\S3ReferenceDataSourceDescription' {fileKey} -> fileKey) (\s@S3ReferenceDataSourceDescription' {} a -> s {fileKey = a} :: S3ReferenceDataSourceDescription)

instance
  Data.FromJSON
    S3ReferenceDataSourceDescription
  where
  parseJSON =
    Data.withObject
      "S3ReferenceDataSourceDescription"
      ( \x ->
          S3ReferenceDataSourceDescription'
            Prelude.<$> (x Data..:? "ReferenceRoleARN")
            Prelude.<*> (x Data..: "BucketARN")
            Prelude.<*> (x Data..: "FileKey")
      )

instance
  Prelude.Hashable
    S3ReferenceDataSourceDescription
  where
  hashWithSalt
    _salt
    S3ReferenceDataSourceDescription' {..} =
      _salt
        `Prelude.hashWithSalt` referenceRoleARN
        `Prelude.hashWithSalt` bucketARN
        `Prelude.hashWithSalt` fileKey

instance
  Prelude.NFData
    S3ReferenceDataSourceDescription
  where
  rnf S3ReferenceDataSourceDescription' {..} =
    Prelude.rnf referenceRoleARN
      `Prelude.seq` Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf fileKey
