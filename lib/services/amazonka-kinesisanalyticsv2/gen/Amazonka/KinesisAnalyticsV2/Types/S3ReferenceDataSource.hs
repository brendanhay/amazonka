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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, identifies the
-- Amazon S3 bucket and object that contains the reference data.
--
-- A Kinesis Data Analytics application loads reference data only once. If
-- the data changes, you call the UpdateApplication operation to trigger
-- reloading of data into your application.
--
-- /See:/ 'newS3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { -- | The Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Prelude.Maybe Prelude.Text,
    -- | The object key name containing the reference data.
    fileKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ReferenceDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketARN', 's3ReferenceDataSource_bucketARN' - The Amazon Resource Name (ARN) of the S3 bucket.
--
-- 'fileKey', 's3ReferenceDataSource_fileKey' - The object key name containing the reference data.
newS3ReferenceDataSource ::
  S3ReferenceDataSource
newS3ReferenceDataSource =
  S3ReferenceDataSource'
    { bucketARN = Prelude.Nothing,
      fileKey = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the S3 bucket.
s3ReferenceDataSource_bucketARN :: Lens.Lens' S3ReferenceDataSource (Prelude.Maybe Prelude.Text)
s3ReferenceDataSource_bucketARN = Lens.lens (\S3ReferenceDataSource' {bucketARN} -> bucketARN) (\s@S3ReferenceDataSource' {} a -> s {bucketARN = a} :: S3ReferenceDataSource)

-- | The object key name containing the reference data.
s3ReferenceDataSource_fileKey :: Lens.Lens' S3ReferenceDataSource (Prelude.Maybe Prelude.Text)
s3ReferenceDataSource_fileKey = Lens.lens (\S3ReferenceDataSource' {fileKey} -> fileKey) (\s@S3ReferenceDataSource' {} a -> s {fileKey = a} :: S3ReferenceDataSource)

instance Prelude.Hashable S3ReferenceDataSource where
  hashWithSalt _salt S3ReferenceDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` bucketARN
      `Prelude.hashWithSalt` fileKey

instance Prelude.NFData S3ReferenceDataSource where
  rnf S3ReferenceDataSource' {..} =
    Prelude.rnf bucketARN
      `Prelude.seq` Prelude.rnf fileKey

instance Data.ToJSON S3ReferenceDataSource where
  toJSON S3ReferenceDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketARN" Data..=) Prelude.<$> bucketARN,
            ("FileKey" Data..=) Prelude.<$> fileKey
          ]
      )
