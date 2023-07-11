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
-- Module      : Amazonka.S3.Types.AnalyticsS3BucketDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.AnalyticsS3BucketDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.AnalyticsS3ExportFileFormat

-- | Contains information about where to publish the analytics results.
--
-- /See:/ 'newAnalyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { -- | The account ID that owns the destination S3 bucket. If no account ID is
    -- provided, the owner is not validated before exporting data.
    --
    -- Although this value is optional, we strongly recommend that you set it
    -- to help prevent problems if the destination bucket ownership changes.
    bucketAccountId :: Prelude.Maybe Prelude.Text,
    -- | The prefix to use when exporting data. The prefix is prepended to all
    -- results.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies the file format used when exporting data to Amazon S3.
    format :: AnalyticsS3ExportFileFormat,
    -- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyticsS3BucketDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketAccountId', 'analyticsS3BucketDestination_bucketAccountId' - The account ID that owns the destination S3 bucket. If no account ID is
-- provided, the owner is not validated before exporting data.
--
-- Although this value is optional, we strongly recommend that you set it
-- to help prevent problems if the destination bucket ownership changes.
--
-- 'prefix', 'analyticsS3BucketDestination_prefix' - The prefix to use when exporting data. The prefix is prepended to all
-- results.
--
-- 'format', 'analyticsS3BucketDestination_format' - Specifies the file format used when exporting data to Amazon S3.
--
-- 'bucket', 'analyticsS3BucketDestination_bucket' - The Amazon Resource Name (ARN) of the bucket to which data is exported.
newAnalyticsS3BucketDestination ::
  -- | 'format'
  AnalyticsS3ExportFileFormat ->
  -- | 'bucket'
  BucketName ->
  AnalyticsS3BucketDestination
newAnalyticsS3BucketDestination pFormat_ pBucket_ =
  AnalyticsS3BucketDestination'
    { bucketAccountId =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      format = pFormat_,
      bucket = pBucket_
    }

-- | The account ID that owns the destination S3 bucket. If no account ID is
-- provided, the owner is not validated before exporting data.
--
-- Although this value is optional, we strongly recommend that you set it
-- to help prevent problems if the destination bucket ownership changes.
analyticsS3BucketDestination_bucketAccountId :: Lens.Lens' AnalyticsS3BucketDestination (Prelude.Maybe Prelude.Text)
analyticsS3BucketDestination_bucketAccountId = Lens.lens (\AnalyticsS3BucketDestination' {bucketAccountId} -> bucketAccountId) (\s@AnalyticsS3BucketDestination' {} a -> s {bucketAccountId = a} :: AnalyticsS3BucketDestination)

-- | The prefix to use when exporting data. The prefix is prepended to all
-- results.
analyticsS3BucketDestination_prefix :: Lens.Lens' AnalyticsS3BucketDestination (Prelude.Maybe Prelude.Text)
analyticsS3BucketDestination_prefix = Lens.lens (\AnalyticsS3BucketDestination' {prefix} -> prefix) (\s@AnalyticsS3BucketDestination' {} a -> s {prefix = a} :: AnalyticsS3BucketDestination)

-- | Specifies the file format used when exporting data to Amazon S3.
analyticsS3BucketDestination_format :: Lens.Lens' AnalyticsS3BucketDestination AnalyticsS3ExportFileFormat
analyticsS3BucketDestination_format = Lens.lens (\AnalyticsS3BucketDestination' {format} -> format) (\s@AnalyticsS3BucketDestination' {} a -> s {format = a} :: AnalyticsS3BucketDestination)

-- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
analyticsS3BucketDestination_bucket :: Lens.Lens' AnalyticsS3BucketDestination BucketName
analyticsS3BucketDestination_bucket = Lens.lens (\AnalyticsS3BucketDestination' {bucket} -> bucket) (\s@AnalyticsS3BucketDestination' {} a -> s {bucket = a} :: AnalyticsS3BucketDestination)

instance Data.FromXML AnalyticsS3BucketDestination where
  parseXML x =
    AnalyticsS3BucketDestination'
      Prelude.<$> (x Data..@? "BucketAccountId")
      Prelude.<*> (x Data..@? "Prefix")
      Prelude.<*> (x Data..@ "Format")
      Prelude.<*> (x Data..@ "Bucket")

instance
  Prelude.Hashable
    AnalyticsS3BucketDestination
  where
  hashWithSalt _salt AnalyticsS3BucketDestination' {..} =
    _salt
      `Prelude.hashWithSalt` bucketAccountId
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData AnalyticsS3BucketDestination where
  rnf AnalyticsS3BucketDestination' {..} =
    Prelude.rnf bucketAccountId
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToXML AnalyticsS3BucketDestination where
  toXML AnalyticsS3BucketDestination' {..} =
    Prelude.mconcat
      [ "BucketAccountId" Data.@= bucketAccountId,
        "Prefix" Data.@= prefix,
        "Format" Data.@= format,
        "Bucket" Data.@= bucket
      ]
