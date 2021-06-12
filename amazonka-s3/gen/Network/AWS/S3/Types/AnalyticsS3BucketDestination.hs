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
-- Module      : Network.AWS.S3.Types.AnalyticsS3BucketDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsS3BucketDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsS3ExportFileFormat

-- | Contains information about where to publish the analytics results.
--
-- /See:/ 'newAnalyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { -- | The prefix to use when exporting data. The prefix is prepended to all
    -- results.
    prefix :: Core.Maybe Core.Text,
    -- | The account ID that owns the destination S3 bucket. If no account ID is
    -- provided, the owner is not validated before exporting data.
    --
    -- Although this value is optional, we strongly recommend that you set it
    -- to help prevent problems if the destination bucket ownership changes.
    bucketAccountId :: Core.Maybe Core.Text,
    -- | Specifies the file format used when exporting data to Amazon S3.
    format :: AnalyticsS3ExportFileFormat,
    -- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalyticsS3BucketDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'analyticsS3BucketDestination_prefix' - The prefix to use when exporting data. The prefix is prepended to all
-- results.
--
-- 'bucketAccountId', 'analyticsS3BucketDestination_bucketAccountId' - The account ID that owns the destination S3 bucket. If no account ID is
-- provided, the owner is not validated before exporting data.
--
-- Although this value is optional, we strongly recommend that you set it
-- to help prevent problems if the destination bucket ownership changes.
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
    { prefix =
        Core.Nothing,
      bucketAccountId = Core.Nothing,
      format = pFormat_,
      bucket = pBucket_
    }

-- | The prefix to use when exporting data. The prefix is prepended to all
-- results.
analyticsS3BucketDestination_prefix :: Lens.Lens' AnalyticsS3BucketDestination (Core.Maybe Core.Text)
analyticsS3BucketDestination_prefix = Lens.lens (\AnalyticsS3BucketDestination' {prefix} -> prefix) (\s@AnalyticsS3BucketDestination' {} a -> s {prefix = a} :: AnalyticsS3BucketDestination)

-- | The account ID that owns the destination S3 bucket. If no account ID is
-- provided, the owner is not validated before exporting data.
--
-- Although this value is optional, we strongly recommend that you set it
-- to help prevent problems if the destination bucket ownership changes.
analyticsS3BucketDestination_bucketAccountId :: Lens.Lens' AnalyticsS3BucketDestination (Core.Maybe Core.Text)
analyticsS3BucketDestination_bucketAccountId = Lens.lens (\AnalyticsS3BucketDestination' {bucketAccountId} -> bucketAccountId) (\s@AnalyticsS3BucketDestination' {} a -> s {bucketAccountId = a} :: AnalyticsS3BucketDestination)

-- | Specifies the file format used when exporting data to Amazon S3.
analyticsS3BucketDestination_format :: Lens.Lens' AnalyticsS3BucketDestination AnalyticsS3ExportFileFormat
analyticsS3BucketDestination_format = Lens.lens (\AnalyticsS3BucketDestination' {format} -> format) (\s@AnalyticsS3BucketDestination' {} a -> s {format = a} :: AnalyticsS3BucketDestination)

-- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
analyticsS3BucketDestination_bucket :: Lens.Lens' AnalyticsS3BucketDestination BucketName
analyticsS3BucketDestination_bucket = Lens.lens (\AnalyticsS3BucketDestination' {bucket} -> bucket) (\s@AnalyticsS3BucketDestination' {} a -> s {bucket = a} :: AnalyticsS3BucketDestination)

instance Core.FromXML AnalyticsS3BucketDestination where
  parseXML x =
    AnalyticsS3BucketDestination'
      Core.<$> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "BucketAccountId")
      Core.<*> (x Core..@ "Format")
      Core.<*> (x Core..@ "Bucket")

instance Core.Hashable AnalyticsS3BucketDestination

instance Core.NFData AnalyticsS3BucketDestination

instance Core.ToXML AnalyticsS3BucketDestination where
  toXML AnalyticsS3BucketDestination' {..} =
    Core.mconcat
      [ "Prefix" Core.@= prefix,
        "BucketAccountId" Core.@= bucketAccountId,
        "Format" Core.@= format,
        "Bucket" Core.@= bucket
      ]
