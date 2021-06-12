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
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the bucket name and object key name that stores the reference
-- data.
--
-- /See:/ 'newS3ReferenceDataSourceDescription' smart constructor.
data S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription'
  { -- | Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Core.Text,
    -- | Amazon S3 object key name.
    fileKey :: Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the
    -- Amazon S3 object on your behalf to populate the in-application reference
    -- table.
    referenceRoleARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3ReferenceDataSourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketARN', 's3ReferenceDataSourceDescription_bucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- 'fileKey', 's3ReferenceDataSourceDescription_fileKey' - Amazon S3 object key name.
--
-- 'referenceRoleARN', 's3ReferenceDataSourceDescription_referenceRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the
-- Amazon S3 object on your behalf to populate the in-application reference
-- table.
newS3ReferenceDataSourceDescription ::
  -- | 'bucketARN'
  Core.Text ->
  -- | 'fileKey'
  Core.Text ->
  -- | 'referenceRoleARN'
  Core.Text ->
  S3ReferenceDataSourceDescription
newS3ReferenceDataSourceDescription
  pBucketARN_
  pFileKey_
  pReferenceRoleARN_ =
    S3ReferenceDataSourceDescription'
      { bucketARN =
          pBucketARN_,
        fileKey = pFileKey_,
        referenceRoleARN = pReferenceRoleARN_
      }

-- | Amazon Resource Name (ARN) of the S3 bucket.
s3ReferenceDataSourceDescription_bucketARN :: Lens.Lens' S3ReferenceDataSourceDescription Core.Text
s3ReferenceDataSourceDescription_bucketARN = Lens.lens (\S3ReferenceDataSourceDescription' {bucketARN} -> bucketARN) (\s@S3ReferenceDataSourceDescription' {} a -> s {bucketARN = a} :: S3ReferenceDataSourceDescription)

-- | Amazon S3 object key name.
s3ReferenceDataSourceDescription_fileKey :: Lens.Lens' S3ReferenceDataSourceDescription Core.Text
s3ReferenceDataSourceDescription_fileKey = Lens.lens (\S3ReferenceDataSourceDescription' {fileKey} -> fileKey) (\s@S3ReferenceDataSourceDescription' {} a -> s {fileKey = a} :: S3ReferenceDataSourceDescription)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the
-- Amazon S3 object on your behalf to populate the in-application reference
-- table.
s3ReferenceDataSourceDescription_referenceRoleARN :: Lens.Lens' S3ReferenceDataSourceDescription Core.Text
s3ReferenceDataSourceDescription_referenceRoleARN = Lens.lens (\S3ReferenceDataSourceDescription' {referenceRoleARN} -> referenceRoleARN) (\s@S3ReferenceDataSourceDescription' {} a -> s {referenceRoleARN = a} :: S3ReferenceDataSourceDescription)

instance
  Core.FromJSON
    S3ReferenceDataSourceDescription
  where
  parseJSON =
    Core.withObject
      "S3ReferenceDataSourceDescription"
      ( \x ->
          S3ReferenceDataSourceDescription'
            Core.<$> (x Core..: "BucketARN")
            Core.<*> (x Core..: "FileKey")
            Core.<*> (x Core..: "ReferenceRoleARN")
      )

instance
  Core.Hashable
    S3ReferenceDataSourceDescription

instance Core.NFData S3ReferenceDataSourceDescription
