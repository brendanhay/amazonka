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
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies the S3 bucket and object that contains the reference data.
-- Also identifies the IAM role Amazon Kinesis Analytics can assume to read
-- this object on your behalf.
--
-- An Amazon Kinesis Analytics application loads reference data only once.
-- If the data changes, you call the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication>
-- operation to trigger reloading of data into your application.
--
-- /See:/ 'newS3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { -- | Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Prelude.Text,
    -- | Object key name containing reference data.
    fileKey :: Prelude.Text,
    -- | ARN of the IAM role that the service can assume to read data on your
    -- behalf. This role must have permission for the @s3:GetObject@ action on
    -- the object and trust policy that allows Amazon Kinesis Analytics service
    -- principal to assume this role.
    referenceRoleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3ReferenceDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketARN', 's3ReferenceDataSource_bucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- 'fileKey', 's3ReferenceDataSource_fileKey' - Object key name containing reference data.
--
-- 'referenceRoleARN', 's3ReferenceDataSource_referenceRoleARN' - ARN of the IAM role that the service can assume to read data on your
-- behalf. This role must have permission for the @s3:GetObject@ action on
-- the object and trust policy that allows Amazon Kinesis Analytics service
-- principal to assume this role.
newS3ReferenceDataSource ::
  -- | 'bucketARN'
  Prelude.Text ->
  -- | 'fileKey'
  Prelude.Text ->
  -- | 'referenceRoleARN'
  Prelude.Text ->
  S3ReferenceDataSource
newS3ReferenceDataSource
  pBucketARN_
  pFileKey_
  pReferenceRoleARN_ =
    S3ReferenceDataSource'
      { bucketARN = pBucketARN_,
        fileKey = pFileKey_,
        referenceRoleARN = pReferenceRoleARN_
      }

-- | Amazon Resource Name (ARN) of the S3 bucket.
s3ReferenceDataSource_bucketARN :: Lens.Lens' S3ReferenceDataSource Prelude.Text
s3ReferenceDataSource_bucketARN = Lens.lens (\S3ReferenceDataSource' {bucketARN} -> bucketARN) (\s@S3ReferenceDataSource' {} a -> s {bucketARN = a} :: S3ReferenceDataSource)

-- | Object key name containing reference data.
s3ReferenceDataSource_fileKey :: Lens.Lens' S3ReferenceDataSource Prelude.Text
s3ReferenceDataSource_fileKey = Lens.lens (\S3ReferenceDataSource' {fileKey} -> fileKey) (\s@S3ReferenceDataSource' {} a -> s {fileKey = a} :: S3ReferenceDataSource)

-- | ARN of the IAM role that the service can assume to read data on your
-- behalf. This role must have permission for the @s3:GetObject@ action on
-- the object and trust policy that allows Amazon Kinesis Analytics service
-- principal to assume this role.
s3ReferenceDataSource_referenceRoleARN :: Lens.Lens' S3ReferenceDataSource Prelude.Text
s3ReferenceDataSource_referenceRoleARN = Lens.lens (\S3ReferenceDataSource' {referenceRoleARN} -> referenceRoleARN) (\s@S3ReferenceDataSource' {} a -> s {referenceRoleARN = a} :: S3ReferenceDataSource)

instance Prelude.Hashable S3ReferenceDataSource

instance Prelude.NFData S3ReferenceDataSource

instance Prelude.ToJSON S3ReferenceDataSource where
  toJSON S3ReferenceDataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("BucketARN" Prelude..= bucketARN),
            Prelude.Just ("FileKey" Prelude..= fileKey),
            Prelude.Just
              ("ReferenceRoleARN" Prelude..= referenceRoleARN)
          ]
      )
