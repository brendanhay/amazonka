{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
  ( S3ReferenceDataSource (..),

    -- * Smart constructor
    mkS3ReferenceDataSource,

    -- * Lenses
    srdsBucketARN,
    srdsFileKey,
    srdsReferenceRoleARN,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.BucketARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.FileKey as Types
import qualified Network.AWS.KinesisAnalytics.Types.ReferenceRoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.
--
-- An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation to trigger reloading of data into your application.
--
-- /See:/ 'mkS3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { -- | Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Types.BucketARN,
    -- | Object key name containing reference data.
    fileKey :: Types.FileKey,
    -- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
    referenceRoleARN :: Types.ReferenceRoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3ReferenceDataSource' value with any optional fields omitted.
mkS3ReferenceDataSource ::
  -- | 'bucketARN'
  Types.BucketARN ->
  -- | 'fileKey'
  Types.FileKey ->
  -- | 'referenceRoleARN'
  Types.ReferenceRoleARN ->
  S3ReferenceDataSource
mkS3ReferenceDataSource bucketARN fileKey referenceRoleARN =
  S3ReferenceDataSource' {bucketARN, fileKey, referenceRoleARN}

-- | Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsBucketARN :: Lens.Lens' S3ReferenceDataSource Types.BucketARN
srdsBucketARN = Lens.field @"bucketARN"
{-# DEPRECATED srdsBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | Object key name containing reference data.
--
-- /Note:/ Consider using 'fileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsFileKey :: Lens.Lens' S3ReferenceDataSource Types.FileKey
srdsFileKey = Lens.field @"fileKey"
{-# DEPRECATED srdsFileKey "Use generic-lens or generic-optics with 'fileKey' instead." #-}

-- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
--
-- /Note:/ Consider using 'referenceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsReferenceRoleARN :: Lens.Lens' S3ReferenceDataSource Types.ReferenceRoleARN
srdsReferenceRoleARN = Lens.field @"referenceRoleARN"
{-# DEPRECATED srdsReferenceRoleARN "Use generic-lens or generic-optics with 'referenceRoleARN' instead." #-}

instance Core.FromJSON S3ReferenceDataSource where
  toJSON S3ReferenceDataSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BucketARN" Core..= bucketARN),
            Core.Just ("FileKey" Core..= fileKey),
            Core.Just ("ReferenceRoleARN" Core..= referenceRoleARN)
          ]
      )
