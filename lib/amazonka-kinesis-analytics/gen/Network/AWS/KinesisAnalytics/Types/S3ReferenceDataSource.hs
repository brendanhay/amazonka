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
    srdsReferenceRoleARN,
    srdsBucketARN,
    srdsFileKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.
--
-- An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation to trigger reloading of data into your application.
--
-- /See:/ 'mkS3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { -- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
    referenceRoleARN :: Lude.Text,
    -- | Amazon Resource Name (ARN) of the S3 bucket.
    bucketARN :: Lude.Text,
    -- | Object key name containing reference data.
    fileKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3ReferenceDataSource' with the minimum fields required to make a request.
--
-- * 'referenceRoleARN' - ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
-- * 'bucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
-- * 'fileKey' - Object key name containing reference data.
mkS3ReferenceDataSource ::
  -- | 'referenceRoleARN'
  Lude.Text ->
  -- | 'bucketARN'
  Lude.Text ->
  -- | 'fileKey'
  Lude.Text ->
  S3ReferenceDataSource
mkS3ReferenceDataSource pReferenceRoleARN_ pBucketARN_ pFileKey_ =
  S3ReferenceDataSource'
    { referenceRoleARN = pReferenceRoleARN_,
      bucketARN = pBucketARN_,
      fileKey = pFileKey_
    }

-- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
--
-- /Note:/ Consider using 'referenceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsReferenceRoleARN :: Lens.Lens' S3ReferenceDataSource Lude.Text
srdsReferenceRoleARN = Lens.lens (referenceRoleARN :: S3ReferenceDataSource -> Lude.Text) (\s a -> s {referenceRoleARN = a} :: S3ReferenceDataSource)
{-# DEPRECATED srdsReferenceRoleARN "Use generic-lens or generic-optics with 'referenceRoleARN' instead." #-}

-- | Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsBucketARN :: Lens.Lens' S3ReferenceDataSource Lude.Text
srdsBucketARN = Lens.lens (bucketARN :: S3ReferenceDataSource -> Lude.Text) (\s a -> s {bucketARN = a} :: S3ReferenceDataSource)
{-# DEPRECATED srdsBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | Object key name containing reference data.
--
-- /Note:/ Consider using 'fileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsFileKey :: Lens.Lens' S3ReferenceDataSource Lude.Text
srdsFileKey = Lens.lens (fileKey :: S3ReferenceDataSource -> Lude.Text) (\s a -> s {fileKey = a} :: S3ReferenceDataSource)
{-# DEPRECATED srdsFileKey "Use generic-lens or generic-optics with 'fileKey' instead." #-}

instance Lude.ToJSON S3ReferenceDataSource where
  toJSON S3ReferenceDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReferenceRoleARN" Lude..= referenceRoleARN),
            Lude.Just ("BucketARN" Lude..= bucketARN),
            Lude.Just ("FileKey" Lude..= fileKey)
          ]
      )
