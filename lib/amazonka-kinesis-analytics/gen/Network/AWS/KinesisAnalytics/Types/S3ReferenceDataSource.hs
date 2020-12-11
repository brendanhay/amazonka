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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.
--
-- An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation to trigger reloading of data into your application.
--
-- /See:/ 'mkS3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { bucketARN ::
      Lude.Text,
    fileKey :: Lude.Text,
    referenceRoleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3ReferenceDataSource' with the minimum fields required to make a request.
--
-- * 'bucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
-- * 'fileKey' - Object key name containing reference data.
-- * 'referenceRoleARN' - ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
mkS3ReferenceDataSource ::
  -- | 'bucketARN'
  Lude.Text ->
  -- | 'fileKey'
  Lude.Text ->
  -- | 'referenceRoleARN'
  Lude.Text ->
  S3ReferenceDataSource
mkS3ReferenceDataSource pBucketARN_ pFileKey_ pReferenceRoleARN_ =
  S3ReferenceDataSource'
    { bucketARN = pBucketARN_,
      fileKey = pFileKey_,
      referenceRoleARN = pReferenceRoleARN_
    }

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

-- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
--
-- /Note:/ Consider using 'referenceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsReferenceRoleARN :: Lens.Lens' S3ReferenceDataSource Lude.Text
srdsReferenceRoleARN = Lens.lens (referenceRoleARN :: S3ReferenceDataSource -> Lude.Text) (\s a -> s {referenceRoleARN = a} :: S3ReferenceDataSource)
{-# DEPRECATED srdsReferenceRoleARN "Use generic-lens or generic-optics with 'referenceRoleARN' instead." #-}

instance Lude.ToJSON S3ReferenceDataSource where
  toJSON S3ReferenceDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BucketARN" Lude..= bucketARN),
            Lude.Just ("FileKey" Lude..= fileKey),
            Lude.Just ("ReferenceRoleARN" Lude..= referenceRoleARN)
          ]
      )
