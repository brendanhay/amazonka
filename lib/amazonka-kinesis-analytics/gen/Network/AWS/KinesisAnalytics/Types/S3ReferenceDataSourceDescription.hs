-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
  ( S3ReferenceDataSourceDescription (..),

    -- * Smart constructor
    mkS3ReferenceDataSourceDescription,

    -- * Lenses
    srdsdBucketARN,
    srdsdFileKey,
    srdsdReferenceRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the bucket name and object key name that stores the reference data.
--
-- /See:/ 'mkS3ReferenceDataSourceDescription' smart constructor.
data S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription'
  { bucketARN ::
      Lude.Text,
    fileKey :: Lude.Text,
    referenceRoleARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3ReferenceDataSourceDescription' with the minimum fields required to make a request.
--
-- * 'bucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
-- * 'fileKey' - Amazon S3 object key name.
-- * 'referenceRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
mkS3ReferenceDataSourceDescription ::
  -- | 'bucketARN'
  Lude.Text ->
  -- | 'fileKey'
  Lude.Text ->
  -- | 'referenceRoleARN'
  Lude.Text ->
  S3ReferenceDataSourceDescription
mkS3ReferenceDataSourceDescription
  pBucketARN_
  pFileKey_
  pReferenceRoleARN_ =
    S3ReferenceDataSourceDescription'
      { bucketARN = pBucketARN_,
        fileKey = pFileKey_,
        referenceRoleARN = pReferenceRoleARN_
      }

-- | Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsdBucketARN :: Lens.Lens' S3ReferenceDataSourceDescription Lude.Text
srdsdBucketARN = Lens.lens (bucketARN :: S3ReferenceDataSourceDescription -> Lude.Text) (\s a -> s {bucketARN = a} :: S3ReferenceDataSourceDescription)
{-# DEPRECATED srdsdBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | Amazon S3 object key name.
--
-- /Note:/ Consider using 'fileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsdFileKey :: Lens.Lens' S3ReferenceDataSourceDescription Lude.Text
srdsdFileKey = Lens.lens (fileKey :: S3ReferenceDataSourceDescription -> Lude.Text) (\s a -> s {fileKey = a} :: S3ReferenceDataSourceDescription)
{-# DEPRECATED srdsdFileKey "Use generic-lens or generic-optics with 'fileKey' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
--
-- /Note:/ Consider using 'referenceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsdReferenceRoleARN :: Lens.Lens' S3ReferenceDataSourceDescription Lude.Text
srdsdReferenceRoleARN = Lens.lens (referenceRoleARN :: S3ReferenceDataSourceDescription -> Lude.Text) (\s a -> s {referenceRoleARN = a} :: S3ReferenceDataSourceDescription)
{-# DEPRECATED srdsdReferenceRoleARN "Use generic-lens or generic-optics with 'referenceRoleARN' instead." #-}

instance Lude.FromJSON S3ReferenceDataSourceDescription where
  parseJSON =
    Lude.withObject
      "S3ReferenceDataSourceDescription"
      ( \x ->
          S3ReferenceDataSourceDescription'
            Lude.<$> (x Lude..: "BucketARN")
            Lude.<*> (x Lude..: "FileKey")
            Lude.<*> (x Lude..: "ReferenceRoleARN")
      )
