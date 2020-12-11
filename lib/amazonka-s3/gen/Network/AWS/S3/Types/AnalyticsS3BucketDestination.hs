-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsS3BucketDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsS3BucketDestination
  ( AnalyticsS3BucketDestination (..),

    -- * Smart constructor
    mkAnalyticsS3BucketDestination,

    -- * Lenses
    asbdBucketAccountId,
    asbdPrefix,
    asbdFormat,
    asbdBucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsS3ExportFileFormat

-- | Contains information about where to publish the analytics results.
--
-- /See:/ 'mkAnalyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { bucketAccountId ::
      Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    format ::
      AnalyticsS3ExportFileFormat,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalyticsS3BucketDestination' with the minimum fields required to make a request.
--
-- * 'bucket' - The Amazon Resource Name (ARN) of the bucket to which data is exported.
-- * 'bucketAccountId' - The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
-- * 'format' - Specifies the file format used when exporting data to Amazon S3.
-- * 'prefix' - The prefix to use when exporting data. The prefix is prepended to all results.
mkAnalyticsS3BucketDestination ::
  -- | 'format'
  AnalyticsS3ExportFileFormat ->
  -- | 'bucket'
  BucketName ->
  AnalyticsS3BucketDestination
mkAnalyticsS3BucketDestination pFormat_ pBucket_ =
  AnalyticsS3BucketDestination'
    { bucketAccountId = Lude.Nothing,
      prefix = Lude.Nothing,
      format = pFormat_,
      bucket = pBucket_
    }

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- /Note:/ Consider using 'bucketAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdBucketAccountId :: Lens.Lens' AnalyticsS3BucketDestination (Lude.Maybe Lude.Text)
asbdBucketAccountId = Lens.lens (bucketAccountId :: AnalyticsS3BucketDestination -> Lude.Maybe Lude.Text) (\s a -> s {bucketAccountId = a} :: AnalyticsS3BucketDestination)
{-# DEPRECATED asbdBucketAccountId "Use generic-lens or generic-optics with 'bucketAccountId' instead." #-}

-- | The prefix to use when exporting data. The prefix is prepended to all results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdPrefix :: Lens.Lens' AnalyticsS3BucketDestination (Lude.Maybe Lude.Text)
asbdPrefix = Lens.lens (prefix :: AnalyticsS3BucketDestination -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: AnalyticsS3BucketDestination)
{-# DEPRECATED asbdPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Specifies the file format used when exporting data to Amazon S3.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdFormat :: Lens.Lens' AnalyticsS3BucketDestination AnalyticsS3ExportFileFormat
asbdFormat = Lens.lens (format :: AnalyticsS3BucketDestination -> AnalyticsS3ExportFileFormat) (\s a -> s {format = a} :: AnalyticsS3BucketDestination)
{-# DEPRECATED asbdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdBucket :: Lens.Lens' AnalyticsS3BucketDestination BucketName
asbdBucket = Lens.lens (bucket :: AnalyticsS3BucketDestination -> BucketName) (\s a -> s {bucket = a} :: AnalyticsS3BucketDestination)
{-# DEPRECATED asbdBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.FromXML AnalyticsS3BucketDestination where
  parseXML x =
    AnalyticsS3BucketDestination'
      Lude.<$> (x Lude..@? "BucketAccountId")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@ "Format")
      Lude.<*> (x Lude..@ "Bucket")

instance Lude.ToXML AnalyticsS3BucketDestination where
  toXML AnalyticsS3BucketDestination' {..} =
    Lude.mconcat
      [ "BucketAccountId" Lude.@= bucketAccountId,
        "Prefix" Lude.@= prefix,
        "Format" Lude.@= format,
        "Bucket" Lude.@= bucket
      ]
