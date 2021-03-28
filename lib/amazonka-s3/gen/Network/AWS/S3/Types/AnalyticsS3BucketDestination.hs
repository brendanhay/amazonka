{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsS3BucketDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.AnalyticsS3BucketDestination
  ( AnalyticsS3BucketDestination (..)
  -- * Smart constructor
  , mkAnalyticsS3BucketDestination
  -- * Lenses
  , asbdFormat
  , asbdBucket
  , asbdBucketAccountId
  , asbdPrefix
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AccountId as Types
import qualified Network.AWS.S3.Types.AnalyticsS3ExportFileFormat as Types
import qualified Network.AWS.S3.Types.Prefix as Types

-- | Contains information about where to publish the analytics results.
--
-- /See:/ 'mkAnalyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { format :: Types.AnalyticsS3ExportFileFormat
    -- ^ Specifies the file format used when exporting data to Amazon S3.
  , bucket :: Types.BucketName
    -- ^ The Amazon Resource Name (ARN) of the bucket to which data is exported.
  , bucketAccountId :: Core.Maybe Types.AccountId
    -- ^ The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The prefix to use when exporting data. The prefix is prepended to all results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsS3BucketDestination' value with any optional fields omitted.
mkAnalyticsS3BucketDestination
    :: Types.AnalyticsS3ExportFileFormat -- ^ 'format'
    -> Types.BucketName -- ^ 'bucket'
    -> AnalyticsS3BucketDestination
mkAnalyticsS3BucketDestination format bucket
  = AnalyticsS3BucketDestination'{format, bucket,
                                  bucketAccountId = Core.Nothing, prefix = Core.Nothing}

-- | Specifies the file format used when exporting data to Amazon S3.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdFormat :: Lens.Lens' AnalyticsS3BucketDestination Types.AnalyticsS3ExportFileFormat
asbdFormat = Lens.field @"format"
{-# INLINEABLE asbdFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdBucket :: Lens.Lens' AnalyticsS3BucketDestination Types.BucketName
asbdBucket = Lens.field @"bucket"
{-# INLINEABLE asbdBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- /Note:/ Consider using 'bucketAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdBucketAccountId :: Lens.Lens' AnalyticsS3BucketDestination (Core.Maybe Types.AccountId)
asbdBucketAccountId = Lens.field @"bucketAccountId"
{-# INLINEABLE asbdBucketAccountId #-}
{-# DEPRECATED bucketAccountId "Use generic-lens or generic-optics with 'bucketAccountId' instead"  #-}

-- | The prefix to use when exporting data. The prefix is prepended to all results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asbdPrefix :: Lens.Lens' AnalyticsS3BucketDestination (Core.Maybe Types.Prefix)
asbdPrefix = Lens.field @"prefix"
{-# INLINEABLE asbdPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.ToXML AnalyticsS3BucketDestination where
        toXML AnalyticsS3BucketDestination{..}
          = Core.toXMLElement "Format" format Core.<>
              Core.toXMLElement "Bucket" bucket
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "BucketAccountId")
                bucketAccountId
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix

instance Core.FromXML AnalyticsS3BucketDestination where
        parseXML x
          = AnalyticsS3BucketDestination' Core.<$>
              (x Core..@ "Format") Core.<*> x Core..@ "Bucket" Core.<*>
                x Core..@? "BucketAccountId"
                Core.<*> x Core..@? "Prefix"
