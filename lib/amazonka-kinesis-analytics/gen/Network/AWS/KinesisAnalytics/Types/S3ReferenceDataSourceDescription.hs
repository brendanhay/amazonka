{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
  ( S3ReferenceDataSourceDescription (..)
  -- * Smart constructor
  , mkS3ReferenceDataSourceDescription
  -- * Lenses
  , srdsdBucketARN
  , srdsdFileKey
  , srdsdReferenceRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.BucketARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.FileKey as Types
import qualified Network.AWS.KinesisAnalytics.Types.ReferenceRoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the bucket name and object key name that stores the reference data.
--
-- /See:/ 'mkS3ReferenceDataSourceDescription' smart constructor.
data S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription'
  { bucketARN :: Types.BucketARN
    -- ^ Amazon Resource Name (ARN) of the S3 bucket.
  , fileKey :: Types.FileKey
    -- ^ Amazon S3 object key name.
  , referenceRoleARN :: Types.ReferenceRoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3ReferenceDataSourceDescription' value with any optional fields omitted.
mkS3ReferenceDataSourceDescription
    :: Types.BucketARN -- ^ 'bucketARN'
    -> Types.FileKey -- ^ 'fileKey'
    -> Types.ReferenceRoleARN -- ^ 'referenceRoleARN'
    -> S3ReferenceDataSourceDescription
mkS3ReferenceDataSourceDescription bucketARN fileKey
  referenceRoleARN
  = S3ReferenceDataSourceDescription'{bucketARN, fileKey,
                                      referenceRoleARN}

-- | Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsdBucketARN :: Lens.Lens' S3ReferenceDataSourceDescription Types.BucketARN
srdsdBucketARN = Lens.field @"bucketARN"
{-# INLINEABLE srdsdBucketARN #-}
{-# DEPRECATED bucketARN "Use generic-lens or generic-optics with 'bucketARN' instead"  #-}

-- | Amazon S3 object key name.
--
-- /Note:/ Consider using 'fileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsdFileKey :: Lens.Lens' S3ReferenceDataSourceDescription Types.FileKey
srdsdFileKey = Lens.field @"fileKey"
{-# INLINEABLE srdsdFileKey #-}
{-# DEPRECATED fileKey "Use generic-lens or generic-optics with 'fileKey' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
--
-- /Note:/ Consider using 'referenceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsdReferenceRoleARN :: Lens.Lens' S3ReferenceDataSourceDescription Types.ReferenceRoleARN
srdsdReferenceRoleARN = Lens.field @"referenceRoleARN"
{-# INLINEABLE srdsdReferenceRoleARN #-}
{-# DEPRECATED referenceRoleARN "Use generic-lens or generic-optics with 'referenceRoleARN' instead"  #-}

instance Core.FromJSON S3ReferenceDataSourceDescription where
        parseJSON
          = Core.withObject "S3ReferenceDataSourceDescription" Core.$
              \ x ->
                S3ReferenceDataSourceDescription' Core.<$>
                  (x Core..: "BucketARN") Core.<*> x Core..: "FileKey" Core.<*>
                    x Core..: "ReferenceRoleARN"
