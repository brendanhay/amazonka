{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
  ( S3ReferenceDataSourceUpdate (..),

    -- * Smart constructor
    mkS3ReferenceDataSourceUpdate,

    -- * Lenses
    srdsuBucketARNUpdate,
    srdsuFileKeyUpdate,
    srdsuReferenceRoleARNUpdate,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.BucketARNUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.FileKeyUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.ReferenceRoleARNUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
-- /See:/ 'mkS3ReferenceDataSourceUpdate' smart constructor.
data S3ReferenceDataSourceUpdate = S3ReferenceDataSourceUpdate'
  { -- | Amazon Resource Name (ARN) of the S3 bucket.
    bucketARNUpdate :: Core.Maybe Types.BucketARNUpdate,
    -- | Object key name.
    fileKeyUpdate :: Core.Maybe Types.FileKeyUpdate,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application.
    referenceRoleARNUpdate :: Core.Maybe Types.ReferenceRoleARNUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3ReferenceDataSourceUpdate' value with any optional fields omitted.
mkS3ReferenceDataSourceUpdate ::
  S3ReferenceDataSourceUpdate
mkS3ReferenceDataSourceUpdate =
  S3ReferenceDataSourceUpdate'
    { bucketARNUpdate = Core.Nothing,
      fileKeyUpdate = Core.Nothing,
      referenceRoleARNUpdate = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'bucketARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsuBucketARNUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Core.Maybe Types.BucketARNUpdate)
srdsuBucketARNUpdate = Lens.field @"bucketARNUpdate"
{-# DEPRECATED srdsuBucketARNUpdate "Use generic-lens or generic-optics with 'bucketARNUpdate' instead." #-}

-- | Object key name.
--
-- /Note:/ Consider using 'fileKeyUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsuFileKeyUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Core.Maybe Types.FileKeyUpdate)
srdsuFileKeyUpdate = Lens.field @"fileKeyUpdate"
{-# DEPRECATED srdsuFileKeyUpdate "Use generic-lens or generic-optics with 'fileKeyUpdate' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application.
--
-- /Note:/ Consider using 'referenceRoleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsuReferenceRoleARNUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Core.Maybe Types.ReferenceRoleARNUpdate)
srdsuReferenceRoleARNUpdate = Lens.field @"referenceRoleARNUpdate"
{-# DEPRECATED srdsuReferenceRoleARNUpdate "Use generic-lens or generic-optics with 'referenceRoleARNUpdate' instead." #-}

instance Core.FromJSON S3ReferenceDataSourceUpdate where
  toJSON S3ReferenceDataSourceUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("BucketARNUpdate" Core..=) Core.<$> bucketARNUpdate,
            ("FileKeyUpdate" Core..=) Core.<$> fileKeyUpdate,
            ("ReferenceRoleARNUpdate" Core..=)
              Core.<$> referenceRoleARNUpdate
          ]
      )
