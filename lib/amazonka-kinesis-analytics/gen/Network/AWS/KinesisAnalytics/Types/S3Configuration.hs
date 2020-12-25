{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3Configuration
  ( S3Configuration (..),

    -- * Smart constructor
    mkS3Configuration,

    -- * Lenses
    scRoleARN,
    scBucketARN,
    scFileKey,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.BucketARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.FileKey as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the Amazon S3 object that contains the data.
--
-- /See:/ 'mkS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | IAM ARN of the role used to access the data.
    roleARN :: Types.RoleARN,
    -- | ARN of the S3 bucket that contains the data.
    bucketARN :: Types.BucketARN,
    -- | The name of the object that contains the data.
    fileKey :: Types.FileKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Configuration' value with any optional fields omitted.
mkS3Configuration ::
  -- | 'roleARN'
  Types.RoleARN ->
  -- | 'bucketARN'
  Types.BucketARN ->
  -- | 'fileKey'
  Types.FileKey ->
  S3Configuration
mkS3Configuration roleARN bucketARN fileKey =
  S3Configuration' {roleARN, bucketARN, fileKey}

-- | IAM ARN of the role used to access the data.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scRoleARN :: Lens.Lens' S3Configuration Types.RoleARN
scRoleARN = Lens.field @"roleARN"
{-# DEPRECATED scRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | ARN of the S3 bucket that contains the data.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBucketARN :: Lens.Lens' S3Configuration Types.BucketARN
scBucketARN = Lens.field @"bucketARN"
{-# DEPRECATED scBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The name of the object that contains the data.
--
-- /Note:/ Consider using 'fileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFileKey :: Lens.Lens' S3Configuration Types.FileKey
scFileKey = Lens.field @"fileKey"
{-# DEPRECATED scFileKey "Use generic-lens or generic-optics with 'fileKey' instead." #-}

instance Core.FromJSON S3Configuration where
  toJSON S3Configuration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RoleARN" Core..= roleARN),
            Core.Just ("BucketARN" Core..= bucketARN),
            Core.Just ("FileKey" Core..= fileKey)
          ]
      )
