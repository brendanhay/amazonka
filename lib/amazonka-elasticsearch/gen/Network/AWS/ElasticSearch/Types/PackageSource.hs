{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageSource
  ( PackageSource (..),

    -- * Smart constructor
    mkPackageSource,

    -- * Lenses
    psS3BucketName,
    psS3Key,
  )
where

import qualified Network.AWS.ElasticSearch.Types.S3BucketName as Types
import qualified Network.AWS.ElasticSearch.Types.S3Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The S3 location for importing the package specified as @S3BucketName@ and @S3Key@
--
-- /See:/ 'mkPackageSource' smart constructor.
data PackageSource = PackageSource'
  { -- | Name of the bucket containing the package.
    s3BucketName :: Core.Maybe Types.S3BucketName,
    -- | Key (file name) of the package.
    s3Key :: Core.Maybe Types.S3Key
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PackageSource' value with any optional fields omitted.
mkPackageSource ::
  PackageSource
mkPackageSource =
  PackageSource' {s3BucketName = Core.Nothing, s3Key = Core.Nothing}

-- | Name of the bucket containing the package.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psS3BucketName :: Lens.Lens' PackageSource (Core.Maybe Types.S3BucketName)
psS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED psS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Key (file name) of the package.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psS3Key :: Lens.Lens' PackageSource (Core.Maybe Types.S3Key)
psS3Key = Lens.field @"s3Key"
{-# DEPRECATED psS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

instance Core.FromJSON PackageSource where
  toJSON PackageSource {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3BucketName" Core..=) Core.<$> s3BucketName,
            ("S3Key" Core..=) Core.<$> s3Key
          ]
      )
