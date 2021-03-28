{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.S3ArtifactLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.S3ArtifactLocation
  ( S3ArtifactLocation (..)
  -- * Smart constructor
  , mkS3ArtifactLocation
  -- * Lenses
  , salBucketName
  , salObjectKey
  ) where

import qualified Network.AWS.CodePipeline.Types.BucketName as Types
import qualified Network.AWS.CodePipeline.Types.S3ObjectKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location of the S3 bucket that contains a revision.
--
-- /See:/ 'mkS3ArtifactLocation' smart constructor.
data S3ArtifactLocation = S3ArtifactLocation'
  { bucketName :: Types.BucketName
    -- ^ The name of the S3 bucket.
  , objectKey :: Types.S3ObjectKey
    -- ^ The key of the object in the S3 bucket, which uniquely identifies the object in the bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3ArtifactLocation' value with any optional fields omitted.
mkS3ArtifactLocation
    :: Types.BucketName -- ^ 'bucketName'
    -> Types.S3ObjectKey -- ^ 'objectKey'
    -> S3ArtifactLocation
mkS3ArtifactLocation bucketName objectKey
  = S3ArtifactLocation'{bucketName, objectKey}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salBucketName :: Lens.Lens' S3ArtifactLocation Types.BucketName
salBucketName = Lens.field @"bucketName"
{-# INLINEABLE salBucketName #-}
{-# DEPRECATED bucketName "Use generic-lens or generic-optics with 'bucketName' instead"  #-}

-- | The key of the object in the S3 bucket, which uniquely identifies the object in the bucket.
--
-- /Note:/ Consider using 'objectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salObjectKey :: Lens.Lens' S3ArtifactLocation Types.S3ObjectKey
salObjectKey = Lens.field @"objectKey"
{-# INLINEABLE salObjectKey #-}
{-# DEPRECATED objectKey "Use generic-lens or generic-optics with 'objectKey' instead"  #-}

instance Core.FromJSON S3ArtifactLocation where
        parseJSON
          = Core.withObject "S3ArtifactLocation" Core.$
              \ x ->
                S3ArtifactLocation' Core.<$>
                  (x Core..: "bucketName") Core.<*> x Core..: "objectKey"
