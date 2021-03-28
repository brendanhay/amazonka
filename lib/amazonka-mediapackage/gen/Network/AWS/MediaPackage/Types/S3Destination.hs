{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.S3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.S3Destination
  ( S3Destination (..)
  -- * Smart constructor
  , mkS3Destination
  -- * Lenses
  , sdManifestKey
  , sdBucketName
  , sdRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration parameters for where in an S3 bucket to place the harvested content
--
-- /See:/ 'mkS3Destination' smart constructor.
data S3Destination = S3Destination'
  { manifestKey :: Core.Text
    -- ^ The key in the specified S3 bucket where the harvested top-level manifest will be placed.
  , bucketName :: Core.Text
    -- ^ The name of an S3 bucket within which harvested content will be exported
  , roleArn :: Core.Text
    -- ^ The IAM role used to write to the specified S3 bucket
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Destination' value with any optional fields omitted.
mkS3Destination
    :: Core.Text -- ^ 'manifestKey'
    -> Core.Text -- ^ 'bucketName'
    -> Core.Text -- ^ 'roleArn'
    -> S3Destination
mkS3Destination manifestKey bucketName roleArn
  = S3Destination'{manifestKey, bucketName, roleArn}

-- | The key in the specified S3 bucket where the harvested top-level manifest will be placed.
--
-- /Note:/ Consider using 'manifestKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdManifestKey :: Lens.Lens' S3Destination Core.Text
sdManifestKey = Lens.field @"manifestKey"
{-# INLINEABLE sdManifestKey #-}
{-# DEPRECATED manifestKey "Use generic-lens or generic-optics with 'manifestKey' instead"  #-}

-- | The name of an S3 bucket within which harvested content will be exported
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBucketName :: Lens.Lens' S3Destination Core.Text
sdBucketName = Lens.field @"bucketName"
{-# INLINEABLE sdBucketName #-}
{-# DEPRECATED bucketName "Use generic-lens or generic-optics with 'bucketName' instead"  #-}

-- | The IAM role used to write to the specified S3 bucket
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdRoleArn :: Lens.Lens' S3Destination Core.Text
sdRoleArn = Lens.field @"roleArn"
{-# INLINEABLE sdRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON S3Destination where
        toJSON S3Destination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("manifestKey" Core..= manifestKey),
                  Core.Just ("bucketName" Core..= bucketName),
                  Core.Just ("roleArn" Core..= roleArn)])

instance Core.FromJSON S3Destination where
        parseJSON
          = Core.withObject "S3Destination" Core.$
              \ x ->
                S3Destination' Core.<$>
                  (x Core..: "manifestKey") Core.<*> x Core..: "bucketName" Core.<*>
                    x Core..: "roleArn"
