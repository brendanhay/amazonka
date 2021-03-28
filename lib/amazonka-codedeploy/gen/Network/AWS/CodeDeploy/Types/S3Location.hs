{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.S3Location
  ( S3Location (..)
  -- * Smart constructor
  , mkS3Location
  -- * Lenses
  , slBucket
  , slBundleType
  , slETag
  , slKey
  , slVersion
  ) where

import qualified Network.AWS.CodeDeploy.Types.Bucket as Types
import qualified Network.AWS.CodeDeploy.Types.BundleType as Types
import qualified Network.AWS.CodeDeploy.Types.ETag as Types
import qualified Network.AWS.CodeDeploy.Types.S3Key as Types
import qualified Network.AWS.CodeDeploy.Types.VersionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the location of application artifacts stored in Amazon S3.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { bucket :: Core.Maybe Types.Bucket
    -- ^ The name of the Amazon S3 bucket where the application revision is stored.
  , bundleType :: Core.Maybe Types.BundleType
    -- ^ The file type of the application revision. Must be one of the following:
--
--
--     * @tar@ : A tar archive file.
--
--
--     * @tgz@ : A compressed tar archive file.
--
--
--     * @zip@ : A zip archive file.
--
--
  , eTag :: Core.Maybe Types.ETag
    -- ^ The ETag of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of the object is skipped.
  , key :: Core.Maybe Types.S3Key
    -- ^ The name of the Amazon S3 object that represents the bundled artifacts for the application revision.
  , version :: Core.Maybe Types.VersionId
    -- ^ A specific version of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version by default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location
    :: S3Location
mkS3Location
  = S3Location'{bucket = Core.Nothing, bundleType = Core.Nothing,
                eTag = Core.Nothing, key = Core.Nothing, version = Core.Nothing}

-- | The name of the Amazon S3 bucket where the application revision is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Core.Maybe Types.Bucket)
slBucket = Lens.field @"bucket"
{-# INLINEABLE slBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The file type of the application revision. Must be one of the following:
--
--
--     * @tar@ : A tar archive file.
--
--
--     * @tgz@ : A compressed tar archive file.
--
--
--     * @zip@ : A zip archive file.
--
--
--
-- /Note:/ Consider using 'bundleType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBundleType :: Lens.Lens' S3Location (Core.Maybe Types.BundleType)
slBundleType = Lens.field @"bundleType"
{-# INLINEABLE slBundleType #-}
{-# DEPRECATED bundleType "Use generic-lens or generic-optics with 'bundleType' instead"  #-}

-- | The ETag of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of the object is skipped.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slETag :: Lens.Lens' S3Location (Core.Maybe Types.ETag)
slETag = Lens.field @"eTag"
{-# INLINEABLE slETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The name of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Core.Maybe Types.S3Key)
slKey = Lens.field @"key"
{-# INLINEABLE slKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | A specific version of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version by default.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slVersion :: Lens.Lens' S3Location (Core.Maybe Types.VersionId)
slVersion = Lens.field @"version"
{-# INLINEABLE slVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON S3Location where
        toJSON S3Location{..}
          = Core.object
              (Core.catMaybes
                 [("bucket" Core..=) Core.<$> bucket,
                  ("bundleType" Core..=) Core.<$> bundleType,
                  ("eTag" Core..=) Core.<$> eTag, ("key" Core..=) Core.<$> key,
                  ("version" Core..=) Core.<$> version])

instance Core.FromJSON S3Location where
        parseJSON
          = Core.withObject "S3Location" Core.$
              \ x ->
                S3Location' Core.<$>
                  (x Core..:? "bucket") Core.<*> x Core..:? "bundleType" Core.<*>
                    x Core..:? "eTag"
                    Core.<*> x Core..:? "key"
                    Core.<*> x Core..:? "version"
