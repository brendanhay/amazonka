{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.ImageIdentifier
  ( ImageIdentifier (..)
  -- * Smart constructor
  , mkImageIdentifier
  -- * Lenses
  , iiImageDigest
  , iiImageTag
  ) where

import qualified Network.AWS.ECR.Types.ImageDigest as Types
import qualified Network.AWS.ECR.Types.ImageTag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object with identifying information for an Amazon ECR image.
--
-- /See:/ 'mkImageIdentifier' smart constructor.
data ImageIdentifier = ImageIdentifier'
  { imageDigest :: Core.Maybe Types.ImageDigest
    -- ^ The @sha256@ digest of the image manifest.
  , imageTag :: Core.Maybe Types.ImageTag
    -- ^ The tag used for the image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageIdentifier' value with any optional fields omitted.
mkImageIdentifier
    :: ImageIdentifier
mkImageIdentifier
  = ImageIdentifier'{imageDigest = Core.Nothing,
                     imageTag = Core.Nothing}

-- | The @sha256@ digest of the image manifest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageDigest :: Lens.Lens' ImageIdentifier (Core.Maybe Types.ImageDigest)
iiImageDigest = Lens.field @"imageDigest"
{-# INLINEABLE iiImageDigest #-}
{-# DEPRECATED imageDigest "Use generic-lens or generic-optics with 'imageDigest' instead"  #-}

-- | The tag used for the image.
--
-- /Note:/ Consider using 'imageTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiImageTag :: Lens.Lens' ImageIdentifier (Core.Maybe Types.ImageTag)
iiImageTag = Lens.field @"imageTag"
{-# INLINEABLE iiImageTag #-}
{-# DEPRECATED imageTag "Use generic-lens or generic-optics with 'imageTag' instead"  #-}

instance Core.FromJSON ImageIdentifier where
        toJSON ImageIdentifier{..}
          = Core.object
              (Core.catMaybes
                 [("imageDigest" Core..=) Core.<$> imageDigest,
                  ("imageTag" Core..=) Core.<$> imageTag])

instance Core.FromJSON ImageIdentifier where
        parseJSON
          = Core.withObject "ImageIdentifier" Core.$
              \ x ->
                ImageIdentifier' Core.<$>
                  (x Core..:? "imageDigest") Core.<*> x Core..:? "imageTag"
