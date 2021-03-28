{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.ImageScanningConfiguration
  ( ImageScanningConfiguration (..)
  -- * Smart constructor
  , mkImageScanningConfiguration
  -- * Lenses
  , iscScanOnPush
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The image scanning configuration for a repository.
--
-- /See:/ 'mkImageScanningConfiguration' smart constructor.
newtype ImageScanningConfiguration = ImageScanningConfiguration'
  { scanOnPush :: Core.Maybe Core.Bool
    -- ^ The setting that determines whether images are scanned after being pushed to a repository. If set to @true@ , images will be scanned after being pushed. If this parameter is not specified, it will default to @false@ and images will not be scanned unless a scan is manually started with the 'StartImageScan' API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImageScanningConfiguration' value with any optional fields omitted.
mkImageScanningConfiguration
    :: ImageScanningConfiguration
mkImageScanningConfiguration
  = ImageScanningConfiguration'{scanOnPush = Core.Nothing}

-- | The setting that determines whether images are scanned after being pushed to a repository. If set to @true@ , images will be scanned after being pushed. If this parameter is not specified, it will default to @false@ and images will not be scanned unless a scan is manually started with the 'StartImageScan' API.
--
-- /Note:/ Consider using 'scanOnPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscScanOnPush :: Lens.Lens' ImageScanningConfiguration (Core.Maybe Core.Bool)
iscScanOnPush = Lens.field @"scanOnPush"
{-# INLINEABLE iscScanOnPush #-}
{-# DEPRECATED scanOnPush "Use generic-lens or generic-optics with 'scanOnPush' instead"  #-}

instance Core.FromJSON ImageScanningConfiguration where
        toJSON ImageScanningConfiguration{..}
          = Core.object
              (Core.catMaybes [("scanOnPush" Core..=) Core.<$> scanOnPush])

instance Core.FromJSON ImageScanningConfiguration where
        parseJSON
          = Core.withObject "ImageScanningConfiguration" Core.$
              \ x ->
                ImageScanningConfiguration' Core.<$> (x Core..:? "scanOnPush")
