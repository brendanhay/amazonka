{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanStatus
  ( ImageScanStatus (..),

    -- * Smart constructor
    mkImageScanStatus,

    -- * Lenses
    issDescription,
    issStatus,
  )
where

import qualified Network.AWS.ECR.Types.ScanStatus as Types
import qualified Network.AWS.ECR.Types.ScanStatusDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current status of an image scan.
--
-- /See:/ 'mkImageScanStatus' smart constructor.
data ImageScanStatus = ImageScanStatus'
  { -- | The description of the image scan status.
    description :: Core.Maybe Types.ScanStatusDescription,
    -- | The current state of an image scan.
    status :: Core.Maybe Types.ScanStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageScanStatus' value with any optional fields omitted.
mkImageScanStatus ::
  ImageScanStatus
mkImageScanStatus =
  ImageScanStatus'
    { description = Core.Nothing,
      status = Core.Nothing
    }

-- | The description of the image scan status.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issDescription :: Lens.Lens' ImageScanStatus (Core.Maybe Types.ScanStatusDescription)
issDescription = Lens.field @"description"
{-# DEPRECATED issDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The current state of an image scan.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issStatus :: Lens.Lens' ImageScanStatus (Core.Maybe Types.ScanStatus)
issStatus = Lens.field @"status"
{-# DEPRECATED issStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ImageScanStatus where
  parseJSON =
    Core.withObject "ImageScanStatus" Core.$
      \x ->
        ImageScanStatus'
          Core.<$> (x Core..:? "description") Core.<*> (x Core..:? "status")
