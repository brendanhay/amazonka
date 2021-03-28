{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.ImageScanFindings
  ( ImageScanFindings (..)
  -- * Smart constructor
  , mkImageScanFindings
  -- * Lenses
  , isfFindingSeverityCounts
  , isfFindings
  , isfImageScanCompletedAt
  , isfVulnerabilitySourceUpdatedAt
  ) where

import qualified Network.AWS.ECR.Types.FindingSeverity as Types
import qualified Network.AWS.ECR.Types.ImageScanFinding as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of an image scan.
--
-- /See:/ 'mkImageScanFindings' smart constructor.
data ImageScanFindings = ImageScanFindings'
  { findingSeverityCounts :: Core.Maybe (Core.HashMap Types.FindingSeverity Core.Natural)
    -- ^ The image vulnerability counts, sorted by severity.
  , findings :: Core.Maybe [Types.ImageScanFinding]
    -- ^ The findings from the image scan.
  , imageScanCompletedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the last completed image scan.
  , vulnerabilitySourceUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the vulnerability data was last scanned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImageScanFindings' value with any optional fields omitted.
mkImageScanFindings
    :: ImageScanFindings
mkImageScanFindings
  = ImageScanFindings'{findingSeverityCounts = Core.Nothing,
                       findings = Core.Nothing, imageScanCompletedAt = Core.Nothing,
                       vulnerabilitySourceUpdatedAt = Core.Nothing}

-- | The image vulnerability counts, sorted by severity.
--
-- /Note:/ Consider using 'findingSeverityCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfFindingSeverityCounts :: Lens.Lens' ImageScanFindings (Core.Maybe (Core.HashMap Types.FindingSeverity Core.Natural))
isfFindingSeverityCounts = Lens.field @"findingSeverityCounts"
{-# INLINEABLE isfFindingSeverityCounts #-}
{-# DEPRECATED findingSeverityCounts "Use generic-lens or generic-optics with 'findingSeverityCounts' instead"  #-}

-- | The findings from the image scan.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfFindings :: Lens.Lens' ImageScanFindings (Core.Maybe [Types.ImageScanFinding])
isfFindings = Lens.field @"findings"
{-# INLINEABLE isfFindings #-}
{-# DEPRECATED findings "Use generic-lens or generic-optics with 'findings' instead"  #-}

-- | The time of the last completed image scan.
--
-- /Note:/ Consider using 'imageScanCompletedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfImageScanCompletedAt :: Lens.Lens' ImageScanFindings (Core.Maybe Core.NominalDiffTime)
isfImageScanCompletedAt = Lens.field @"imageScanCompletedAt"
{-# INLINEABLE isfImageScanCompletedAt #-}
{-# DEPRECATED imageScanCompletedAt "Use generic-lens or generic-optics with 'imageScanCompletedAt' instead"  #-}

-- | The time when the vulnerability data was last scanned.
--
-- /Note:/ Consider using 'vulnerabilitySourceUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfVulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindings (Core.Maybe Core.NominalDiffTime)
isfVulnerabilitySourceUpdatedAt = Lens.field @"vulnerabilitySourceUpdatedAt"
{-# INLINEABLE isfVulnerabilitySourceUpdatedAt #-}
{-# DEPRECATED vulnerabilitySourceUpdatedAt "Use generic-lens or generic-optics with 'vulnerabilitySourceUpdatedAt' instead"  #-}

instance Core.FromJSON ImageScanFindings where
        parseJSON
          = Core.withObject "ImageScanFindings" Core.$
              \ x ->
                ImageScanFindings' Core.<$>
                  (x Core..:? "findingSeverityCounts") Core.<*> x Core..:? "findings"
                    Core.<*> x Core..:? "imageScanCompletedAt"
                    Core.<*> x Core..:? "vulnerabilitySourceUpdatedAt"
