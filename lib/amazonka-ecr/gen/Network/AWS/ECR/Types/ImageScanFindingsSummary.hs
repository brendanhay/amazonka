{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFindingsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.ImageScanFindingsSummary
  ( ImageScanFindingsSummary (..)
  -- * Smart constructor
  , mkImageScanFindingsSummary
  -- * Lenses
  , isfsFindingSeverityCounts
  , isfsImageScanCompletedAt
  , isfsVulnerabilitySourceUpdatedAt
  ) where

import qualified Network.AWS.ECR.Types.FindingSeverity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of the last completed image scan.
--
-- /See:/ 'mkImageScanFindingsSummary' smart constructor.
data ImageScanFindingsSummary = ImageScanFindingsSummary'
  { findingSeverityCounts :: Core.Maybe (Core.HashMap Types.FindingSeverity Core.Natural)
    -- ^ The image vulnerability counts, sorted by severity.
  , imageScanCompletedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the last completed image scan.
  , vulnerabilitySourceUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the vulnerability data was last scanned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImageScanFindingsSummary' value with any optional fields omitted.
mkImageScanFindingsSummary
    :: ImageScanFindingsSummary
mkImageScanFindingsSummary
  = ImageScanFindingsSummary'{findingSeverityCounts = Core.Nothing,
                              imageScanCompletedAt = Core.Nothing,
                              vulnerabilitySourceUpdatedAt = Core.Nothing}

-- | The image vulnerability counts, sorted by severity.
--
-- /Note:/ Consider using 'findingSeverityCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfsFindingSeverityCounts :: Lens.Lens' ImageScanFindingsSummary (Core.Maybe (Core.HashMap Types.FindingSeverity Core.Natural))
isfsFindingSeverityCounts = Lens.field @"findingSeverityCounts"
{-# INLINEABLE isfsFindingSeverityCounts #-}
{-# DEPRECATED findingSeverityCounts "Use generic-lens or generic-optics with 'findingSeverityCounts' instead"  #-}

-- | The time of the last completed image scan.
--
-- /Note:/ Consider using 'imageScanCompletedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfsImageScanCompletedAt :: Lens.Lens' ImageScanFindingsSummary (Core.Maybe Core.NominalDiffTime)
isfsImageScanCompletedAt = Lens.field @"imageScanCompletedAt"
{-# INLINEABLE isfsImageScanCompletedAt #-}
{-# DEPRECATED imageScanCompletedAt "Use generic-lens or generic-optics with 'imageScanCompletedAt' instead"  #-}

-- | The time when the vulnerability data was last scanned.
--
-- /Note:/ Consider using 'vulnerabilitySourceUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfsVulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindingsSummary (Core.Maybe Core.NominalDiffTime)
isfsVulnerabilitySourceUpdatedAt = Lens.field @"vulnerabilitySourceUpdatedAt"
{-# INLINEABLE isfsVulnerabilitySourceUpdatedAt #-}
{-# DEPRECATED vulnerabilitySourceUpdatedAt "Use generic-lens or generic-optics with 'vulnerabilitySourceUpdatedAt' instead"  #-}

instance Core.FromJSON ImageScanFindingsSummary where
        parseJSON
          = Core.withObject "ImageScanFindingsSummary" Core.$
              \ x ->
                ImageScanFindingsSummary' Core.<$>
                  (x Core..:? "findingSeverityCounts") Core.<*>
                    x Core..:? "imageScanCompletedAt"
                    Core.<*> x Core..:? "vulnerabilitySourceUpdatedAt"
