-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFindingsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFindingsSummary
  ( ImageScanFindingsSummary (..),

    -- * Smart constructor
    mkImageScanFindingsSummary,

    -- * Lenses
    isfsImageScanCompletedAt,
    isfsFindingSeverityCounts,
    isfsVulnerabilitySourceUpdatedAt,
  )
where

import Network.AWS.ECR.Types.FindingSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of the last completed image scan.
--
-- /See:/ 'mkImageScanFindingsSummary' smart constructor.
data ImageScanFindingsSummary = ImageScanFindingsSummary'
  { imageScanCompletedAt ::
      Lude.Maybe Lude.Timestamp,
    findingSeverityCounts ::
      Lude.Maybe
        ( Lude.HashMap
            FindingSeverity
            (Lude.Natural)
        ),
    vulnerabilitySourceUpdatedAt ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageScanFindingsSummary' with the minimum fields required to make a request.
--
-- * 'findingSeverityCounts' - The image vulnerability counts, sorted by severity.
-- * 'imageScanCompletedAt' - The time of the last completed image scan.
-- * 'vulnerabilitySourceUpdatedAt' - The time when the vulnerability data was last scanned.
mkImageScanFindingsSummary ::
  ImageScanFindingsSummary
mkImageScanFindingsSummary =
  ImageScanFindingsSummary'
    { imageScanCompletedAt = Lude.Nothing,
      findingSeverityCounts = Lude.Nothing,
      vulnerabilitySourceUpdatedAt = Lude.Nothing
    }

-- | The time of the last completed image scan.
--
-- /Note:/ Consider using 'imageScanCompletedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfsImageScanCompletedAt :: Lens.Lens' ImageScanFindingsSummary (Lude.Maybe Lude.Timestamp)
isfsImageScanCompletedAt = Lens.lens (imageScanCompletedAt :: ImageScanFindingsSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {imageScanCompletedAt = a} :: ImageScanFindingsSummary)
{-# DEPRECATED isfsImageScanCompletedAt "Use generic-lens or generic-optics with 'imageScanCompletedAt' instead." #-}

-- | The image vulnerability counts, sorted by severity.
--
-- /Note:/ Consider using 'findingSeverityCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfsFindingSeverityCounts :: Lens.Lens' ImageScanFindingsSummary (Lude.Maybe (Lude.HashMap FindingSeverity (Lude.Natural)))
isfsFindingSeverityCounts = Lens.lens (findingSeverityCounts :: ImageScanFindingsSummary -> Lude.Maybe (Lude.HashMap FindingSeverity (Lude.Natural))) (\s a -> s {findingSeverityCounts = a} :: ImageScanFindingsSummary)
{-# DEPRECATED isfsFindingSeverityCounts "Use generic-lens or generic-optics with 'findingSeverityCounts' instead." #-}

-- | The time when the vulnerability data was last scanned.
--
-- /Note:/ Consider using 'vulnerabilitySourceUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfsVulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindingsSummary (Lude.Maybe Lude.Timestamp)
isfsVulnerabilitySourceUpdatedAt = Lens.lens (vulnerabilitySourceUpdatedAt :: ImageScanFindingsSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindingsSummary)
{-# DEPRECATED isfsVulnerabilitySourceUpdatedAt "Use generic-lens or generic-optics with 'vulnerabilitySourceUpdatedAt' instead." #-}

instance Lude.FromJSON ImageScanFindingsSummary where
  parseJSON =
    Lude.withObject
      "ImageScanFindingsSummary"
      ( \x ->
          ImageScanFindingsSummary'
            Lude.<$> (x Lude..:? "imageScanCompletedAt")
            Lude.<*> (x Lude..:? "findingSeverityCounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "vulnerabilitySourceUpdatedAt")
      )
