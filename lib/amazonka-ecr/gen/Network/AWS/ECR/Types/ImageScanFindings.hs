{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFindings
  ( ImageScanFindings (..),

    -- * Smart constructor
    mkImageScanFindings,

    -- * Lenses
    isfImageScanCompletedAt,
    isfFindings,
    isfFindingSeverityCounts,
    isfVulnerabilitySourceUpdatedAt,
  )
where

import Network.AWS.ECR.Types.FindingSeverity
import Network.AWS.ECR.Types.ImageScanFinding
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of an image scan.
--
-- /See:/ 'mkImageScanFindings' smart constructor.
data ImageScanFindings = ImageScanFindings'
  { imageScanCompletedAt ::
      Lude.Maybe Lude.Timestamp,
    findings :: Lude.Maybe [ImageScanFinding],
    findingSeverityCounts ::
      Lude.Maybe
        (Lude.HashMap FindingSeverity (Lude.Natural)),
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

-- | Creates a value of 'ImageScanFindings' with the minimum fields required to make a request.
--
-- * 'findingSeverityCounts' - The image vulnerability counts, sorted by severity.
-- * 'findings' - The findings from the image scan.
-- * 'imageScanCompletedAt' - The time of the last completed image scan.
-- * 'vulnerabilitySourceUpdatedAt' - The time when the vulnerability data was last scanned.
mkImageScanFindings ::
  ImageScanFindings
mkImageScanFindings =
  ImageScanFindings'
    { imageScanCompletedAt = Lude.Nothing,
      findings = Lude.Nothing,
      findingSeverityCounts = Lude.Nothing,
      vulnerabilitySourceUpdatedAt = Lude.Nothing
    }

-- | The time of the last completed image scan.
--
-- /Note:/ Consider using 'imageScanCompletedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfImageScanCompletedAt :: Lens.Lens' ImageScanFindings (Lude.Maybe Lude.Timestamp)
isfImageScanCompletedAt = Lens.lens (imageScanCompletedAt :: ImageScanFindings -> Lude.Maybe Lude.Timestamp) (\s a -> s {imageScanCompletedAt = a} :: ImageScanFindings)
{-# DEPRECATED isfImageScanCompletedAt "Use generic-lens or generic-optics with 'imageScanCompletedAt' instead." #-}

-- | The findings from the image scan.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfFindings :: Lens.Lens' ImageScanFindings (Lude.Maybe [ImageScanFinding])
isfFindings = Lens.lens (findings :: ImageScanFindings -> Lude.Maybe [ImageScanFinding]) (\s a -> s {findings = a} :: ImageScanFindings)
{-# DEPRECATED isfFindings "Use generic-lens or generic-optics with 'findings' instead." #-}

-- | The image vulnerability counts, sorted by severity.
--
-- /Note:/ Consider using 'findingSeverityCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfFindingSeverityCounts :: Lens.Lens' ImageScanFindings (Lude.Maybe (Lude.HashMap FindingSeverity (Lude.Natural)))
isfFindingSeverityCounts = Lens.lens (findingSeverityCounts :: ImageScanFindings -> Lude.Maybe (Lude.HashMap FindingSeverity (Lude.Natural))) (\s a -> s {findingSeverityCounts = a} :: ImageScanFindings)
{-# DEPRECATED isfFindingSeverityCounts "Use generic-lens or generic-optics with 'findingSeverityCounts' instead." #-}

-- | The time when the vulnerability data was last scanned.
--
-- /Note:/ Consider using 'vulnerabilitySourceUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfVulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindings (Lude.Maybe Lude.Timestamp)
isfVulnerabilitySourceUpdatedAt = Lens.lens (vulnerabilitySourceUpdatedAt :: ImageScanFindings -> Lude.Maybe Lude.Timestamp) (\s a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindings)
{-# DEPRECATED isfVulnerabilitySourceUpdatedAt "Use generic-lens or generic-optics with 'vulnerabilitySourceUpdatedAt' instead." #-}

instance Lude.FromJSON ImageScanFindings where
  parseJSON =
    Lude.withObject
      "ImageScanFindings"
      ( \x ->
          ImageScanFindings'
            Lude.<$> (x Lude..:? "imageScanCompletedAt")
            Lude.<*> (x Lude..:? "findings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "findingSeverityCounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "vulnerabilitySourceUpdatedAt")
      )
