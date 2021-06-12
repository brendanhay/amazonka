{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFindingsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFindingsSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.FindingSeverity
import qualified Network.AWS.Lens as Lens

-- | A summary of the last completed image scan.
--
-- /See:/ 'newImageScanFindingsSummary' smart constructor.
data ImageScanFindingsSummary = ImageScanFindingsSummary'
  { -- | The time of the last completed image scan.
    imageScanCompletedAt :: Core.Maybe Core.POSIX,
    -- | The time when the vulnerability data was last scanned.
    vulnerabilitySourceUpdatedAt :: Core.Maybe Core.POSIX,
    -- | The image vulnerability counts, sorted by severity.
    findingSeverityCounts :: Core.Maybe (Core.HashMap FindingSeverity Core.Natural)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageScanFindingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageScanCompletedAt', 'imageScanFindingsSummary_imageScanCompletedAt' - The time of the last completed image scan.
--
-- 'vulnerabilitySourceUpdatedAt', 'imageScanFindingsSummary_vulnerabilitySourceUpdatedAt' - The time when the vulnerability data was last scanned.
--
-- 'findingSeverityCounts', 'imageScanFindingsSummary_findingSeverityCounts' - The image vulnerability counts, sorted by severity.
newImageScanFindingsSummary ::
  ImageScanFindingsSummary
newImageScanFindingsSummary =
  ImageScanFindingsSummary'
    { imageScanCompletedAt =
        Core.Nothing,
      vulnerabilitySourceUpdatedAt = Core.Nothing,
      findingSeverityCounts = Core.Nothing
    }

-- | The time of the last completed image scan.
imageScanFindingsSummary_imageScanCompletedAt :: Lens.Lens' ImageScanFindingsSummary (Core.Maybe Core.UTCTime)
imageScanFindingsSummary_imageScanCompletedAt = Lens.lens (\ImageScanFindingsSummary' {imageScanCompletedAt} -> imageScanCompletedAt) (\s@ImageScanFindingsSummary' {} a -> s {imageScanCompletedAt = a} :: ImageScanFindingsSummary) Core.. Lens.mapping Core._Time

-- | The time when the vulnerability data was last scanned.
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindingsSummary (Core.Maybe Core.UTCTime)
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt = Lens.lens (\ImageScanFindingsSummary' {vulnerabilitySourceUpdatedAt} -> vulnerabilitySourceUpdatedAt) (\s@ImageScanFindingsSummary' {} a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindingsSummary) Core.. Lens.mapping Core._Time

-- | The image vulnerability counts, sorted by severity.
imageScanFindingsSummary_findingSeverityCounts :: Lens.Lens' ImageScanFindingsSummary (Core.Maybe (Core.HashMap FindingSeverity Core.Natural))
imageScanFindingsSummary_findingSeverityCounts = Lens.lens (\ImageScanFindingsSummary' {findingSeverityCounts} -> findingSeverityCounts) (\s@ImageScanFindingsSummary' {} a -> s {findingSeverityCounts = a} :: ImageScanFindingsSummary) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ImageScanFindingsSummary where
  parseJSON =
    Core.withObject
      "ImageScanFindingsSummary"
      ( \x ->
          ImageScanFindingsSummary'
            Core.<$> (x Core..:? "imageScanCompletedAt")
            Core.<*> (x Core..:? "vulnerabilitySourceUpdatedAt")
            Core.<*> ( x Core..:? "findingSeverityCounts"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable ImageScanFindingsSummary

instance Core.NFData ImageScanFindingsSummary
