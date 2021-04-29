{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECR.Types.FindingSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of the last completed image scan.
--
-- /See:/ 'newImageScanFindingsSummary' smart constructor.
data ImageScanFindingsSummary = ImageScanFindingsSummary'
  { -- | The time of the last completed image scan.
    imageScanCompletedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The time when the vulnerability data was last scanned.
    vulnerabilitySourceUpdatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The image vulnerability counts, sorted by severity.
    findingSeverityCounts :: Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      vulnerabilitySourceUpdatedAt = Prelude.Nothing,
      findingSeverityCounts = Prelude.Nothing
    }

-- | The time of the last completed image scan.
imageScanFindingsSummary_imageScanCompletedAt :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe Prelude.UTCTime)
imageScanFindingsSummary_imageScanCompletedAt = Lens.lens (\ImageScanFindingsSummary' {imageScanCompletedAt} -> imageScanCompletedAt) (\s@ImageScanFindingsSummary' {} a -> s {imageScanCompletedAt = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Prelude._Time

-- | The time when the vulnerability data was last scanned.
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe Prelude.UTCTime)
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt = Lens.lens (\ImageScanFindingsSummary' {vulnerabilitySourceUpdatedAt} -> vulnerabilitySourceUpdatedAt) (\s@ImageScanFindingsSummary' {} a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Prelude._Time

-- | The image vulnerability counts, sorted by severity.
imageScanFindingsSummary_findingSeverityCounts :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural))
imageScanFindingsSummary_findingSeverityCounts = Lens.lens (\ImageScanFindingsSummary' {findingSeverityCounts} -> findingSeverityCounts) (\s@ImageScanFindingsSummary' {} a -> s {findingSeverityCounts = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ImageScanFindingsSummary where
  parseJSON =
    Prelude.withObject
      "ImageScanFindingsSummary"
      ( \x ->
          ImageScanFindingsSummary'
            Prelude.<$> (x Prelude..:? "imageScanCompletedAt")
            Prelude.<*> (x Prelude..:? "vulnerabilitySourceUpdatedAt")
            Prelude.<*> ( x Prelude..:? "findingSeverityCounts"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ImageScanFindingsSummary

instance Prelude.NFData ImageScanFindingsSummary
