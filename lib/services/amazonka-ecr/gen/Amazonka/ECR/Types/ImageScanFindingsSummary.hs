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
-- Module      : Amazonka.ECR.Types.ImageScanFindingsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageScanFindingsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.FindingSeverity
import qualified Amazonka.Prelude as Prelude

-- | A summary of the last completed image scan.
--
-- /See:/ 'newImageScanFindingsSummary' smart constructor.
data ImageScanFindingsSummary = ImageScanFindingsSummary'
  { -- | The image vulnerability counts, sorted by severity.
    findingSeverityCounts :: Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural),
    -- | The time of the last completed image scan.
    imageScanCompletedAt :: Prelude.Maybe Data.POSIX,
    -- | The time when the vulnerability data was last scanned.
    vulnerabilitySourceUpdatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanFindingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingSeverityCounts', 'imageScanFindingsSummary_findingSeverityCounts' - The image vulnerability counts, sorted by severity.
--
-- 'imageScanCompletedAt', 'imageScanFindingsSummary_imageScanCompletedAt' - The time of the last completed image scan.
--
-- 'vulnerabilitySourceUpdatedAt', 'imageScanFindingsSummary_vulnerabilitySourceUpdatedAt' - The time when the vulnerability data was last scanned.
newImageScanFindingsSummary ::
  ImageScanFindingsSummary
newImageScanFindingsSummary =
  ImageScanFindingsSummary'
    { findingSeverityCounts =
        Prelude.Nothing,
      imageScanCompletedAt = Prelude.Nothing,
      vulnerabilitySourceUpdatedAt = Prelude.Nothing
    }

-- | The image vulnerability counts, sorted by severity.
imageScanFindingsSummary_findingSeverityCounts :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural))
imageScanFindingsSummary_findingSeverityCounts = Lens.lens (\ImageScanFindingsSummary' {findingSeverityCounts} -> findingSeverityCounts) (\s@ImageScanFindingsSummary' {} a -> s {findingSeverityCounts = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time of the last completed image scan.
imageScanFindingsSummary_imageScanCompletedAt :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe Prelude.UTCTime)
imageScanFindingsSummary_imageScanCompletedAt = Lens.lens (\ImageScanFindingsSummary' {imageScanCompletedAt} -> imageScanCompletedAt) (\s@ImageScanFindingsSummary' {} a -> s {imageScanCompletedAt = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Data._Time

-- | The time when the vulnerability data was last scanned.
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe Prelude.UTCTime)
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt = Lens.lens (\ImageScanFindingsSummary' {vulnerabilitySourceUpdatedAt} -> vulnerabilitySourceUpdatedAt) (\s@ImageScanFindingsSummary' {} a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ImageScanFindingsSummary where
  parseJSON =
    Data.withObject
      "ImageScanFindingsSummary"
      ( \x ->
          ImageScanFindingsSummary'
            Prelude.<$> ( x Data..:? "findingSeverityCounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "imageScanCompletedAt")
            Prelude.<*> (x Data..:? "vulnerabilitySourceUpdatedAt")
      )

instance Prelude.Hashable ImageScanFindingsSummary where
  hashWithSalt _salt ImageScanFindingsSummary' {..} =
    _salt `Prelude.hashWithSalt` findingSeverityCounts
      `Prelude.hashWithSalt` imageScanCompletedAt
      `Prelude.hashWithSalt` vulnerabilitySourceUpdatedAt

instance Prelude.NFData ImageScanFindingsSummary where
  rnf ImageScanFindingsSummary' {..} =
    Prelude.rnf findingSeverityCounts
      `Prelude.seq` Prelude.rnf imageScanCompletedAt
      `Prelude.seq` Prelude.rnf vulnerabilitySourceUpdatedAt
