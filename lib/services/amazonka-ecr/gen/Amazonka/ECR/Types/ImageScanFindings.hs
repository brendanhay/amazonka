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
-- Module      : Amazonka.ECR.Types.ImageScanFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageScanFindings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.EnhancedImageScanFinding
import Amazonka.ECR.Types.FindingSeverity
import Amazonka.ECR.Types.ImageScanFinding
import qualified Amazonka.Prelude as Prelude

-- | The details of an image scan.
--
-- /See:/ 'newImageScanFindings' smart constructor.
data ImageScanFindings = ImageScanFindings'
  { -- | Details about the enhanced scan findings from Amazon Inspector.
    enhancedFindings :: Prelude.Maybe [EnhancedImageScanFinding],
    -- | The image vulnerability counts, sorted by severity.
    findingSeverityCounts :: Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural),
    -- | The findings from the image scan.
    findings :: Prelude.Maybe [ImageScanFinding],
    -- | The time of the last completed image scan.
    imageScanCompletedAt :: Prelude.Maybe Data.POSIX,
    -- | The time when the vulnerability data was last scanned.
    vulnerabilitySourceUpdatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enhancedFindings', 'imageScanFindings_enhancedFindings' - Details about the enhanced scan findings from Amazon Inspector.
--
-- 'findingSeverityCounts', 'imageScanFindings_findingSeverityCounts' - The image vulnerability counts, sorted by severity.
--
-- 'findings', 'imageScanFindings_findings' - The findings from the image scan.
--
-- 'imageScanCompletedAt', 'imageScanFindings_imageScanCompletedAt' - The time of the last completed image scan.
--
-- 'vulnerabilitySourceUpdatedAt', 'imageScanFindings_vulnerabilitySourceUpdatedAt' - The time when the vulnerability data was last scanned.
newImageScanFindings ::
  ImageScanFindings
newImageScanFindings =
  ImageScanFindings'
    { enhancedFindings =
        Prelude.Nothing,
      findingSeverityCounts = Prelude.Nothing,
      findings = Prelude.Nothing,
      imageScanCompletedAt = Prelude.Nothing,
      vulnerabilitySourceUpdatedAt = Prelude.Nothing
    }

-- | Details about the enhanced scan findings from Amazon Inspector.
imageScanFindings_enhancedFindings :: Lens.Lens' ImageScanFindings (Prelude.Maybe [EnhancedImageScanFinding])
imageScanFindings_enhancedFindings = Lens.lens (\ImageScanFindings' {enhancedFindings} -> enhancedFindings) (\s@ImageScanFindings' {} a -> s {enhancedFindings = a} :: ImageScanFindings) Prelude.. Lens.mapping Lens.coerced

-- | The image vulnerability counts, sorted by severity.
imageScanFindings_findingSeverityCounts :: Lens.Lens' ImageScanFindings (Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural))
imageScanFindings_findingSeverityCounts = Lens.lens (\ImageScanFindings' {findingSeverityCounts} -> findingSeverityCounts) (\s@ImageScanFindings' {} a -> s {findingSeverityCounts = a} :: ImageScanFindings) Prelude.. Lens.mapping Lens.coerced

-- | The findings from the image scan.
imageScanFindings_findings :: Lens.Lens' ImageScanFindings (Prelude.Maybe [ImageScanFinding])
imageScanFindings_findings = Lens.lens (\ImageScanFindings' {findings} -> findings) (\s@ImageScanFindings' {} a -> s {findings = a} :: ImageScanFindings) Prelude.. Lens.mapping Lens.coerced

-- | The time of the last completed image scan.
imageScanFindings_imageScanCompletedAt :: Lens.Lens' ImageScanFindings (Prelude.Maybe Prelude.UTCTime)
imageScanFindings_imageScanCompletedAt = Lens.lens (\ImageScanFindings' {imageScanCompletedAt} -> imageScanCompletedAt) (\s@ImageScanFindings' {} a -> s {imageScanCompletedAt = a} :: ImageScanFindings) Prelude.. Lens.mapping Data._Time

-- | The time when the vulnerability data was last scanned.
imageScanFindings_vulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindings (Prelude.Maybe Prelude.UTCTime)
imageScanFindings_vulnerabilitySourceUpdatedAt = Lens.lens (\ImageScanFindings' {vulnerabilitySourceUpdatedAt} -> vulnerabilitySourceUpdatedAt) (\s@ImageScanFindings' {} a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindings) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ImageScanFindings where
  parseJSON =
    Data.withObject
      "ImageScanFindings"
      ( \x ->
          ImageScanFindings'
            Prelude.<$> ( x Data..:? "enhancedFindings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "findingSeverityCounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "findings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "imageScanCompletedAt")
            Prelude.<*> (x Data..:? "vulnerabilitySourceUpdatedAt")
      )

instance Prelude.Hashable ImageScanFindings where
  hashWithSalt _salt ImageScanFindings' {..} =
    _salt `Prelude.hashWithSalt` enhancedFindings
      `Prelude.hashWithSalt` findingSeverityCounts
      `Prelude.hashWithSalt` findings
      `Prelude.hashWithSalt` imageScanCompletedAt
      `Prelude.hashWithSalt` vulnerabilitySourceUpdatedAt

instance Prelude.NFData ImageScanFindings where
  rnf ImageScanFindings' {..} =
    Prelude.rnf enhancedFindings
      `Prelude.seq` Prelude.rnf findingSeverityCounts
      `Prelude.seq` Prelude.rnf findings
      `Prelude.seq` Prelude.rnf imageScanCompletedAt
      `Prelude.seq` Prelude.rnf vulnerabilitySourceUpdatedAt
