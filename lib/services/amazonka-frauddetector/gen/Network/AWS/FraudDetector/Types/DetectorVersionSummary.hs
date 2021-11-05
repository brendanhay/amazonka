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
-- Module      : Amazonka.FraudDetector.Types.DetectorVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.DetectorVersionSummary where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.DetectorVersionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of the detector version.
--
-- /See:/ 'newDetectorVersionSummary' smart constructor.
data DetectorVersionSummary = DetectorVersionSummary'
  { -- | The detector version status.
    status :: Prelude.Maybe DetectorVersionStatus,
    -- | Timestamp of when the detector version was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Maybe Prelude.Text,
    -- | The detector version description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'detectorVersionSummary_status' - The detector version status.
--
-- 'lastUpdatedTime', 'detectorVersionSummary_lastUpdatedTime' - Timestamp of when the detector version was last updated.
--
-- 'detectorVersionId', 'detectorVersionSummary_detectorVersionId' - The detector version ID.
--
-- 'description', 'detectorVersionSummary_description' - The detector version description.
newDetectorVersionSummary ::
  DetectorVersionSummary
newDetectorVersionSummary =
  DetectorVersionSummary'
    { status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      detectorVersionId = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The detector version status.
detectorVersionSummary_status :: Lens.Lens' DetectorVersionSummary (Prelude.Maybe DetectorVersionStatus)
detectorVersionSummary_status = Lens.lens (\DetectorVersionSummary' {status} -> status) (\s@DetectorVersionSummary' {} a -> s {status = a} :: DetectorVersionSummary)

-- | Timestamp of when the detector version was last updated.
detectorVersionSummary_lastUpdatedTime :: Lens.Lens' DetectorVersionSummary (Prelude.Maybe Prelude.Text)
detectorVersionSummary_lastUpdatedTime = Lens.lens (\DetectorVersionSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@DetectorVersionSummary' {} a -> s {lastUpdatedTime = a} :: DetectorVersionSummary)

-- | The detector version ID.
detectorVersionSummary_detectorVersionId :: Lens.Lens' DetectorVersionSummary (Prelude.Maybe Prelude.Text)
detectorVersionSummary_detectorVersionId = Lens.lens (\DetectorVersionSummary' {detectorVersionId} -> detectorVersionId) (\s@DetectorVersionSummary' {} a -> s {detectorVersionId = a} :: DetectorVersionSummary)

-- | The detector version description.
detectorVersionSummary_description :: Lens.Lens' DetectorVersionSummary (Prelude.Maybe Prelude.Text)
detectorVersionSummary_description = Lens.lens (\DetectorVersionSummary' {description} -> description) (\s@DetectorVersionSummary' {} a -> s {description = a} :: DetectorVersionSummary)

instance Core.FromJSON DetectorVersionSummary where
  parseJSON =
    Core.withObject
      "DetectorVersionSummary"
      ( \x ->
          DetectorVersionSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "detectorVersionId")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable DetectorVersionSummary

instance Prelude.NFData DetectorVersionSummary
