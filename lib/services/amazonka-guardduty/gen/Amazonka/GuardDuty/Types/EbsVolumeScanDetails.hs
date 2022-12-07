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
-- Module      : Amazonka.GuardDuty.Types.EbsVolumeScanDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.EbsVolumeScanDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ScanDetections
import qualified Amazonka.Prelude as Prelude

-- | Contains details from the malware scan that created a finding.
--
-- /See:/ 'newEbsVolumeScanDetails' smart constructor.
data EbsVolumeScanDetails = EbsVolumeScanDetails'
  { -- | Contains list of threat intelligence sources used to detect threats.
    sources :: Prelude.Maybe [Prelude.Text],
    -- | GuardDuty finding ID that triggered a malware scan.
    triggerFindingId :: Prelude.Maybe Prelude.Text,
    -- | Contains a complete view providing malware scan result details.
    scanDetections :: Prelude.Maybe ScanDetections,
    -- | Unique Id of the malware scan that generated the finding.
    scanId :: Prelude.Maybe Prelude.Text,
    -- | Returns the completion date and time of the malware scan.
    scanCompletedAt :: Prelude.Maybe Data.POSIX,
    -- | Returns the start date and time of the malware scan.
    scanStartedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsVolumeScanDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'ebsVolumeScanDetails_sources' - Contains list of threat intelligence sources used to detect threats.
--
-- 'triggerFindingId', 'ebsVolumeScanDetails_triggerFindingId' - GuardDuty finding ID that triggered a malware scan.
--
-- 'scanDetections', 'ebsVolumeScanDetails_scanDetections' - Contains a complete view providing malware scan result details.
--
-- 'scanId', 'ebsVolumeScanDetails_scanId' - Unique Id of the malware scan that generated the finding.
--
-- 'scanCompletedAt', 'ebsVolumeScanDetails_scanCompletedAt' - Returns the completion date and time of the malware scan.
--
-- 'scanStartedAt', 'ebsVolumeScanDetails_scanStartedAt' - Returns the start date and time of the malware scan.
newEbsVolumeScanDetails ::
  EbsVolumeScanDetails
newEbsVolumeScanDetails =
  EbsVolumeScanDetails'
    { sources = Prelude.Nothing,
      triggerFindingId = Prelude.Nothing,
      scanDetections = Prelude.Nothing,
      scanId = Prelude.Nothing,
      scanCompletedAt = Prelude.Nothing,
      scanStartedAt = Prelude.Nothing
    }

-- | Contains list of threat intelligence sources used to detect threats.
ebsVolumeScanDetails_sources :: Lens.Lens' EbsVolumeScanDetails (Prelude.Maybe [Prelude.Text])
ebsVolumeScanDetails_sources = Lens.lens (\EbsVolumeScanDetails' {sources} -> sources) (\s@EbsVolumeScanDetails' {} a -> s {sources = a} :: EbsVolumeScanDetails) Prelude.. Lens.mapping Lens.coerced

-- | GuardDuty finding ID that triggered a malware scan.
ebsVolumeScanDetails_triggerFindingId :: Lens.Lens' EbsVolumeScanDetails (Prelude.Maybe Prelude.Text)
ebsVolumeScanDetails_triggerFindingId = Lens.lens (\EbsVolumeScanDetails' {triggerFindingId} -> triggerFindingId) (\s@EbsVolumeScanDetails' {} a -> s {triggerFindingId = a} :: EbsVolumeScanDetails)

-- | Contains a complete view providing malware scan result details.
ebsVolumeScanDetails_scanDetections :: Lens.Lens' EbsVolumeScanDetails (Prelude.Maybe ScanDetections)
ebsVolumeScanDetails_scanDetections = Lens.lens (\EbsVolumeScanDetails' {scanDetections} -> scanDetections) (\s@EbsVolumeScanDetails' {} a -> s {scanDetections = a} :: EbsVolumeScanDetails)

-- | Unique Id of the malware scan that generated the finding.
ebsVolumeScanDetails_scanId :: Lens.Lens' EbsVolumeScanDetails (Prelude.Maybe Prelude.Text)
ebsVolumeScanDetails_scanId = Lens.lens (\EbsVolumeScanDetails' {scanId} -> scanId) (\s@EbsVolumeScanDetails' {} a -> s {scanId = a} :: EbsVolumeScanDetails)

-- | Returns the completion date and time of the malware scan.
ebsVolumeScanDetails_scanCompletedAt :: Lens.Lens' EbsVolumeScanDetails (Prelude.Maybe Prelude.UTCTime)
ebsVolumeScanDetails_scanCompletedAt = Lens.lens (\EbsVolumeScanDetails' {scanCompletedAt} -> scanCompletedAt) (\s@EbsVolumeScanDetails' {} a -> s {scanCompletedAt = a} :: EbsVolumeScanDetails) Prelude.. Lens.mapping Data._Time

-- | Returns the start date and time of the malware scan.
ebsVolumeScanDetails_scanStartedAt :: Lens.Lens' EbsVolumeScanDetails (Prelude.Maybe Prelude.UTCTime)
ebsVolumeScanDetails_scanStartedAt = Lens.lens (\EbsVolumeScanDetails' {scanStartedAt} -> scanStartedAt) (\s@EbsVolumeScanDetails' {} a -> s {scanStartedAt = a} :: EbsVolumeScanDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON EbsVolumeScanDetails where
  parseJSON =
    Data.withObject
      "EbsVolumeScanDetails"
      ( \x ->
          EbsVolumeScanDetails'
            Prelude.<$> (x Data..:? "sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "triggerFindingId")
            Prelude.<*> (x Data..:? "scanDetections")
            Prelude.<*> (x Data..:? "scanId")
            Prelude.<*> (x Data..:? "scanCompletedAt")
            Prelude.<*> (x Data..:? "scanStartedAt")
      )

instance Prelude.Hashable EbsVolumeScanDetails where
  hashWithSalt _salt EbsVolumeScanDetails' {..} =
    _salt `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` triggerFindingId
      `Prelude.hashWithSalt` scanDetections
      `Prelude.hashWithSalt` scanId
      `Prelude.hashWithSalt` scanCompletedAt
      `Prelude.hashWithSalt` scanStartedAt

instance Prelude.NFData EbsVolumeScanDetails where
  rnf EbsVolumeScanDetails' {..} =
    Prelude.rnf sources
      `Prelude.seq` Prelude.rnf triggerFindingId
      `Prelude.seq` Prelude.rnf scanDetections
      `Prelude.seq` Prelude.rnf scanId
      `Prelude.seq` Prelude.rnf scanCompletedAt
      `Prelude.seq` Prelude.rnf scanStartedAt
