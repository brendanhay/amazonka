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
-- Module      : Amazonka.GuardDuty.Types.ScanDetections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanDetections where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.HighestSeverityThreatDetails
import Amazonka.GuardDuty.Types.ScannedItemCount
import Amazonka.GuardDuty.Types.ThreatDetectedByName
import Amazonka.GuardDuty.Types.ThreatsDetectedItemCount
import qualified Amazonka.Prelude as Prelude

-- | Contains a complete view providing malware scan result details.
--
-- /See:/ 'newScanDetections' smart constructor.
data ScanDetections = ScanDetections'
  { -- | Details of the highest severity threat detected during malware scan and
    -- number of infected files.
    highestSeverityThreatDetails :: Prelude.Maybe HighestSeverityThreatDetails,
    -- | Total number of scanned files.
    scannedItemCount :: Prelude.Maybe ScannedItemCount,
    -- | Contains details about identified threats organized by threat name.
    threatDetectedByName :: Prelude.Maybe ThreatDetectedByName,
    -- | Total number of infected files.
    threatsDetectedItemCount :: Prelude.Maybe ThreatsDetectedItemCount
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanDetections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'highestSeverityThreatDetails', 'scanDetections_highestSeverityThreatDetails' - Details of the highest severity threat detected during malware scan and
-- number of infected files.
--
-- 'scannedItemCount', 'scanDetections_scannedItemCount' - Total number of scanned files.
--
-- 'threatDetectedByName', 'scanDetections_threatDetectedByName' - Contains details about identified threats organized by threat name.
--
-- 'threatsDetectedItemCount', 'scanDetections_threatsDetectedItemCount' - Total number of infected files.
newScanDetections ::
  ScanDetections
newScanDetections =
  ScanDetections'
    { highestSeverityThreatDetails =
        Prelude.Nothing,
      scannedItemCount = Prelude.Nothing,
      threatDetectedByName = Prelude.Nothing,
      threatsDetectedItemCount = Prelude.Nothing
    }

-- | Details of the highest severity threat detected during malware scan and
-- number of infected files.
scanDetections_highestSeverityThreatDetails :: Lens.Lens' ScanDetections (Prelude.Maybe HighestSeverityThreatDetails)
scanDetections_highestSeverityThreatDetails = Lens.lens (\ScanDetections' {highestSeverityThreatDetails} -> highestSeverityThreatDetails) (\s@ScanDetections' {} a -> s {highestSeverityThreatDetails = a} :: ScanDetections)

-- | Total number of scanned files.
scanDetections_scannedItemCount :: Lens.Lens' ScanDetections (Prelude.Maybe ScannedItemCount)
scanDetections_scannedItemCount = Lens.lens (\ScanDetections' {scannedItemCount} -> scannedItemCount) (\s@ScanDetections' {} a -> s {scannedItemCount = a} :: ScanDetections)

-- | Contains details about identified threats organized by threat name.
scanDetections_threatDetectedByName :: Lens.Lens' ScanDetections (Prelude.Maybe ThreatDetectedByName)
scanDetections_threatDetectedByName = Lens.lens (\ScanDetections' {threatDetectedByName} -> threatDetectedByName) (\s@ScanDetections' {} a -> s {threatDetectedByName = a} :: ScanDetections)

-- | Total number of infected files.
scanDetections_threatsDetectedItemCount :: Lens.Lens' ScanDetections (Prelude.Maybe ThreatsDetectedItemCount)
scanDetections_threatsDetectedItemCount = Lens.lens (\ScanDetections' {threatsDetectedItemCount} -> threatsDetectedItemCount) (\s@ScanDetections' {} a -> s {threatsDetectedItemCount = a} :: ScanDetections)

instance Data.FromJSON ScanDetections where
  parseJSON =
    Data.withObject
      "ScanDetections"
      ( \x ->
          ScanDetections'
            Prelude.<$> (x Data..:? "highestSeverityThreatDetails")
            Prelude.<*> (x Data..:? "scannedItemCount")
            Prelude.<*> (x Data..:? "threatDetectedByName")
            Prelude.<*> (x Data..:? "threatsDetectedItemCount")
      )

instance Prelude.Hashable ScanDetections where
  hashWithSalt _salt ScanDetections' {..} =
    _salt
      `Prelude.hashWithSalt` highestSeverityThreatDetails
      `Prelude.hashWithSalt` scannedItemCount
      `Prelude.hashWithSalt` threatDetectedByName
      `Prelude.hashWithSalt` threatsDetectedItemCount

instance Prelude.NFData ScanDetections where
  rnf ScanDetections' {..} =
    Prelude.rnf highestSeverityThreatDetails `Prelude.seq`
      Prelude.rnf scannedItemCount `Prelude.seq`
        Prelude.rnf threatDetectedByName `Prelude.seq`
          Prelude.rnf threatsDetectedItemCount
