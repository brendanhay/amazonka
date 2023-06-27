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
-- Module      : Amazonka.IoTEventsData.Types.DetectorSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.DetectorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.DetectorStateSummary
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector (instance).
--
-- /See:/ 'newDetectorSummary' smart constructor.
data DetectorSummary = DetectorSummary'
  { -- | The time the detector (instance) was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the detector model that created this detector (instance).
    detectorModelName :: Prelude.Maybe Prelude.Text,
    -- | The version of the detector model that created this detector (instance).
    detectorModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The value of the key (identifying the device or system) that caused the
    -- creation of this detector (instance).
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The time the detector (instance) was last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The current state of the detector (instance).
    state :: Prelude.Maybe DetectorStateSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'detectorSummary_creationTime' - The time the detector (instance) was created.
--
-- 'detectorModelName', 'detectorSummary_detectorModelName' - The name of the detector model that created this detector (instance).
--
-- 'detectorModelVersion', 'detectorSummary_detectorModelVersion' - The version of the detector model that created this detector (instance).
--
-- 'keyValue', 'detectorSummary_keyValue' - The value of the key (identifying the device or system) that caused the
-- creation of this detector (instance).
--
-- 'lastUpdateTime', 'detectorSummary_lastUpdateTime' - The time the detector (instance) was last updated.
--
-- 'state', 'detectorSummary_state' - The current state of the detector (instance).
newDetectorSummary ::
  DetectorSummary
newDetectorSummary =
  DetectorSummary'
    { creationTime = Prelude.Nothing,
      detectorModelName = Prelude.Nothing,
      detectorModelVersion = Prelude.Nothing,
      keyValue = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The time the detector (instance) was created.
detectorSummary_creationTime :: Lens.Lens' DetectorSummary (Prelude.Maybe Prelude.UTCTime)
detectorSummary_creationTime = Lens.lens (\DetectorSummary' {creationTime} -> creationTime) (\s@DetectorSummary' {} a -> s {creationTime = a} :: DetectorSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the detector model that created this detector (instance).
detectorSummary_detectorModelName :: Lens.Lens' DetectorSummary (Prelude.Maybe Prelude.Text)
detectorSummary_detectorModelName = Lens.lens (\DetectorSummary' {detectorModelName} -> detectorModelName) (\s@DetectorSummary' {} a -> s {detectorModelName = a} :: DetectorSummary)

-- | The version of the detector model that created this detector (instance).
detectorSummary_detectorModelVersion :: Lens.Lens' DetectorSummary (Prelude.Maybe Prelude.Text)
detectorSummary_detectorModelVersion = Lens.lens (\DetectorSummary' {detectorModelVersion} -> detectorModelVersion) (\s@DetectorSummary' {} a -> s {detectorModelVersion = a} :: DetectorSummary)

-- | The value of the key (identifying the device or system) that caused the
-- creation of this detector (instance).
detectorSummary_keyValue :: Lens.Lens' DetectorSummary (Prelude.Maybe Prelude.Text)
detectorSummary_keyValue = Lens.lens (\DetectorSummary' {keyValue} -> keyValue) (\s@DetectorSummary' {} a -> s {keyValue = a} :: DetectorSummary)

-- | The time the detector (instance) was last updated.
detectorSummary_lastUpdateTime :: Lens.Lens' DetectorSummary (Prelude.Maybe Prelude.UTCTime)
detectorSummary_lastUpdateTime = Lens.lens (\DetectorSummary' {lastUpdateTime} -> lastUpdateTime) (\s@DetectorSummary' {} a -> s {lastUpdateTime = a} :: DetectorSummary) Prelude.. Lens.mapping Data._Time

-- | The current state of the detector (instance).
detectorSummary_state :: Lens.Lens' DetectorSummary (Prelude.Maybe DetectorStateSummary)
detectorSummary_state = Lens.lens (\DetectorSummary' {state} -> state) (\s@DetectorSummary' {} a -> s {state = a} :: DetectorSummary)

instance Data.FromJSON DetectorSummary where
  parseJSON =
    Data.withObject
      "DetectorSummary"
      ( \x ->
          DetectorSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "detectorModelName")
            Prelude.<*> (x Data..:? "detectorModelVersion")
            Prelude.<*> (x Data..:? "keyValue")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable DetectorSummary where
  hashWithSalt _salt DetectorSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` detectorModelName
      `Prelude.hashWithSalt` detectorModelVersion
      `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` state

instance Prelude.NFData DetectorSummary where
  rnf DetectorSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf detectorModelName
      `Prelude.seq` Prelude.rnf detectorModelVersion
      `Prelude.seq` Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf state
