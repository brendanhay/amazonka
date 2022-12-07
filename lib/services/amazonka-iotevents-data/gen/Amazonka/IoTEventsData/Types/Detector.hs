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
-- Module      : Amazonka.IoTEventsData.Types.Detector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.Detector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.DetectorState
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector (instance).
--
-- /See:/ 'newDetector' smart constructor.
data Detector = Detector'
  { -- | The current state of the detector (instance).
    state :: Prelude.Maybe DetectorState,
    -- | The name of the detector model that created this detector (instance).
    detectorModelName :: Prelude.Maybe Prelude.Text,
    -- | The version of the detector model that created this detector (instance).
    detectorModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The value of the key (identifying the device or system) that caused the
    -- creation of this detector (instance).
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The time the detector (instance) was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time the detector (instance) was last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Detector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'detector_state' - The current state of the detector (instance).
--
-- 'detectorModelName', 'detector_detectorModelName' - The name of the detector model that created this detector (instance).
--
-- 'detectorModelVersion', 'detector_detectorModelVersion' - The version of the detector model that created this detector (instance).
--
-- 'keyValue', 'detector_keyValue' - The value of the key (identifying the device or system) that caused the
-- creation of this detector (instance).
--
-- 'creationTime', 'detector_creationTime' - The time the detector (instance) was created.
--
-- 'lastUpdateTime', 'detector_lastUpdateTime' - The time the detector (instance) was last updated.
newDetector ::
  Detector
newDetector =
  Detector'
    { state = Prelude.Nothing,
      detectorModelName = Prelude.Nothing,
      detectorModelVersion = Prelude.Nothing,
      keyValue = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | The current state of the detector (instance).
detector_state :: Lens.Lens' Detector (Prelude.Maybe DetectorState)
detector_state = Lens.lens (\Detector' {state} -> state) (\s@Detector' {} a -> s {state = a} :: Detector)

-- | The name of the detector model that created this detector (instance).
detector_detectorModelName :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_detectorModelName = Lens.lens (\Detector' {detectorModelName} -> detectorModelName) (\s@Detector' {} a -> s {detectorModelName = a} :: Detector)

-- | The version of the detector model that created this detector (instance).
detector_detectorModelVersion :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_detectorModelVersion = Lens.lens (\Detector' {detectorModelVersion} -> detectorModelVersion) (\s@Detector' {} a -> s {detectorModelVersion = a} :: Detector)

-- | The value of the key (identifying the device or system) that caused the
-- creation of this detector (instance).
detector_keyValue :: Lens.Lens' Detector (Prelude.Maybe Prelude.Text)
detector_keyValue = Lens.lens (\Detector' {keyValue} -> keyValue) (\s@Detector' {} a -> s {keyValue = a} :: Detector)

-- | The time the detector (instance) was created.
detector_creationTime :: Lens.Lens' Detector (Prelude.Maybe Prelude.UTCTime)
detector_creationTime = Lens.lens (\Detector' {creationTime} -> creationTime) (\s@Detector' {} a -> s {creationTime = a} :: Detector) Prelude.. Lens.mapping Data._Time

-- | The time the detector (instance) was last updated.
detector_lastUpdateTime :: Lens.Lens' Detector (Prelude.Maybe Prelude.UTCTime)
detector_lastUpdateTime = Lens.lens (\Detector' {lastUpdateTime} -> lastUpdateTime) (\s@Detector' {} a -> s {lastUpdateTime = a} :: Detector) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Detector where
  parseJSON =
    Data.withObject
      "Detector"
      ( \x ->
          Detector'
            Prelude.<$> (x Data..:? "state")
            Prelude.<*> (x Data..:? "detectorModelName")
            Prelude.<*> (x Data..:? "detectorModelVersion")
            Prelude.<*> (x Data..:? "keyValue")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
      )

instance Prelude.Hashable Detector where
  hashWithSalt _salt Detector' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` detectorModelName
      `Prelude.hashWithSalt` detectorModelVersion
      `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData Detector where
  rnf Detector' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf detectorModelName
      `Prelude.seq` Prelude.rnf detectorModelVersion
      `Prelude.seq` Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
