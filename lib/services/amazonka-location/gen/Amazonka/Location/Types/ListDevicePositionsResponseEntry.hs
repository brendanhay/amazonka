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
-- Module      : Amazonka.Location.Types.ListDevicePositionsResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListDevicePositionsResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the tracker resource details.
--
-- /See:/ 'newListDevicePositionsResponseEntry' smart constructor.
data ListDevicePositionsResponseEntry = ListDevicePositionsResponseEntry'
  { -- | The ID of the device for this position.
    deviceId :: Prelude.Text,
    -- | The last known device position. Empty if no positions currently stored.
    position :: Core.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The timestamp at which the device position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    sampleTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicePositionsResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'listDevicePositionsResponseEntry_deviceId' - The ID of the device for this position.
--
-- 'position', 'listDevicePositionsResponseEntry_position' - The last known device position. Empty if no positions currently stored.
--
-- 'sampleTime', 'listDevicePositionsResponseEntry_sampleTime' - The timestamp at which the device position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newListDevicePositionsResponseEntry ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'sampleTime'
  Prelude.UTCTime ->
  ListDevicePositionsResponseEntry
newListDevicePositionsResponseEntry
  pDeviceId_
  pPosition_
  pSampleTime_ =
    ListDevicePositionsResponseEntry'
      { deviceId =
          pDeviceId_,
        position =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_,
        sampleTime =
          Core._Time Lens.# pSampleTime_
      }

-- | The ID of the device for this position.
listDevicePositionsResponseEntry_deviceId :: Lens.Lens' ListDevicePositionsResponseEntry Prelude.Text
listDevicePositionsResponseEntry_deviceId = Lens.lens (\ListDevicePositionsResponseEntry' {deviceId} -> deviceId) (\s@ListDevicePositionsResponseEntry' {} a -> s {deviceId = a} :: ListDevicePositionsResponseEntry)

-- | The last known device position. Empty if no positions currently stored.
listDevicePositionsResponseEntry_position :: Lens.Lens' ListDevicePositionsResponseEntry (Prelude.NonEmpty Prelude.Double)
listDevicePositionsResponseEntry_position = Lens.lens (\ListDevicePositionsResponseEntry' {position} -> position) (\s@ListDevicePositionsResponseEntry' {} a -> s {position = a} :: ListDevicePositionsResponseEntry) Prelude.. Core._Sensitive Prelude.. Lens.coerced

-- | The timestamp at which the device position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listDevicePositionsResponseEntry_sampleTime :: Lens.Lens' ListDevicePositionsResponseEntry Prelude.UTCTime
listDevicePositionsResponseEntry_sampleTime = Lens.lens (\ListDevicePositionsResponseEntry' {sampleTime} -> sampleTime) (\s@ListDevicePositionsResponseEntry' {} a -> s {sampleTime = a} :: ListDevicePositionsResponseEntry) Prelude.. Core._Time

instance
  Core.FromJSON
    ListDevicePositionsResponseEntry
  where
  parseJSON =
    Core.withObject
      "ListDevicePositionsResponseEntry"
      ( \x ->
          ListDevicePositionsResponseEntry'
            Prelude.<$> (x Core..: "DeviceId")
            Prelude.<*> (x Core..: "Position")
            Prelude.<*> (x Core..: "SampleTime")
      )

instance
  Prelude.Hashable
    ListDevicePositionsResponseEntry
  where
  hashWithSalt
    salt'
    ListDevicePositionsResponseEntry' {..} =
      salt' `Prelude.hashWithSalt` sampleTime
        `Prelude.hashWithSalt` position
        `Prelude.hashWithSalt` deviceId

instance
  Prelude.NFData
    ListDevicePositionsResponseEntry
  where
  rnf ListDevicePositionsResponseEntry' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf sampleTime
      `Prelude.seq` Prelude.rnf position
