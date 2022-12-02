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
-- Module      : Amazonka.Location.Types.DevicePositionUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.DevicePositionUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PositionalAccuracy
import qualified Amazonka.Prelude as Prelude

-- | Contains the position update details for a device.
--
-- /See:/ 'newDevicePositionUpdate' smart constructor.
data DevicePositionUpdate = DevicePositionUpdate'
  { -- | The accuracy of the device position.
    accuracy :: Prelude.Maybe PositionalAccuracy,
    -- | Associates one of more properties with the position update. A property
    -- is a key-value pair stored with the position update and added to any
    -- geofence event the update may trigger.
    --
    -- Format: @\"key\" : \"value\"@
    positionProperties :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The device associated to the position update.
    deviceId :: Prelude.Text,
    -- | The latest device position defined in
    -- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 WGS 84>
    -- format: @[X or longitude, Y or latitude]@.
    position :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    sampleTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevicePositionUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accuracy', 'devicePositionUpdate_accuracy' - The accuracy of the device position.
--
-- 'positionProperties', 'devicePositionUpdate_positionProperties' - Associates one of more properties with the position update. A property
-- is a key-value pair stored with the position update and added to any
-- geofence event the update may trigger.
--
-- Format: @\"key\" : \"value\"@
--
-- 'deviceId', 'devicePositionUpdate_deviceId' - The device associated to the position update.
--
-- 'position', 'devicePositionUpdate_position' - The latest device position defined in
-- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 WGS 84>
-- format: @[X or longitude, Y or latitude]@.
--
-- 'sampleTime', 'devicePositionUpdate_sampleTime' - The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newDevicePositionUpdate ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'sampleTime'
  Prelude.UTCTime ->
  DevicePositionUpdate
newDevicePositionUpdate
  pDeviceId_
  pPosition_
  pSampleTime_ =
    DevicePositionUpdate'
      { accuracy = Prelude.Nothing,
        positionProperties = Prelude.Nothing,
        deviceId = pDeviceId_,
        position =
          Data._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_,
        sampleTime = Data._Time Lens.# pSampleTime_
      }

-- | The accuracy of the device position.
devicePositionUpdate_accuracy :: Lens.Lens' DevicePositionUpdate (Prelude.Maybe PositionalAccuracy)
devicePositionUpdate_accuracy = Lens.lens (\DevicePositionUpdate' {accuracy} -> accuracy) (\s@DevicePositionUpdate' {} a -> s {accuracy = a} :: DevicePositionUpdate)

-- | Associates one of more properties with the position update. A property
-- is a key-value pair stored with the position update and added to any
-- geofence event the update may trigger.
--
-- Format: @\"key\" : \"value\"@
devicePositionUpdate_positionProperties :: Lens.Lens' DevicePositionUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
devicePositionUpdate_positionProperties = Lens.lens (\DevicePositionUpdate' {positionProperties} -> positionProperties) (\s@DevicePositionUpdate' {} a -> s {positionProperties = a} :: DevicePositionUpdate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The device associated to the position update.
devicePositionUpdate_deviceId :: Lens.Lens' DevicePositionUpdate Prelude.Text
devicePositionUpdate_deviceId = Lens.lens (\DevicePositionUpdate' {deviceId} -> deviceId) (\s@DevicePositionUpdate' {} a -> s {deviceId = a} :: DevicePositionUpdate)

-- | The latest device position defined in
-- <https://earth-info.nga.mil/index.php?dir=wgs84&action=wgs84 WGS 84>
-- format: @[X or longitude, Y or latitude]@.
devicePositionUpdate_position :: Lens.Lens' DevicePositionUpdate (Prelude.NonEmpty Prelude.Double)
devicePositionUpdate_position = Lens.lens (\DevicePositionUpdate' {position} -> position) (\s@DevicePositionUpdate' {} a -> s {position = a} :: DevicePositionUpdate) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
devicePositionUpdate_sampleTime :: Lens.Lens' DevicePositionUpdate Prelude.UTCTime
devicePositionUpdate_sampleTime = Lens.lens (\DevicePositionUpdate' {sampleTime} -> sampleTime) (\s@DevicePositionUpdate' {} a -> s {sampleTime = a} :: DevicePositionUpdate) Prelude.. Data._Time

instance Prelude.Hashable DevicePositionUpdate where
  hashWithSalt _salt DevicePositionUpdate' {..} =
    _salt `Prelude.hashWithSalt` accuracy
      `Prelude.hashWithSalt` positionProperties
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` sampleTime

instance Prelude.NFData DevicePositionUpdate where
  rnf DevicePositionUpdate' {..} =
    Prelude.rnf accuracy
      `Prelude.seq` Prelude.rnf positionProperties
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf sampleTime

instance Data.ToJSON DevicePositionUpdate where
  toJSON DevicePositionUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Accuracy" Data..=) Prelude.<$> accuracy,
            ("PositionProperties" Data..=)
              Prelude.<$> positionProperties,
            Prelude.Just ("DeviceId" Data..= deviceId),
            Prelude.Just ("Position" Data..= position),
            Prelude.Just ("SampleTime" Data..= sampleTime)
          ]
      )
