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
-- Module      : Amazonka.Location.Types.DevicePosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.DevicePosition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PositionalAccuracy
import qualified Amazonka.Prelude as Prelude

-- | Contains the device position details.
--
-- /See:/ 'newDevicePosition' smart constructor.
data DevicePosition = DevicePosition'
  { -- | The accuracy of the device position.
    accuracy :: Prelude.Maybe PositionalAccuracy,
    -- | The device whose position you retrieved.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The properties associated with the position.
    positionProperties :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The last known device position.
    position :: Data.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The timestamp for when the tracker resource received the device position
    -- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    receivedTime :: Data.ISO8601,
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    sampleTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevicePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accuracy', 'devicePosition_accuracy' - The accuracy of the device position.
--
-- 'deviceId', 'devicePosition_deviceId' - The device whose position you retrieved.
--
-- 'positionProperties', 'devicePosition_positionProperties' - The properties associated with the position.
--
-- 'position', 'devicePosition_position' - The last known device position.
--
-- 'receivedTime', 'devicePosition_receivedTime' - The timestamp for when the tracker resource received the device position
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'sampleTime', 'devicePosition_sampleTime' - The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newDevicePosition ::
  -- | 'position'
  Prelude.NonEmpty Prelude.Double ->
  -- | 'receivedTime'
  Prelude.UTCTime ->
  -- | 'sampleTime'
  Prelude.UTCTime ->
  DevicePosition
newDevicePosition
  pPosition_
  pReceivedTime_
  pSampleTime_ =
    DevicePosition'
      { accuracy = Prelude.Nothing,
        deviceId = Prelude.Nothing,
        positionProperties = Prelude.Nothing,
        position =
          Data._Sensitive
            Prelude.. Lens.coerced
            Lens.# pPosition_,
        receivedTime = Data._Time Lens.# pReceivedTime_,
        sampleTime = Data._Time Lens.# pSampleTime_
      }

-- | The accuracy of the device position.
devicePosition_accuracy :: Lens.Lens' DevicePosition (Prelude.Maybe PositionalAccuracy)
devicePosition_accuracy = Lens.lens (\DevicePosition' {accuracy} -> accuracy) (\s@DevicePosition' {} a -> s {accuracy = a} :: DevicePosition)

-- | The device whose position you retrieved.
devicePosition_deviceId :: Lens.Lens' DevicePosition (Prelude.Maybe Prelude.Text)
devicePosition_deviceId = Lens.lens (\DevicePosition' {deviceId} -> deviceId) (\s@DevicePosition' {} a -> s {deviceId = a} :: DevicePosition)

-- | The properties associated with the position.
devicePosition_positionProperties :: Lens.Lens' DevicePosition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
devicePosition_positionProperties = Lens.lens (\DevicePosition' {positionProperties} -> positionProperties) (\s@DevicePosition' {} a -> s {positionProperties = a} :: DevicePosition) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The last known device position.
devicePosition_position :: Lens.Lens' DevicePosition (Prelude.NonEmpty Prelude.Double)
devicePosition_position = Lens.lens (\DevicePosition' {position} -> position) (\s@DevicePosition' {} a -> s {position = a} :: DevicePosition) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The timestamp for when the tracker resource received the device position
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
devicePosition_receivedTime :: Lens.Lens' DevicePosition Prelude.UTCTime
devicePosition_receivedTime = Lens.lens (\DevicePosition' {receivedTime} -> receivedTime) (\s@DevicePosition' {} a -> s {receivedTime = a} :: DevicePosition) Prelude.. Data._Time

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
devicePosition_sampleTime :: Lens.Lens' DevicePosition Prelude.UTCTime
devicePosition_sampleTime = Lens.lens (\DevicePosition' {sampleTime} -> sampleTime) (\s@DevicePosition' {} a -> s {sampleTime = a} :: DevicePosition) Prelude.. Data._Time

instance Data.FromJSON DevicePosition where
  parseJSON =
    Data.withObject
      "DevicePosition"
      ( \x ->
          DevicePosition'
            Prelude.<$> (x Data..:? "Accuracy")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> ( x
                            Data..:? "PositionProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "Position")
            Prelude.<*> (x Data..: "ReceivedTime")
            Prelude.<*> (x Data..: "SampleTime")
      )

instance Prelude.Hashable DevicePosition where
  hashWithSalt _salt DevicePosition' {..} =
    _salt
      `Prelude.hashWithSalt` accuracy
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` positionProperties
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` receivedTime
      `Prelude.hashWithSalt` sampleTime

instance Prelude.NFData DevicePosition where
  rnf DevicePosition' {..} =
    Prelude.rnf accuracy `Prelude.seq`
      Prelude.rnf deviceId `Prelude.seq`
        Prelude.rnf positionProperties `Prelude.seq`
          Prelude.rnf position `Prelude.seq`
            Prelude.rnf receivedTime `Prelude.seq`
              Prelude.rnf sampleTime
