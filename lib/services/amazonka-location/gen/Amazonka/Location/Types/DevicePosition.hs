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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.DevicePosition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types.PositionalAccuracy
import qualified Amazonka.Prelude as Prelude

-- | Contains the device position details.
--
-- /See:/ 'newDevicePosition' smart constructor.
data DevicePosition = DevicePosition'
  { -- | The device whose position you retrieved.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The accuracy of the device position.
    accuracy :: Prelude.Maybe PositionalAccuracy,
    -- | The properties associated with the position.
    positionProperties :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The last known device position.
    position :: Core.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The timestamp for when the tracker resource received the device position
    -- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    receivedTime :: Core.POSIX,
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    sampleTime :: Core.POSIX
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
-- 'deviceId', 'devicePosition_deviceId' - The device whose position you retrieved.
--
-- 'accuracy', 'devicePosition_accuracy' - The accuracy of the device position.
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
      { deviceId = Prelude.Nothing,
        accuracy = Prelude.Nothing,
        positionProperties = Prelude.Nothing,
        position =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_,
        receivedTime = Core._Time Lens.# pReceivedTime_,
        sampleTime = Core._Time Lens.# pSampleTime_
      }

-- | The device whose position you retrieved.
devicePosition_deviceId :: Lens.Lens' DevicePosition (Prelude.Maybe Prelude.Text)
devicePosition_deviceId = Lens.lens (\DevicePosition' {deviceId} -> deviceId) (\s@DevicePosition' {} a -> s {deviceId = a} :: DevicePosition)

-- | The accuracy of the device position.
devicePosition_accuracy :: Lens.Lens' DevicePosition (Prelude.Maybe PositionalAccuracy)
devicePosition_accuracy = Lens.lens (\DevicePosition' {accuracy} -> accuracy) (\s@DevicePosition' {} a -> s {accuracy = a} :: DevicePosition)

-- | The properties associated with the position.
devicePosition_positionProperties :: Lens.Lens' DevicePosition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
devicePosition_positionProperties = Lens.lens (\DevicePosition' {positionProperties} -> positionProperties) (\s@DevicePosition' {} a -> s {positionProperties = a} :: DevicePosition) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The last known device position.
devicePosition_position :: Lens.Lens' DevicePosition (Prelude.NonEmpty Prelude.Double)
devicePosition_position = Lens.lens (\DevicePosition' {position} -> position) (\s@DevicePosition' {} a -> s {position = a} :: DevicePosition) Prelude.. Core._Sensitive Prelude.. Lens.coerced

-- | The timestamp for when the tracker resource received the device position
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
devicePosition_receivedTime :: Lens.Lens' DevicePosition Prelude.UTCTime
devicePosition_receivedTime = Lens.lens (\DevicePosition' {receivedTime} -> receivedTime) (\s@DevicePosition' {} a -> s {receivedTime = a} :: DevicePosition) Prelude.. Core._Time

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
devicePosition_sampleTime :: Lens.Lens' DevicePosition Prelude.UTCTime
devicePosition_sampleTime = Lens.lens (\DevicePosition' {sampleTime} -> sampleTime) (\s@DevicePosition' {} a -> s {sampleTime = a} :: DevicePosition) Prelude.. Core._Time

instance Core.FromJSON DevicePosition where
  parseJSON =
    Core.withObject
      "DevicePosition"
      ( \x ->
          DevicePosition'
            Prelude.<$> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "Accuracy")
            Prelude.<*> ( x Core..:? "PositionProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Position")
            Prelude.<*> (x Core..: "ReceivedTime")
            Prelude.<*> (x Core..: "SampleTime")
      )

instance Prelude.Hashable DevicePosition where
  hashWithSalt _salt DevicePosition' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` accuracy
      `Prelude.hashWithSalt` positionProperties
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` receivedTime
      `Prelude.hashWithSalt` sampleTime

instance Prelude.NFData DevicePosition where
  rnf DevicePosition' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf accuracy
      `Prelude.seq` Prelude.rnf positionProperties
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf receivedTime
      `Prelude.seq` Prelude.rnf sampleTime
