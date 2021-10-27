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
-- Module      : Network.AWS.Location.Types.DevicePosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.DevicePosition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the device position details.
--
-- /See:/ 'newDevicePosition' smart constructor.
data DevicePosition = DevicePosition'
  { -- | The device whose position you retrieved.
    deviceId :: Prelude.Maybe Prelude.Text,
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
        position =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_,
        receivedTime = Core._Time Lens.# pReceivedTime_,
        sampleTime = Core._Time Lens.# pSampleTime_
      }

-- | The device whose position you retrieved.
devicePosition_deviceId :: Lens.Lens' DevicePosition (Prelude.Maybe Prelude.Text)
devicePosition_deviceId = Lens.lens (\DevicePosition' {deviceId} -> deviceId) (\s@DevicePosition' {} a -> s {deviceId = a} :: DevicePosition)

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
            Prelude.<*> (x Core..: "Position")
            Prelude.<*> (x Core..: "ReceivedTime")
            Prelude.<*> (x Core..: "SampleTime")
      )

instance Prelude.Hashable DevicePosition

instance Prelude.NFData DevicePosition
