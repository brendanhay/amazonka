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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.DevicePositionUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the position update details for a device.
--
-- /See:/ 'newDevicePositionUpdate' smart constructor.
data DevicePositionUpdate = DevicePositionUpdate'
  { -- | The device associated to the position update.
    deviceId :: Prelude.Text,
    -- | The latest device position defined in
    -- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
    -- @[X or longitude, Y or latitude]@.
    position :: Core.Sensitive (Prelude.NonEmpty Prelude.Double),
    -- | The timestamp at which the device\'s position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    sampleTime :: Core.POSIX
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
-- 'deviceId', 'devicePositionUpdate_deviceId' - The device associated to the position update.
--
-- 'position', 'devicePositionUpdate_position' - The latest device position defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[X or longitude, Y or latitude]@.
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
      { deviceId = pDeviceId_,
        position =
          Core._Sensitive Prelude.. Lens.coerced
            Lens.# pPosition_,
        sampleTime = Core._Time Lens.# pSampleTime_
      }

-- | The device associated to the position update.
devicePositionUpdate_deviceId :: Lens.Lens' DevicePositionUpdate Prelude.Text
devicePositionUpdate_deviceId = Lens.lens (\DevicePositionUpdate' {deviceId} -> deviceId) (\s@DevicePositionUpdate' {} a -> s {deviceId = a} :: DevicePositionUpdate)

-- | The latest device position defined in
-- <https://earth-info.nga.mil/GandG/wgs84/index.html WGS 84> format:
-- @[X or longitude, Y or latitude]@.
devicePositionUpdate_position :: Lens.Lens' DevicePositionUpdate (Prelude.NonEmpty Prelude.Double)
devicePositionUpdate_position = Lens.lens (\DevicePositionUpdate' {position} -> position) (\s@DevicePositionUpdate' {} a -> s {position = a} :: DevicePositionUpdate) Prelude.. Core._Sensitive Prelude.. Lens.coerced

-- | The timestamp at which the device\'s position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
devicePositionUpdate_sampleTime :: Lens.Lens' DevicePositionUpdate Prelude.UTCTime
devicePositionUpdate_sampleTime = Lens.lens (\DevicePositionUpdate' {sampleTime} -> sampleTime) (\s@DevicePositionUpdate' {} a -> s {sampleTime = a} :: DevicePositionUpdate) Prelude.. Core._Time

instance Prelude.Hashable DevicePositionUpdate

instance Prelude.NFData DevicePositionUpdate

instance Core.ToJSON DevicePositionUpdate where
  toJSON DevicePositionUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DeviceId" Core..= deviceId),
            Prelude.Just ("Position" Core..= position),
            Prelude.Just ("SampleTime" Core..= sampleTime)
          ]
      )
