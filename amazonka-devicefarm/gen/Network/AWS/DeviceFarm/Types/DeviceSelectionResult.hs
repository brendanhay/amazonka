{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceSelectionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceSelectionResult where

import Network.AWS.DeviceFarm.Types.DeviceFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the run results requested by the device selection configuration
-- and how many devices were returned. For an example of the JSON response
-- syntax, see ScheduleRun.
--
-- /See:/ 'newDeviceSelectionResult' smart constructor.
data DeviceSelectionResult = DeviceSelectionResult'
  { -- | The maximum number of devices to be selected by a device filter and
    -- included in a test run.
    maxDevices :: Prelude.Maybe Prelude.Int,
    -- | The filters in a device selection result.
    filters :: Prelude.Maybe [DeviceFilter],
    -- | The number of devices that matched the device filter selection criteria.
    matchedDevicesCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeviceSelectionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxDevices', 'deviceSelectionResult_maxDevices' - The maximum number of devices to be selected by a device filter and
-- included in a test run.
--
-- 'filters', 'deviceSelectionResult_filters' - The filters in a device selection result.
--
-- 'matchedDevicesCount', 'deviceSelectionResult_matchedDevicesCount' - The number of devices that matched the device filter selection criteria.
newDeviceSelectionResult ::
  DeviceSelectionResult
newDeviceSelectionResult =
  DeviceSelectionResult'
    { maxDevices =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      matchedDevicesCount = Prelude.Nothing
    }

-- | The maximum number of devices to be selected by a device filter and
-- included in a test run.
deviceSelectionResult_maxDevices :: Lens.Lens' DeviceSelectionResult (Prelude.Maybe Prelude.Int)
deviceSelectionResult_maxDevices = Lens.lens (\DeviceSelectionResult' {maxDevices} -> maxDevices) (\s@DeviceSelectionResult' {} a -> s {maxDevices = a} :: DeviceSelectionResult)

-- | The filters in a device selection result.
deviceSelectionResult_filters :: Lens.Lens' DeviceSelectionResult (Prelude.Maybe [DeviceFilter])
deviceSelectionResult_filters = Lens.lens (\DeviceSelectionResult' {filters} -> filters) (\s@DeviceSelectionResult' {} a -> s {filters = a} :: DeviceSelectionResult) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of devices that matched the device filter selection criteria.
deviceSelectionResult_matchedDevicesCount :: Lens.Lens' DeviceSelectionResult (Prelude.Maybe Prelude.Int)
deviceSelectionResult_matchedDevicesCount = Lens.lens (\DeviceSelectionResult' {matchedDevicesCount} -> matchedDevicesCount) (\s@DeviceSelectionResult' {} a -> s {matchedDevicesCount = a} :: DeviceSelectionResult)

instance Prelude.FromJSON DeviceSelectionResult where
  parseJSON =
    Prelude.withObject
      "DeviceSelectionResult"
      ( \x ->
          DeviceSelectionResult'
            Prelude.<$> (x Prelude..:? "maxDevices")
            Prelude.<*> (x Prelude..:? "filters" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "matchedDevicesCount")
      )

instance Prelude.Hashable DeviceSelectionResult

instance Prelude.NFData DeviceSelectionResult
