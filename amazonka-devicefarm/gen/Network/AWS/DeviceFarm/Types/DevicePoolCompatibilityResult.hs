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
-- Module      : Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import qualified Network.AWS.Lens as Lens

-- | Represents a device pool compatibility result.
--
-- /See:/ 'newDevicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
  { -- | Information about the compatibility.
    incompatibilityMessages :: Core.Maybe [IncompatibilityMessage],
    -- | Whether the result was compatible with the device pool.
    compatible :: Core.Maybe Core.Bool,
    -- | The device (phone or tablet) to return information about.
    device :: Core.Maybe Device
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DevicePoolCompatibilityResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'incompatibilityMessages', 'devicePoolCompatibilityResult_incompatibilityMessages' - Information about the compatibility.
--
-- 'compatible', 'devicePoolCompatibilityResult_compatible' - Whether the result was compatible with the device pool.
--
-- 'device', 'devicePoolCompatibilityResult_device' - The device (phone or tablet) to return information about.
newDevicePoolCompatibilityResult ::
  DevicePoolCompatibilityResult
newDevicePoolCompatibilityResult =
  DevicePoolCompatibilityResult'
    { incompatibilityMessages =
        Core.Nothing,
      compatible = Core.Nothing,
      device = Core.Nothing
    }

-- | Information about the compatibility.
devicePoolCompatibilityResult_incompatibilityMessages :: Lens.Lens' DevicePoolCompatibilityResult (Core.Maybe [IncompatibilityMessage])
devicePoolCompatibilityResult_incompatibilityMessages = Lens.lens (\DevicePoolCompatibilityResult' {incompatibilityMessages} -> incompatibilityMessages) (\s@DevicePoolCompatibilityResult' {} a -> s {incompatibilityMessages = a} :: DevicePoolCompatibilityResult) Core.. Lens.mapping Lens._Coerce

-- | Whether the result was compatible with the device pool.
devicePoolCompatibilityResult_compatible :: Lens.Lens' DevicePoolCompatibilityResult (Core.Maybe Core.Bool)
devicePoolCompatibilityResult_compatible = Lens.lens (\DevicePoolCompatibilityResult' {compatible} -> compatible) (\s@DevicePoolCompatibilityResult' {} a -> s {compatible = a} :: DevicePoolCompatibilityResult)

-- | The device (phone or tablet) to return information about.
devicePoolCompatibilityResult_device :: Lens.Lens' DevicePoolCompatibilityResult (Core.Maybe Device)
devicePoolCompatibilityResult_device = Lens.lens (\DevicePoolCompatibilityResult' {device} -> device) (\s@DevicePoolCompatibilityResult' {} a -> s {device = a} :: DevicePoolCompatibilityResult)

instance Core.FromJSON DevicePoolCompatibilityResult where
  parseJSON =
    Core.withObject
      "DevicePoolCompatibilityResult"
      ( \x ->
          DevicePoolCompatibilityResult'
            Core.<$> ( x Core..:? "incompatibilityMessages"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "compatible")
            Core.<*> (x Core..:? "device")
      )

instance Core.Hashable DevicePoolCompatibilityResult

instance Core.NFData DevicePoolCompatibilityResult
