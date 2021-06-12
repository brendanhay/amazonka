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
-- Module      : Network.AWS.Greengrass.Types.DeviceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeviceDefinitionVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.Device
import qualified Network.AWS.Lens as Lens

-- | Information about a device definition version.
--
-- /See:/ 'newDeviceDefinitionVersion' smart constructor.
data DeviceDefinitionVersion = DeviceDefinitionVersion'
  { -- | A list of devices in the definition version.
    devices :: Core.Maybe [Device]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devices', 'deviceDefinitionVersion_devices' - A list of devices in the definition version.
newDeviceDefinitionVersion ::
  DeviceDefinitionVersion
newDeviceDefinitionVersion =
  DeviceDefinitionVersion' {devices = Core.Nothing}

-- | A list of devices in the definition version.
deviceDefinitionVersion_devices :: Lens.Lens' DeviceDefinitionVersion (Core.Maybe [Device])
deviceDefinitionVersion_devices = Lens.lens (\DeviceDefinitionVersion' {devices} -> devices) (\s@DeviceDefinitionVersion' {} a -> s {devices = a} :: DeviceDefinitionVersion) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON DeviceDefinitionVersion where
  parseJSON =
    Core.withObject
      "DeviceDefinitionVersion"
      ( \x ->
          DeviceDefinitionVersion'
            Core.<$> (x Core..:? "Devices" Core..!= Core.mempty)
      )

instance Core.Hashable DeviceDefinitionVersion

instance Core.NFData DeviceDefinitionVersion

instance Core.ToJSON DeviceDefinitionVersion where
  toJSON DeviceDefinitionVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("Devices" Core..=) Core.<$> devices]
      )
