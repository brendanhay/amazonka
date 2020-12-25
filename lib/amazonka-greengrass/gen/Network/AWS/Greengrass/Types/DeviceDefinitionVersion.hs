{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeviceDefinitionVersion
  ( DeviceDefinitionVersion (..),

    -- * Smart constructor
    mkDeviceDefinitionVersion,

    -- * Lenses
    ddvDevices,
  )
where

import qualified Network.AWS.Greengrass.Types.Device as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a device definition version.
--
-- /See:/ 'mkDeviceDefinitionVersion' smart constructor.
newtype DeviceDefinitionVersion = DeviceDefinitionVersion'
  { -- | A list of devices in the definition version.
    devices :: Core.Maybe [Types.Device]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceDefinitionVersion' value with any optional fields omitted.
mkDeviceDefinitionVersion ::
  DeviceDefinitionVersion
mkDeviceDefinitionVersion =
  DeviceDefinitionVersion' {devices = Core.Nothing}

-- | A list of devices in the definition version.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvDevices :: Lens.Lens' DeviceDefinitionVersion (Core.Maybe [Types.Device])
ddvDevices = Lens.field @"devices"
{-# DEPRECATED ddvDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

instance Core.FromJSON DeviceDefinitionVersion where
  toJSON DeviceDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Devices" Core..=) Core.<$> devices])

instance Core.FromJSON DeviceDefinitionVersion where
  parseJSON =
    Core.withObject "DeviceDefinitionVersion" Core.$
      \x -> DeviceDefinitionVersion' Core.<$> (x Core..:? "Devices")
