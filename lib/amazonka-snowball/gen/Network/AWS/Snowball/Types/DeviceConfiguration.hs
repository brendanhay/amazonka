{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.DeviceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.DeviceConfiguration
  ( DeviceConfiguration (..),

    -- * Smart constructor
    mkDeviceConfiguration,

    -- * Lenses
    dcSnowconeDeviceConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.SnowconeDeviceConfiguration as Types

-- | The container for @SnowconeDeviceConfiguration@ .
--
-- /See:/ 'mkDeviceConfiguration' smart constructor.
newtype DeviceConfiguration = DeviceConfiguration'
  { -- | Returns information about the device configuration for an AWS Snowcone job.
    snowconeDeviceConfiguration :: Core.Maybe Types.SnowconeDeviceConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceConfiguration' value with any optional fields omitted.
mkDeviceConfiguration ::
  DeviceConfiguration
mkDeviceConfiguration =
  DeviceConfiguration' {snowconeDeviceConfiguration = Core.Nothing}

-- | Returns information about the device configuration for an AWS Snowcone job.
--
-- /Note:/ Consider using 'snowconeDeviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSnowconeDeviceConfiguration :: Lens.Lens' DeviceConfiguration (Core.Maybe Types.SnowconeDeviceConfiguration)
dcSnowconeDeviceConfiguration = Lens.field @"snowconeDeviceConfiguration"
{-# DEPRECATED dcSnowconeDeviceConfiguration "Use generic-lens or generic-optics with 'snowconeDeviceConfiguration' instead." #-}

instance Core.FromJSON DeviceConfiguration where
  toJSON DeviceConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("SnowconeDeviceConfiguration" Core..=)
              Core.<$> snowconeDeviceConfiguration
          ]
      )

instance Core.FromJSON DeviceConfiguration where
  parseJSON =
    Core.withObject "DeviceConfiguration" Core.$
      \x ->
        DeviceConfiguration'
          Core.<$> (x Core..:? "SnowconeDeviceConfiguration")
