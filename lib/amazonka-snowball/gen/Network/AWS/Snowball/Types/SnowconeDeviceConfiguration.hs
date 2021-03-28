{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
  ( SnowconeDeviceConfiguration (..)
  -- * Smart constructor
  , mkSnowconeDeviceConfiguration
  -- * Lenses
  , sdcWirelessConnection
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.WirelessConnection as Types

-- | Specifies the device configuration for an AWS Snowcone job. 
--
-- /See:/ 'mkSnowconeDeviceConfiguration' smart constructor.
newtype SnowconeDeviceConfiguration = SnowconeDeviceConfiguration'
  { wirelessConnection :: Core.Maybe Types.WirelessConnection
    -- ^ Configures the wireless connection for the AWS Snowcone device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SnowconeDeviceConfiguration' value with any optional fields omitted.
mkSnowconeDeviceConfiguration
    :: SnowconeDeviceConfiguration
mkSnowconeDeviceConfiguration
  = SnowconeDeviceConfiguration'{wirelessConnection = Core.Nothing}

-- | Configures the wireless connection for the AWS Snowcone device.
--
-- /Note:/ Consider using 'wirelessConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcWirelessConnection :: Lens.Lens' SnowconeDeviceConfiguration (Core.Maybe Types.WirelessConnection)
sdcWirelessConnection = Lens.field @"wirelessConnection"
{-# INLINEABLE sdcWirelessConnection #-}
{-# DEPRECATED wirelessConnection "Use generic-lens or generic-optics with 'wirelessConnection' instead"  #-}

instance Core.FromJSON SnowconeDeviceConfiguration where
        toJSON SnowconeDeviceConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("WirelessConnection" Core..=) Core.<$> wirelessConnection])

instance Core.FromJSON SnowconeDeviceConfiguration where
        parseJSON
          = Core.withObject "SnowconeDeviceConfiguration" Core.$
              \ x ->
                SnowconeDeviceConfiguration' Core.<$>
                  (x Core..:? "WirelessConnection")
