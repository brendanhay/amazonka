{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.WirelessConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.WirelessConnection
  ( WirelessConnection (..)
  -- * Smart constructor
  , mkWirelessConnection
  -- * Lenses
  , wcIsWifiEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configures the wireless connection on an AWS Snowcone device.
--
-- /See:/ 'mkWirelessConnection' smart constructor.
newtype WirelessConnection = WirelessConnection'
  { isWifiEnabled :: Core.Maybe Core.Bool
    -- ^ Enables the Wi-Fi adapter on an AWS Snowcone device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'WirelessConnection' value with any optional fields omitted.
mkWirelessConnection
    :: WirelessConnection
mkWirelessConnection
  = WirelessConnection'{isWifiEnabled = Core.Nothing}

-- | Enables the Wi-Fi adapter on an AWS Snowcone device.
--
-- /Note:/ Consider using 'isWifiEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcIsWifiEnabled :: Lens.Lens' WirelessConnection (Core.Maybe Core.Bool)
wcIsWifiEnabled = Lens.field @"isWifiEnabled"
{-# INLINEABLE wcIsWifiEnabled #-}
{-# DEPRECATED isWifiEnabled "Use generic-lens or generic-optics with 'isWifiEnabled' instead"  #-}

instance Core.FromJSON WirelessConnection where
        toJSON WirelessConnection{..}
          = Core.object
              (Core.catMaybes [("IsWifiEnabled" Core..=) Core.<$> isWifiEnabled])

instance Core.FromJSON WirelessConnection where
        parseJSON
          = Core.withObject "WirelessConnection" Core.$
              \ x -> WirelessConnection' Core.<$> (x Core..:? "IsWifiEnabled")
