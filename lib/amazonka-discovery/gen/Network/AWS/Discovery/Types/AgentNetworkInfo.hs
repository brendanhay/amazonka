{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentNetworkInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.AgentNetworkInfo
  ( AgentNetworkInfo (..)
  -- * Smart constructor
  , mkAgentNetworkInfo
  -- * Lenses
  , aniIpAddress
  , aniMacAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Network details about the host where the agent/connector resides.
--
-- /See:/ 'mkAgentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { ipAddress :: Core.Maybe Core.Text
    -- ^ The IP address for the host where the agent/connector resides.
  , macAddress :: Core.Maybe Core.Text
    -- ^ The MAC address for the host where the agent/connector resides.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AgentNetworkInfo' value with any optional fields omitted.
mkAgentNetworkInfo
    :: AgentNetworkInfo
mkAgentNetworkInfo
  = AgentNetworkInfo'{ipAddress = Core.Nothing,
                      macAddress = Core.Nothing}

-- | The IP address for the host where the agent/connector resides.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniIpAddress :: Lens.Lens' AgentNetworkInfo (Core.Maybe Core.Text)
aniIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE aniIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | The MAC address for the host where the agent/connector resides.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aniMacAddress :: Lens.Lens' AgentNetworkInfo (Core.Maybe Core.Text)
aniMacAddress = Lens.field @"macAddress"
{-# INLINEABLE aniMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

instance Core.FromJSON AgentNetworkInfo where
        parseJSON
          = Core.withObject "AgentNetworkInfo" Core.$
              \ x ->
                AgentNetworkInfo' Core.<$>
                  (x Core..:? "ipAddress") Core.<*> x Core..:? "macAddress"
