{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.NetworkConnectionAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.NetworkConnectionAction
  ( NetworkConnectionAction (..),

    -- * Smart constructor
    mkNetworkConnectionAction,

    -- * Lenses
    ncaBlocked,
    ncaConnectionDirection,
    ncaLocalIpDetails,
    ncaLocalPortDetails,
    ncaProtocol,
    ncaRemoteIpDetails,
    ncaRemotePortDetails,
  )
where

import qualified Network.AWS.GuardDuty.Types.LocalIpDetails as Types
import qualified Network.AWS.GuardDuty.Types.LocalPortDetails as Types
import qualified Network.AWS.GuardDuty.Types.RemoteIpDetails as Types
import qualified Network.AWS.GuardDuty.Types.RemotePortDetails as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the NETWORK_CONNECTION action described in the finding.
--
-- /See:/ 'mkNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { -- | Indicates whether EC2 blocked the network connection to your instance.
    blocked :: Core.Maybe Core.Bool,
    -- | The network connection direction.
    connectionDirection :: Core.Maybe Types.String,
    -- | The local IP information of the connection.
    localIpDetails :: Core.Maybe Types.LocalIpDetails,
    -- | The local port information of the connection.
    localPortDetails :: Core.Maybe Types.LocalPortDetails,
    -- | The network connection protocol.
    protocol :: Core.Maybe Types.String,
    -- | The remote IP information of the connection.
    remoteIpDetails :: Core.Maybe Types.RemoteIpDetails,
    -- | The remote port information of the connection.
    remotePortDetails :: Core.Maybe Types.RemotePortDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkConnectionAction' value with any optional fields omitted.
mkNetworkConnectionAction ::
  NetworkConnectionAction
mkNetworkConnectionAction =
  NetworkConnectionAction'
    { blocked = Core.Nothing,
      connectionDirection = Core.Nothing,
      localIpDetails = Core.Nothing,
      localPortDetails = Core.Nothing,
      protocol = Core.Nothing,
      remoteIpDetails = Core.Nothing,
      remotePortDetails = Core.Nothing
    }

-- | Indicates whether EC2 blocked the network connection to your instance.
--
-- /Note:/ Consider using 'blocked' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaBlocked :: Lens.Lens' NetworkConnectionAction (Core.Maybe Core.Bool)
ncaBlocked = Lens.field @"blocked"
{-# DEPRECATED ncaBlocked "Use generic-lens or generic-optics with 'blocked' instead." #-}

-- | The network connection direction.
--
-- /Note:/ Consider using 'connectionDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaConnectionDirection :: Lens.Lens' NetworkConnectionAction (Core.Maybe Types.String)
ncaConnectionDirection = Lens.field @"connectionDirection"
{-# DEPRECATED ncaConnectionDirection "Use generic-lens or generic-optics with 'connectionDirection' instead." #-}

-- | The local IP information of the connection.
--
-- /Note:/ Consider using 'localIpDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaLocalIpDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe Types.LocalIpDetails)
ncaLocalIpDetails = Lens.field @"localIpDetails"
{-# DEPRECATED ncaLocalIpDetails "Use generic-lens or generic-optics with 'localIpDetails' instead." #-}

-- | The local port information of the connection.
--
-- /Note:/ Consider using 'localPortDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaLocalPortDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe Types.LocalPortDetails)
ncaLocalPortDetails = Lens.field @"localPortDetails"
{-# DEPRECATED ncaLocalPortDetails "Use generic-lens or generic-optics with 'localPortDetails' instead." #-}

-- | The network connection protocol.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaProtocol :: Lens.Lens' NetworkConnectionAction (Core.Maybe Types.String)
ncaProtocol = Lens.field @"protocol"
{-# DEPRECATED ncaProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The remote IP information of the connection.
--
-- /Note:/ Consider using 'remoteIpDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaRemoteIpDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe Types.RemoteIpDetails)
ncaRemoteIpDetails = Lens.field @"remoteIpDetails"
{-# DEPRECATED ncaRemoteIpDetails "Use generic-lens or generic-optics with 'remoteIpDetails' instead." #-}

-- | The remote port information of the connection.
--
-- /Note:/ Consider using 'remotePortDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaRemotePortDetails :: Lens.Lens' NetworkConnectionAction (Core.Maybe Types.RemotePortDetails)
ncaRemotePortDetails = Lens.field @"remotePortDetails"
{-# DEPRECATED ncaRemotePortDetails "Use generic-lens or generic-optics with 'remotePortDetails' instead." #-}

instance Core.FromJSON NetworkConnectionAction where
  parseJSON =
    Core.withObject "NetworkConnectionAction" Core.$
      \x ->
        NetworkConnectionAction'
          Core.<$> (x Core..:? "blocked")
          Core.<*> (x Core..:? "connectionDirection")
          Core.<*> (x Core..:? "localIpDetails")
          Core.<*> (x Core..:? "localPortDetails")
          Core.<*> (x Core..:? "protocol")
          Core.<*> (x Core..:? "remoteIpDetails")
          Core.<*> (x Core..:? "remotePortDetails")
