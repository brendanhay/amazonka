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
    ncaRemoteIPDetails,
    ncaProtocol,
    ncaLocalIPDetails,
    ncaRemotePortDetails,
    ncaBlocked,
    ncaConnectionDirection,
    ncaLocalPortDetails,
  )
where

import Network.AWS.GuardDuty.Types.LocalIPDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the NETWORK_CONNECTION action described in the finding.
--
-- /See:/ 'mkNetworkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { remoteIPDetails ::
      Lude.Maybe RemoteIPDetails,
    protocol :: Lude.Maybe Lude.Text,
    localIPDetails :: Lude.Maybe LocalIPDetails,
    remotePortDetails ::
      Lude.Maybe RemotePortDetails,
    blocked :: Lude.Maybe Lude.Bool,
    connectionDirection :: Lude.Maybe Lude.Text,
    localPortDetails ::
      Lude.Maybe LocalPortDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkConnectionAction' with the minimum fields required to make a request.
--
-- * 'blocked' - Indicates whether EC2 blocked the network connection to your instance.
-- * 'connectionDirection' - The network connection direction.
-- * 'localIPDetails' - The local IP information of the connection.
-- * 'localPortDetails' - The local port information of the connection.
-- * 'protocol' - The network connection protocol.
-- * 'remoteIPDetails' - The remote IP information of the connection.
-- * 'remotePortDetails' - The remote port information of the connection.
mkNetworkConnectionAction ::
  NetworkConnectionAction
mkNetworkConnectionAction =
  NetworkConnectionAction'
    { remoteIPDetails = Lude.Nothing,
      protocol = Lude.Nothing,
      localIPDetails = Lude.Nothing,
      remotePortDetails = Lude.Nothing,
      blocked = Lude.Nothing,
      connectionDirection = Lude.Nothing,
      localPortDetails = Lude.Nothing
    }

-- | The remote IP information of the connection.
--
-- /Note:/ Consider using 'remoteIPDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaRemoteIPDetails :: Lens.Lens' NetworkConnectionAction (Lude.Maybe RemoteIPDetails)
ncaRemoteIPDetails = Lens.lens (remoteIPDetails :: NetworkConnectionAction -> Lude.Maybe RemoteIPDetails) (\s a -> s {remoteIPDetails = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaRemoteIPDetails "Use generic-lens or generic-optics with 'remoteIPDetails' instead." #-}

-- | The network connection protocol.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaProtocol :: Lens.Lens' NetworkConnectionAction (Lude.Maybe Lude.Text)
ncaProtocol = Lens.lens (protocol :: NetworkConnectionAction -> Lude.Maybe Lude.Text) (\s a -> s {protocol = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The local IP information of the connection.
--
-- /Note:/ Consider using 'localIPDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaLocalIPDetails :: Lens.Lens' NetworkConnectionAction (Lude.Maybe LocalIPDetails)
ncaLocalIPDetails = Lens.lens (localIPDetails :: NetworkConnectionAction -> Lude.Maybe LocalIPDetails) (\s a -> s {localIPDetails = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaLocalIPDetails "Use generic-lens or generic-optics with 'localIPDetails' instead." #-}

-- | The remote port information of the connection.
--
-- /Note:/ Consider using 'remotePortDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaRemotePortDetails :: Lens.Lens' NetworkConnectionAction (Lude.Maybe RemotePortDetails)
ncaRemotePortDetails = Lens.lens (remotePortDetails :: NetworkConnectionAction -> Lude.Maybe RemotePortDetails) (\s a -> s {remotePortDetails = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaRemotePortDetails "Use generic-lens or generic-optics with 'remotePortDetails' instead." #-}

-- | Indicates whether EC2 blocked the network connection to your instance.
--
-- /Note:/ Consider using 'blocked' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaBlocked :: Lens.Lens' NetworkConnectionAction (Lude.Maybe Lude.Bool)
ncaBlocked = Lens.lens (blocked :: NetworkConnectionAction -> Lude.Maybe Lude.Bool) (\s a -> s {blocked = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaBlocked "Use generic-lens or generic-optics with 'blocked' instead." #-}

-- | The network connection direction.
--
-- /Note:/ Consider using 'connectionDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaConnectionDirection :: Lens.Lens' NetworkConnectionAction (Lude.Maybe Lude.Text)
ncaConnectionDirection = Lens.lens (connectionDirection :: NetworkConnectionAction -> Lude.Maybe Lude.Text) (\s a -> s {connectionDirection = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaConnectionDirection "Use generic-lens or generic-optics with 'connectionDirection' instead." #-}

-- | The local port information of the connection.
--
-- /Note:/ Consider using 'localPortDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncaLocalPortDetails :: Lens.Lens' NetworkConnectionAction (Lude.Maybe LocalPortDetails)
ncaLocalPortDetails = Lens.lens (localPortDetails :: NetworkConnectionAction -> Lude.Maybe LocalPortDetails) (\s a -> s {localPortDetails = a} :: NetworkConnectionAction)
{-# DEPRECATED ncaLocalPortDetails "Use generic-lens or generic-optics with 'localPortDetails' instead." #-}

instance Lude.FromJSON NetworkConnectionAction where
  parseJSON =
    Lude.withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            Lude.<$> (x Lude..:? "remoteIpDetails")
            Lude.<*> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "localIpDetails")
            Lude.<*> (x Lude..:? "remotePortDetails")
            Lude.<*> (x Lude..:? "blocked")
            Lude.<*> (x Lude..:? "connectionDirection")
            Lude.<*> (x Lude..:? "localPortDetails")
      )
