-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.IPPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.IPPermission
  ( IPPermission (..),

    -- * Smart constructor
    mkIPPermission,

    -- * Lenses
    ipFromPort,
    ipToPort,
    ipIPRange,
    ipProtocol,
  )
where

import Network.AWS.GameLift.Types.IPProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A range of IP addresses and port settings that allow inbound traffic to connect to server processes on an Amazon GameLift hosting resource. New game sessions that are started on the fleet are assigned an IP address/port number combination, which must fall into the fleet's allowed ranges. For fleets created with a custom game server, the ranges reflect the server's game session assignments. For Realtime Servers fleets, Amazon GameLift automatically opens two port ranges, one for TCP messaging and one for UDP for use by the Realtime servers.
--
-- /See:/ 'mkIPPermission' smart constructor.
data IPPermission = IPPermission'
  { fromPort :: Lude.Natural,
    toPort :: Lude.Natural,
    ipRange :: Lude.Text,
    protocol :: IPProtocol
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPPermission' with the minimum fields required to make a request.
--
-- * 'fromPort' - A starting value for a range of allowed port numbers.
-- * 'ipRange' - A range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
-- * 'protocol' - The network communication protocol used by the fleet.
-- * 'toPort' - An ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
mkIPPermission ::
  -- | 'fromPort'
  Lude.Natural ->
  -- | 'toPort'
  Lude.Natural ->
  -- | 'ipRange'
  Lude.Text ->
  -- | 'protocol'
  IPProtocol ->
  IPPermission
mkIPPermission pFromPort_ pToPort_ pIPRange_ pProtocol_ =
  IPPermission'
    { fromPort = pFromPort_,
      toPort = pToPort_,
      ipRange = pIPRange_,
      protocol = pProtocol_
    }

-- | A starting value for a range of allowed port numbers.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipFromPort :: Lens.Lens' IPPermission Lude.Natural
ipFromPort = Lens.lens (fromPort :: IPPermission -> Lude.Natural) (\s a -> s {fromPort = a} :: IPPermission)
{-# DEPRECATED ipFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | An ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipToPort :: Lens.Lens' IPPermission Lude.Natural
ipToPort = Lens.lens (toPort :: IPPermission -> Lude.Natural) (\s a -> s {toPort = a} :: IPPermission)
{-# DEPRECATED ipToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | A range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
--
-- /Note:/ Consider using 'ipRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIPRange :: Lens.Lens' IPPermission Lude.Text
ipIPRange = Lens.lens (ipRange :: IPPermission -> Lude.Text) (\s a -> s {ipRange = a} :: IPPermission)
{-# DEPRECATED ipIPRange "Use generic-lens or generic-optics with 'ipRange' instead." #-}

-- | The network communication protocol used by the fleet.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipProtocol :: Lens.Lens' IPPermission IPProtocol
ipProtocol = Lens.lens (protocol :: IPPermission -> IPProtocol) (\s a -> s {protocol = a} :: IPPermission)
{-# DEPRECATED ipProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Lude.FromJSON IPPermission where
  parseJSON =
    Lude.withObject
      "IPPermission"
      ( \x ->
          IPPermission'
            Lude.<$> (x Lude..: "FromPort")
            Lude.<*> (x Lude..: "ToPort")
            Lude.<*> (x Lude..: "IpRange")
            Lude.<*> (x Lude..: "Protocol")
      )

instance Lude.ToJSON IPPermission where
  toJSON IPPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FromPort" Lude..= fromPort),
            Lude.Just ("ToPort" Lude..= toPort),
            Lude.Just ("IpRange" Lude..= ipRange),
            Lude.Just ("Protocol" Lude..= protocol)
          ]
      )
