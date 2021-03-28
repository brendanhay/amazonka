{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.IpPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.IpPermission
  ( IpPermission (..)
  -- * Smart constructor
  , mkIpPermission
  -- * Lenses
  , ipFromPort
  , ipToPort
  , ipIpRange
  , ipProtocol
  ) where

import qualified Network.AWS.GameLift.Types.IpProtocol as Types
import qualified Network.AWS.GameLift.Types.IpRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A range of IP addresses and port settings that allow inbound traffic to connect to server processes on an Amazon GameLift hosting resource. New game sessions that are started on the fleet are assigned an IP address/port number combination, which must fall into the fleet's allowed ranges. For fleets created with a custom game server, the ranges reflect the server's game session assignments. For Realtime Servers fleets, Amazon GameLift automatically opens two port ranges, one for TCP messaging and one for UDP for use by the Realtime servers.
--
-- /See:/ 'mkIpPermission' smart constructor.
data IpPermission = IpPermission'
  { fromPort :: Core.Natural
    -- ^ A starting value for a range of allowed port numbers.
  , toPort :: Core.Natural
    -- ^ An ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
  , ipRange :: Types.IpRange
    -- ^ A range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
  , protocol :: Types.IpProtocol
    -- ^ The network communication protocol used by the fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IpPermission' value with any optional fields omitted.
mkIpPermission
    :: Core.Natural -- ^ 'fromPort'
    -> Core.Natural -- ^ 'toPort'
    -> Types.IpRange -- ^ 'ipRange'
    -> Types.IpProtocol -- ^ 'protocol'
    -> IpPermission
mkIpPermission fromPort toPort ipRange protocol
  = IpPermission'{fromPort, toPort, ipRange, protocol}

-- | A starting value for a range of allowed port numbers.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipFromPort :: Lens.Lens' IpPermission Core.Natural
ipFromPort = Lens.field @"fromPort"
{-# INLINEABLE ipFromPort #-}
{-# DEPRECATED fromPort "Use generic-lens or generic-optics with 'fromPort' instead"  #-}

-- | An ending value for a range of allowed port numbers. Port numbers are end-inclusive. This value must be higher than @FromPort@ .
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipToPort :: Lens.Lens' IpPermission Core.Natural
ipToPort = Lens.field @"toPort"
{-# INLINEABLE ipToPort #-}
{-# DEPRECATED toPort "Use generic-lens or generic-optics with 'toPort' instead"  #-}

-- | A range of allowed IP addresses. This value must be expressed in CIDR notation. Example: "@000.000.000.000/[subnet mask]@ " or optionally the shortened version "@0.0.0.0/[subnet mask]@ ".
--
-- /Note:/ Consider using 'ipRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIpRange :: Lens.Lens' IpPermission Types.IpRange
ipIpRange = Lens.field @"ipRange"
{-# INLINEABLE ipIpRange #-}
{-# DEPRECATED ipRange "Use generic-lens or generic-optics with 'ipRange' instead"  #-}

-- | The network communication protocol used by the fleet.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipProtocol :: Lens.Lens' IpPermission Types.IpProtocol
ipProtocol = Lens.field @"protocol"
{-# INLINEABLE ipProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

instance Core.FromJSON IpPermission where
        toJSON IpPermission{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FromPort" Core..= fromPort),
                  Core.Just ("ToPort" Core..= toPort),
                  Core.Just ("IpRange" Core..= ipRange),
                  Core.Just ("Protocol" Core..= protocol)])

instance Core.FromJSON IpPermission where
        parseJSON
          = Core.withObject "IpPermission" Core.$
              \ x ->
                IpPermission' Core.<$>
                  (x Core..: "FromPort") Core.<*> x Core..: "ToPort" Core.<*>
                    x Core..: "IpRange"
                    Core.<*> x Core..: "Protocol"
