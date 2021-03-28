{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TunnelOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TunnelOption
  ( TunnelOption (..)
  -- * Smart constructor
  , mkTunnelOption
  -- * Lenses
  , toDpdTimeoutAction
  , toDpdTimeoutSeconds
  , toIkeVersions
  , toOutsideIpAddress
  , toPhase1DHGroupNumbers
  , toPhase1EncryptionAlgorithms
  , toPhase1IntegrityAlgorithms
  , toPhase1LifetimeSeconds
  , toPhase2DHGroupNumbers
  , toPhase2EncryptionAlgorithms
  , toPhase2IntegrityAlgorithms
  , toPhase2LifetimeSeconds
  , toPreSharedKey
  , toRekeyFuzzPercentage
  , toRekeyMarginTimeSeconds
  , toReplayWindowSize
  , toStartupAction
  , toTunnelInsideCidr
  , toTunnelInsideIpv6Cidr
  ) where

import qualified Network.AWS.EC2.Types.IKEVersionsListValue as Types
import qualified Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue as Types
import qualified Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue as Types
import qualified Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue as Types
import qualified Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue as Types
import qualified Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue as Types
import qualified Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The VPN tunnel options.
--
-- /See:/ 'mkTunnelOption' smart constructor.
data TunnelOption = TunnelOption'
  { dpdTimeoutAction :: Core.Maybe Core.Text
    -- ^ The action to take after a DPD timeout occurs.
  , dpdTimeoutSeconds :: Core.Maybe Core.Int
    -- ^ The number of seconds after which a DPD timeout occurs.
  , ikeVersions :: Core.Maybe [Types.IKEVersionsListValue]
    -- ^ The IKE versions that are permitted for the VPN tunnel.
  , outsideIpAddress :: Core.Maybe Core.Text
    -- ^ The external IP address of the VPN tunnel.
  , phase1DHGroupNumbers :: Core.Maybe [Types.Phase1DHGroupNumbersListValue]
    -- ^ The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 1 IKE negotiations.
  , phase1EncryptionAlgorithms :: Core.Maybe [Types.Phase1EncryptionAlgorithmsListValue]
    -- ^ The permitted encryption algorithms for the VPN tunnel for phase 1 IKE negotiations.
  , phase1IntegrityAlgorithms :: Core.Maybe [Types.Phase1IntegrityAlgorithmsListValue]
    -- ^ The permitted integrity algorithms for the VPN tunnel for phase 1 IKE negotiations.
  , phase1LifetimeSeconds :: Core.Maybe Core.Int
    -- ^ The lifetime for phase 1 of the IKE negotiation, in seconds.
  , phase2DHGroupNumbers :: Core.Maybe [Types.Phase2DHGroupNumbersListValue]
    -- ^ The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 2 IKE negotiations.
  , phase2EncryptionAlgorithms :: Core.Maybe [Types.Phase2EncryptionAlgorithmsListValue]
    -- ^ The permitted encryption algorithms for the VPN tunnel for phase 2 IKE negotiations.
  , phase2IntegrityAlgorithms :: Core.Maybe [Types.Phase2IntegrityAlgorithmsListValue]
    -- ^ The permitted integrity algorithms for the VPN tunnel for phase 2 IKE negotiations.
  , phase2LifetimeSeconds :: Core.Maybe Core.Int
    -- ^ The lifetime for phase 2 of the IKE negotiation, in seconds.
  , preSharedKey :: Core.Maybe Core.Text
    -- ^ The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
  , rekeyFuzzPercentage :: Core.Maybe Core.Int
    -- ^ The percentage of the rekey window determined by @RekeyMarginTimeSeconds@ during which the rekey time is randomly selected.
  , rekeyMarginTimeSeconds :: Core.Maybe Core.Int
    -- ^ The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey.
  , replayWindowSize :: Core.Maybe Core.Int
    -- ^ The number of packets in an IKE replay window.
  , startupAction :: Core.Maybe Core.Text
    -- ^ The action to take when the establishing the VPN tunnels for a VPN connection.
  , tunnelInsideCidr :: Core.Maybe Core.Text
    -- ^ The range of inside IPv4 addresses for the tunnel.
  , tunnelInsideIpv6Cidr :: Core.Maybe Core.Text
    -- ^ The range of inside IPv6 addresses for the tunnel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TunnelOption' value with any optional fields omitted.
mkTunnelOption
    :: TunnelOption
mkTunnelOption
  = TunnelOption'{dpdTimeoutAction = Core.Nothing,
                  dpdTimeoutSeconds = Core.Nothing, ikeVersions = Core.Nothing,
                  outsideIpAddress = Core.Nothing,
                  phase1DHGroupNumbers = Core.Nothing,
                  phase1EncryptionAlgorithms = Core.Nothing,
                  phase1IntegrityAlgorithms = Core.Nothing,
                  phase1LifetimeSeconds = Core.Nothing,
                  phase2DHGroupNumbers = Core.Nothing,
                  phase2EncryptionAlgorithms = Core.Nothing,
                  phase2IntegrityAlgorithms = Core.Nothing,
                  phase2LifetimeSeconds = Core.Nothing, preSharedKey = Core.Nothing,
                  rekeyFuzzPercentage = Core.Nothing,
                  rekeyMarginTimeSeconds = Core.Nothing,
                  replayWindowSize = Core.Nothing, startupAction = Core.Nothing,
                  tunnelInsideCidr = Core.Nothing,
                  tunnelInsideIpv6Cidr = Core.Nothing}

-- | The action to take after a DPD timeout occurs.
--
-- /Note:/ Consider using 'dpdTimeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toDpdTimeoutAction :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
toDpdTimeoutAction = Lens.field @"dpdTimeoutAction"
{-# INLINEABLE toDpdTimeoutAction #-}
{-# DEPRECATED dpdTimeoutAction "Use generic-lens or generic-optics with 'dpdTimeoutAction' instead"  #-}

-- | The number of seconds after which a DPD timeout occurs.
--
-- /Note:/ Consider using 'dpdTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toDpdTimeoutSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
toDpdTimeoutSeconds = Lens.field @"dpdTimeoutSeconds"
{-# INLINEABLE toDpdTimeoutSeconds #-}
{-# DEPRECATED dpdTimeoutSeconds "Use generic-lens or generic-optics with 'dpdTimeoutSeconds' instead"  #-}

-- | The IKE versions that are permitted for the VPN tunnel.
--
-- /Note:/ Consider using 'ikeVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toIkeVersions :: Lens.Lens' TunnelOption (Core.Maybe [Types.IKEVersionsListValue])
toIkeVersions = Lens.field @"ikeVersions"
{-# INLINEABLE toIkeVersions #-}
{-# DEPRECATED ikeVersions "Use generic-lens or generic-optics with 'ikeVersions' instead"  #-}

-- | The external IP address of the VPN tunnel.
--
-- /Note:/ Consider using 'outsideIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toOutsideIpAddress :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
toOutsideIpAddress = Lens.field @"outsideIpAddress"
{-# INLINEABLE toOutsideIpAddress #-}
{-# DEPRECATED outsideIpAddress "Use generic-lens or generic-optics with 'outsideIpAddress' instead"  #-}

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 1 IKE negotiations.
--
-- /Note:/ Consider using 'phase1DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1DHGroupNumbers :: Lens.Lens' TunnelOption (Core.Maybe [Types.Phase1DHGroupNumbersListValue])
toPhase1DHGroupNumbers = Lens.field @"phase1DHGroupNumbers"
{-# INLINEABLE toPhase1DHGroupNumbers #-}
{-# DEPRECATED phase1DHGroupNumbers "Use generic-lens or generic-optics with 'phase1DHGroupNumbers' instead"  #-}

-- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE negotiations.
--
-- /Note:/ Consider using 'phase1EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1EncryptionAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Types.Phase1EncryptionAlgorithmsListValue])
toPhase1EncryptionAlgorithms = Lens.field @"phase1EncryptionAlgorithms"
{-# INLINEABLE toPhase1EncryptionAlgorithms #-}
{-# DEPRECATED phase1EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase1EncryptionAlgorithms' instead"  #-}

-- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE negotiations.
--
-- /Note:/ Consider using 'phase1IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1IntegrityAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Types.Phase1IntegrityAlgorithmsListValue])
toPhase1IntegrityAlgorithms = Lens.field @"phase1IntegrityAlgorithms"
{-# INLINEABLE toPhase1IntegrityAlgorithms #-}
{-# DEPRECATED phase1IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase1IntegrityAlgorithms' instead"  #-}

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- /Note:/ Consider using 'phase1LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1LifetimeSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
toPhase1LifetimeSeconds = Lens.field @"phase1LifetimeSeconds"
{-# INLINEABLE toPhase1LifetimeSeconds #-}
{-# DEPRECATED phase1LifetimeSeconds "Use generic-lens or generic-optics with 'phase1LifetimeSeconds' instead"  #-}

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 2 IKE negotiations.
--
-- /Note:/ Consider using 'phase2DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2DHGroupNumbers :: Lens.Lens' TunnelOption (Core.Maybe [Types.Phase2DHGroupNumbersListValue])
toPhase2DHGroupNumbers = Lens.field @"phase2DHGroupNumbers"
{-# INLINEABLE toPhase2DHGroupNumbers #-}
{-# DEPRECATED phase2DHGroupNumbers "Use generic-lens or generic-optics with 'phase2DHGroupNumbers' instead"  #-}

-- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE negotiations.
--
-- /Note:/ Consider using 'phase2EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2EncryptionAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Types.Phase2EncryptionAlgorithmsListValue])
toPhase2EncryptionAlgorithms = Lens.field @"phase2EncryptionAlgorithms"
{-# INLINEABLE toPhase2EncryptionAlgorithms #-}
{-# DEPRECATED phase2EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase2EncryptionAlgorithms' instead"  #-}

-- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE negotiations.
--
-- /Note:/ Consider using 'phase2IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2IntegrityAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Types.Phase2IntegrityAlgorithmsListValue])
toPhase2IntegrityAlgorithms = Lens.field @"phase2IntegrityAlgorithms"
{-# INLINEABLE toPhase2IntegrityAlgorithms #-}
{-# DEPRECATED phase2IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase2IntegrityAlgorithms' instead"  #-}

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- /Note:/ Consider using 'phase2LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2LifetimeSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
toPhase2LifetimeSeconds = Lens.field @"phase2LifetimeSeconds"
{-# INLINEABLE toPhase2LifetimeSeconds #-}
{-# DEPRECATED phase2LifetimeSeconds "Use generic-lens or generic-optics with 'phase2LifetimeSeconds' instead"  #-}

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- /Note:/ Consider using 'preSharedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPreSharedKey :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
toPreSharedKey = Lens.field @"preSharedKey"
{-# INLINEABLE toPreSharedKey #-}
{-# DEPRECATED preSharedKey "Use generic-lens or generic-optics with 'preSharedKey' instead"  #-}

-- | The percentage of the rekey window determined by @RekeyMarginTimeSeconds@ during which the rekey time is randomly selected.
--
-- /Note:/ Consider using 'rekeyFuzzPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toRekeyFuzzPercentage :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
toRekeyFuzzPercentage = Lens.field @"rekeyFuzzPercentage"
{-# INLINEABLE toRekeyFuzzPercentage #-}
{-# DEPRECATED rekeyFuzzPercentage "Use generic-lens or generic-optics with 'rekeyFuzzPercentage' instead"  #-}

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey.
--
-- /Note:/ Consider using 'rekeyMarginTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toRekeyMarginTimeSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
toRekeyMarginTimeSeconds = Lens.field @"rekeyMarginTimeSeconds"
{-# INLINEABLE toRekeyMarginTimeSeconds #-}
{-# DEPRECATED rekeyMarginTimeSeconds "Use generic-lens or generic-optics with 'rekeyMarginTimeSeconds' instead"  #-}

-- | The number of packets in an IKE replay window.
--
-- /Note:/ Consider using 'replayWindowSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toReplayWindowSize :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
toReplayWindowSize = Lens.field @"replayWindowSize"
{-# INLINEABLE toReplayWindowSize #-}
{-# DEPRECATED replayWindowSize "Use generic-lens or generic-optics with 'replayWindowSize' instead"  #-}

-- | The action to take when the establishing the VPN tunnels for a VPN connection.
--
-- /Note:/ Consider using 'startupAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toStartupAction :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
toStartupAction = Lens.field @"startupAction"
{-# INLINEABLE toStartupAction #-}
{-# DEPRECATED startupAction "Use generic-lens or generic-optics with 'startupAction' instead"  #-}

-- | The range of inside IPv4 addresses for the tunnel.
--
-- /Note:/ Consider using 'tunnelInsideCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTunnelInsideCidr :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
toTunnelInsideCidr = Lens.field @"tunnelInsideCidr"
{-# INLINEABLE toTunnelInsideCidr #-}
{-# DEPRECATED tunnelInsideCidr "Use generic-lens or generic-optics with 'tunnelInsideCidr' instead"  #-}

-- | The range of inside IPv6 addresses for the tunnel.
--
-- /Note:/ Consider using 'tunnelInsideIpv6Cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTunnelInsideIpv6Cidr :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
toTunnelInsideIpv6Cidr = Lens.field @"tunnelInsideIpv6Cidr"
{-# INLINEABLE toTunnelInsideIpv6Cidr #-}
{-# DEPRECATED tunnelInsideIpv6Cidr "Use generic-lens or generic-optics with 'tunnelInsideIpv6Cidr' instead"  #-}

instance Core.FromXML TunnelOption where
        parseXML x
          = TunnelOption' Core.<$>
              (x Core..@? "dpdTimeoutAction") Core.<*>
                x Core..@? "dpdTimeoutSeconds"
                Core.<*>
                x Core..@? "ikeVersionSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "outsideIpAddress"
                Core.<*>
                x Core..@? "phase1DHGroupNumberSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "phase1EncryptionAlgorithmSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "phase1IntegrityAlgorithmSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "phase1LifetimeSeconds"
                Core.<*>
                x Core..@? "phase2DHGroupNumberSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "phase2EncryptionAlgorithmSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "phase2IntegrityAlgorithmSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "phase2LifetimeSeconds"
                Core.<*> x Core..@? "preSharedKey"
                Core.<*> x Core..@? "rekeyFuzzPercentage"
                Core.<*> x Core..@? "rekeyMarginTimeSeconds"
                Core.<*> x Core..@? "replayWindowSize"
                Core.<*> x Core..@? "startupAction"
                Core.<*> x Core..@? "tunnelInsideCidr"
                Core.<*> x Core..@? "tunnelInsideIpv6Cidr"
