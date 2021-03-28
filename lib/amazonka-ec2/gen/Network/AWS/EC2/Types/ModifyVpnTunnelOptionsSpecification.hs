{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyVpnTunnelOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ModifyVpnTunnelOptionsSpecification
  ( ModifyVpnTunnelOptionsSpecification (..)
  -- * Smart constructor
  , mkModifyVpnTunnelOptionsSpecification
  -- * Lenses
  , mvtosDPDTimeoutAction
  , mvtosDPDTimeoutSeconds
  , mvtosIKEVersions
  , mvtosPhase1DHGroupNumbers
  , mvtosPhase1EncryptionAlgorithms
  , mvtosPhase1IntegrityAlgorithms
  , mvtosPhase1LifetimeSeconds
  , mvtosPhase2DHGroupNumbers
  , mvtosPhase2EncryptionAlgorithms
  , mvtosPhase2IntegrityAlgorithms
  , mvtosPhase2LifetimeSeconds
  , mvtosPreSharedKey
  , mvtosRekeyFuzzPercentage
  , mvtosRekeyMarginTimeSeconds
  , mvtosReplayWindowSize
  , mvtosStartupAction
  , mvtosTunnelInsideCidr
  , mvtosTunnelInsideIpv6Cidr
  ) where

import qualified Network.AWS.EC2.Types.IKEVersionsRequestListValue as Types
import qualified Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue as Types
import qualified Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue as Types
import qualified Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue as Types
import qualified Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue as Types
import qualified Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue as Types
import qualified Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS Site-to-Site VPN tunnel options to modify.
--
-- /See:/ 'mkModifyVpnTunnelOptionsSpecification' smart constructor.
data ModifyVpnTunnelOptionsSpecification = ModifyVpnTunnelOptionsSpecification'
  { dPDTimeoutAction :: Core.Maybe Core.Text
    -- ^ The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@ 
-- Default: @clear@ 
  , dPDTimeoutSeconds :: Core.Maybe Core.Int
    -- ^ The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value between 0 and 30.
-- Default: @30@ 
  , iKEVersions :: Core.Maybe [Types.IKEVersionsRequestListValue]
    -- ^ The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@ 
  , phase1DHGroupNumbers :: Core.Maybe [Types.Phase1DHGroupNumbersRequestListValue]
    -- ^ One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@ 
  , phase1EncryptionAlgorithms :: Core.Maybe [Types.Phase1EncryptionAlgorithmsRequestListValue]
    -- ^ One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@ 
  , phase1IntegrityAlgorithms :: Core.Maybe [Types.Phase1IntegrityAlgorithmsRequestListValue]
    -- ^ One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@ 
  , phase1LifetimeSeconds :: Core.Maybe Core.Int
    -- ^ The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
-- Default: @28800@ 
  , phase2DHGroupNumbers :: Core.Maybe [Types.Phase2DHGroupNumbersRequestListValue]
    -- ^ One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@ 
  , phase2EncryptionAlgorithms :: Core.Maybe [Types.Phase2EncryptionAlgorithmsRequestListValue]
    -- ^ One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@ 
  , phase2IntegrityAlgorithms :: Core.Maybe [Types.Phase2IntegrityAlgorithmsRequestListValue]
    -- ^ One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@ 
  , phase2LifetimeSeconds :: Core.Maybe Core.Int
    -- ^ The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ .
-- Default: @3600@ 
  , preSharedKey :: Core.Maybe Core.Text
    -- ^ The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
  , rekeyFuzzPercentage :: Core.Maybe Core.Int
    -- ^ The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected.
--
-- Constraints: A value between 0 and 100.
-- Default: @100@ 
  , rekeyMarginTimeSeconds :: Core.Maybe Core.Int
    -- ^ The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ .
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ .
-- Default: @540@ 
  , replayWindowSize :: Core.Maybe Core.Int
    -- ^ The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
-- Default: @1024@ 
  , startupAction :: Core.Maybe Core.Text
    -- ^ The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@ 
-- Default: @add@ 
  , tunnelInsideCidr :: Core.Maybe Core.Text
    -- ^ The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway. 
--
-- Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:
--
--     * @169.254.0.0/30@ 
--
--
--     * @169.254.1.0/30@ 
--
--
--     * @169.254.2.0/30@ 
--
--
--     * @169.254.3.0/30@ 
--
--
--     * @169.254.4.0/30@ 
--
--
--     * @169.254.5.0/30@ 
--
--
--     * @169.254.169.252/30@ 
--
--
  , tunnelInsideIpv6Cidr :: Core.Maybe Core.Text
    -- ^ The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway.
--
-- Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpnTunnelOptionsSpecification' value with any optional fields omitted.
mkModifyVpnTunnelOptionsSpecification
    :: ModifyVpnTunnelOptionsSpecification
mkModifyVpnTunnelOptionsSpecification
  = ModifyVpnTunnelOptionsSpecification'{dPDTimeoutAction =
                                           Core.Nothing,
                                         dPDTimeoutSeconds = Core.Nothing,
                                         iKEVersions = Core.Nothing,
                                         phase1DHGroupNumbers = Core.Nothing,
                                         phase1EncryptionAlgorithms = Core.Nothing,
                                         phase1IntegrityAlgorithms = Core.Nothing,
                                         phase1LifetimeSeconds = Core.Nothing,
                                         phase2DHGroupNumbers = Core.Nothing,
                                         phase2EncryptionAlgorithms = Core.Nothing,
                                         phase2IntegrityAlgorithms = Core.Nothing,
                                         phase2LifetimeSeconds = Core.Nothing,
                                         preSharedKey = Core.Nothing,
                                         rekeyFuzzPercentage = Core.Nothing,
                                         rekeyMarginTimeSeconds = Core.Nothing,
                                         replayWindowSize = Core.Nothing,
                                         startupAction = Core.Nothing,
                                         tunnelInsideCidr = Core.Nothing,
                                         tunnelInsideIpv6Cidr = Core.Nothing}

-- | The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@ 
-- Default: @clear@ 
--
-- /Note:/ Consider using 'dPDTimeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosDPDTimeoutAction :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Text)
mvtosDPDTimeoutAction = Lens.field @"dPDTimeoutAction"
{-# INLINEABLE mvtosDPDTimeoutAction #-}
{-# DEPRECATED dPDTimeoutAction "Use generic-lens or generic-optics with 'dPDTimeoutAction' instead"  #-}

-- | The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value between 0 and 30.
-- Default: @30@ 
--
-- /Note:/ Consider using 'dPDTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosDPDTimeoutSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Int)
mvtosDPDTimeoutSeconds = Lens.field @"dPDTimeoutSeconds"
{-# INLINEABLE mvtosDPDTimeoutSeconds #-}
{-# DEPRECATED dPDTimeoutSeconds "Use generic-lens or generic-optics with 'dPDTimeoutSeconds' instead"  #-}

-- | The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@ 
--
-- /Note:/ Consider using 'iKEVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosIKEVersions :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.IKEVersionsRequestListValue])
mvtosIKEVersions = Lens.field @"iKEVersions"
{-# INLINEABLE mvtosIKEVersions #-}
{-# DEPRECATED iKEVersions "Use generic-lens or generic-optics with 'iKEVersions' instead"  #-}

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@ 
--
-- /Note:/ Consider using 'phase1DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1DHGroupNumbers :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.Phase1DHGroupNumbersRequestListValue])
mvtosPhase1DHGroupNumbers = Lens.field @"phase1DHGroupNumbers"
{-# INLINEABLE mvtosPhase1DHGroupNumbers #-}
{-# DEPRECATED phase1DHGroupNumbers "Use generic-lens or generic-optics with 'phase1DHGroupNumbers' instead"  #-}

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@ 
--
-- /Note:/ Consider using 'phase1EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1EncryptionAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.Phase1EncryptionAlgorithmsRequestListValue])
mvtosPhase1EncryptionAlgorithms = Lens.field @"phase1EncryptionAlgorithms"
{-# INLINEABLE mvtosPhase1EncryptionAlgorithms #-}
{-# DEPRECATED phase1EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase1EncryptionAlgorithms' instead"  #-}

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@ 
--
-- /Note:/ Consider using 'phase1IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1IntegrityAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.Phase1IntegrityAlgorithmsRequestListValue])
mvtosPhase1IntegrityAlgorithms = Lens.field @"phase1IntegrityAlgorithms"
{-# INLINEABLE mvtosPhase1IntegrityAlgorithms #-}
{-# DEPRECATED phase1IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase1IntegrityAlgorithms' instead"  #-}

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
-- Default: @28800@ 
--
-- /Note:/ Consider using 'phase1LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1LifetimeSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Int)
mvtosPhase1LifetimeSeconds = Lens.field @"phase1LifetimeSeconds"
{-# INLINEABLE mvtosPhase1LifetimeSeconds #-}
{-# DEPRECATED phase1LifetimeSeconds "Use generic-lens or generic-optics with 'phase1LifetimeSeconds' instead"  #-}

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@ 
--
-- /Note:/ Consider using 'phase2DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2DHGroupNumbers :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.Phase2DHGroupNumbersRequestListValue])
mvtosPhase2DHGroupNumbers = Lens.field @"phase2DHGroupNumbers"
{-# INLINEABLE mvtosPhase2DHGroupNumbers #-}
{-# DEPRECATED phase2DHGroupNumbers "Use generic-lens or generic-optics with 'phase2DHGroupNumbers' instead"  #-}

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@ 
--
-- /Note:/ Consider using 'phase2EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2EncryptionAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.Phase2EncryptionAlgorithmsRequestListValue])
mvtosPhase2EncryptionAlgorithms = Lens.field @"phase2EncryptionAlgorithms"
{-# INLINEABLE mvtosPhase2EncryptionAlgorithms #-}
{-# DEPRECATED phase2EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase2EncryptionAlgorithms' instead"  #-}

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@ 
--
-- /Note:/ Consider using 'phase2IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2IntegrityAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe [Types.Phase2IntegrityAlgorithmsRequestListValue])
mvtosPhase2IntegrityAlgorithms = Lens.field @"phase2IntegrityAlgorithms"
{-# INLINEABLE mvtosPhase2IntegrityAlgorithms #-}
{-# DEPRECATED phase2IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase2IntegrityAlgorithms' instead"  #-}

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ .
-- Default: @3600@ 
--
-- /Note:/ Consider using 'phase2LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2LifetimeSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Int)
mvtosPhase2LifetimeSeconds = Lens.field @"phase2LifetimeSeconds"
{-# INLINEABLE mvtosPhase2LifetimeSeconds #-}
{-# DEPRECATED phase2LifetimeSeconds "Use generic-lens or generic-optics with 'phase2LifetimeSeconds' instead"  #-}

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
--
-- /Note:/ Consider using 'preSharedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPreSharedKey :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Text)
mvtosPreSharedKey = Lens.field @"preSharedKey"
{-# INLINEABLE mvtosPreSharedKey #-}
{-# DEPRECATED preSharedKey "Use generic-lens or generic-optics with 'preSharedKey' instead"  #-}

-- | The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected.
--
-- Constraints: A value between 0 and 100.
-- Default: @100@ 
--
-- /Note:/ Consider using 'rekeyFuzzPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosRekeyFuzzPercentage :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Int)
mvtosRekeyFuzzPercentage = Lens.field @"rekeyFuzzPercentage"
{-# INLINEABLE mvtosRekeyFuzzPercentage #-}
{-# DEPRECATED rekeyFuzzPercentage "Use generic-lens or generic-optics with 'rekeyFuzzPercentage' instead"  #-}

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ .
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ .
-- Default: @540@ 
--
-- /Note:/ Consider using 'rekeyMarginTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosRekeyMarginTimeSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Int)
mvtosRekeyMarginTimeSeconds = Lens.field @"rekeyMarginTimeSeconds"
{-# INLINEABLE mvtosRekeyMarginTimeSeconds #-}
{-# DEPRECATED rekeyMarginTimeSeconds "Use generic-lens or generic-optics with 'rekeyMarginTimeSeconds' instead"  #-}

-- | The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
-- Default: @1024@ 
--
-- /Note:/ Consider using 'replayWindowSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosReplayWindowSize :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Int)
mvtosReplayWindowSize = Lens.field @"replayWindowSize"
{-# INLINEABLE mvtosReplayWindowSize #-}
{-# DEPRECATED replayWindowSize "Use generic-lens or generic-optics with 'replayWindowSize' instead"  #-}

-- | The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@ 
-- Default: @add@ 
--
-- /Note:/ Consider using 'startupAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosStartupAction :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Text)
mvtosStartupAction = Lens.field @"startupAction"
{-# INLINEABLE mvtosStartupAction #-}
{-# DEPRECATED startupAction "Use generic-lens or generic-optics with 'startupAction' instead"  #-}

-- | The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway. 
--
-- Constraints: A size /30 CIDR block from the @169.254.0.0/16@ range. The following CIDR blocks are reserved and cannot be used:
--
--     * @169.254.0.0/30@ 
--
--
--     * @169.254.1.0/30@ 
--
--
--     * @169.254.2.0/30@ 
--
--
--     * @169.254.3.0/30@ 
--
--
--     * @169.254.4.0/30@ 
--
--
--     * @169.254.5.0/30@ 
--
--
--     * @169.254.169.252/30@ 
--
--
--
-- /Note:/ Consider using 'tunnelInsideCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosTunnelInsideCidr :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Text)
mvtosTunnelInsideCidr = Lens.field @"tunnelInsideCidr"
{-# INLINEABLE mvtosTunnelInsideCidr #-}
{-# DEPRECATED tunnelInsideCidr "Use generic-lens or generic-optics with 'tunnelInsideCidr' instead"  #-}

-- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway.
--
-- Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
--
-- /Note:/ Consider using 'tunnelInsideIpv6Cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosTunnelInsideIpv6Cidr :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Core.Maybe Core.Text)
mvtosTunnelInsideIpv6Cidr = Lens.field @"tunnelInsideIpv6Cidr"
{-# INLINEABLE mvtosTunnelInsideIpv6Cidr #-}
{-# DEPRECATED tunnelInsideIpv6Cidr "Use generic-lens or generic-optics with 'tunnelInsideIpv6Cidr' instead"  #-}

instance Core.ToQuery ModifyVpnTunnelOptionsSpecification where
        toQuery ModifyVpnTunnelOptionsSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DPDTimeoutAction")
              dPDTimeoutAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DPDTimeoutSeconds")
                dPDTimeoutSeconds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "IKEVersion") iKEVersions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Phase1DHGroupNumber")
                phase1DHGroupNumbers
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "Phase1EncryptionAlgorithm")
                phase1EncryptionAlgorithms
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "Phase1IntegrityAlgorithm")
                phase1IntegrityAlgorithms
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Phase1LifetimeSeconds")
                phase1LifetimeSeconds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Phase2DHGroupNumber")
                phase2DHGroupNumbers
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "Phase2EncryptionAlgorithm")
                phase2EncryptionAlgorithms
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "Phase2IntegrityAlgorithm")
                phase2IntegrityAlgorithms
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Phase2LifetimeSeconds")
                phase2LifetimeSeconds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreSharedKey")
                preSharedKey
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RekeyFuzzPercentage")
                rekeyFuzzPercentage
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RekeyMarginTimeSeconds")
                rekeyMarginTimeSeconds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplayWindowSize")
                replayWindowSize
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartupAction")
                startupAction
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TunnelInsideCidr")
                tunnelInsideCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TunnelInsideIpv6Cidr")
                tunnelInsideIpv6Cidr
