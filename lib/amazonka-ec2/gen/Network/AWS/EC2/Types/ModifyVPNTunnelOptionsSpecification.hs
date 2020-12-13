{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyVPNTunnelOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ModifyVPNTunnelOptionsSpecification
  ( ModifyVPNTunnelOptionsSpecification (..),

    -- * Smart constructor
    mkModifyVPNTunnelOptionsSpecification,

    -- * Lenses
    mvtosReplayWindowSize,
    mvtosDPDTimeoutAction,
    mvtosRekeyFuzzPercentage,
    mvtosPhase1LifetimeSeconds,
    mvtosIKEVersions,
    mvtosPhase2IntegrityAlgorithms,
    mvtosPhase2LifetimeSeconds,
    mvtosPhase1EncryptionAlgorithms,
    mvtosPhase1DHGroupNumbers,
    mvtosPhase1IntegrityAlgorithms,
    mvtosRekeyMarginTimeSeconds,
    mvtosDPDTimeoutSeconds,
    mvtosTunnelInsideCidr,
    mvtosStartupAction,
    mvtosPhase2EncryptionAlgorithms,
    mvtosPhase2DHGroupNumbers,
    mvtosPreSharedKey,
    mvtosTunnelInsideIPv6Cidr,
  )
where

import Network.AWS.EC2.Types.IKEVersionsRequestListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS Site-to-Site VPN tunnel options to modify.
--
-- /See:/ 'mkModifyVPNTunnelOptionsSpecification' smart constructor.
data ModifyVPNTunnelOptionsSpecification = ModifyVPNTunnelOptionsSpecification'
  { -- | The number of packets in an IKE replay window.
    --
    -- Constraints: A value between 64 and 2048.
    -- Default: @1024@
    replayWindowSize :: Lude.Maybe Lude.Int,
    -- | The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session.
    --
    -- Valid Values: @clear@ | @none@ | @restart@
    -- Default: @clear@
    dPDTimeoutAction :: Lude.Maybe Lude.Text,
    -- | The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected.
    --
    -- Constraints: A value between 0 and 100.
    -- Default: @100@
    rekeyFuzzPercentage :: Lude.Maybe Lude.Int,
    -- | The lifetime for phase 1 of the IKE negotiation, in seconds.
    --
    -- Constraints: A value between 900 and 28,800.
    -- Default: @28800@
    phase1LifetimeSeconds :: Lude.Maybe Lude.Int,
    -- | The IKE versions that are permitted for the VPN tunnel.
    --
    -- Valid values: @ikev1@ | @ikev2@
    iKEVersions :: Lude.Maybe [IKEVersionsRequestListValue],
    -- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
    --
    -- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
    phase2IntegrityAlgorithms :: Lude.Maybe [Phase2IntegrityAlgorithmsRequestListValue],
    -- | The lifetime for phase 2 of the IKE negotiation, in seconds.
    --
    -- Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ .
    -- Default: @3600@
    phase2LifetimeSeconds :: Lude.Maybe Lude.Int,
    -- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
    --
    -- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
    phase1EncryptionAlgorithms :: Lude.Maybe [Phase1EncryptionAlgorithmsRequestListValue],
    -- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations.
    --
    -- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
    phase1DHGroupNumbers :: Lude.Maybe [Phase1DHGroupNumbersRequestListValue],
    -- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
    --
    -- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
    phase1IntegrityAlgorithms :: Lude.Maybe [Phase1IntegrityAlgorithmsRequestListValue],
    -- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ .
    --
    -- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ .
    -- Default: @540@
    rekeyMarginTimeSeconds :: Lude.Maybe Lude.Int,
    -- | The number of seconds after which a DPD timeout occurs.
    --
    -- Constraints: A value between 0 and 30.
    -- Default: @30@
    dPDTimeoutSeconds :: Lude.Maybe Lude.Int,
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
    tunnelInsideCidr :: Lude.Maybe Lude.Text,
    -- | The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation.
    --
    -- Valid Values: @add@ | @start@
    -- Default: @add@
    startupAction :: Lude.Maybe Lude.Text,
    -- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
    --
    -- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
    phase2EncryptionAlgorithms :: Lude.Maybe [Phase2EncryptionAlgorithmsRequestListValue],
    -- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations.
    --
    -- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
    phase2DHGroupNumbers :: Lude.Maybe [Phase2DHGroupNumbersRequestListValue],
    -- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
    --
    -- Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
    preSharedKey :: Lude.Maybe Lude.Text,
    -- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway.
    --
    -- Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
    tunnelInsideIPv6Cidr :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPNTunnelOptionsSpecification' with the minimum fields required to make a request.
--
-- * 'replayWindowSize' - The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
-- Default: @1024@
-- * 'dPDTimeoutAction' - The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@
-- Default: @clear@
-- * 'rekeyFuzzPercentage' - The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected.
--
-- Constraints: A value between 0 and 100.
-- Default: @100@
-- * 'phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
-- Default: @28800@
-- * 'iKEVersions' - The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@
-- * 'phase2IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
-- * 'phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ .
-- Default: @3600@
-- * 'phase1EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
-- * 'phase1DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
-- * 'phase1IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
-- * 'rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ .
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ .
-- Default: @540@
-- * 'dPDTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value between 0 and 30.
-- Default: @30@
-- * 'tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same virtual private gateway.
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
-- * 'startupAction' - The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@
-- Default: @add@
-- * 'phase2EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
-- * 'phase2DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
-- * 'preSharedKey' - The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
-- * 'tunnelInsideIPv6Cidr' - The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway.
--
-- Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
mkModifyVPNTunnelOptionsSpecification ::
  ModifyVPNTunnelOptionsSpecification
mkModifyVPNTunnelOptionsSpecification =
  ModifyVPNTunnelOptionsSpecification'
    { replayWindowSize =
        Lude.Nothing,
      dPDTimeoutAction = Lude.Nothing,
      rekeyFuzzPercentage = Lude.Nothing,
      phase1LifetimeSeconds = Lude.Nothing,
      iKEVersions = Lude.Nothing,
      phase2IntegrityAlgorithms = Lude.Nothing,
      phase2LifetimeSeconds = Lude.Nothing,
      phase1EncryptionAlgorithms = Lude.Nothing,
      phase1DHGroupNumbers = Lude.Nothing,
      phase1IntegrityAlgorithms = Lude.Nothing,
      rekeyMarginTimeSeconds = Lude.Nothing,
      dPDTimeoutSeconds = Lude.Nothing,
      tunnelInsideCidr = Lude.Nothing,
      startupAction = Lude.Nothing,
      phase2EncryptionAlgorithms = Lude.Nothing,
      phase2DHGroupNumbers = Lude.Nothing,
      preSharedKey = Lude.Nothing,
      tunnelInsideIPv6Cidr = Lude.Nothing
    }

-- | The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
-- Default: @1024@
--
-- /Note:/ Consider using 'replayWindowSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosReplayWindowSize :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Int)
mvtosReplayWindowSize = Lens.lens (replayWindowSize :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Int) (\s a -> s {replayWindowSize = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosReplayWindowSize "Use generic-lens or generic-optics with 'replayWindowSize' instead." #-}

-- | The action to take after DPD timeout occurs. Specify @restart@ to restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@
-- Default: @clear@
--
-- /Note:/ Consider using 'dPDTimeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosDPDTimeoutAction :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Text)
mvtosDPDTimeoutAction = Lens.lens (dPDTimeoutAction :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {dPDTimeoutAction = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosDPDTimeoutAction "Use generic-lens or generic-optics with 'dPDTimeoutAction' instead." #-}

-- | The percentage of the rekey window (determined by @RekeyMarginTimeSeconds@ ) during which the rekey time is randomly selected.
--
-- Constraints: A value between 0 and 100.
-- Default: @100@
--
-- /Note:/ Consider using 'rekeyFuzzPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosRekeyFuzzPercentage :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Int)
mvtosRekeyFuzzPercentage = Lens.lens (rekeyFuzzPercentage :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Int) (\s a -> s {rekeyFuzzPercentage = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosRekeyFuzzPercentage "Use generic-lens or generic-optics with 'rekeyFuzzPercentage' instead." #-}

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
-- Default: @28800@
--
-- /Note:/ Consider using 'phase1LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1LifetimeSeconds :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Int)
mvtosPhase1LifetimeSeconds = Lens.lens (phase1LifetimeSeconds :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Int) (\s a -> s {phase1LifetimeSeconds = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase1LifetimeSeconds "Use generic-lens or generic-optics with 'phase1LifetimeSeconds' instead." #-}

-- | The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@
--
-- /Note:/ Consider using 'iKEVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosIKEVersions :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [IKEVersionsRequestListValue])
mvtosIKEVersions = Lens.lens (iKEVersions :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [IKEVersionsRequestListValue]) (\s a -> s {iKEVersions = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosIKEVersions "Use generic-lens or generic-optics with 'iKEVersions' instead." #-}

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- /Note:/ Consider using 'phase2IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2IntegrityAlgorithms :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [Phase2IntegrityAlgorithmsRequestListValue])
mvtosPhase2IntegrityAlgorithms = Lens.lens (phase2IntegrityAlgorithms :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [Phase2IntegrityAlgorithmsRequestListValue]) (\s a -> s {phase2IntegrityAlgorithms = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase2IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase2IntegrityAlgorithms' instead." #-}

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than the value for @Phase1LifetimeSeconds@ .
-- Default: @3600@
--
-- /Note:/ Consider using 'phase2LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2LifetimeSeconds :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Int)
mvtosPhase2LifetimeSeconds = Lens.lens (phase2LifetimeSeconds :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Int) (\s a -> s {phase2LifetimeSeconds = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase2LifetimeSeconds "Use generic-lens or generic-optics with 'phase2LifetimeSeconds' instead." #-}

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- /Note:/ Consider using 'phase1EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1EncryptionAlgorithms :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [Phase1EncryptionAlgorithmsRequestListValue])
mvtosPhase1EncryptionAlgorithms = Lens.lens (phase1EncryptionAlgorithms :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [Phase1EncryptionAlgorithmsRequestListValue]) (\s a -> s {phase1EncryptionAlgorithms = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase1EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase1EncryptionAlgorithms' instead." #-}

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
--
-- /Note:/ Consider using 'phase1DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1DHGroupNumbers :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [Phase1DHGroupNumbersRequestListValue])
mvtosPhase1DHGroupNumbers = Lens.lens (phase1DHGroupNumbers :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [Phase1DHGroupNumbersRequestListValue]) (\s a -> s {phase1DHGroupNumbers = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase1DHGroupNumbers "Use generic-lens or generic-optics with 'phase1DHGroupNumbers' instead." #-}

-- | One or more integrity algorithms that are permitted for the VPN tunnel for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- /Note:/ Consider using 'phase1IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase1IntegrityAlgorithms :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [Phase1IntegrityAlgorithmsRequestListValue])
mvtosPhase1IntegrityAlgorithms = Lens.lens (phase1IntegrityAlgorithms :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [Phase1IntegrityAlgorithmsRequestListValue]) (\s a -> s {phase1IntegrityAlgorithms = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase1IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase1IntegrityAlgorithms' instead." #-}

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey. The exact time of the rekey is randomly selected based on the value for @RekeyFuzzPercentage@ .
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@ .
-- Default: @540@
--
-- /Note:/ Consider using 'rekeyMarginTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosRekeyMarginTimeSeconds :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Int)
mvtosRekeyMarginTimeSeconds = Lens.lens (rekeyMarginTimeSeconds :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Int) (\s a -> s {rekeyMarginTimeSeconds = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosRekeyMarginTimeSeconds "Use generic-lens or generic-optics with 'rekeyMarginTimeSeconds' instead." #-}

-- | The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value between 0 and 30.
-- Default: @30@
--
-- /Note:/ Consider using 'dPDTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosDPDTimeoutSeconds :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Int)
mvtosDPDTimeoutSeconds = Lens.lens (dPDTimeoutSeconds :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Int) (\s a -> s {dPDTimeoutSeconds = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosDPDTimeoutSeconds "Use generic-lens or generic-optics with 'dPDTimeoutSeconds' instead." #-}

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
mvtosTunnelInsideCidr :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Text)
mvtosTunnelInsideCidr = Lens.lens (tunnelInsideCidr :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {tunnelInsideCidr = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosTunnelInsideCidr "Use generic-lens or generic-optics with 'tunnelInsideCidr' instead." #-}

-- | The action to take when the establishing the tunnel for the VPN connection. By default, your customer gateway device must initiate the IKE negotiation and bring up the tunnel. Specify @start@ for AWS to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@
-- Default: @add@
--
-- /Note:/ Consider using 'startupAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosStartupAction :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Text)
mvtosStartupAction = Lens.lens (startupAction :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {startupAction = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosStartupAction "Use generic-lens or generic-optics with 'startupAction' instead." #-}

-- | One or more encryption algorithms that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- /Note:/ Consider using 'phase2EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2EncryptionAlgorithms :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [Phase2EncryptionAlgorithmsRequestListValue])
mvtosPhase2EncryptionAlgorithms = Lens.lens (phase2EncryptionAlgorithms :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [Phase2EncryptionAlgorithmsRequestListValue]) (\s a -> s {phase2EncryptionAlgorithms = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase2EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase2EncryptionAlgorithms' instead." #-}

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ | @21@ | @22@ | @23@ | @24@
--
-- /Note:/ Consider using 'phase2DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPhase2DHGroupNumbers :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe [Phase2DHGroupNumbersRequestListValue])
mvtosPhase2DHGroupNumbers = Lens.lens (phase2DHGroupNumbers :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe [Phase2DHGroupNumbersRequestListValue]) (\s a -> s {phase2DHGroupNumbers = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPhase2DHGroupNumbers "Use generic-lens or generic-optics with 'phase2DHGroupNumbers' instead." #-}

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods (.), and underscores (_). Must be between 8 and 64 characters in length and cannot start with zero (0).
--
-- /Note:/ Consider using 'preSharedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosPreSharedKey :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Text)
mvtosPreSharedKey = Lens.lens (preSharedKey :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {preSharedKey = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosPreSharedKey "Use generic-lens or generic-optics with 'preSharedKey' instead." #-}

-- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR blocks must be unique across all VPN connections that use the same transit gateway.
--
-- Constraints: A size /126 CIDR block from the local @fd00::/8@ range.
--
-- /Note:/ Consider using 'tunnelInsideIPv6Cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtosTunnelInsideIPv6Cidr :: Lens.Lens' ModifyVPNTunnelOptionsSpecification (Lude.Maybe Lude.Text)
mvtosTunnelInsideIPv6Cidr = Lens.lens (tunnelInsideIPv6Cidr :: ModifyVPNTunnelOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {tunnelInsideIPv6Cidr = a} :: ModifyVPNTunnelOptionsSpecification)
{-# DEPRECATED mvtosTunnelInsideIPv6Cidr "Use generic-lens or generic-optics with 'tunnelInsideIPv6Cidr' instead." #-}

instance Lude.ToQuery ModifyVPNTunnelOptionsSpecification where
  toQuery ModifyVPNTunnelOptionsSpecification' {..} =
    Lude.mconcat
      [ "ReplayWindowSize" Lude.=: replayWindowSize,
        "DPDTimeoutAction" Lude.=: dPDTimeoutAction,
        "RekeyFuzzPercentage" Lude.=: rekeyFuzzPercentage,
        "Phase1LifetimeSeconds" Lude.=: phase1LifetimeSeconds,
        Lude.toQuery (Lude.toQueryList "IKEVersion" Lude.<$> iKEVersions),
        Lude.toQuery
          ( Lude.toQueryList "Phase2IntegrityAlgorithm"
              Lude.<$> phase2IntegrityAlgorithms
          ),
        "Phase2LifetimeSeconds" Lude.=: phase2LifetimeSeconds,
        Lude.toQuery
          ( Lude.toQueryList "Phase1EncryptionAlgorithm"
              Lude.<$> phase1EncryptionAlgorithms
          ),
        Lude.toQuery
          ( Lude.toQueryList "Phase1DHGroupNumber"
              Lude.<$> phase1DHGroupNumbers
          ),
        Lude.toQuery
          ( Lude.toQueryList "Phase1IntegrityAlgorithm"
              Lude.<$> phase1IntegrityAlgorithms
          ),
        "RekeyMarginTimeSeconds" Lude.=: rekeyMarginTimeSeconds,
        "DPDTimeoutSeconds" Lude.=: dPDTimeoutSeconds,
        "TunnelInsideCidr" Lude.=: tunnelInsideCidr,
        "StartupAction" Lude.=: startupAction,
        Lude.toQuery
          ( Lude.toQueryList "Phase2EncryptionAlgorithm"
              Lude.<$> phase2EncryptionAlgorithms
          ),
        Lude.toQuery
          ( Lude.toQueryList "Phase2DHGroupNumber"
              Lude.<$> phase2DHGroupNumbers
          ),
        "PreSharedKey" Lude.=: preSharedKey,
        "TunnelInsideIpv6Cidr" Lude.=: tunnelInsideIPv6Cidr
      ]
