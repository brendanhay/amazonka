{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.VpnTunnelOptionsSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnTunnelOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IKEVersionsRequestListValue
import Amazonka.EC2.Types.Phase1DHGroupNumbersRequestListValue
import Amazonka.EC2.Types.Phase1EncryptionAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase2DHGroupNumbersRequestListValue
import Amazonka.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
import Amazonka.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
import Amazonka.EC2.Types.VpnTunnelLogOptionsSpecification
import qualified Amazonka.Prelude as Prelude

-- | The tunnel options for a single VPN tunnel.
--
-- /See:/ 'newVpnTunnelOptionsSpecification' smart constructor.
data VpnTunnelOptionsSpecification = VpnTunnelOptionsSpecification'
  { -- | The action to take after DPD timeout occurs. Specify @restart@ to
    -- restart the IKE initiation. Specify @clear@ to end the IKE session.
    --
    -- Valid Values: @clear@ | @none@ | @restart@
    --
    -- Default: @clear@
    dPDTimeoutAction :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds after which a DPD timeout occurs.
    --
    -- Constraints: A value greater than or equal to 30.
    --
    -- Default: @30@
    dPDTimeoutSeconds :: Prelude.Maybe Prelude.Int,
    -- | The IKE versions that are permitted for the VPN tunnel.
    --
    -- Valid values: @ikev1@ | @ikev2@
    iKEVersions :: Prelude.Maybe [IKEVersionsRequestListValue],
    -- | Options for logging VPN tunnel activity.
    logOptions :: Prelude.Maybe VpnTunnelLogOptionsSpecification,
    -- | One or more Diffie-Hellman group numbers that are permitted for the VPN
    -- tunnel for phase 1 IKE negotiations.
    --
    -- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ |
    -- @21@ | @22@ | @23@ | @24@
    phase1DHGroupNumbers :: Prelude.Maybe [Phase1DHGroupNumbersRequestListValue],
    -- | One or more encryption algorithms that are permitted for the VPN tunnel
    -- for phase 1 IKE negotiations.
    --
    -- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
    phase1EncryptionAlgorithms :: Prelude.Maybe [Phase1EncryptionAlgorithmsRequestListValue],
    -- | One or more integrity algorithms that are permitted for the VPN tunnel
    -- for phase 1 IKE negotiations.
    --
    -- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
    phase1IntegrityAlgorithms :: Prelude.Maybe [Phase1IntegrityAlgorithmsRequestListValue],
    -- | The lifetime for phase 1 of the IKE negotiation, in seconds.
    --
    -- Constraints: A value between 900 and 28,800.
    --
    -- Default: @28800@
    phase1LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | One or more Diffie-Hellman group numbers that are permitted for the VPN
    -- tunnel for phase 2 IKE negotiations.
    --
    -- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@
    -- | @21@ | @22@ | @23@ | @24@
    phase2DHGroupNumbers :: Prelude.Maybe [Phase2DHGroupNumbersRequestListValue],
    -- | One or more encryption algorithms that are permitted for the VPN tunnel
    -- for phase 2 IKE negotiations.
    --
    -- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
    phase2EncryptionAlgorithms :: Prelude.Maybe [Phase2EncryptionAlgorithmsRequestListValue],
    -- | One or more integrity algorithms that are permitted for the VPN tunnel
    -- for phase 2 IKE negotiations.
    --
    -- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
    phase2IntegrityAlgorithms :: Prelude.Maybe [Phase2IntegrityAlgorithmsRequestListValue],
    -- | The lifetime for phase 2 of the IKE negotiation, in seconds.
    --
    -- Constraints: A value between 900 and 3,600. The value must be less than
    -- the value for @Phase1LifetimeSeconds@.
    --
    -- Default: @3600@
    phase2LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The pre-shared key (PSK) to establish initial authentication between the
    -- virtual private gateway and customer gateway.
    --
    -- Constraints: Allowed characters are alphanumeric characters, periods
    -- (.), and underscores (_). Must be between 8 and 64 characters in length
    -- and cannot start with zero (0).
    preSharedKey :: Prelude.Maybe Prelude.Text,
    -- | The percentage of the rekey window (determined by
    -- @RekeyMarginTimeSeconds@) during which the rekey time is randomly
    -- selected.
    --
    -- Constraints: A value between 0 and 100.
    --
    -- Default: @100@
    rekeyFuzzPercentage :: Prelude.Maybe Prelude.Int,
    -- | The margin time, in seconds, before the phase 2 lifetime expires, during
    -- which the Amazon Web Services side of the VPN connection performs an IKE
    -- rekey. The exact time of the rekey is randomly selected based on the
    -- value for @RekeyFuzzPercentage@.
    --
    -- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@.
    --
    -- Default: @540@
    rekeyMarginTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The number of packets in an IKE replay window.
    --
    -- Constraints: A value between 64 and 2048.
    --
    -- Default: @1024@
    replayWindowSize :: Prelude.Maybe Prelude.Int,
    -- | The action to take when the establishing the tunnel for the VPN
    -- connection. By default, your customer gateway device must initiate the
    -- IKE negotiation and bring up the tunnel. Specify @start@ for Amazon Web
    -- Services to initiate the IKE negotiation.
    --
    -- Valid Values: @add@ | @start@
    --
    -- Default: @add@
    startupAction :: Prelude.Maybe Prelude.Text,
    -- | The range of inside IPv4 addresses for the tunnel. Any specified CIDR
    -- blocks must be unique across all VPN connections that use the same
    -- virtual private gateway.
    --
    -- Constraints: A size \/30 CIDR block from the @169.254.0.0\/16@ range.
    -- The following CIDR blocks are reserved and cannot be used:
    --
    -- -   @169.254.0.0\/30@
    --
    -- -   @169.254.1.0\/30@
    --
    -- -   @169.254.2.0\/30@
    --
    -- -   @169.254.3.0\/30@
    --
    -- -   @169.254.4.0\/30@
    --
    -- -   @169.254.5.0\/30@
    --
    -- -   @169.254.169.252\/30@
    tunnelInsideCidr :: Prelude.Maybe Prelude.Text,
    -- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR
    -- blocks must be unique across all VPN connections that use the same
    -- transit gateway.
    --
    -- Constraints: A size \/126 CIDR block from the local @fd00::\/8@ range.
    tunnelInsideIpv6Cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnTunnelOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dPDTimeoutAction', 'vpnTunnelOptionsSpecification_dPDTimeoutAction' - The action to take after DPD timeout occurs. Specify @restart@ to
-- restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@
--
-- Default: @clear@
--
-- 'dPDTimeoutSeconds', 'vpnTunnelOptionsSpecification_dPDTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value greater than or equal to 30.
--
-- Default: @30@
--
-- 'iKEVersions', 'vpnTunnelOptionsSpecification_iKEVersions' - The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@
--
-- 'logOptions', 'vpnTunnelOptionsSpecification_logOptions' - Options for logging VPN tunnel activity.
--
-- 'phase1DHGroupNumbers', 'vpnTunnelOptionsSpecification_phase1DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ |
-- @21@ | @22@ | @23@ | @24@
--
-- 'phase1EncryptionAlgorithms', 'vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- 'phase1IntegrityAlgorithms', 'vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- 'phase1LifetimeSeconds', 'vpnTunnelOptionsSpecification_phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
--
-- Default: @28800@
--
-- 'phase2DHGroupNumbers', 'vpnTunnelOptionsSpecification_phase2DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@
-- | @21@ | @22@ | @23@ | @24@
--
-- 'phase2EncryptionAlgorithms', 'vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- 'phase2IntegrityAlgorithms', 'vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- 'phase2LifetimeSeconds', 'vpnTunnelOptionsSpecification_phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than
-- the value for @Phase1LifetimeSeconds@.
--
-- Default: @3600@
--
-- 'preSharedKey', 'vpnTunnelOptionsSpecification_preSharedKey' - The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods
-- (.), and underscores (_). Must be between 8 and 64 characters in length
-- and cannot start with zero (0).
--
-- 'rekeyFuzzPercentage', 'vpnTunnelOptionsSpecification_rekeyFuzzPercentage' - The percentage of the rekey window (determined by
-- @RekeyMarginTimeSeconds@) during which the rekey time is randomly
-- selected.
--
-- Constraints: A value between 0 and 100.
--
-- Default: @100@
--
-- 'rekeyMarginTimeSeconds', 'vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey. The exact time of the rekey is randomly selected based on the
-- value for @RekeyFuzzPercentage@.
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@.
--
-- Default: @540@
--
-- 'replayWindowSize', 'vpnTunnelOptionsSpecification_replayWindowSize' - The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
--
-- Default: @1024@
--
-- 'startupAction', 'vpnTunnelOptionsSpecification_startupAction' - The action to take when the establishing the tunnel for the VPN
-- connection. By default, your customer gateway device must initiate the
-- IKE negotiation and bring up the tunnel. Specify @start@ for Amazon Web
-- Services to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@
--
-- Default: @add@
--
-- 'tunnelInsideCidr', 'vpnTunnelOptionsSpecification_tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel. Any specified CIDR
-- blocks must be unique across all VPN connections that use the same
-- virtual private gateway.
--
-- Constraints: A size \/30 CIDR block from the @169.254.0.0\/16@ range.
-- The following CIDR blocks are reserved and cannot be used:
--
-- -   @169.254.0.0\/30@
--
-- -   @169.254.1.0\/30@
--
-- -   @169.254.2.0\/30@
--
-- -   @169.254.3.0\/30@
--
-- -   @169.254.4.0\/30@
--
-- -   @169.254.5.0\/30@
--
-- -   @169.254.169.252\/30@
--
-- 'tunnelInsideIpv6Cidr', 'vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr' - The range of inside IPv6 addresses for the tunnel. Any specified CIDR
-- blocks must be unique across all VPN connections that use the same
-- transit gateway.
--
-- Constraints: A size \/126 CIDR block from the local @fd00::\/8@ range.
newVpnTunnelOptionsSpecification ::
  VpnTunnelOptionsSpecification
newVpnTunnelOptionsSpecification =
  VpnTunnelOptionsSpecification'
    { dPDTimeoutAction =
        Prelude.Nothing,
      dPDTimeoutSeconds = Prelude.Nothing,
      iKEVersions = Prelude.Nothing,
      logOptions = Prelude.Nothing,
      phase1DHGroupNumbers = Prelude.Nothing,
      phase1EncryptionAlgorithms = Prelude.Nothing,
      phase1IntegrityAlgorithms = Prelude.Nothing,
      phase1LifetimeSeconds = Prelude.Nothing,
      phase2DHGroupNumbers = Prelude.Nothing,
      phase2EncryptionAlgorithms = Prelude.Nothing,
      phase2IntegrityAlgorithms = Prelude.Nothing,
      phase2LifetimeSeconds = Prelude.Nothing,
      preSharedKey = Prelude.Nothing,
      rekeyFuzzPercentage = Prelude.Nothing,
      rekeyMarginTimeSeconds = Prelude.Nothing,
      replayWindowSize = Prelude.Nothing,
      startupAction = Prelude.Nothing,
      tunnelInsideCidr = Prelude.Nothing,
      tunnelInsideIpv6Cidr = Prelude.Nothing
    }

-- | The action to take after DPD timeout occurs. Specify @restart@ to
-- restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@
--
-- Default: @clear@
vpnTunnelOptionsSpecification_dPDTimeoutAction :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnTunnelOptionsSpecification_dPDTimeoutAction = Lens.lens (\VpnTunnelOptionsSpecification' {dPDTimeoutAction} -> dPDTimeoutAction) (\s@VpnTunnelOptionsSpecification' {} a -> s {dPDTimeoutAction = a} :: VpnTunnelOptionsSpecification)

-- | The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value greater than or equal to 30.
--
-- Default: @30@
vpnTunnelOptionsSpecification_dPDTimeoutSeconds :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
vpnTunnelOptionsSpecification_dPDTimeoutSeconds = Lens.lens (\VpnTunnelOptionsSpecification' {dPDTimeoutSeconds} -> dPDTimeoutSeconds) (\s@VpnTunnelOptionsSpecification' {} a -> s {dPDTimeoutSeconds = a} :: VpnTunnelOptionsSpecification)

-- | The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@
vpnTunnelOptionsSpecification_iKEVersions :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [IKEVersionsRequestListValue])
vpnTunnelOptionsSpecification_iKEVersions = Lens.lens (\VpnTunnelOptionsSpecification' {iKEVersions} -> iKEVersions) (\s@VpnTunnelOptionsSpecification' {} a -> s {iKEVersions = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Options for logging VPN tunnel activity.
vpnTunnelOptionsSpecification_logOptions :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe VpnTunnelLogOptionsSpecification)
vpnTunnelOptionsSpecification_logOptions = Lens.lens (\VpnTunnelOptionsSpecification' {logOptions} -> logOptions) (\s@VpnTunnelOptionsSpecification' {} a -> s {logOptions = a} :: VpnTunnelOptionsSpecification)

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ |
-- @21@ | @22@ | @23@ | @24@
vpnTunnelOptionsSpecification_phase1DHGroupNumbers :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [Phase1DHGroupNumbersRequestListValue])
vpnTunnelOptionsSpecification_phase1DHGroupNumbers = Lens.lens (\VpnTunnelOptionsSpecification' {phase1DHGroupNumbers} -> phase1DHGroupNumbers) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase1DHGroupNumbers = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [Phase1EncryptionAlgorithmsRequestListValue])
vpnTunnelOptionsSpecification_phase1EncryptionAlgorithms = Lens.lens (\VpnTunnelOptionsSpecification' {phase1EncryptionAlgorithms} -> phase1EncryptionAlgorithms) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase1EncryptionAlgorithms = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [Phase1IntegrityAlgorithmsRequestListValue])
vpnTunnelOptionsSpecification_phase1IntegrityAlgorithms = Lens.lens (\VpnTunnelOptionsSpecification' {phase1IntegrityAlgorithms} -> phase1IntegrityAlgorithms) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase1IntegrityAlgorithms = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
--
-- Default: @28800@
vpnTunnelOptionsSpecification_phase1LifetimeSeconds :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
vpnTunnelOptionsSpecification_phase1LifetimeSeconds = Lens.lens (\VpnTunnelOptionsSpecification' {phase1LifetimeSeconds} -> phase1LifetimeSeconds) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase1LifetimeSeconds = a} :: VpnTunnelOptionsSpecification)

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@
-- | @21@ | @22@ | @23@ | @24@
vpnTunnelOptionsSpecification_phase2DHGroupNumbers :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [Phase2DHGroupNumbersRequestListValue])
vpnTunnelOptionsSpecification_phase2DHGroupNumbers = Lens.lens (\VpnTunnelOptionsSpecification' {phase2DHGroupNumbers} -> phase2DHGroupNumbers) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase2DHGroupNumbers = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [Phase2EncryptionAlgorithmsRequestListValue])
vpnTunnelOptionsSpecification_phase2EncryptionAlgorithms = Lens.lens (\VpnTunnelOptionsSpecification' {phase2EncryptionAlgorithms} -> phase2EncryptionAlgorithms) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase2EncryptionAlgorithms = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe [Phase2IntegrityAlgorithmsRequestListValue])
vpnTunnelOptionsSpecification_phase2IntegrityAlgorithms = Lens.lens (\VpnTunnelOptionsSpecification' {phase2IntegrityAlgorithms} -> phase2IntegrityAlgorithms) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase2IntegrityAlgorithms = a} :: VpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than
-- the value for @Phase1LifetimeSeconds@.
--
-- Default: @3600@
vpnTunnelOptionsSpecification_phase2LifetimeSeconds :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
vpnTunnelOptionsSpecification_phase2LifetimeSeconds = Lens.lens (\VpnTunnelOptionsSpecification' {phase2LifetimeSeconds} -> phase2LifetimeSeconds) (\s@VpnTunnelOptionsSpecification' {} a -> s {phase2LifetimeSeconds = a} :: VpnTunnelOptionsSpecification)

-- | The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods
-- (.), and underscores (_). Must be between 8 and 64 characters in length
-- and cannot start with zero (0).
vpnTunnelOptionsSpecification_preSharedKey :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnTunnelOptionsSpecification_preSharedKey = Lens.lens (\VpnTunnelOptionsSpecification' {preSharedKey} -> preSharedKey) (\s@VpnTunnelOptionsSpecification' {} a -> s {preSharedKey = a} :: VpnTunnelOptionsSpecification)

-- | The percentage of the rekey window (determined by
-- @RekeyMarginTimeSeconds@) during which the rekey time is randomly
-- selected.
--
-- Constraints: A value between 0 and 100.
--
-- Default: @100@
vpnTunnelOptionsSpecification_rekeyFuzzPercentage :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
vpnTunnelOptionsSpecification_rekeyFuzzPercentage = Lens.lens (\VpnTunnelOptionsSpecification' {rekeyFuzzPercentage} -> rekeyFuzzPercentage) (\s@VpnTunnelOptionsSpecification' {} a -> s {rekeyFuzzPercentage = a} :: VpnTunnelOptionsSpecification)

-- | The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey. The exact time of the rekey is randomly selected based on the
-- value for @RekeyFuzzPercentage@.
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@.
--
-- Default: @540@
vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
vpnTunnelOptionsSpecification_rekeyMarginTimeSeconds = Lens.lens (\VpnTunnelOptionsSpecification' {rekeyMarginTimeSeconds} -> rekeyMarginTimeSeconds) (\s@VpnTunnelOptionsSpecification' {} a -> s {rekeyMarginTimeSeconds = a} :: VpnTunnelOptionsSpecification)

-- | The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
--
-- Default: @1024@
vpnTunnelOptionsSpecification_replayWindowSize :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
vpnTunnelOptionsSpecification_replayWindowSize = Lens.lens (\VpnTunnelOptionsSpecification' {replayWindowSize} -> replayWindowSize) (\s@VpnTunnelOptionsSpecification' {} a -> s {replayWindowSize = a} :: VpnTunnelOptionsSpecification)

-- | The action to take when the establishing the tunnel for the VPN
-- connection. By default, your customer gateway device must initiate the
-- IKE negotiation and bring up the tunnel. Specify @start@ for Amazon Web
-- Services to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@
--
-- Default: @add@
vpnTunnelOptionsSpecification_startupAction :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnTunnelOptionsSpecification_startupAction = Lens.lens (\VpnTunnelOptionsSpecification' {startupAction} -> startupAction) (\s@VpnTunnelOptionsSpecification' {} a -> s {startupAction = a} :: VpnTunnelOptionsSpecification)

-- | The range of inside IPv4 addresses for the tunnel. Any specified CIDR
-- blocks must be unique across all VPN connections that use the same
-- virtual private gateway.
--
-- Constraints: A size \/30 CIDR block from the @169.254.0.0\/16@ range.
-- The following CIDR blocks are reserved and cannot be used:
--
-- -   @169.254.0.0\/30@
--
-- -   @169.254.1.0\/30@
--
-- -   @169.254.2.0\/30@
--
-- -   @169.254.3.0\/30@
--
-- -   @169.254.4.0\/30@
--
-- -   @169.254.5.0\/30@
--
-- -   @169.254.169.252\/30@
vpnTunnelOptionsSpecification_tunnelInsideCidr :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnTunnelOptionsSpecification_tunnelInsideCidr = Lens.lens (\VpnTunnelOptionsSpecification' {tunnelInsideCidr} -> tunnelInsideCidr) (\s@VpnTunnelOptionsSpecification' {} a -> s {tunnelInsideCidr = a} :: VpnTunnelOptionsSpecification)

-- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR
-- blocks must be unique across all VPN connections that use the same
-- transit gateway.
--
-- Constraints: A size \/126 CIDR block from the local @fd00::\/8@ range.
vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr :: Lens.Lens' VpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr = Lens.lens (\VpnTunnelOptionsSpecification' {tunnelInsideIpv6Cidr} -> tunnelInsideIpv6Cidr) (\s@VpnTunnelOptionsSpecification' {} a -> s {tunnelInsideIpv6Cidr = a} :: VpnTunnelOptionsSpecification)

instance
  Prelude.Hashable
    VpnTunnelOptionsSpecification
  where
  hashWithSalt _salt VpnTunnelOptionsSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` dPDTimeoutAction
      `Prelude.hashWithSalt` dPDTimeoutSeconds
      `Prelude.hashWithSalt` iKEVersions
      `Prelude.hashWithSalt` logOptions
      `Prelude.hashWithSalt` phase1DHGroupNumbers
      `Prelude.hashWithSalt` phase1EncryptionAlgorithms
      `Prelude.hashWithSalt` phase1IntegrityAlgorithms
      `Prelude.hashWithSalt` phase1LifetimeSeconds
      `Prelude.hashWithSalt` phase2DHGroupNumbers
      `Prelude.hashWithSalt` phase2EncryptionAlgorithms
      `Prelude.hashWithSalt` phase2IntegrityAlgorithms
      `Prelude.hashWithSalt` phase2LifetimeSeconds
      `Prelude.hashWithSalt` preSharedKey
      `Prelude.hashWithSalt` rekeyFuzzPercentage
      `Prelude.hashWithSalt` rekeyMarginTimeSeconds
      `Prelude.hashWithSalt` replayWindowSize
      `Prelude.hashWithSalt` startupAction
      `Prelude.hashWithSalt` tunnelInsideCidr
      `Prelude.hashWithSalt` tunnelInsideIpv6Cidr

instance Prelude.NFData VpnTunnelOptionsSpecification where
  rnf VpnTunnelOptionsSpecification' {..} =
    Prelude.rnf dPDTimeoutAction `Prelude.seq`
      Prelude.rnf dPDTimeoutSeconds `Prelude.seq`
        Prelude.rnf iKEVersions `Prelude.seq`
          Prelude.rnf logOptions `Prelude.seq`
            Prelude.rnf phase1DHGroupNumbers `Prelude.seq`
              Prelude.rnf phase1EncryptionAlgorithms `Prelude.seq`
                Prelude.rnf phase1IntegrityAlgorithms `Prelude.seq`
                  Prelude.rnf phase1LifetimeSeconds `Prelude.seq`
                    Prelude.rnf phase2DHGroupNumbers `Prelude.seq`
                      Prelude.rnf phase2EncryptionAlgorithms `Prelude.seq`
                        Prelude.rnf phase2IntegrityAlgorithms `Prelude.seq`
                          Prelude.rnf phase2LifetimeSeconds `Prelude.seq`
                            Prelude.rnf preSharedKey `Prelude.seq`
                              Prelude.rnf rekeyFuzzPercentage `Prelude.seq`
                                Prelude.rnf rekeyMarginTimeSeconds `Prelude.seq`
                                  Prelude.rnf replayWindowSize `Prelude.seq`
                                    Prelude.rnf startupAction `Prelude.seq`
                                      Prelude.rnf tunnelInsideCidr `Prelude.seq`
                                        Prelude.rnf tunnelInsideIpv6Cidr

instance Data.ToQuery VpnTunnelOptionsSpecification where
  toQuery VpnTunnelOptionsSpecification' {..} =
    Prelude.mconcat
      [ "DPDTimeoutAction" Data.=: dPDTimeoutAction,
        "DPDTimeoutSeconds" Data.=: dPDTimeoutSeconds,
        Data.toQuery
          ( Data.toQueryList "IKEVersion"
              Prelude.<$> iKEVersions
          ),
        "LogOptions" Data.=: logOptions,
        Data.toQuery
          ( Data.toQueryList "Phase1DHGroupNumber"
              Prelude.<$> phase1DHGroupNumbers
          ),
        Data.toQuery
          ( Data.toQueryList "Phase1EncryptionAlgorithm"
              Prelude.<$> phase1EncryptionAlgorithms
          ),
        Data.toQuery
          ( Data.toQueryList "Phase1IntegrityAlgorithm"
              Prelude.<$> phase1IntegrityAlgorithms
          ),
        "Phase1LifetimeSeconds"
          Data.=: phase1LifetimeSeconds,
        Data.toQuery
          ( Data.toQueryList "Phase2DHGroupNumber"
              Prelude.<$> phase2DHGroupNumbers
          ),
        Data.toQuery
          ( Data.toQueryList "Phase2EncryptionAlgorithm"
              Prelude.<$> phase2EncryptionAlgorithms
          ),
        Data.toQuery
          ( Data.toQueryList "Phase2IntegrityAlgorithm"
              Prelude.<$> phase2IntegrityAlgorithms
          ),
        "Phase2LifetimeSeconds"
          Data.=: phase2LifetimeSeconds,
        "PreSharedKey" Data.=: preSharedKey,
        "RekeyFuzzPercentage" Data.=: rekeyFuzzPercentage,
        "RekeyMarginTimeSeconds"
          Data.=: rekeyMarginTimeSeconds,
        "ReplayWindowSize" Data.=: replayWindowSize,
        "StartupAction" Data.=: startupAction,
        "TunnelInsideCidr" Data.=: tunnelInsideCidr,
        "TunnelInsideIpv6Cidr" Data.=: tunnelInsideIpv6Cidr
      ]
