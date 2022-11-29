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
-- Module      : Amazonka.EC2.Types.ModifyVpnTunnelOptionsSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyVpnTunnelOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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

-- | The Amazon Web Services Site-to-Site VPN tunnel options to modify.
--
-- /See:/ 'newModifyVpnTunnelOptionsSpecification' smart constructor.
data ModifyVpnTunnelOptionsSpecification = ModifyVpnTunnelOptionsSpecification'
  { -- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR
    -- blocks must be unique across all VPN connections that use the same
    -- transit gateway.
    --
    -- Constraints: A size \/126 CIDR block from the local @fd00::\/8@ range.
    tunnelInsideIpv6Cidr :: Prelude.Maybe Prelude.Text,
    -- | The lifetime for phase 1 of the IKE negotiation, in seconds.
    --
    -- Constraints: A value between 900 and 28,800.
    --
    -- Default: @28800@
    phase1LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The lifetime for phase 2 of the IKE negotiation, in seconds.
    --
    -- Constraints: A value between 900 and 3,600. The value must be less than
    -- the value for @Phase1LifetimeSeconds@.
    --
    -- Default: @3600@
    phase2LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | One or more encryption algorithms that are permitted for the VPN tunnel
    -- for phase 2 IKE negotiations.
    --
    -- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
    phase2EncryptionAlgorithms :: Prelude.Maybe [Phase2EncryptionAlgorithmsRequestListValue],
    -- | One or more Diffie-Hellman group numbers that are permitted for the VPN
    -- tunnel for phase 1 IKE negotiations.
    --
    -- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ |
    -- @21@ | @22@ | @23@ | @24@
    phase1DHGroupNumbers :: Prelude.Maybe [Phase1DHGroupNumbersRequestListValue],
    -- | One or more integrity algorithms that are permitted for the VPN tunnel
    -- for phase 1 IKE negotiations.
    --
    -- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
    phase1IntegrityAlgorithms :: Prelude.Maybe [Phase1IntegrityAlgorithmsRequestListValue],
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
    -- | The pre-shared key (PSK) to establish initial authentication between the
    -- virtual private gateway and the customer gateway.
    --
    -- Constraints: Allowed characters are alphanumeric characters, periods
    -- (.), and underscores (_). Must be between 8 and 64 characters in length
    -- and cannot start with zero (0).
    preSharedKey :: Prelude.Maybe Prelude.Text,
    -- | The action to take after DPD timeout occurs. Specify @restart@ to
    -- restart the IKE initiation. Specify @clear@ to end the IKE session.
    --
    -- Valid Values: @clear@ | @none@ | @restart@
    --
    -- Default: @clear@
    dPDTimeoutAction :: Prelude.Maybe Prelude.Text,
    -- | Options for logging VPN tunnel activity.
    logOptions :: Prelude.Maybe VpnTunnelLogOptionsSpecification,
    -- | One or more Diffie-Hellman group numbers that are permitted for the VPN
    -- tunnel for phase 2 IKE negotiations.
    --
    -- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@
    -- | @21@ | @22@ | @23@ | @24@
    phase2DHGroupNumbers :: Prelude.Maybe [Phase2DHGroupNumbersRequestListValue],
    -- | The percentage of the rekey window (determined by
    -- @RekeyMarginTimeSeconds@) during which the rekey time is randomly
    -- selected.
    --
    -- Constraints: A value between 0 and 100.
    --
    -- Default: @100@
    rekeyFuzzPercentage :: Prelude.Maybe Prelude.Int,
    -- | The action to take when the establishing the tunnel for the VPN
    -- connection. By default, your customer gateway device must initiate the
    -- IKE negotiation and bring up the tunnel. Specify @start@ for Amazon Web
    -- Services to initiate the IKE negotiation.
    --
    -- Valid Values: @add@ | @start@
    --
    -- Default: @add@
    startupAction :: Prelude.Maybe Prelude.Text,
    -- | The margin time, in seconds, before the phase 2 lifetime expires, during
    -- which the Amazon Web Services side of the VPN connection performs an IKE
    -- rekey. The exact time of the rekey is randomly selected based on the
    -- value for @RekeyFuzzPercentage@.
    --
    -- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@.
    --
    -- Default: @540@
    rekeyMarginTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | One or more integrity algorithms that are permitted for the VPN tunnel
    -- for phase 2 IKE negotiations.
    --
    -- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
    phase2IntegrityAlgorithms :: Prelude.Maybe [Phase2IntegrityAlgorithmsRequestListValue],
    -- | One or more encryption algorithms that are permitted for the VPN tunnel
    -- for phase 1 IKE negotiations.
    --
    -- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
    phase1EncryptionAlgorithms :: Prelude.Maybe [Phase1EncryptionAlgorithmsRequestListValue],
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
    -- | The number of packets in an IKE replay window.
    --
    -- Constraints: A value between 64 and 2048.
    --
    -- Default: @1024@
    replayWindowSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnTunnelOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tunnelInsideIpv6Cidr', 'modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr' - The range of inside IPv6 addresses for the tunnel. Any specified CIDR
-- blocks must be unique across all VPN connections that use the same
-- transit gateway.
--
-- Constraints: A size \/126 CIDR block from the local @fd00::\/8@ range.
--
-- 'phase1LifetimeSeconds', 'modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
--
-- Default: @28800@
--
-- 'phase2LifetimeSeconds', 'modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than
-- the value for @Phase1LifetimeSeconds@.
--
-- Default: @3600@
--
-- 'phase2EncryptionAlgorithms', 'modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- 'phase1DHGroupNumbers', 'modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ |
-- @21@ | @22@ | @23@ | @24@
--
-- 'phase1IntegrityAlgorithms', 'modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- 'dPDTimeoutSeconds', 'modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value greater than or equal to 30.
--
-- Default: @30@
--
-- 'iKEVersions', 'modifyVpnTunnelOptionsSpecification_iKEVersions' - The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@
--
-- 'preSharedKey', 'modifyVpnTunnelOptionsSpecification_preSharedKey' - The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and the customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods
-- (.), and underscores (_). Must be between 8 and 64 characters in length
-- and cannot start with zero (0).
--
-- 'dPDTimeoutAction', 'modifyVpnTunnelOptionsSpecification_dPDTimeoutAction' - The action to take after DPD timeout occurs. Specify @restart@ to
-- restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@
--
-- Default: @clear@
--
-- 'logOptions', 'modifyVpnTunnelOptionsSpecification_logOptions' - Options for logging VPN tunnel activity.
--
-- 'phase2DHGroupNumbers', 'modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers' - One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@
-- | @21@ | @22@ | @23@ | @24@
--
-- 'rekeyFuzzPercentage', 'modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage' - The percentage of the rekey window (determined by
-- @RekeyMarginTimeSeconds@) during which the rekey time is randomly
-- selected.
--
-- Constraints: A value between 0 and 100.
--
-- Default: @100@
--
-- 'startupAction', 'modifyVpnTunnelOptionsSpecification_startupAction' - The action to take when the establishing the tunnel for the VPN
-- connection. By default, your customer gateway device must initiate the
-- IKE negotiation and bring up the tunnel. Specify @start@ for Amazon Web
-- Services to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@
--
-- Default: @add@
--
-- 'rekeyMarginTimeSeconds', 'modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey. The exact time of the rekey is randomly selected based on the
-- value for @RekeyFuzzPercentage@.
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@.
--
-- Default: @540@
--
-- 'phase2IntegrityAlgorithms', 'modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms' - One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
--
-- 'phase1EncryptionAlgorithms', 'modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms' - One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
--
-- 'tunnelInsideCidr', 'modifyVpnTunnelOptionsSpecification_tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel. Any specified CIDR
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
-- 'replayWindowSize', 'modifyVpnTunnelOptionsSpecification_replayWindowSize' - The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
--
-- Default: @1024@
newModifyVpnTunnelOptionsSpecification ::
  ModifyVpnTunnelOptionsSpecification
newModifyVpnTunnelOptionsSpecification =
  ModifyVpnTunnelOptionsSpecification'
    { tunnelInsideIpv6Cidr =
        Prelude.Nothing,
      phase1LifetimeSeconds =
        Prelude.Nothing,
      phase2LifetimeSeconds =
        Prelude.Nothing,
      phase2EncryptionAlgorithms =
        Prelude.Nothing,
      phase1DHGroupNumbers = Prelude.Nothing,
      phase1IntegrityAlgorithms =
        Prelude.Nothing,
      dPDTimeoutSeconds = Prelude.Nothing,
      iKEVersions = Prelude.Nothing,
      preSharedKey = Prelude.Nothing,
      dPDTimeoutAction = Prelude.Nothing,
      logOptions = Prelude.Nothing,
      phase2DHGroupNumbers = Prelude.Nothing,
      rekeyFuzzPercentage = Prelude.Nothing,
      startupAction = Prelude.Nothing,
      rekeyMarginTimeSeconds =
        Prelude.Nothing,
      phase2IntegrityAlgorithms =
        Prelude.Nothing,
      phase1EncryptionAlgorithms =
        Prelude.Nothing,
      tunnelInsideCidr = Prelude.Nothing,
      replayWindowSize = Prelude.Nothing
    }

-- | The range of inside IPv6 addresses for the tunnel. Any specified CIDR
-- blocks must be unique across all VPN connections that use the same
-- transit gateway.
--
-- Constraints: A size \/126 CIDR block from the local @fd00::\/8@ range.
modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
modifyVpnTunnelOptionsSpecification_tunnelInsideIpv6Cidr = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {tunnelInsideIpv6Cidr} -> tunnelInsideIpv6Cidr) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {tunnelInsideIpv6Cidr = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 28,800.
--
-- Default: @28800@
modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
modifyVpnTunnelOptionsSpecification_phase1LifetimeSeconds = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase1LifetimeSeconds} -> phase1LifetimeSeconds) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase1LifetimeSeconds = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- Constraints: A value between 900 and 3,600. The value must be less than
-- the value for @Phase1LifetimeSeconds@.
--
-- Default: @3600@
modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
modifyVpnTunnelOptionsSpecification_phase2LifetimeSeconds = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase2LifetimeSeconds} -> phase2LifetimeSeconds) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase2LifetimeSeconds = a} :: ModifyVpnTunnelOptionsSpecification)

-- | One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [Phase2EncryptionAlgorithmsRequestListValue])
modifyVpnTunnelOptionsSpecification_phase2EncryptionAlgorithms = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase2EncryptionAlgorithms} -> phase2EncryptionAlgorithms) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase2EncryptionAlgorithms = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 1 IKE negotiations.
--
-- Valid values: @2@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@ |
-- @21@ | @22@ | @23@ | @24@
modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [Phase1DHGroupNumbersRequestListValue])
modifyVpnTunnelOptionsSpecification_phase1DHGroupNumbers = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase1DHGroupNumbers} -> phase1DHGroupNumbers) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase1DHGroupNumbers = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [Phase1IntegrityAlgorithmsRequestListValue])
modifyVpnTunnelOptionsSpecification_phase1IntegrityAlgorithms = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase1IntegrityAlgorithms} -> phase1IntegrityAlgorithms) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase1IntegrityAlgorithms = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds after which a DPD timeout occurs.
--
-- Constraints: A value greater than or equal to 30.
--
-- Default: @30@
modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
modifyVpnTunnelOptionsSpecification_dPDTimeoutSeconds = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {dPDTimeoutSeconds} -> dPDTimeoutSeconds) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {dPDTimeoutSeconds = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The IKE versions that are permitted for the VPN tunnel.
--
-- Valid values: @ikev1@ | @ikev2@
modifyVpnTunnelOptionsSpecification_iKEVersions :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [IKEVersionsRequestListValue])
modifyVpnTunnelOptionsSpecification_iKEVersions = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {iKEVersions} -> iKEVersions) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {iKEVersions = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and the customer gateway.
--
-- Constraints: Allowed characters are alphanumeric characters, periods
-- (.), and underscores (_). Must be between 8 and 64 characters in length
-- and cannot start with zero (0).
modifyVpnTunnelOptionsSpecification_preSharedKey :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
modifyVpnTunnelOptionsSpecification_preSharedKey = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {preSharedKey} -> preSharedKey) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {preSharedKey = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The action to take after DPD timeout occurs. Specify @restart@ to
-- restart the IKE initiation. Specify @clear@ to end the IKE session.
--
-- Valid Values: @clear@ | @none@ | @restart@
--
-- Default: @clear@
modifyVpnTunnelOptionsSpecification_dPDTimeoutAction :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
modifyVpnTunnelOptionsSpecification_dPDTimeoutAction = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {dPDTimeoutAction} -> dPDTimeoutAction) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {dPDTimeoutAction = a} :: ModifyVpnTunnelOptionsSpecification)

-- | Options for logging VPN tunnel activity.
modifyVpnTunnelOptionsSpecification_logOptions :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe VpnTunnelLogOptionsSpecification)
modifyVpnTunnelOptionsSpecification_logOptions = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {logOptions} -> logOptions) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {logOptions = a} :: ModifyVpnTunnelOptionsSpecification)

-- | One or more Diffie-Hellman group numbers that are permitted for the VPN
-- tunnel for phase 2 IKE negotiations.
--
-- Valid values: @2@ | @5@ | @14@ | @15@ | @16@ | @17@ | @18@ | @19@ | @20@
-- | @21@ | @22@ | @23@ | @24@
modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [Phase2DHGroupNumbersRequestListValue])
modifyVpnTunnelOptionsSpecification_phase2DHGroupNumbers = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase2DHGroupNumbers} -> phase2DHGroupNumbers) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase2DHGroupNumbers = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of the rekey window (determined by
-- @RekeyMarginTimeSeconds@) during which the rekey time is randomly
-- selected.
--
-- Constraints: A value between 0 and 100.
--
-- Default: @100@
modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
modifyVpnTunnelOptionsSpecification_rekeyFuzzPercentage = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {rekeyFuzzPercentage} -> rekeyFuzzPercentage) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {rekeyFuzzPercentage = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The action to take when the establishing the tunnel for the VPN
-- connection. By default, your customer gateway device must initiate the
-- IKE negotiation and bring up the tunnel. Specify @start@ for Amazon Web
-- Services to initiate the IKE negotiation.
--
-- Valid Values: @add@ | @start@
--
-- Default: @add@
modifyVpnTunnelOptionsSpecification_startupAction :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
modifyVpnTunnelOptionsSpecification_startupAction = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {startupAction} -> startupAction) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {startupAction = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey. The exact time of the rekey is randomly selected based on the
-- value for @RekeyFuzzPercentage@.
--
-- Constraints: A value between 60 and half of @Phase2LifetimeSeconds@.
--
-- Default: @540@
modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
modifyVpnTunnelOptionsSpecification_rekeyMarginTimeSeconds = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {rekeyMarginTimeSeconds} -> rekeyMarginTimeSeconds) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {rekeyMarginTimeSeconds = a} :: ModifyVpnTunnelOptionsSpecification)

-- | One or more integrity algorithms that are permitted for the VPN tunnel
-- for phase 2 IKE negotiations.
--
-- Valid values: @SHA1@ | @SHA2-256@ | @SHA2-384@ | @SHA2-512@
modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [Phase2IntegrityAlgorithmsRequestListValue])
modifyVpnTunnelOptionsSpecification_phase2IntegrityAlgorithms = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase2IntegrityAlgorithms} -> phase2IntegrityAlgorithms) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase2IntegrityAlgorithms = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

-- | One or more encryption algorithms that are permitted for the VPN tunnel
-- for phase 1 IKE negotiations.
--
-- Valid values: @AES128@ | @AES256@ | @AES128-GCM-16@ | @AES256-GCM-16@
modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe [Phase1EncryptionAlgorithmsRequestListValue])
modifyVpnTunnelOptionsSpecification_phase1EncryptionAlgorithms = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {phase1EncryptionAlgorithms} -> phase1EncryptionAlgorithms) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {phase1EncryptionAlgorithms = a} :: ModifyVpnTunnelOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

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
modifyVpnTunnelOptionsSpecification_tunnelInsideCidr :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Text)
modifyVpnTunnelOptionsSpecification_tunnelInsideCidr = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {tunnelInsideCidr} -> tunnelInsideCidr) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {tunnelInsideCidr = a} :: ModifyVpnTunnelOptionsSpecification)

-- | The number of packets in an IKE replay window.
--
-- Constraints: A value between 64 and 2048.
--
-- Default: @1024@
modifyVpnTunnelOptionsSpecification_replayWindowSize :: Lens.Lens' ModifyVpnTunnelOptionsSpecification (Prelude.Maybe Prelude.Int)
modifyVpnTunnelOptionsSpecification_replayWindowSize = Lens.lens (\ModifyVpnTunnelOptionsSpecification' {replayWindowSize} -> replayWindowSize) (\s@ModifyVpnTunnelOptionsSpecification' {} a -> s {replayWindowSize = a} :: ModifyVpnTunnelOptionsSpecification)

instance
  Prelude.Hashable
    ModifyVpnTunnelOptionsSpecification
  where
  hashWithSalt
    _salt
    ModifyVpnTunnelOptionsSpecification' {..} =
      _salt `Prelude.hashWithSalt` tunnelInsideIpv6Cidr
        `Prelude.hashWithSalt` phase1LifetimeSeconds
        `Prelude.hashWithSalt` phase2LifetimeSeconds
        `Prelude.hashWithSalt` phase2EncryptionAlgorithms
        `Prelude.hashWithSalt` phase1DHGroupNumbers
        `Prelude.hashWithSalt` phase1IntegrityAlgorithms
        `Prelude.hashWithSalt` dPDTimeoutSeconds
        `Prelude.hashWithSalt` iKEVersions
        `Prelude.hashWithSalt` preSharedKey
        `Prelude.hashWithSalt` dPDTimeoutAction
        `Prelude.hashWithSalt` logOptions
        `Prelude.hashWithSalt` phase2DHGroupNumbers
        `Prelude.hashWithSalt` rekeyFuzzPercentage
        `Prelude.hashWithSalt` startupAction
        `Prelude.hashWithSalt` rekeyMarginTimeSeconds
        `Prelude.hashWithSalt` phase2IntegrityAlgorithms
        `Prelude.hashWithSalt` phase1EncryptionAlgorithms
        `Prelude.hashWithSalt` tunnelInsideCidr
        `Prelude.hashWithSalt` replayWindowSize

instance
  Prelude.NFData
    ModifyVpnTunnelOptionsSpecification
  where
  rnf ModifyVpnTunnelOptionsSpecification' {..} =
    Prelude.rnf tunnelInsideIpv6Cidr
      `Prelude.seq` Prelude.rnf phase1LifetimeSeconds
      `Prelude.seq` Prelude.rnf phase2LifetimeSeconds
      `Prelude.seq` Prelude.rnf phase2EncryptionAlgorithms
      `Prelude.seq` Prelude.rnf phase1DHGroupNumbers
      `Prelude.seq` Prelude.rnf phase1IntegrityAlgorithms
      `Prelude.seq` Prelude.rnf dPDTimeoutSeconds
      `Prelude.seq` Prelude.rnf iKEVersions
      `Prelude.seq` Prelude.rnf preSharedKey
      `Prelude.seq` Prelude.rnf dPDTimeoutAction
      `Prelude.seq` Prelude.rnf logOptions
      `Prelude.seq` Prelude.rnf phase2DHGroupNumbers
      `Prelude.seq` Prelude.rnf rekeyFuzzPercentage
      `Prelude.seq` Prelude.rnf startupAction
      `Prelude.seq` Prelude.rnf rekeyMarginTimeSeconds
      `Prelude.seq` Prelude.rnf phase2IntegrityAlgorithms
      `Prelude.seq` Prelude.rnf
        phase1EncryptionAlgorithms
      `Prelude.seq` Prelude.rnf tunnelInsideCidr
      `Prelude.seq` Prelude.rnf replayWindowSize

instance
  Core.ToQuery
    ModifyVpnTunnelOptionsSpecification
  where
  toQuery ModifyVpnTunnelOptionsSpecification' {..} =
    Prelude.mconcat
      [ "TunnelInsideIpv6Cidr" Core.=: tunnelInsideIpv6Cidr,
        "Phase1LifetimeSeconds"
          Core.=: phase1LifetimeSeconds,
        "Phase2LifetimeSeconds"
          Core.=: phase2LifetimeSeconds,
        Core.toQuery
          ( Core.toQueryList "Phase2EncryptionAlgorithm"
              Prelude.<$> phase2EncryptionAlgorithms
          ),
        Core.toQuery
          ( Core.toQueryList "Phase1DHGroupNumber"
              Prelude.<$> phase1DHGroupNumbers
          ),
        Core.toQuery
          ( Core.toQueryList "Phase1IntegrityAlgorithm"
              Prelude.<$> phase1IntegrityAlgorithms
          ),
        "DPDTimeoutSeconds" Core.=: dPDTimeoutSeconds,
        Core.toQuery
          ( Core.toQueryList "IKEVersion"
              Prelude.<$> iKEVersions
          ),
        "PreSharedKey" Core.=: preSharedKey,
        "DPDTimeoutAction" Core.=: dPDTimeoutAction,
        "LogOptions" Core.=: logOptions,
        Core.toQuery
          ( Core.toQueryList "Phase2DHGroupNumber"
              Prelude.<$> phase2DHGroupNumbers
          ),
        "RekeyFuzzPercentage" Core.=: rekeyFuzzPercentage,
        "StartupAction" Core.=: startupAction,
        "RekeyMarginTimeSeconds"
          Core.=: rekeyMarginTimeSeconds,
        Core.toQuery
          ( Core.toQueryList "Phase2IntegrityAlgorithm"
              Prelude.<$> phase2IntegrityAlgorithms
          ),
        Core.toQuery
          ( Core.toQueryList "Phase1EncryptionAlgorithm"
              Prelude.<$> phase1EncryptionAlgorithms
          ),
        "TunnelInsideCidr" Core.=: tunnelInsideCidr,
        "ReplayWindowSize" Core.=: replayWindowSize
      ]
