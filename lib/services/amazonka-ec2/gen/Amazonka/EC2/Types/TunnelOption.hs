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
-- Module      : Amazonka.EC2.Types.TunnelOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TunnelOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IKEVersionsListValue
import Amazonka.EC2.Types.Phase1DHGroupNumbersListValue
import Amazonka.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Amazonka.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Amazonka.EC2.Types.Phase2DHGroupNumbersListValue
import Amazonka.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Amazonka.EC2.Types.Phase2IntegrityAlgorithmsListValue
import Amazonka.EC2.Types.VpnTunnelLogOptions
import qualified Amazonka.Prelude as Prelude

-- | The VPN tunnel options.
--
-- /See:/ 'newTunnelOption' smart constructor.
data TunnelOption = TunnelOption'
  { -- | The range of inside IPv6 addresses for the tunnel.
    tunnelInsideIpv6Cidr :: Prelude.Maybe Prelude.Text,
    -- | The lifetime for phase 1 of the IKE negotiation, in seconds.
    phase1LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The lifetime for phase 2 of the IKE negotiation, in seconds.
    phase2LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
    -- negotiations.
    phase2EncryptionAlgorithms :: Prelude.Maybe [Phase2EncryptionAlgorithmsListValue],
    -- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
    -- 1 IKE negotiations.
    phase1DHGroupNumbers :: Prelude.Maybe [Phase1DHGroupNumbersListValue],
    -- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
    -- negotiations.
    phase1IntegrityAlgorithms :: Prelude.Maybe [Phase1IntegrityAlgorithmsListValue],
    -- | The number of seconds after which a DPD timeout occurs.
    dpdTimeoutSeconds :: Prelude.Maybe Prelude.Int,
    -- | The IKE versions that are permitted for the VPN tunnel.
    ikeVersions :: Prelude.Maybe [IKEVersionsListValue],
    -- | The pre-shared key (PSK) to establish initial authentication between the
    -- virtual private gateway and the customer gateway.
    preSharedKey :: Prelude.Maybe Prelude.Text,
    -- | The action to take after a DPD timeout occurs.
    dpdTimeoutAction :: Prelude.Maybe Prelude.Text,
    -- | Options for logging VPN tunnel activity.
    logOptions :: Prelude.Maybe VpnTunnelLogOptions,
    -- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
    -- 2 IKE negotiations.
    phase2DHGroupNumbers :: Prelude.Maybe [Phase2DHGroupNumbersListValue],
    -- | The percentage of the rekey window determined by
    -- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
    -- selected.
    rekeyFuzzPercentage :: Prelude.Maybe Prelude.Int,
    -- | The action to take when the establishing the VPN tunnels for a VPN
    -- connection.
    startupAction :: Prelude.Maybe Prelude.Text,
    -- | The margin time, in seconds, before the phase 2 lifetime expires, during
    -- which the Amazon Web Services side of the VPN connection performs an IKE
    -- rekey.
    rekeyMarginTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
    -- negotiations.
    phase2IntegrityAlgorithms :: Prelude.Maybe [Phase2IntegrityAlgorithmsListValue],
    -- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
    -- negotiations.
    phase1EncryptionAlgorithms :: Prelude.Maybe [Phase1EncryptionAlgorithmsListValue],
    -- | The range of inside IPv4 addresses for the tunnel.
    tunnelInsideCidr :: Prelude.Maybe Prelude.Text,
    -- | The external IP address of the VPN tunnel.
    outsideIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of packets in an IKE replay window.
    replayWindowSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TunnelOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tunnelInsideIpv6Cidr', 'tunnelOption_tunnelInsideIpv6Cidr' - The range of inside IPv6 addresses for the tunnel.
--
-- 'phase1LifetimeSeconds', 'tunnelOption_phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- 'phase2LifetimeSeconds', 'tunnelOption_phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- 'phase2EncryptionAlgorithms', 'tunnelOption_phase2EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- 'phase1DHGroupNumbers', 'tunnelOption_phase1DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 1 IKE negotiations.
--
-- 'phase1IntegrityAlgorithms', 'tunnelOption_phase1IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- 'dpdTimeoutSeconds', 'tunnelOption_dpdTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
--
-- 'ikeVersions', 'tunnelOption_ikeVersions' - The IKE versions that are permitted for the VPN tunnel.
--
-- 'preSharedKey', 'tunnelOption_preSharedKey' - The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and the customer gateway.
--
-- 'dpdTimeoutAction', 'tunnelOption_dpdTimeoutAction' - The action to take after a DPD timeout occurs.
--
-- 'logOptions', 'tunnelOption_logOptions' - Options for logging VPN tunnel activity.
--
-- 'phase2DHGroupNumbers', 'tunnelOption_phase2DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 2 IKE negotiations.
--
-- 'rekeyFuzzPercentage', 'tunnelOption_rekeyFuzzPercentage' - The percentage of the rekey window determined by
-- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
-- selected.
--
-- 'startupAction', 'tunnelOption_startupAction' - The action to take when the establishing the VPN tunnels for a VPN
-- connection.
--
-- 'rekeyMarginTimeSeconds', 'tunnelOption_rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey.
--
-- 'phase2IntegrityAlgorithms', 'tunnelOption_phase2IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- 'phase1EncryptionAlgorithms', 'tunnelOption_phase1EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- 'tunnelInsideCidr', 'tunnelOption_tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel.
--
-- 'outsideIpAddress', 'tunnelOption_outsideIpAddress' - The external IP address of the VPN tunnel.
--
-- 'replayWindowSize', 'tunnelOption_replayWindowSize' - The number of packets in an IKE replay window.
newTunnelOption ::
  TunnelOption
newTunnelOption =
  TunnelOption'
    { tunnelInsideIpv6Cidr =
        Prelude.Nothing,
      phase1LifetimeSeconds = Prelude.Nothing,
      phase2LifetimeSeconds = Prelude.Nothing,
      phase2EncryptionAlgorithms = Prelude.Nothing,
      phase1DHGroupNumbers = Prelude.Nothing,
      phase1IntegrityAlgorithms = Prelude.Nothing,
      dpdTimeoutSeconds = Prelude.Nothing,
      ikeVersions = Prelude.Nothing,
      preSharedKey = Prelude.Nothing,
      dpdTimeoutAction = Prelude.Nothing,
      logOptions = Prelude.Nothing,
      phase2DHGroupNumbers = Prelude.Nothing,
      rekeyFuzzPercentage = Prelude.Nothing,
      startupAction = Prelude.Nothing,
      rekeyMarginTimeSeconds = Prelude.Nothing,
      phase2IntegrityAlgorithms = Prelude.Nothing,
      phase1EncryptionAlgorithms = Prelude.Nothing,
      tunnelInsideCidr = Prelude.Nothing,
      outsideIpAddress = Prelude.Nothing,
      replayWindowSize = Prelude.Nothing
    }

-- | The range of inside IPv6 addresses for the tunnel.
tunnelOption_tunnelInsideIpv6Cidr :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Text)
tunnelOption_tunnelInsideIpv6Cidr = Lens.lens (\TunnelOption' {tunnelInsideIpv6Cidr} -> tunnelInsideIpv6Cidr) (\s@TunnelOption' {} a -> s {tunnelInsideIpv6Cidr = a} :: TunnelOption)

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
tunnelOption_phase1LifetimeSeconds :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Int)
tunnelOption_phase1LifetimeSeconds = Lens.lens (\TunnelOption' {phase1LifetimeSeconds} -> phase1LifetimeSeconds) (\s@TunnelOption' {} a -> s {phase1LifetimeSeconds = a} :: TunnelOption)

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
tunnelOption_phase2LifetimeSeconds :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Int)
tunnelOption_phase2LifetimeSeconds = Lens.lens (\TunnelOption' {phase2LifetimeSeconds} -> phase2LifetimeSeconds) (\s@TunnelOption' {} a -> s {phase2LifetimeSeconds = a} :: TunnelOption)

-- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
tunnelOption_phase2EncryptionAlgorithms :: Lens.Lens' TunnelOption (Prelude.Maybe [Phase2EncryptionAlgorithmsListValue])
tunnelOption_phase2EncryptionAlgorithms = Lens.lens (\TunnelOption' {phase2EncryptionAlgorithms} -> phase2EncryptionAlgorithms) (\s@TunnelOption' {} a -> s {phase2EncryptionAlgorithms = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 1 IKE negotiations.
tunnelOption_phase1DHGroupNumbers :: Lens.Lens' TunnelOption (Prelude.Maybe [Phase1DHGroupNumbersListValue])
tunnelOption_phase1DHGroupNumbers = Lens.lens (\TunnelOption' {phase1DHGroupNumbers} -> phase1DHGroupNumbers) (\s@TunnelOption' {} a -> s {phase1DHGroupNumbers = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
tunnelOption_phase1IntegrityAlgorithms :: Lens.Lens' TunnelOption (Prelude.Maybe [Phase1IntegrityAlgorithmsListValue])
tunnelOption_phase1IntegrityAlgorithms = Lens.lens (\TunnelOption' {phase1IntegrityAlgorithms} -> phase1IntegrityAlgorithms) (\s@TunnelOption' {} a -> s {phase1IntegrityAlgorithms = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds after which a DPD timeout occurs.
tunnelOption_dpdTimeoutSeconds :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Int)
tunnelOption_dpdTimeoutSeconds = Lens.lens (\TunnelOption' {dpdTimeoutSeconds} -> dpdTimeoutSeconds) (\s@TunnelOption' {} a -> s {dpdTimeoutSeconds = a} :: TunnelOption)

-- | The IKE versions that are permitted for the VPN tunnel.
tunnelOption_ikeVersions :: Lens.Lens' TunnelOption (Prelude.Maybe [IKEVersionsListValue])
tunnelOption_ikeVersions = Lens.lens (\TunnelOption' {ikeVersions} -> ikeVersions) (\s@TunnelOption' {} a -> s {ikeVersions = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and the customer gateway.
tunnelOption_preSharedKey :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Text)
tunnelOption_preSharedKey = Lens.lens (\TunnelOption' {preSharedKey} -> preSharedKey) (\s@TunnelOption' {} a -> s {preSharedKey = a} :: TunnelOption)

-- | The action to take after a DPD timeout occurs.
tunnelOption_dpdTimeoutAction :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Text)
tunnelOption_dpdTimeoutAction = Lens.lens (\TunnelOption' {dpdTimeoutAction} -> dpdTimeoutAction) (\s@TunnelOption' {} a -> s {dpdTimeoutAction = a} :: TunnelOption)

-- | Options for logging VPN tunnel activity.
tunnelOption_logOptions :: Lens.Lens' TunnelOption (Prelude.Maybe VpnTunnelLogOptions)
tunnelOption_logOptions = Lens.lens (\TunnelOption' {logOptions} -> logOptions) (\s@TunnelOption' {} a -> s {logOptions = a} :: TunnelOption)

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 2 IKE negotiations.
tunnelOption_phase2DHGroupNumbers :: Lens.Lens' TunnelOption (Prelude.Maybe [Phase2DHGroupNumbersListValue])
tunnelOption_phase2DHGroupNumbers = Lens.lens (\TunnelOption' {phase2DHGroupNumbers} -> phase2DHGroupNumbers) (\s@TunnelOption' {} a -> s {phase2DHGroupNumbers = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of the rekey window determined by
-- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
-- selected.
tunnelOption_rekeyFuzzPercentage :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Int)
tunnelOption_rekeyFuzzPercentage = Lens.lens (\TunnelOption' {rekeyFuzzPercentage} -> rekeyFuzzPercentage) (\s@TunnelOption' {} a -> s {rekeyFuzzPercentage = a} :: TunnelOption)

-- | The action to take when the establishing the VPN tunnels for a VPN
-- connection.
tunnelOption_startupAction :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Text)
tunnelOption_startupAction = Lens.lens (\TunnelOption' {startupAction} -> startupAction) (\s@TunnelOption' {} a -> s {startupAction = a} :: TunnelOption)

-- | The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey.
tunnelOption_rekeyMarginTimeSeconds :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Int)
tunnelOption_rekeyMarginTimeSeconds = Lens.lens (\TunnelOption' {rekeyMarginTimeSeconds} -> rekeyMarginTimeSeconds) (\s@TunnelOption' {} a -> s {rekeyMarginTimeSeconds = a} :: TunnelOption)

-- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
tunnelOption_phase2IntegrityAlgorithms :: Lens.Lens' TunnelOption (Prelude.Maybe [Phase2IntegrityAlgorithmsListValue])
tunnelOption_phase2IntegrityAlgorithms = Lens.lens (\TunnelOption' {phase2IntegrityAlgorithms} -> phase2IntegrityAlgorithms) (\s@TunnelOption' {} a -> s {phase2IntegrityAlgorithms = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
tunnelOption_phase1EncryptionAlgorithms :: Lens.Lens' TunnelOption (Prelude.Maybe [Phase1EncryptionAlgorithmsListValue])
tunnelOption_phase1EncryptionAlgorithms = Lens.lens (\TunnelOption' {phase1EncryptionAlgorithms} -> phase1EncryptionAlgorithms) (\s@TunnelOption' {} a -> s {phase1EncryptionAlgorithms = a} :: TunnelOption) Prelude.. Lens.mapping Lens.coerced

-- | The range of inside IPv4 addresses for the tunnel.
tunnelOption_tunnelInsideCidr :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Text)
tunnelOption_tunnelInsideCidr = Lens.lens (\TunnelOption' {tunnelInsideCidr} -> tunnelInsideCidr) (\s@TunnelOption' {} a -> s {tunnelInsideCidr = a} :: TunnelOption)

-- | The external IP address of the VPN tunnel.
tunnelOption_outsideIpAddress :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Text)
tunnelOption_outsideIpAddress = Lens.lens (\TunnelOption' {outsideIpAddress} -> outsideIpAddress) (\s@TunnelOption' {} a -> s {outsideIpAddress = a} :: TunnelOption)

-- | The number of packets in an IKE replay window.
tunnelOption_replayWindowSize :: Lens.Lens' TunnelOption (Prelude.Maybe Prelude.Int)
tunnelOption_replayWindowSize = Lens.lens (\TunnelOption' {replayWindowSize} -> replayWindowSize) (\s@TunnelOption' {} a -> s {replayWindowSize = a} :: TunnelOption)

instance Data.FromXML TunnelOption where
  parseXML x =
    TunnelOption'
      Prelude.<$> (x Data..@? "tunnelInsideIpv6Cidr")
      Prelude.<*> (x Data..@? "phase1LifetimeSeconds")
      Prelude.<*> (x Data..@? "phase2LifetimeSeconds")
      Prelude.<*> ( x Data..@? "phase2EncryptionAlgorithmSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "phase1DHGroupNumberSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "phase1IntegrityAlgorithmSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "dpdTimeoutSeconds")
      Prelude.<*> ( x Data..@? "ikeVersionSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "preSharedKey")
      Prelude.<*> (x Data..@? "dpdTimeoutAction")
      Prelude.<*> (x Data..@? "logOptions")
      Prelude.<*> ( x Data..@? "phase2DHGroupNumberSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "rekeyFuzzPercentage")
      Prelude.<*> (x Data..@? "startupAction")
      Prelude.<*> (x Data..@? "rekeyMarginTimeSeconds")
      Prelude.<*> ( x Data..@? "phase2IntegrityAlgorithmSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "phase1EncryptionAlgorithmSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "tunnelInsideCidr")
      Prelude.<*> (x Data..@? "outsideIpAddress")
      Prelude.<*> (x Data..@? "replayWindowSize")

instance Prelude.Hashable TunnelOption where
  hashWithSalt _salt TunnelOption' {..} =
    _salt `Prelude.hashWithSalt` tunnelInsideIpv6Cidr
      `Prelude.hashWithSalt` phase1LifetimeSeconds
      `Prelude.hashWithSalt` phase2LifetimeSeconds
      `Prelude.hashWithSalt` phase2EncryptionAlgorithms
      `Prelude.hashWithSalt` phase1DHGroupNumbers
      `Prelude.hashWithSalt` phase1IntegrityAlgorithms
      `Prelude.hashWithSalt` dpdTimeoutSeconds
      `Prelude.hashWithSalt` ikeVersions
      `Prelude.hashWithSalt` preSharedKey
      `Prelude.hashWithSalt` dpdTimeoutAction
      `Prelude.hashWithSalt` logOptions
      `Prelude.hashWithSalt` phase2DHGroupNumbers
      `Prelude.hashWithSalt` rekeyFuzzPercentage
      `Prelude.hashWithSalt` startupAction
      `Prelude.hashWithSalt` rekeyMarginTimeSeconds
      `Prelude.hashWithSalt` phase2IntegrityAlgorithms
      `Prelude.hashWithSalt` phase1EncryptionAlgorithms
      `Prelude.hashWithSalt` tunnelInsideCidr
      `Prelude.hashWithSalt` outsideIpAddress
      `Prelude.hashWithSalt` replayWindowSize

instance Prelude.NFData TunnelOption where
  rnf TunnelOption' {..} =
    Prelude.rnf tunnelInsideIpv6Cidr
      `Prelude.seq` Prelude.rnf phase1LifetimeSeconds
      `Prelude.seq` Prelude.rnf phase2LifetimeSeconds
      `Prelude.seq` Prelude.rnf phase2EncryptionAlgorithms
      `Prelude.seq` Prelude.rnf phase1DHGroupNumbers
      `Prelude.seq` Prelude.rnf phase1IntegrityAlgorithms
      `Prelude.seq` Prelude.rnf dpdTimeoutSeconds
      `Prelude.seq` Prelude.rnf ikeVersions
      `Prelude.seq` Prelude.rnf preSharedKey
      `Prelude.seq` Prelude.rnf dpdTimeoutAction
      `Prelude.seq` Prelude.rnf logOptions
      `Prelude.seq` Prelude.rnf phase2DHGroupNumbers
      `Prelude.seq` Prelude.rnf rekeyFuzzPercentage
      `Prelude.seq` Prelude.rnf startupAction
      `Prelude.seq` Prelude.rnf rekeyMarginTimeSeconds
      `Prelude.seq` Prelude.rnf phase2IntegrityAlgorithms
      `Prelude.seq` Prelude.rnf
        phase1EncryptionAlgorithms
      `Prelude.seq` Prelude.rnf tunnelInsideCidr
      `Prelude.seq` Prelude.rnf outsideIpAddress
      `Prelude.seq` Prelude.rnf replayWindowSize
