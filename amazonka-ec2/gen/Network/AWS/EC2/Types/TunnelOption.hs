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
-- Module      : Network.AWS.EC2.Types.TunnelOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TunnelOption where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
import qualified Network.AWS.Lens as Lens

-- | The VPN tunnel options.
--
-- /See:/ 'newTunnelOption' smart constructor.
data TunnelOption = TunnelOption'
  { -- | The lifetime for phase 1 of the IKE negotiation, in seconds.
    phase1LifetimeSeconds :: Core.Maybe Core.Int,
    -- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
    -- 2 IKE negotiations.
    phase2DHGroupNumbers :: Core.Maybe [Phase2DHGroupNumbersListValue],
    -- | The IKE versions that are permitted for the VPN tunnel.
    ikeVersions :: Core.Maybe [IKEVersionsListValue],
    -- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
    -- negotiations.
    phase2EncryptionAlgorithms :: Core.Maybe [Phase2EncryptionAlgorithmsListValue],
    -- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
    -- negotiations.
    phase2IntegrityAlgorithms :: Core.Maybe [Phase2IntegrityAlgorithmsListValue],
    -- | The action to take when the establishing the VPN tunnels for a VPN
    -- connection.
    startupAction :: Core.Maybe Core.Text,
    -- | The number of seconds after which a DPD timeout occurs.
    dpdTimeoutSeconds :: Core.Maybe Core.Int,
    -- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
    -- 1 IKE negotiations.
    phase1DHGroupNumbers :: Core.Maybe [Phase1DHGroupNumbersListValue],
    -- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
    -- negotiations.
    phase1EncryptionAlgorithms :: Core.Maybe [Phase1EncryptionAlgorithmsListValue],
    -- | The number of packets in an IKE replay window.
    replayWindowSize :: Core.Maybe Core.Int,
    -- | The external IP address of the VPN tunnel.
    outsideIpAddress :: Core.Maybe Core.Text,
    -- | The pre-shared key (PSK) to establish initial authentication between the
    -- virtual private gateway and the customer gateway.
    preSharedKey :: Core.Maybe Core.Text,
    -- | The range of inside IPv6 addresses for the tunnel.
    tunnelInsideIpv6Cidr :: Core.Maybe Core.Text,
    -- | The percentage of the rekey window determined by
    -- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
    -- selected.
    rekeyFuzzPercentage :: Core.Maybe Core.Int,
    -- | The margin time, in seconds, before the phase 2 lifetime expires, during
    -- which the AWS side of the VPN connection performs an IKE rekey.
    rekeyMarginTimeSeconds :: Core.Maybe Core.Int,
    -- | The range of inside IPv4 addresses for the tunnel.
    tunnelInsideCidr :: Core.Maybe Core.Text,
    -- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
    -- negotiations.
    phase1IntegrityAlgorithms :: Core.Maybe [Phase1IntegrityAlgorithmsListValue],
    -- | The action to take after a DPD timeout occurs.
    dpdTimeoutAction :: Core.Maybe Core.Text,
    -- | The lifetime for phase 2 of the IKE negotiation, in seconds.
    phase2LifetimeSeconds :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TunnelOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phase1LifetimeSeconds', 'tunnelOption_phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- 'phase2DHGroupNumbers', 'tunnelOption_phase2DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 2 IKE negotiations.
--
-- 'ikeVersions', 'tunnelOption_ikeVersions' - The IKE versions that are permitted for the VPN tunnel.
--
-- 'phase2EncryptionAlgorithms', 'tunnelOption_phase2EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- 'phase2IntegrityAlgorithms', 'tunnelOption_phase2IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- 'startupAction', 'tunnelOption_startupAction' - The action to take when the establishing the VPN tunnels for a VPN
-- connection.
--
-- 'dpdTimeoutSeconds', 'tunnelOption_dpdTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
--
-- 'phase1DHGroupNumbers', 'tunnelOption_phase1DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 1 IKE negotiations.
--
-- 'phase1EncryptionAlgorithms', 'tunnelOption_phase1EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- 'replayWindowSize', 'tunnelOption_replayWindowSize' - The number of packets in an IKE replay window.
--
-- 'outsideIpAddress', 'tunnelOption_outsideIpAddress' - The external IP address of the VPN tunnel.
--
-- 'preSharedKey', 'tunnelOption_preSharedKey' - The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and the customer gateway.
--
-- 'tunnelInsideIpv6Cidr', 'tunnelOption_tunnelInsideIpv6Cidr' - The range of inside IPv6 addresses for the tunnel.
--
-- 'rekeyFuzzPercentage', 'tunnelOption_rekeyFuzzPercentage' - The percentage of the rekey window determined by
-- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
-- selected.
--
-- 'rekeyMarginTimeSeconds', 'tunnelOption_rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the AWS side of the VPN connection performs an IKE rekey.
--
-- 'tunnelInsideCidr', 'tunnelOption_tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel.
--
-- 'phase1IntegrityAlgorithms', 'tunnelOption_phase1IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- 'dpdTimeoutAction', 'tunnelOption_dpdTimeoutAction' - The action to take after a DPD timeout occurs.
--
-- 'phase2LifetimeSeconds', 'tunnelOption_phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
newTunnelOption ::
  TunnelOption
newTunnelOption =
  TunnelOption'
    { phase1LifetimeSeconds = Core.Nothing,
      phase2DHGroupNumbers = Core.Nothing,
      ikeVersions = Core.Nothing,
      phase2EncryptionAlgorithms = Core.Nothing,
      phase2IntegrityAlgorithms = Core.Nothing,
      startupAction = Core.Nothing,
      dpdTimeoutSeconds = Core.Nothing,
      phase1DHGroupNumbers = Core.Nothing,
      phase1EncryptionAlgorithms = Core.Nothing,
      replayWindowSize = Core.Nothing,
      outsideIpAddress = Core.Nothing,
      preSharedKey = Core.Nothing,
      tunnelInsideIpv6Cidr = Core.Nothing,
      rekeyFuzzPercentage = Core.Nothing,
      rekeyMarginTimeSeconds = Core.Nothing,
      tunnelInsideCidr = Core.Nothing,
      phase1IntegrityAlgorithms = Core.Nothing,
      dpdTimeoutAction = Core.Nothing,
      phase2LifetimeSeconds = Core.Nothing
    }

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
tunnelOption_phase1LifetimeSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
tunnelOption_phase1LifetimeSeconds = Lens.lens (\TunnelOption' {phase1LifetimeSeconds} -> phase1LifetimeSeconds) (\s@TunnelOption' {} a -> s {phase1LifetimeSeconds = a} :: TunnelOption)

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 2 IKE negotiations.
tunnelOption_phase2DHGroupNumbers :: Lens.Lens' TunnelOption (Core.Maybe [Phase2DHGroupNumbersListValue])
tunnelOption_phase2DHGroupNumbers = Lens.lens (\TunnelOption' {phase2DHGroupNumbers} -> phase2DHGroupNumbers) (\s@TunnelOption' {} a -> s {phase2DHGroupNumbers = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The IKE versions that are permitted for the VPN tunnel.
tunnelOption_ikeVersions :: Lens.Lens' TunnelOption (Core.Maybe [IKEVersionsListValue])
tunnelOption_ikeVersions = Lens.lens (\TunnelOption' {ikeVersions} -> ikeVersions) (\s@TunnelOption' {} a -> s {ikeVersions = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
tunnelOption_phase2EncryptionAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Phase2EncryptionAlgorithmsListValue])
tunnelOption_phase2EncryptionAlgorithms = Lens.lens (\TunnelOption' {phase2EncryptionAlgorithms} -> phase2EncryptionAlgorithms) (\s@TunnelOption' {} a -> s {phase2EncryptionAlgorithms = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
tunnelOption_phase2IntegrityAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Phase2IntegrityAlgorithmsListValue])
tunnelOption_phase2IntegrityAlgorithms = Lens.lens (\TunnelOption' {phase2IntegrityAlgorithms} -> phase2IntegrityAlgorithms) (\s@TunnelOption' {} a -> s {phase2IntegrityAlgorithms = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The action to take when the establishing the VPN tunnels for a VPN
-- connection.
tunnelOption_startupAction :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
tunnelOption_startupAction = Lens.lens (\TunnelOption' {startupAction} -> startupAction) (\s@TunnelOption' {} a -> s {startupAction = a} :: TunnelOption)

-- | The number of seconds after which a DPD timeout occurs.
tunnelOption_dpdTimeoutSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
tunnelOption_dpdTimeoutSeconds = Lens.lens (\TunnelOption' {dpdTimeoutSeconds} -> dpdTimeoutSeconds) (\s@TunnelOption' {} a -> s {dpdTimeoutSeconds = a} :: TunnelOption)

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 1 IKE negotiations.
tunnelOption_phase1DHGroupNumbers :: Lens.Lens' TunnelOption (Core.Maybe [Phase1DHGroupNumbersListValue])
tunnelOption_phase1DHGroupNumbers = Lens.lens (\TunnelOption' {phase1DHGroupNumbers} -> phase1DHGroupNumbers) (\s@TunnelOption' {} a -> s {phase1DHGroupNumbers = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
tunnelOption_phase1EncryptionAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Phase1EncryptionAlgorithmsListValue])
tunnelOption_phase1EncryptionAlgorithms = Lens.lens (\TunnelOption' {phase1EncryptionAlgorithms} -> phase1EncryptionAlgorithms) (\s@TunnelOption' {} a -> s {phase1EncryptionAlgorithms = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The number of packets in an IKE replay window.
tunnelOption_replayWindowSize :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
tunnelOption_replayWindowSize = Lens.lens (\TunnelOption' {replayWindowSize} -> replayWindowSize) (\s@TunnelOption' {} a -> s {replayWindowSize = a} :: TunnelOption)

-- | The external IP address of the VPN tunnel.
tunnelOption_outsideIpAddress :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
tunnelOption_outsideIpAddress = Lens.lens (\TunnelOption' {outsideIpAddress} -> outsideIpAddress) (\s@TunnelOption' {} a -> s {outsideIpAddress = a} :: TunnelOption)

-- | The pre-shared key (PSK) to establish initial authentication between the
-- virtual private gateway and the customer gateway.
tunnelOption_preSharedKey :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
tunnelOption_preSharedKey = Lens.lens (\TunnelOption' {preSharedKey} -> preSharedKey) (\s@TunnelOption' {} a -> s {preSharedKey = a} :: TunnelOption)

-- | The range of inside IPv6 addresses for the tunnel.
tunnelOption_tunnelInsideIpv6Cidr :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
tunnelOption_tunnelInsideIpv6Cidr = Lens.lens (\TunnelOption' {tunnelInsideIpv6Cidr} -> tunnelInsideIpv6Cidr) (\s@TunnelOption' {} a -> s {tunnelInsideIpv6Cidr = a} :: TunnelOption)

-- | The percentage of the rekey window determined by
-- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
-- selected.
tunnelOption_rekeyFuzzPercentage :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
tunnelOption_rekeyFuzzPercentage = Lens.lens (\TunnelOption' {rekeyFuzzPercentage} -> rekeyFuzzPercentage) (\s@TunnelOption' {} a -> s {rekeyFuzzPercentage = a} :: TunnelOption)

-- | The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the AWS side of the VPN connection performs an IKE rekey.
tunnelOption_rekeyMarginTimeSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
tunnelOption_rekeyMarginTimeSeconds = Lens.lens (\TunnelOption' {rekeyMarginTimeSeconds} -> rekeyMarginTimeSeconds) (\s@TunnelOption' {} a -> s {rekeyMarginTimeSeconds = a} :: TunnelOption)

-- | The range of inside IPv4 addresses for the tunnel.
tunnelOption_tunnelInsideCidr :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
tunnelOption_tunnelInsideCidr = Lens.lens (\TunnelOption' {tunnelInsideCidr} -> tunnelInsideCidr) (\s@TunnelOption' {} a -> s {tunnelInsideCidr = a} :: TunnelOption)

-- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
tunnelOption_phase1IntegrityAlgorithms :: Lens.Lens' TunnelOption (Core.Maybe [Phase1IntegrityAlgorithmsListValue])
tunnelOption_phase1IntegrityAlgorithms = Lens.lens (\TunnelOption' {phase1IntegrityAlgorithms} -> phase1IntegrityAlgorithms) (\s@TunnelOption' {} a -> s {phase1IntegrityAlgorithms = a} :: TunnelOption) Core.. Lens.mapping Lens._Coerce

-- | The action to take after a DPD timeout occurs.
tunnelOption_dpdTimeoutAction :: Lens.Lens' TunnelOption (Core.Maybe Core.Text)
tunnelOption_dpdTimeoutAction = Lens.lens (\TunnelOption' {dpdTimeoutAction} -> dpdTimeoutAction) (\s@TunnelOption' {} a -> s {dpdTimeoutAction = a} :: TunnelOption)

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
tunnelOption_phase2LifetimeSeconds :: Lens.Lens' TunnelOption (Core.Maybe Core.Int)
tunnelOption_phase2LifetimeSeconds = Lens.lens (\TunnelOption' {phase2LifetimeSeconds} -> phase2LifetimeSeconds) (\s@TunnelOption' {} a -> s {phase2LifetimeSeconds = a} :: TunnelOption)

instance Core.FromXML TunnelOption where
  parseXML x =
    TunnelOption'
      Core.<$> (x Core..@? "phase1LifetimeSeconds")
      Core.<*> ( x Core..@? "phase2DHGroupNumberSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "ikeVersionSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "phase2EncryptionAlgorithmSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "phase2IntegrityAlgorithmSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "startupAction")
      Core.<*> (x Core..@? "dpdTimeoutSeconds")
      Core.<*> ( x Core..@? "phase1DHGroupNumberSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "phase1EncryptionAlgorithmSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "replayWindowSize")
      Core.<*> (x Core..@? "outsideIpAddress")
      Core.<*> (x Core..@? "preSharedKey")
      Core.<*> (x Core..@? "tunnelInsideIpv6Cidr")
      Core.<*> (x Core..@? "rekeyFuzzPercentage")
      Core.<*> (x Core..@? "rekeyMarginTimeSeconds")
      Core.<*> (x Core..@? "tunnelInsideCidr")
      Core.<*> ( x Core..@? "phase1IntegrityAlgorithmSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "dpdTimeoutAction")
      Core.<*> (x Core..@? "phase2LifetimeSeconds")

instance Core.Hashable TunnelOption

instance Core.NFData TunnelOption
