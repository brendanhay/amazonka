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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsTunnelOptionsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsTunnelOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPN tunnel options.
--
-- /See:/ 'newAwsEc2VpnConnectionOptionsTunnelOptionsDetails' smart constructor.
data AwsEc2VpnConnectionOptionsTunnelOptionsDetails = AwsEc2VpnConnectionOptionsTunnelOptionsDetails'
  { -- | The external IP address of the VPN tunnel.
    outsideIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of packets in an IKE replay window.
    replayWindowSize :: Prelude.Maybe Prelude.Int,
    -- | The percentage of the rekey window, which is determined by
    -- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
    -- selected.
    rekeyFuzzPercentage :: Prelude.Maybe Prelude.Int,
    -- | The lifetime for phase 1 of the IKE negotiation, in seconds.
    phase1LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The Internet Key Exchange (IKE) versions that are permitted for the VPN
    -- tunnel.
    ikeVersions :: Prelude.Maybe [Prelude.Text],
    -- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
    -- negotiations.
    phase2IntegrityAlgorithms :: Prelude.Maybe [Prelude.Text],
    -- | The lifetime for phase 2 of the IKE negotiation, in seconds.
    phase2LifetimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
    -- negotiations.
    phase1EncryptionAlgorithms :: Prelude.Maybe [Prelude.Text],
    -- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
    -- 1 IKE negotiations.
    phase1DhGroupNumbers :: Prelude.Maybe [Prelude.Int],
    -- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
    -- negotiations.
    phase1IntegrityAlgorithms :: Prelude.Maybe [Prelude.Text],
    -- | The margin time, in seconds, before the phase 2 lifetime expires, during
    -- which the Amazon Web Services side of the VPN connection performs an IKE
    -- rekey.
    rekeyMarginTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The number of seconds after which a Dead Peer Detection (DPD) timeout
    -- occurs.
    dpdTimeoutSeconds :: Prelude.Maybe Prelude.Int,
    -- | The range of inside IPv4 addresses for the tunnel.
    tunnelInsideCidr :: Prelude.Maybe Prelude.Text,
    -- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
    -- negotiations.
    phase2EncryptionAlgorithms :: Prelude.Maybe [Prelude.Text],
    -- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
    -- 2 IKE negotiations.
    phase2DhGroupNumbers :: Prelude.Maybe [Prelude.Int],
    -- | The preshared key to establish initial authentication between the
    -- virtual private gateway and the customer gateway.
    preSharedKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpnConnectionOptionsTunnelOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outsideIpAddress', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress' - The external IP address of the VPN tunnel.
--
-- 'replayWindowSize', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize' - The number of packets in an IKE replay window.
--
-- 'rekeyFuzzPercentage', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage' - The percentage of the rekey window, which is determined by
-- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
-- selected.
--
-- 'phase1LifetimeSeconds', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- 'ikeVersions', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions' - The Internet Key Exchange (IKE) versions that are permitted for the VPN
-- tunnel.
--
-- 'phase2IntegrityAlgorithms', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- 'phase2LifetimeSeconds', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- 'phase1EncryptionAlgorithms', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- 'phase1DhGroupNumbers', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 1 IKE negotiations.
--
-- 'phase1IntegrityAlgorithms', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- 'rekeyMarginTimeSeconds', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey.
--
-- 'dpdTimeoutSeconds', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds' - The number of seconds after which a Dead Peer Detection (DPD) timeout
-- occurs.
--
-- 'tunnelInsideCidr', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel.
--
-- 'phase2EncryptionAlgorithms', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- 'phase2DhGroupNumbers', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 2 IKE negotiations.
--
-- 'preSharedKey', 'awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey' - The preshared key to establish initial authentication between the
-- virtual private gateway and the customer gateway.
newAwsEc2VpnConnectionOptionsTunnelOptionsDetails ::
  AwsEc2VpnConnectionOptionsTunnelOptionsDetails
newAwsEc2VpnConnectionOptionsTunnelOptionsDetails =
  AwsEc2VpnConnectionOptionsTunnelOptionsDetails'
    { outsideIpAddress =
        Prelude.Nothing,
      replayWindowSize =
        Prelude.Nothing,
      rekeyFuzzPercentage =
        Prelude.Nothing,
      phase1LifetimeSeconds =
        Prelude.Nothing,
      ikeVersions =
        Prelude.Nothing,
      phase2IntegrityAlgorithms =
        Prelude.Nothing,
      phase2LifetimeSeconds =
        Prelude.Nothing,
      phase1EncryptionAlgorithms =
        Prelude.Nothing,
      phase1DhGroupNumbers =
        Prelude.Nothing,
      phase1IntegrityAlgorithms =
        Prelude.Nothing,
      rekeyMarginTimeSeconds =
        Prelude.Nothing,
      dpdTimeoutSeconds =
        Prelude.Nothing,
      tunnelInsideCidr =
        Prelude.Nothing,
      phase2EncryptionAlgorithms =
        Prelude.Nothing,
      phase2DhGroupNumbers =
        Prelude.Nothing,
      preSharedKey =
        Prelude.Nothing
    }

-- | The external IP address of the VPN tunnel.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_outsideIpAddress = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {outsideIpAddress} -> outsideIpAddress) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {outsideIpAddress = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The number of packets in an IKE replay window.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_replayWindowSize = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {replayWindowSize} -> replayWindowSize) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {replayWindowSize = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The percentage of the rekey window, which is determined by
-- @RekeyMarginTimeSeconds@ during which the rekey time is randomly
-- selected.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyFuzzPercentage = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {rekeyFuzzPercentage} -> rekeyFuzzPercentage) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {rekeyFuzzPercentage = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1LifetimeSeconds = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase1LifetimeSeconds} -> phase1LifetimeSeconds) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase1LifetimeSeconds = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The Internet Key Exchange (IKE) versions that are permitted for the VPN
-- tunnel.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_ikeVersions = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {ikeVersions} -> ikeVersions) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {ikeVersions = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2IntegrityAlgorithms = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase2IntegrityAlgorithms} -> phase2IntegrityAlgorithms) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase2IntegrityAlgorithms = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2LifetimeSeconds = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase2LifetimeSeconds} -> phase2LifetimeSeconds) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase2LifetimeSeconds = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1EncryptionAlgorithms = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase1EncryptionAlgorithms} -> phase1EncryptionAlgorithms) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase1EncryptionAlgorithms = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 1 IKE negotiations.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Int])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1DhGroupNumbers = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase1DhGroupNumbers} -> phase1DhGroupNumbers) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase1DhGroupNumbers = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE
-- negotiations.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase1IntegrityAlgorithms = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase1IntegrityAlgorithms} -> phase1IntegrityAlgorithms) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase1IntegrityAlgorithms = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The margin time, in seconds, before the phase 2 lifetime expires, during
-- which the Amazon Web Services side of the VPN connection performs an IKE
-- rekey.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_rekeyMarginTimeSeconds = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {rekeyMarginTimeSeconds} -> rekeyMarginTimeSeconds) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {rekeyMarginTimeSeconds = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The number of seconds after which a Dead Peer Detection (DPD) timeout
-- occurs.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_dpdTimeoutSeconds = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {dpdTimeoutSeconds} -> dpdTimeoutSeconds) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {dpdTimeoutSeconds = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The range of inside IPv4 addresses for the tunnel.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_tunnelInsideCidr = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {tunnelInsideCidr} -> tunnelInsideCidr) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {tunnelInsideCidr = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

-- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE
-- negotiations.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2EncryptionAlgorithms = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase2EncryptionAlgorithms} -> phase2EncryptionAlgorithms) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase2EncryptionAlgorithms = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase
-- 2 IKE negotiations.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe [Prelude.Int])
awsEc2VpnConnectionOptionsTunnelOptionsDetails_phase2DhGroupNumbers = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {phase2DhGroupNumbers} -> phase2DhGroupNumbers) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {phase2DhGroupNumbers = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The preshared key to establish initial authentication between the
-- virtual private gateway and the customer gateway.
awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey :: Lens.Lens' AwsEc2VpnConnectionOptionsTunnelOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionOptionsTunnelOptionsDetails_preSharedKey = Lens.lens (\AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {preSharedKey} -> preSharedKey) (\s@AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {} a -> s {preSharedKey = a} :: AwsEc2VpnConnectionOptionsTunnelOptionsDetails)

instance
  Core.FromJSON
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEc2VpnConnectionOptionsTunnelOptionsDetails"
      ( \x ->
          AwsEc2VpnConnectionOptionsTunnelOptionsDetails'
            Prelude.<$> (x Core..:? "OutsideIpAddress")
              Prelude.<*> (x Core..:? "ReplayWindowSize")
              Prelude.<*> (x Core..:? "RekeyFuzzPercentage")
              Prelude.<*> (x Core..:? "Phase1LifetimeSeconds")
              Prelude.<*> (x Core..:? "IkeVersions" Core..!= Prelude.mempty)
              Prelude.<*> ( x Core..:? "Phase2IntegrityAlgorithms"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "Phase2LifetimeSeconds")
              Prelude.<*> ( x Core..:? "Phase1EncryptionAlgorithms"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> ( x Core..:? "Phase1DhGroupNumbers"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> ( x Core..:? "Phase1IntegrityAlgorithms"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "RekeyMarginTimeSeconds")
              Prelude.<*> (x Core..:? "DpdTimeoutSeconds")
              Prelude.<*> (x Core..:? "TunnelInsideCidr")
              Prelude.<*> ( x Core..:? "Phase2EncryptionAlgorithms"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> ( x Core..:? "Phase2DhGroupNumbers"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "PreSharedKey")
      )

instance
  Prelude.Hashable
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails

instance
  Prelude.NFData
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails

instance
  Core.ToJSON
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails
  where
  toJSON
    AwsEc2VpnConnectionOptionsTunnelOptionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("OutsideIpAddress" Core..=)
                Prelude.<$> outsideIpAddress,
              ("ReplayWindowSize" Core..=)
                Prelude.<$> replayWindowSize,
              ("RekeyFuzzPercentage" Core..=)
                Prelude.<$> rekeyFuzzPercentage,
              ("Phase1LifetimeSeconds" Core..=)
                Prelude.<$> phase1LifetimeSeconds,
              ("IkeVersions" Core..=) Prelude.<$> ikeVersions,
              ("Phase2IntegrityAlgorithms" Core..=)
                Prelude.<$> phase2IntegrityAlgorithms,
              ("Phase2LifetimeSeconds" Core..=)
                Prelude.<$> phase2LifetimeSeconds,
              ("Phase1EncryptionAlgorithms" Core..=)
                Prelude.<$> phase1EncryptionAlgorithms,
              ("Phase1DhGroupNumbers" Core..=)
                Prelude.<$> phase1DhGroupNumbers,
              ("Phase1IntegrityAlgorithms" Core..=)
                Prelude.<$> phase1IntegrityAlgorithms,
              ("RekeyMarginTimeSeconds" Core..=)
                Prelude.<$> rekeyMarginTimeSeconds,
              ("DpdTimeoutSeconds" Core..=)
                Prelude.<$> dpdTimeoutSeconds,
              ("TunnelInsideCidr" Core..=)
                Prelude.<$> tunnelInsideCidr,
              ("Phase2EncryptionAlgorithms" Core..=)
                Prelude.<$> phase2EncryptionAlgorithms,
              ("Phase2DhGroupNumbers" Core..=)
                Prelude.<$> phase2DhGroupNumbers,
              ("PreSharedKey" Core..=) Prelude.<$> preSharedKey
            ]
        )
