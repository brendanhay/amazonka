{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TunnelOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TunnelOption
  ( TunnelOption (..),

    -- * Smart constructor
    mkTunnelOption,

    -- * Lenses
    toOutsideIPAddress,
    toReplayWindowSize,
    toDpdTimeoutAction,
    toRekeyFuzzPercentage,
    toPhase1LifetimeSeconds,
    toIkeVersions,
    toPhase2IntegrityAlgorithms,
    toPhase2LifetimeSeconds,
    toPhase1EncryptionAlgorithms,
    toPhase1DHGroupNumbers,
    toPhase1IntegrityAlgorithms,
    toRekeyMarginTimeSeconds,
    toDpdTimeoutSeconds,
    toTunnelInsideCidr,
    toStartupAction,
    toPhase2EncryptionAlgorithms,
    toPhase2DHGroupNumbers,
    toPreSharedKey,
    toTunnelInsideIPv6Cidr,
  )
where

import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The VPN tunnel options.
--
-- /See:/ 'mkTunnelOption' smart constructor.
data TunnelOption = TunnelOption'
  { outsideIPAddress ::
      Lude.Maybe Lude.Text,
    replayWindowSize :: Lude.Maybe Lude.Int,
    dpdTimeoutAction :: Lude.Maybe Lude.Text,
    rekeyFuzzPercentage :: Lude.Maybe Lude.Int,
    phase1LifetimeSeconds :: Lude.Maybe Lude.Int,
    ikeVersions :: Lude.Maybe [IKEVersionsListValue],
    phase2IntegrityAlgorithms ::
      Lude.Maybe [Phase2IntegrityAlgorithmsListValue],
    phase2LifetimeSeconds :: Lude.Maybe Lude.Int,
    phase1EncryptionAlgorithms ::
      Lude.Maybe [Phase1EncryptionAlgorithmsListValue],
    phase1DHGroupNumbers ::
      Lude.Maybe [Phase1DHGroupNumbersListValue],
    phase1IntegrityAlgorithms ::
      Lude.Maybe [Phase1IntegrityAlgorithmsListValue],
    rekeyMarginTimeSeconds :: Lude.Maybe Lude.Int,
    dpdTimeoutSeconds :: Lude.Maybe Lude.Int,
    tunnelInsideCidr :: Lude.Maybe Lude.Text,
    startupAction :: Lude.Maybe Lude.Text,
    phase2EncryptionAlgorithms ::
      Lude.Maybe [Phase2EncryptionAlgorithmsListValue],
    phase2DHGroupNumbers ::
      Lude.Maybe [Phase2DHGroupNumbersListValue],
    preSharedKey :: Lude.Maybe Lude.Text,
    tunnelInsideIPv6Cidr :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TunnelOption' with the minimum fields required to make a request.
--
-- * 'dpdTimeoutAction' - The action to take after a DPD timeout occurs.
-- * 'dpdTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
-- * 'ikeVersions' - The IKE versions that are permitted for the VPN tunnel.
-- * 'outsideIPAddress' - The external IP address of the VPN tunnel.
-- * 'phase1DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 1 IKE negotiations.
-- * 'phase1EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 1 IKE negotiations.
-- * 'phase1IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 1 IKE negotiations.
-- * 'phase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
-- * 'phase2DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 2 IKE negotiations.
-- * 'phase2EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 2 IKE negotiations.
-- * 'phase2IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 2 IKE negotiations.
-- * 'phase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
-- * 'preSharedKey' - The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
-- * 'rekeyFuzzPercentage' - The percentage of the rekey window determined by @RekeyMarginTimeSeconds@ during which the rekey time is randomly selected.
-- * 'rekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey.
-- * 'replayWindowSize' - The number of packets in an IKE replay window.
-- * 'startupAction' - The action to take when the establishing the VPN tunnels for a VPN connection.
-- * 'tunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel.
-- * 'tunnelInsideIPv6Cidr' - The range of inside IPv6 addresses for the tunnel.
mkTunnelOption ::
  TunnelOption
mkTunnelOption =
  TunnelOption'
    { outsideIPAddress = Lude.Nothing,
      replayWindowSize = Lude.Nothing,
      dpdTimeoutAction = Lude.Nothing,
      rekeyFuzzPercentage = Lude.Nothing,
      phase1LifetimeSeconds = Lude.Nothing,
      ikeVersions = Lude.Nothing,
      phase2IntegrityAlgorithms = Lude.Nothing,
      phase2LifetimeSeconds = Lude.Nothing,
      phase1EncryptionAlgorithms = Lude.Nothing,
      phase1DHGroupNumbers = Lude.Nothing,
      phase1IntegrityAlgorithms = Lude.Nothing,
      rekeyMarginTimeSeconds = Lude.Nothing,
      dpdTimeoutSeconds = Lude.Nothing,
      tunnelInsideCidr = Lude.Nothing,
      startupAction = Lude.Nothing,
      phase2EncryptionAlgorithms = Lude.Nothing,
      phase2DHGroupNumbers = Lude.Nothing,
      preSharedKey = Lude.Nothing,
      tunnelInsideIPv6Cidr = Lude.Nothing
    }

-- | The external IP address of the VPN tunnel.
--
-- /Note:/ Consider using 'outsideIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toOutsideIPAddress :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Text)
toOutsideIPAddress = Lens.lens (outsideIPAddress :: TunnelOption -> Lude.Maybe Lude.Text) (\s a -> s {outsideIPAddress = a} :: TunnelOption)
{-# DEPRECATED toOutsideIPAddress "Use generic-lens or generic-optics with 'outsideIPAddress' instead." #-}

-- | The number of packets in an IKE replay window.
--
-- /Note:/ Consider using 'replayWindowSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toReplayWindowSize :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Int)
toReplayWindowSize = Lens.lens (replayWindowSize :: TunnelOption -> Lude.Maybe Lude.Int) (\s a -> s {replayWindowSize = a} :: TunnelOption)
{-# DEPRECATED toReplayWindowSize "Use generic-lens or generic-optics with 'replayWindowSize' instead." #-}

-- | The action to take after a DPD timeout occurs.
--
-- /Note:/ Consider using 'dpdTimeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toDpdTimeoutAction :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Text)
toDpdTimeoutAction = Lens.lens (dpdTimeoutAction :: TunnelOption -> Lude.Maybe Lude.Text) (\s a -> s {dpdTimeoutAction = a} :: TunnelOption)
{-# DEPRECATED toDpdTimeoutAction "Use generic-lens or generic-optics with 'dpdTimeoutAction' instead." #-}

-- | The percentage of the rekey window determined by @RekeyMarginTimeSeconds@ during which the rekey time is randomly selected.
--
-- /Note:/ Consider using 'rekeyFuzzPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toRekeyFuzzPercentage :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Int)
toRekeyFuzzPercentage = Lens.lens (rekeyFuzzPercentage :: TunnelOption -> Lude.Maybe Lude.Int) (\s a -> s {rekeyFuzzPercentage = a} :: TunnelOption)
{-# DEPRECATED toRekeyFuzzPercentage "Use generic-lens or generic-optics with 'rekeyFuzzPercentage' instead." #-}

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- /Note:/ Consider using 'phase1LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1LifetimeSeconds :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Int)
toPhase1LifetimeSeconds = Lens.lens (phase1LifetimeSeconds :: TunnelOption -> Lude.Maybe Lude.Int) (\s a -> s {phase1LifetimeSeconds = a} :: TunnelOption)
{-# DEPRECATED toPhase1LifetimeSeconds "Use generic-lens or generic-optics with 'phase1LifetimeSeconds' instead." #-}

-- | The IKE versions that are permitted for the VPN tunnel.
--
-- /Note:/ Consider using 'ikeVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toIkeVersions :: Lens.Lens' TunnelOption (Lude.Maybe [IKEVersionsListValue])
toIkeVersions = Lens.lens (ikeVersions :: TunnelOption -> Lude.Maybe [IKEVersionsListValue]) (\s a -> s {ikeVersions = a} :: TunnelOption)
{-# DEPRECATED toIkeVersions "Use generic-lens or generic-optics with 'ikeVersions' instead." #-}

-- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE negotiations.
--
-- /Note:/ Consider using 'phase2IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2IntegrityAlgorithms :: Lens.Lens' TunnelOption (Lude.Maybe [Phase2IntegrityAlgorithmsListValue])
toPhase2IntegrityAlgorithms = Lens.lens (phase2IntegrityAlgorithms :: TunnelOption -> Lude.Maybe [Phase2IntegrityAlgorithmsListValue]) (\s a -> s {phase2IntegrityAlgorithms = a} :: TunnelOption)
{-# DEPRECATED toPhase2IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase2IntegrityAlgorithms' instead." #-}

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- /Note:/ Consider using 'phase2LifetimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2LifetimeSeconds :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Int)
toPhase2LifetimeSeconds = Lens.lens (phase2LifetimeSeconds :: TunnelOption -> Lude.Maybe Lude.Int) (\s a -> s {phase2LifetimeSeconds = a} :: TunnelOption)
{-# DEPRECATED toPhase2LifetimeSeconds "Use generic-lens or generic-optics with 'phase2LifetimeSeconds' instead." #-}

-- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE negotiations.
--
-- /Note:/ Consider using 'phase1EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1EncryptionAlgorithms :: Lens.Lens' TunnelOption (Lude.Maybe [Phase1EncryptionAlgorithmsListValue])
toPhase1EncryptionAlgorithms = Lens.lens (phase1EncryptionAlgorithms :: TunnelOption -> Lude.Maybe [Phase1EncryptionAlgorithmsListValue]) (\s a -> s {phase1EncryptionAlgorithms = a} :: TunnelOption)
{-# DEPRECATED toPhase1EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase1EncryptionAlgorithms' instead." #-}

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 1 IKE negotiations.
--
-- /Note:/ Consider using 'phase1DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1DHGroupNumbers :: Lens.Lens' TunnelOption (Lude.Maybe [Phase1DHGroupNumbersListValue])
toPhase1DHGroupNumbers = Lens.lens (phase1DHGroupNumbers :: TunnelOption -> Lude.Maybe [Phase1DHGroupNumbersListValue]) (\s a -> s {phase1DHGroupNumbers = a} :: TunnelOption)
{-# DEPRECATED toPhase1DHGroupNumbers "Use generic-lens or generic-optics with 'phase1DHGroupNumbers' instead." #-}

-- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE negotiations.
--
-- /Note:/ Consider using 'phase1IntegrityAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase1IntegrityAlgorithms :: Lens.Lens' TunnelOption (Lude.Maybe [Phase1IntegrityAlgorithmsListValue])
toPhase1IntegrityAlgorithms = Lens.lens (phase1IntegrityAlgorithms :: TunnelOption -> Lude.Maybe [Phase1IntegrityAlgorithmsListValue]) (\s a -> s {phase1IntegrityAlgorithms = a} :: TunnelOption)
{-# DEPRECATED toPhase1IntegrityAlgorithms "Use generic-lens or generic-optics with 'phase1IntegrityAlgorithms' instead." #-}

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey.
--
-- /Note:/ Consider using 'rekeyMarginTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toRekeyMarginTimeSeconds :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Int)
toRekeyMarginTimeSeconds = Lens.lens (rekeyMarginTimeSeconds :: TunnelOption -> Lude.Maybe Lude.Int) (\s a -> s {rekeyMarginTimeSeconds = a} :: TunnelOption)
{-# DEPRECATED toRekeyMarginTimeSeconds "Use generic-lens or generic-optics with 'rekeyMarginTimeSeconds' instead." #-}

-- | The number of seconds after which a DPD timeout occurs.
--
-- /Note:/ Consider using 'dpdTimeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toDpdTimeoutSeconds :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Int)
toDpdTimeoutSeconds = Lens.lens (dpdTimeoutSeconds :: TunnelOption -> Lude.Maybe Lude.Int) (\s a -> s {dpdTimeoutSeconds = a} :: TunnelOption)
{-# DEPRECATED toDpdTimeoutSeconds "Use generic-lens or generic-optics with 'dpdTimeoutSeconds' instead." #-}

-- | The range of inside IPv4 addresses for the tunnel.
--
-- /Note:/ Consider using 'tunnelInsideCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTunnelInsideCidr :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Text)
toTunnelInsideCidr = Lens.lens (tunnelInsideCidr :: TunnelOption -> Lude.Maybe Lude.Text) (\s a -> s {tunnelInsideCidr = a} :: TunnelOption)
{-# DEPRECATED toTunnelInsideCidr "Use generic-lens or generic-optics with 'tunnelInsideCidr' instead." #-}

-- | The action to take when the establishing the VPN tunnels for a VPN connection.
--
-- /Note:/ Consider using 'startupAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toStartupAction :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Text)
toStartupAction = Lens.lens (startupAction :: TunnelOption -> Lude.Maybe Lude.Text) (\s a -> s {startupAction = a} :: TunnelOption)
{-# DEPRECATED toStartupAction "Use generic-lens or generic-optics with 'startupAction' instead." #-}

-- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE negotiations.
--
-- /Note:/ Consider using 'phase2EncryptionAlgorithms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2EncryptionAlgorithms :: Lens.Lens' TunnelOption (Lude.Maybe [Phase2EncryptionAlgorithmsListValue])
toPhase2EncryptionAlgorithms = Lens.lens (phase2EncryptionAlgorithms :: TunnelOption -> Lude.Maybe [Phase2EncryptionAlgorithmsListValue]) (\s a -> s {phase2EncryptionAlgorithms = a} :: TunnelOption)
{-# DEPRECATED toPhase2EncryptionAlgorithms "Use generic-lens or generic-optics with 'phase2EncryptionAlgorithms' instead." #-}

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 2 IKE negotiations.
--
-- /Note:/ Consider using 'phase2DHGroupNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPhase2DHGroupNumbers :: Lens.Lens' TunnelOption (Lude.Maybe [Phase2DHGroupNumbersListValue])
toPhase2DHGroupNumbers = Lens.lens (phase2DHGroupNumbers :: TunnelOption -> Lude.Maybe [Phase2DHGroupNumbersListValue]) (\s a -> s {phase2DHGroupNumbers = a} :: TunnelOption)
{-# DEPRECATED toPhase2DHGroupNumbers "Use generic-lens or generic-optics with 'phase2DHGroupNumbers' instead." #-}

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- /Note:/ Consider using 'preSharedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPreSharedKey :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Text)
toPreSharedKey = Lens.lens (preSharedKey :: TunnelOption -> Lude.Maybe Lude.Text) (\s a -> s {preSharedKey = a} :: TunnelOption)
{-# DEPRECATED toPreSharedKey "Use generic-lens or generic-optics with 'preSharedKey' instead." #-}

-- | The range of inside IPv6 addresses for the tunnel.
--
-- /Note:/ Consider using 'tunnelInsideIPv6Cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTunnelInsideIPv6Cidr :: Lens.Lens' TunnelOption (Lude.Maybe Lude.Text)
toTunnelInsideIPv6Cidr = Lens.lens (tunnelInsideIPv6Cidr :: TunnelOption -> Lude.Maybe Lude.Text) (\s a -> s {tunnelInsideIPv6Cidr = a} :: TunnelOption)
{-# DEPRECATED toTunnelInsideIPv6Cidr "Use generic-lens or generic-optics with 'tunnelInsideIPv6Cidr' instead." #-}

instance Lude.FromXML TunnelOption where
  parseXML x =
    TunnelOption'
      Lude.<$> (x Lude..@? "outsideIpAddress")
      Lude.<*> (x Lude..@? "replayWindowSize")
      Lude.<*> (x Lude..@? "dpdTimeoutAction")
      Lude.<*> (x Lude..@? "rekeyFuzzPercentage")
      Lude.<*> (x Lude..@? "phase1LifetimeSeconds")
      Lude.<*> ( x Lude..@? "ikeVersionSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "phase2IntegrityAlgorithmSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "phase2LifetimeSeconds")
      Lude.<*> ( x Lude..@? "phase1EncryptionAlgorithmSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "phase1DHGroupNumberSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "phase1IntegrityAlgorithmSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "rekeyMarginTimeSeconds")
      Lude.<*> (x Lude..@? "dpdTimeoutSeconds")
      Lude.<*> (x Lude..@? "tunnelInsideCidr")
      Lude.<*> (x Lude..@? "startupAction")
      Lude.<*> ( x Lude..@? "phase2EncryptionAlgorithmSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "phase2DHGroupNumberSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "preSharedKey")
      Lude.<*> (x Lude..@? "tunnelInsideIpv6Cidr")
