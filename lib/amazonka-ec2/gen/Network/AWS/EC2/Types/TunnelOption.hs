{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TunnelOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TunnelOption where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IKEVersionsListValue
import Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
import Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsListValue
import Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The VPN tunnel options.
--
--
--
-- /See:/ 'tunnelOption' smart constructor.
data TunnelOption = TunnelOption'
  { _toOutsideIPAddress ::
      !(Maybe Text),
    _toReplayWindowSize :: !(Maybe Int),
    _toDpdTimeoutAction :: !(Maybe Text),
    _toRekeyFuzzPercentage :: !(Maybe Int),
    _toPhase1LifetimeSeconds :: !(Maybe Int),
    _toIkeVersions :: !(Maybe [IKEVersionsListValue]),
    _toPhase2IntegrityAlgorithms ::
      !(Maybe [Phase2IntegrityAlgorithmsListValue]),
    _toPhase2LifetimeSeconds :: !(Maybe Int),
    _toPhase1EncryptionAlgorithms ::
      !(Maybe [Phase1EncryptionAlgorithmsListValue]),
    _toPhase1DHGroupNumbers ::
      !(Maybe [Phase1DHGroupNumbersListValue]),
    _toPhase1IntegrityAlgorithms ::
      !(Maybe [Phase1IntegrityAlgorithmsListValue]),
    _toRekeyMarginTimeSeconds :: !(Maybe Int),
    _toDpdTimeoutSeconds :: !(Maybe Int),
    _toTunnelInsideCidr :: !(Maybe Text),
    _toStartupAction :: !(Maybe Text),
    _toPhase2EncryptionAlgorithms ::
      !(Maybe [Phase2EncryptionAlgorithmsListValue]),
    _toPhase2DHGroupNumbers ::
      !(Maybe [Phase2DHGroupNumbersListValue]),
    _toPreSharedKey :: !(Maybe Text),
    _toTunnelInsideIPv6Cidr :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TunnelOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toOutsideIPAddress' - The external IP address of the VPN tunnel.
--
-- * 'toReplayWindowSize' - The number of packets in an IKE replay window.
--
-- * 'toDpdTimeoutAction' - The action to take after a DPD timeout occurs.
--
-- * 'toRekeyFuzzPercentage' - The percentage of the rekey window determined by @RekeyMarginTimeSeconds@ during which the rekey time is randomly selected.
--
-- * 'toPhase1LifetimeSeconds' - The lifetime for phase 1 of the IKE negotiation, in seconds.
--
-- * 'toIkeVersions' - The IKE versions that are permitted for the VPN tunnel.
--
-- * 'toPhase2IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 2 IKE negotiations.
--
-- * 'toPhase2LifetimeSeconds' - The lifetime for phase 2 of the IKE negotiation, in seconds.
--
-- * 'toPhase1EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 1 IKE negotiations.
--
-- * 'toPhase1DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 1 IKE negotiations.
--
-- * 'toPhase1IntegrityAlgorithms' - The permitted integrity algorithms for the VPN tunnel for phase 1 IKE negotiations.
--
-- * 'toRekeyMarginTimeSeconds' - The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey.
--
-- * 'toDpdTimeoutSeconds' - The number of seconds after which a DPD timeout occurs.
--
-- * 'toTunnelInsideCidr' - The range of inside IPv4 addresses for the tunnel.
--
-- * 'toStartupAction' - The action to take when the establishing the VPN tunnels for a VPN connection.
--
-- * 'toPhase2EncryptionAlgorithms' - The permitted encryption algorithms for the VPN tunnel for phase 2 IKE negotiations.
--
-- * 'toPhase2DHGroupNumbers' - The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 2 IKE negotiations.
--
-- * 'toPreSharedKey' - The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
--
-- * 'toTunnelInsideIPv6Cidr' - The range of inside IPv6 addresses for the tunnel.
tunnelOption ::
  TunnelOption
tunnelOption =
  TunnelOption'
    { _toOutsideIPAddress = Nothing,
      _toReplayWindowSize = Nothing,
      _toDpdTimeoutAction = Nothing,
      _toRekeyFuzzPercentage = Nothing,
      _toPhase1LifetimeSeconds = Nothing,
      _toIkeVersions = Nothing,
      _toPhase2IntegrityAlgorithms = Nothing,
      _toPhase2LifetimeSeconds = Nothing,
      _toPhase1EncryptionAlgorithms = Nothing,
      _toPhase1DHGroupNumbers = Nothing,
      _toPhase1IntegrityAlgorithms = Nothing,
      _toRekeyMarginTimeSeconds = Nothing,
      _toDpdTimeoutSeconds = Nothing,
      _toTunnelInsideCidr = Nothing,
      _toStartupAction = Nothing,
      _toPhase2EncryptionAlgorithms = Nothing,
      _toPhase2DHGroupNumbers = Nothing,
      _toPreSharedKey = Nothing,
      _toTunnelInsideIPv6Cidr = Nothing
    }

-- | The external IP address of the VPN tunnel.
toOutsideIPAddress :: Lens' TunnelOption (Maybe Text)
toOutsideIPAddress = lens _toOutsideIPAddress (\s a -> s {_toOutsideIPAddress = a})

-- | The number of packets in an IKE replay window.
toReplayWindowSize :: Lens' TunnelOption (Maybe Int)
toReplayWindowSize = lens _toReplayWindowSize (\s a -> s {_toReplayWindowSize = a})

-- | The action to take after a DPD timeout occurs.
toDpdTimeoutAction :: Lens' TunnelOption (Maybe Text)
toDpdTimeoutAction = lens _toDpdTimeoutAction (\s a -> s {_toDpdTimeoutAction = a})

-- | The percentage of the rekey window determined by @RekeyMarginTimeSeconds@ during which the rekey time is randomly selected.
toRekeyFuzzPercentage :: Lens' TunnelOption (Maybe Int)
toRekeyFuzzPercentage = lens _toRekeyFuzzPercentage (\s a -> s {_toRekeyFuzzPercentage = a})

-- | The lifetime for phase 1 of the IKE negotiation, in seconds.
toPhase1LifetimeSeconds :: Lens' TunnelOption (Maybe Int)
toPhase1LifetimeSeconds = lens _toPhase1LifetimeSeconds (\s a -> s {_toPhase1LifetimeSeconds = a})

-- | The IKE versions that are permitted for the VPN tunnel.
toIkeVersions :: Lens' TunnelOption [IKEVersionsListValue]
toIkeVersions = lens _toIkeVersions (\s a -> s {_toIkeVersions = a}) . _Default . _Coerce

-- | The permitted integrity algorithms for the VPN tunnel for phase 2 IKE negotiations.
toPhase2IntegrityAlgorithms :: Lens' TunnelOption [Phase2IntegrityAlgorithmsListValue]
toPhase2IntegrityAlgorithms = lens _toPhase2IntegrityAlgorithms (\s a -> s {_toPhase2IntegrityAlgorithms = a}) . _Default . _Coerce

-- | The lifetime for phase 2 of the IKE negotiation, in seconds.
toPhase2LifetimeSeconds :: Lens' TunnelOption (Maybe Int)
toPhase2LifetimeSeconds = lens _toPhase2LifetimeSeconds (\s a -> s {_toPhase2LifetimeSeconds = a})

-- | The permitted encryption algorithms for the VPN tunnel for phase 1 IKE negotiations.
toPhase1EncryptionAlgorithms :: Lens' TunnelOption [Phase1EncryptionAlgorithmsListValue]
toPhase1EncryptionAlgorithms = lens _toPhase1EncryptionAlgorithms (\s a -> s {_toPhase1EncryptionAlgorithms = a}) . _Default . _Coerce

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 1 IKE negotiations.
toPhase1DHGroupNumbers :: Lens' TunnelOption [Phase1DHGroupNumbersListValue]
toPhase1DHGroupNumbers = lens _toPhase1DHGroupNumbers (\s a -> s {_toPhase1DHGroupNumbers = a}) . _Default . _Coerce

-- | The permitted integrity algorithms for the VPN tunnel for phase 1 IKE negotiations.
toPhase1IntegrityAlgorithms :: Lens' TunnelOption [Phase1IntegrityAlgorithmsListValue]
toPhase1IntegrityAlgorithms = lens _toPhase1IntegrityAlgorithms (\s a -> s {_toPhase1IntegrityAlgorithms = a}) . _Default . _Coerce

-- | The margin time, in seconds, before the phase 2 lifetime expires, during which the AWS side of the VPN connection performs an IKE rekey.
toRekeyMarginTimeSeconds :: Lens' TunnelOption (Maybe Int)
toRekeyMarginTimeSeconds = lens _toRekeyMarginTimeSeconds (\s a -> s {_toRekeyMarginTimeSeconds = a})

-- | The number of seconds after which a DPD timeout occurs.
toDpdTimeoutSeconds :: Lens' TunnelOption (Maybe Int)
toDpdTimeoutSeconds = lens _toDpdTimeoutSeconds (\s a -> s {_toDpdTimeoutSeconds = a})

-- | The range of inside IPv4 addresses for the tunnel.
toTunnelInsideCidr :: Lens' TunnelOption (Maybe Text)
toTunnelInsideCidr = lens _toTunnelInsideCidr (\s a -> s {_toTunnelInsideCidr = a})

-- | The action to take when the establishing the VPN tunnels for a VPN connection.
toStartupAction :: Lens' TunnelOption (Maybe Text)
toStartupAction = lens _toStartupAction (\s a -> s {_toStartupAction = a})

-- | The permitted encryption algorithms for the VPN tunnel for phase 2 IKE negotiations.
toPhase2EncryptionAlgorithms :: Lens' TunnelOption [Phase2EncryptionAlgorithmsListValue]
toPhase2EncryptionAlgorithms = lens _toPhase2EncryptionAlgorithms (\s a -> s {_toPhase2EncryptionAlgorithms = a}) . _Default . _Coerce

-- | The permitted Diffie-Hellman group numbers for the VPN tunnel for phase 2 IKE negotiations.
toPhase2DHGroupNumbers :: Lens' TunnelOption [Phase2DHGroupNumbersListValue]
toPhase2DHGroupNumbers = lens _toPhase2DHGroupNumbers (\s a -> s {_toPhase2DHGroupNumbers = a}) . _Default . _Coerce

-- | The pre-shared key (PSK) to establish initial authentication between the virtual private gateway and the customer gateway.
toPreSharedKey :: Lens' TunnelOption (Maybe Text)
toPreSharedKey = lens _toPreSharedKey (\s a -> s {_toPreSharedKey = a})

-- | The range of inside IPv6 addresses for the tunnel.
toTunnelInsideIPv6Cidr :: Lens' TunnelOption (Maybe Text)
toTunnelInsideIPv6Cidr = lens _toTunnelInsideIPv6Cidr (\s a -> s {_toTunnelInsideIPv6Cidr = a})

instance FromXML TunnelOption where
  parseXML x =
    TunnelOption'
      <$> (x .@? "outsideIpAddress")
      <*> (x .@? "replayWindowSize")
      <*> (x .@? "dpdTimeoutAction")
      <*> (x .@? "rekeyFuzzPercentage")
      <*> (x .@? "phase1LifetimeSeconds")
      <*> (x .@? "ikeVersionSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> ( x .@? "phase2IntegrityAlgorithmSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "phase2LifetimeSeconds")
      <*> ( x .@? "phase1EncryptionAlgorithmSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "phase1DHGroupNumberSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "phase1IntegrityAlgorithmSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "rekeyMarginTimeSeconds")
      <*> (x .@? "dpdTimeoutSeconds")
      <*> (x .@? "tunnelInsideCidr")
      <*> (x .@? "startupAction")
      <*> ( x .@? "phase2EncryptionAlgorithmSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "phase2DHGroupNumberSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "preSharedKey")
      <*> (x .@? "tunnelInsideIpv6Cidr")

instance Hashable TunnelOption

instance NFData TunnelOption
