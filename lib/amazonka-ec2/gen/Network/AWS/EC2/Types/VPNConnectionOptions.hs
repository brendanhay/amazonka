{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNConnectionOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TunnelInsideIPVersion
import Network.AWS.EC2.Types.TunnelOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes VPN connection options.
--
--
--
-- /See:/ 'vpnConnectionOptions' smart constructor.
data VPNConnectionOptions = VPNConnectionOptions'
  { _vcoTunnelInsideIPVersion ::
      !(Maybe TunnelInsideIPVersion),
    _vcoRemoteIPv4NetworkCidr :: !(Maybe Text),
    _vcoEnableAcceleration :: !(Maybe Bool),
    _vcoLocalIPv4NetworkCidr :: !(Maybe Text),
    _vcoRemoteIPv6NetworkCidr :: !(Maybe Text),
    _vcoTunnelOptions :: !(Maybe [TunnelOption]),
    _vcoLocalIPv6NetworkCidr :: !(Maybe Text),
    _vcoStaticRoutesOnly :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNConnectionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcoTunnelInsideIPVersion' - Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- * 'vcoRemoteIPv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
--
-- * 'vcoEnableAcceleration' - Indicates whether acceleration is enabled for the VPN connection.
--
-- * 'vcoLocalIPv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- * 'vcoRemoteIPv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
--
-- * 'vcoTunnelOptions' - Indicates the VPN tunnel options.
--
-- * 'vcoLocalIPv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- * 'vcoStaticRoutesOnly' - Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
vpnConnectionOptions ::
  VPNConnectionOptions
vpnConnectionOptions =
  VPNConnectionOptions'
    { _vcoTunnelInsideIPVersion = Nothing,
      _vcoRemoteIPv4NetworkCidr = Nothing,
      _vcoEnableAcceleration = Nothing,
      _vcoLocalIPv4NetworkCidr = Nothing,
      _vcoRemoteIPv6NetworkCidr = Nothing,
      _vcoTunnelOptions = Nothing,
      _vcoLocalIPv6NetworkCidr = Nothing,
      _vcoStaticRoutesOnly = Nothing
    }

-- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
vcoTunnelInsideIPVersion :: Lens' VPNConnectionOptions (Maybe TunnelInsideIPVersion)
vcoTunnelInsideIPVersion = lens _vcoTunnelInsideIPVersion (\s a -> s {_vcoTunnelInsideIPVersion = a})

-- | The IPv4 CIDR on the AWS side of the VPN connection.
vcoRemoteIPv4NetworkCidr :: Lens' VPNConnectionOptions (Maybe Text)
vcoRemoteIPv4NetworkCidr = lens _vcoRemoteIPv4NetworkCidr (\s a -> s {_vcoRemoteIPv4NetworkCidr = a})

-- | Indicates whether acceleration is enabled for the VPN connection.
vcoEnableAcceleration :: Lens' VPNConnectionOptions (Maybe Bool)
vcoEnableAcceleration = lens _vcoEnableAcceleration (\s a -> s {_vcoEnableAcceleration = a})

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
vcoLocalIPv4NetworkCidr :: Lens' VPNConnectionOptions (Maybe Text)
vcoLocalIPv4NetworkCidr = lens _vcoLocalIPv4NetworkCidr (\s a -> s {_vcoLocalIPv4NetworkCidr = a})

-- | The IPv6 CIDR on the AWS side of the VPN connection.
vcoRemoteIPv6NetworkCidr :: Lens' VPNConnectionOptions (Maybe Text)
vcoRemoteIPv6NetworkCidr = lens _vcoRemoteIPv6NetworkCidr (\s a -> s {_vcoRemoteIPv6NetworkCidr = a})

-- | Indicates the VPN tunnel options.
vcoTunnelOptions :: Lens' VPNConnectionOptions [TunnelOption]
vcoTunnelOptions = lens _vcoTunnelOptions (\s a -> s {_vcoTunnelOptions = a}) . _Default . _Coerce

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
vcoLocalIPv6NetworkCidr :: Lens' VPNConnectionOptions (Maybe Text)
vcoLocalIPv6NetworkCidr = lens _vcoLocalIPv6NetworkCidr (\s a -> s {_vcoLocalIPv6NetworkCidr = a})

-- | Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
vcoStaticRoutesOnly :: Lens' VPNConnectionOptions (Maybe Bool)
vcoStaticRoutesOnly = lens _vcoStaticRoutesOnly (\s a -> s {_vcoStaticRoutesOnly = a})

instance FromXML VPNConnectionOptions where
  parseXML x =
    VPNConnectionOptions'
      <$> (x .@? "tunnelInsideIpVersion")
      <*> (x .@? "remoteIpv4NetworkCidr")
      <*> (x .@? "enableAcceleration")
      <*> (x .@? "localIpv4NetworkCidr")
      <*> (x .@? "remoteIpv6NetworkCidr")
      <*> (x .@? "tunnelOptionSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "localIpv6NetworkCidr")
      <*> (x .@? "staticRoutesOnly")

instance Hashable VPNConnectionOptions

instance NFData VPNConnectionOptions
