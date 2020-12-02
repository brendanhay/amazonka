{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnectionOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNConnectionOptionsSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TunnelInsideIPVersion
import Network.AWS.EC2.Types.VPNTunnelOptionsSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes VPN connection options.
--
--
--
-- /See:/ 'vpnConnectionOptionsSpecification' smart constructor.
data VPNConnectionOptionsSpecification = VPNConnectionOptionsSpecification'
  { _vcosTunnelInsideIPVersion ::
      !( Maybe
           TunnelInsideIPVersion
       ),
    _vcosRemoteIPv4NetworkCidr ::
      !(Maybe Text),
    _vcosEnableAcceleration ::
      !(Maybe Bool),
    _vcosLocalIPv4NetworkCidr ::
      !(Maybe Text),
    _vcosRemoteIPv6NetworkCidr ::
      !(Maybe Text),
    _vcosTunnelOptions ::
      !( Maybe
           [VPNTunnelOptionsSpecification]
       ),
    _vcosLocalIPv6NetworkCidr ::
      !(Maybe Text),
    _vcosStaticRoutesOnly ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNConnectionOptionsSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcosTunnelInsideIPVersion' - Indicate whether the VPN tunnels process IPv4 or IPv6 traffic. Default: @ipv4@
--
-- * 'vcosRemoteIPv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection. Default: @0.0.0.0/0@
--
-- * 'vcosEnableAcceleration' - Indicate whether to enable acceleration for the VPN connection. Default: @false@
--
-- * 'vcosLocalIPv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection. Default: @0.0.0.0/0@
--
-- * 'vcosRemoteIPv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection. Default: @::/0@
--
-- * 'vcosTunnelOptions' - The tunnel options for the VPN connection.
--
-- * 'vcosLocalIPv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection. Default: @::/0@
--
-- * 'vcosStaticRoutesOnly' - Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route. Default: @false@
vpnConnectionOptionsSpecification ::
  VPNConnectionOptionsSpecification
vpnConnectionOptionsSpecification =
  VPNConnectionOptionsSpecification'
    { _vcosTunnelInsideIPVersion =
        Nothing,
      _vcosRemoteIPv4NetworkCidr = Nothing,
      _vcosEnableAcceleration = Nothing,
      _vcosLocalIPv4NetworkCidr = Nothing,
      _vcosRemoteIPv6NetworkCidr = Nothing,
      _vcosTunnelOptions = Nothing,
      _vcosLocalIPv6NetworkCidr = Nothing,
      _vcosStaticRoutesOnly = Nothing
    }

-- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic. Default: @ipv4@
vcosTunnelInsideIPVersion :: Lens' VPNConnectionOptionsSpecification (Maybe TunnelInsideIPVersion)
vcosTunnelInsideIPVersion = lens _vcosTunnelInsideIPVersion (\s a -> s {_vcosTunnelInsideIPVersion = a})

-- | The IPv4 CIDR on the AWS side of the VPN connection. Default: @0.0.0.0/0@
vcosRemoteIPv4NetworkCidr :: Lens' VPNConnectionOptionsSpecification (Maybe Text)
vcosRemoteIPv4NetworkCidr = lens _vcosRemoteIPv4NetworkCidr (\s a -> s {_vcosRemoteIPv4NetworkCidr = a})

-- | Indicate whether to enable acceleration for the VPN connection. Default: @false@
vcosEnableAcceleration :: Lens' VPNConnectionOptionsSpecification (Maybe Bool)
vcosEnableAcceleration = lens _vcosEnableAcceleration (\s a -> s {_vcosEnableAcceleration = a})

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection. Default: @0.0.0.0/0@
vcosLocalIPv4NetworkCidr :: Lens' VPNConnectionOptionsSpecification (Maybe Text)
vcosLocalIPv4NetworkCidr = lens _vcosLocalIPv4NetworkCidr (\s a -> s {_vcosLocalIPv4NetworkCidr = a})

-- | The IPv6 CIDR on the AWS side of the VPN connection. Default: @::/0@
vcosRemoteIPv6NetworkCidr :: Lens' VPNConnectionOptionsSpecification (Maybe Text)
vcosRemoteIPv6NetworkCidr = lens _vcosRemoteIPv6NetworkCidr (\s a -> s {_vcosRemoteIPv6NetworkCidr = a})

-- | The tunnel options for the VPN connection.
vcosTunnelOptions :: Lens' VPNConnectionOptionsSpecification [VPNTunnelOptionsSpecification]
vcosTunnelOptions = lens _vcosTunnelOptions (\s a -> s {_vcosTunnelOptions = a}) . _Default . _Coerce

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection. Default: @::/0@
vcosLocalIPv6NetworkCidr :: Lens' VPNConnectionOptionsSpecification (Maybe Text)
vcosLocalIPv6NetworkCidr = lens _vcosLocalIPv6NetworkCidr (\s a -> s {_vcosLocalIPv6NetworkCidr = a})

-- | Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route. Default: @false@
vcosStaticRoutesOnly :: Lens' VPNConnectionOptionsSpecification (Maybe Bool)
vcosStaticRoutesOnly = lens _vcosStaticRoutesOnly (\s a -> s {_vcosStaticRoutesOnly = a})

instance Hashable VPNConnectionOptionsSpecification

instance NFData VPNConnectionOptionsSpecification

instance ToQuery VPNConnectionOptionsSpecification where
  toQuery VPNConnectionOptionsSpecification' {..} =
    mconcat
      [ "TunnelInsideIpVersion" =: _vcosTunnelInsideIPVersion,
        "RemoteIpv4NetworkCidr" =: _vcosRemoteIPv4NetworkCidr,
        "EnableAcceleration" =: _vcosEnableAcceleration,
        "LocalIpv4NetworkCidr" =: _vcosLocalIPv4NetworkCidr,
        "RemoteIpv6NetworkCidr" =: _vcosRemoteIPv6NetworkCidr,
        toQuery (toQueryList "TunnelOptions" <$> _vcosTunnelOptions),
        "LocalIpv6NetworkCidr" =: _vcosLocalIPv6NetworkCidr,
        "StaticRoutesOnly" =: _vcosStaticRoutesOnly
      ]
