{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNRoute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVPNRouteStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Client VPN endpoint route.
--
--
--
-- /See:/ 'clientVPNRoute' smart constructor.
data ClientVPNRoute = ClientVPNRoute'
  { _cvrStatus ::
      !(Maybe ClientVPNRouteStatus),
    _cvrOrigin :: !(Maybe Text),
    _cvrClientVPNEndpointId :: !(Maybe Text),
    _cvrTargetSubnet :: !(Maybe Text),
    _cvrDestinationCidr :: !(Maybe Text),
    _cvrType :: !(Maybe Text),
    _cvrDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientVPNRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvrStatus' - The current state of the route.
--
-- * 'cvrOrigin' - Indicates how the route was associated with the Client VPN endpoint. @associate@ indicates that the route was automatically added when the target network was associated with the Client VPN endpoint. @add-route@ indicates that the route was manually added using the __CreateClientVpnRoute__ action.
--
-- * 'cvrClientVPNEndpointId' - The ID of the Client VPN endpoint with which the route is associated.
--
-- * 'cvrTargetSubnet' - The ID of the subnet through which traffic is routed.
--
-- * 'cvrDestinationCidr' - The IPv4 address range, in CIDR notation, of the route destination.
--
-- * 'cvrType' - The route type.
--
-- * 'cvrDescription' - A brief description of the route.
clientVPNRoute ::
  ClientVPNRoute
clientVPNRoute =
  ClientVPNRoute'
    { _cvrStatus = Nothing,
      _cvrOrigin = Nothing,
      _cvrClientVPNEndpointId = Nothing,
      _cvrTargetSubnet = Nothing,
      _cvrDestinationCidr = Nothing,
      _cvrType = Nothing,
      _cvrDescription = Nothing
    }

-- | The current state of the route.
cvrStatus :: Lens' ClientVPNRoute (Maybe ClientVPNRouteStatus)
cvrStatus = lens _cvrStatus (\s a -> s {_cvrStatus = a})

-- | Indicates how the route was associated with the Client VPN endpoint. @associate@ indicates that the route was automatically added when the target network was associated with the Client VPN endpoint. @add-route@ indicates that the route was manually added using the __CreateClientVpnRoute__ action.
cvrOrigin :: Lens' ClientVPNRoute (Maybe Text)
cvrOrigin = lens _cvrOrigin (\s a -> s {_cvrOrigin = a})

-- | The ID of the Client VPN endpoint with which the route is associated.
cvrClientVPNEndpointId :: Lens' ClientVPNRoute (Maybe Text)
cvrClientVPNEndpointId = lens _cvrClientVPNEndpointId (\s a -> s {_cvrClientVPNEndpointId = a})

-- | The ID of the subnet through which traffic is routed.
cvrTargetSubnet :: Lens' ClientVPNRoute (Maybe Text)
cvrTargetSubnet = lens _cvrTargetSubnet (\s a -> s {_cvrTargetSubnet = a})

-- | The IPv4 address range, in CIDR notation, of the route destination.
cvrDestinationCidr :: Lens' ClientVPNRoute (Maybe Text)
cvrDestinationCidr = lens _cvrDestinationCidr (\s a -> s {_cvrDestinationCidr = a})

-- | The route type.
cvrType :: Lens' ClientVPNRoute (Maybe Text)
cvrType = lens _cvrType (\s a -> s {_cvrType = a})

-- | A brief description of the route.
cvrDescription :: Lens' ClientVPNRoute (Maybe Text)
cvrDescription = lens _cvrDescription (\s a -> s {_cvrDescription = a})

instance FromXML ClientVPNRoute where
  parseXML x =
    ClientVPNRoute'
      <$> (x .@? "status")
      <*> (x .@? "origin")
      <*> (x .@? "clientVpnEndpointId")
      <*> (x .@? "targetSubnet")
      <*> (x .@? "destinationCidr")
      <*> (x .@? "type")
      <*> (x .@? "description")

instance Hashable ClientVPNRoute

instance NFData ClientVPNRoute
