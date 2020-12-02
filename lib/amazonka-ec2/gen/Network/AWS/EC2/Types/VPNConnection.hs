{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNConnection where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VGWTelemetry
import Network.AWS.EC2.Types.VPNConnectionOptions
import Network.AWS.EC2.Types.VPNState
import Network.AWS.EC2.Types.VPNStaticRoute
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a VPN connection.
--
--
--
-- /See:/ 'vpnConnection' smart constructor.
data VPNConnection = VPNConnection'
  { _vcCustomerGatewayConfiguration ::
      !(Maybe Text),
    _vcRoutes :: !(Maybe [VPNStaticRoute]),
    _vcVPNGatewayId :: !(Maybe Text),
    _vcCategory :: !(Maybe Text),
    _vcTransitGatewayId :: !(Maybe Text),
    _vcOptions :: !(Maybe VPNConnectionOptions),
    _vcTags :: !(Maybe [Tag]),
    _vcVGWTelemetry :: !(Maybe [VGWTelemetry]),
    _vcVPNConnectionId :: !Text,
    _vcCustomerGatewayId :: !Text,
    _vcState :: !VPNState,
    _vcType :: !GatewayType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcCustomerGatewayConfiguration' - The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
--
-- * 'vcRoutes' - The static routes associated with the VPN connection.
--
-- * 'vcVPNGatewayId' - The ID of the virtual private gateway at the AWS side of the VPN connection.
--
-- * 'vcCategory' - The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
--
-- * 'vcTransitGatewayId' - The ID of the transit gateway associated with the VPN connection.
--
-- * 'vcOptions' - The VPN connection options.
--
-- * 'vcTags' - Any tags assigned to the VPN connection.
--
-- * 'vcVGWTelemetry' - Information about the VPN tunnel.
--
-- * 'vcVPNConnectionId' - The ID of the VPN connection.
--
-- * 'vcCustomerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
--
-- * 'vcState' - The current state of the VPN connection.
--
-- * 'vcType' - The type of VPN connection.
vpnConnection ::
  -- | 'vcVPNConnectionId'
  Text ->
  -- | 'vcCustomerGatewayId'
  Text ->
  -- | 'vcState'
  VPNState ->
  -- | 'vcType'
  GatewayType ->
  VPNConnection
vpnConnection pVPNConnectionId_ pCustomerGatewayId_ pState_ pType_ =
  VPNConnection'
    { _vcCustomerGatewayConfiguration = Nothing,
      _vcRoutes = Nothing,
      _vcVPNGatewayId = Nothing,
      _vcCategory = Nothing,
      _vcTransitGatewayId = Nothing,
      _vcOptions = Nothing,
      _vcTags = Nothing,
      _vcVGWTelemetry = Nothing,
      _vcVPNConnectionId = pVPNConnectionId_,
      _vcCustomerGatewayId = pCustomerGatewayId_,
      _vcState = pState_,
      _vcType = pType_
    }

-- | The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
vcCustomerGatewayConfiguration :: Lens' VPNConnection (Maybe Text)
vcCustomerGatewayConfiguration = lens _vcCustomerGatewayConfiguration (\s a -> s {_vcCustomerGatewayConfiguration = a})

-- | The static routes associated with the VPN connection.
vcRoutes :: Lens' VPNConnection [VPNStaticRoute]
vcRoutes = lens _vcRoutes (\s a -> s {_vcRoutes = a}) . _Default . _Coerce

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
vcVPNGatewayId :: Lens' VPNConnection (Maybe Text)
vcVPNGatewayId = lens _vcVPNGatewayId (\s a -> s {_vcVPNGatewayId = a})

-- | The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
vcCategory :: Lens' VPNConnection (Maybe Text)
vcCategory = lens _vcCategory (\s a -> s {_vcCategory = a})

-- | The ID of the transit gateway associated with the VPN connection.
vcTransitGatewayId :: Lens' VPNConnection (Maybe Text)
vcTransitGatewayId = lens _vcTransitGatewayId (\s a -> s {_vcTransitGatewayId = a})

-- | The VPN connection options.
vcOptions :: Lens' VPNConnection (Maybe VPNConnectionOptions)
vcOptions = lens _vcOptions (\s a -> s {_vcOptions = a})

-- | Any tags assigned to the VPN connection.
vcTags :: Lens' VPNConnection [Tag]
vcTags = lens _vcTags (\s a -> s {_vcTags = a}) . _Default . _Coerce

-- | Information about the VPN tunnel.
vcVGWTelemetry :: Lens' VPNConnection [VGWTelemetry]
vcVGWTelemetry = lens _vcVGWTelemetry (\s a -> s {_vcVGWTelemetry = a}) . _Default . _Coerce

-- | The ID of the VPN connection.
vcVPNConnectionId :: Lens' VPNConnection Text
vcVPNConnectionId = lens _vcVPNConnectionId (\s a -> s {_vcVPNConnectionId = a})

-- | The ID of the customer gateway at your end of the VPN connection.
vcCustomerGatewayId :: Lens' VPNConnection Text
vcCustomerGatewayId = lens _vcCustomerGatewayId (\s a -> s {_vcCustomerGatewayId = a})

-- | The current state of the VPN connection.
vcState :: Lens' VPNConnection VPNState
vcState = lens _vcState (\s a -> s {_vcState = a})

-- | The type of VPN connection.
vcType :: Lens' VPNConnection GatewayType
vcType = lens _vcType (\s a -> s {_vcType = a})

instance FromXML VPNConnection where
  parseXML x =
    VPNConnection'
      <$> (x .@? "customerGatewayConfiguration")
      <*> (x .@? "routes" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vpnGatewayId")
      <*> (x .@? "category")
      <*> (x .@? "transitGatewayId")
      <*> (x .@? "options")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vgwTelemetry" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "vpnConnectionId")
      <*> (x .@ "customerGatewayId")
      <*> (x .@ "state")
      <*> (x .@ "type")

instance Hashable VPNConnection

instance NFData VPNConnection
