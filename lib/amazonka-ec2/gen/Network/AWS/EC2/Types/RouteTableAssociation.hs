{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RouteTableAssociationState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between a route table and a subnet or gateway.
--
--
--
-- /See:/ 'routeTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { _rtaRouteTableId ::
      !(Maybe Text),
    _rtaRouteTableAssociationId :: !(Maybe Text),
    _rtaMain :: !(Maybe Bool),
    _rtaSubnetId :: !(Maybe Text),
    _rtaGatewayId :: !(Maybe Text),
    _rtaAssociationState ::
      !(Maybe RouteTableAssociationState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RouteTableAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtaRouteTableId' - The ID of the route table.
--
-- * 'rtaRouteTableAssociationId' - The ID of the association.
--
-- * 'rtaMain' - Indicates whether this is the main route table.
--
-- * 'rtaSubnetId' - The ID of the subnet. A subnet ID is not returned for an implicit association.
--
-- * 'rtaGatewayId' - The ID of the internet gateway or virtual private gateway.
--
-- * 'rtaAssociationState' - The state of the association.
routeTableAssociation ::
  RouteTableAssociation
routeTableAssociation =
  RouteTableAssociation'
    { _rtaRouteTableId = Nothing,
      _rtaRouteTableAssociationId = Nothing,
      _rtaMain = Nothing,
      _rtaSubnetId = Nothing,
      _rtaGatewayId = Nothing,
      _rtaAssociationState = Nothing
    }

-- | The ID of the route table.
rtaRouteTableId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableId = lens _rtaRouteTableId (\s a -> s {_rtaRouteTableId = a})

-- | The ID of the association.
rtaRouteTableAssociationId :: Lens' RouteTableAssociation (Maybe Text)
rtaRouteTableAssociationId = lens _rtaRouteTableAssociationId (\s a -> s {_rtaRouteTableAssociationId = a})

-- | Indicates whether this is the main route table.
rtaMain :: Lens' RouteTableAssociation (Maybe Bool)
rtaMain = lens _rtaMain (\s a -> s {_rtaMain = a})

-- | The ID of the subnet. A subnet ID is not returned for an implicit association.
rtaSubnetId :: Lens' RouteTableAssociation (Maybe Text)
rtaSubnetId = lens _rtaSubnetId (\s a -> s {_rtaSubnetId = a})

-- | The ID of the internet gateway or virtual private gateway.
rtaGatewayId :: Lens' RouteTableAssociation (Maybe Text)
rtaGatewayId = lens _rtaGatewayId (\s a -> s {_rtaGatewayId = a})

-- | The state of the association.
rtaAssociationState :: Lens' RouteTableAssociation (Maybe RouteTableAssociationState)
rtaAssociationState = lens _rtaAssociationState (\s a -> s {_rtaAssociationState = a})

instance FromXML RouteTableAssociation where
  parseXML x =
    RouteTableAssociation'
      <$> (x .@? "routeTableId")
      <*> (x .@? "routeTableAssociationId")
      <*> (x .@? "main")
      <*> (x .@? "subnetId")
      <*> (x .@? "gatewayId")
      <*> (x .@? "associationState")

instance Hashable RouteTableAssociation

instance NFData RouteTableAssociation
