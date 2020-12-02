{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRoute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LocalGatewayRouteState
import Network.AWS.EC2.Types.LocalGatewayRouteType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a route for a local gateway route table.
--
--
--
-- /See:/ 'localGatewayRoute' smart constructor.
data LocalGatewayRoute = LocalGatewayRoute'
  { _lgrState ::
      !(Maybe LocalGatewayRouteState),
    _lgrLocalGatewayRouteTableARN :: !(Maybe Text),
    _lgrOwnerId :: !(Maybe Text),
    _lgrLocalGatewayRouteTableId :: !(Maybe Text),
    _lgrType :: !(Maybe LocalGatewayRouteType),
    _lgrLocalGatewayVirtualInterfaceGroupId ::
      !(Maybe Text),
    _lgrDestinationCidrBlock :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalGatewayRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrState' - The state of the route.
--
-- * 'lgrLocalGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- * 'lgrOwnerId' - The AWS account ID that owns the local gateway route.
--
-- * 'lgrLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'lgrType' - The route type.
--
-- * 'lgrLocalGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- * 'lgrDestinationCidrBlock' - The CIDR block used for destination matches.
localGatewayRoute ::
  LocalGatewayRoute
localGatewayRoute =
  LocalGatewayRoute'
    { _lgrState = Nothing,
      _lgrLocalGatewayRouteTableARN = Nothing,
      _lgrOwnerId = Nothing,
      _lgrLocalGatewayRouteTableId = Nothing,
      _lgrType = Nothing,
      _lgrLocalGatewayVirtualInterfaceGroupId = Nothing,
      _lgrDestinationCidrBlock = Nothing
    }

-- | The state of the route.
lgrState :: Lens' LocalGatewayRoute (Maybe LocalGatewayRouteState)
lgrState = lens _lgrState (\s a -> s {_lgrState = a})

-- | The Amazon Resource Name (ARN) of the local gateway route table.
lgrLocalGatewayRouteTableARN :: Lens' LocalGatewayRoute (Maybe Text)
lgrLocalGatewayRouteTableARN = lens _lgrLocalGatewayRouteTableARN (\s a -> s {_lgrLocalGatewayRouteTableARN = a})

-- | The AWS account ID that owns the local gateway route.
lgrOwnerId :: Lens' LocalGatewayRoute (Maybe Text)
lgrOwnerId = lens _lgrOwnerId (\s a -> s {_lgrOwnerId = a})

-- | The ID of the local gateway route table.
lgrLocalGatewayRouteTableId :: Lens' LocalGatewayRoute (Maybe Text)
lgrLocalGatewayRouteTableId = lens _lgrLocalGatewayRouteTableId (\s a -> s {_lgrLocalGatewayRouteTableId = a})

-- | The route type.
lgrType :: Lens' LocalGatewayRoute (Maybe LocalGatewayRouteType)
lgrType = lens _lgrType (\s a -> s {_lgrType = a})

-- | The ID of the virtual interface group.
lgrLocalGatewayVirtualInterfaceGroupId :: Lens' LocalGatewayRoute (Maybe Text)
lgrLocalGatewayVirtualInterfaceGroupId = lens _lgrLocalGatewayVirtualInterfaceGroupId (\s a -> s {_lgrLocalGatewayVirtualInterfaceGroupId = a})

-- | The CIDR block used for destination matches.
lgrDestinationCidrBlock :: Lens' LocalGatewayRoute (Maybe Text)
lgrDestinationCidrBlock = lens _lgrDestinationCidrBlock (\s a -> s {_lgrDestinationCidrBlock = a})

instance FromXML LocalGatewayRoute where
  parseXML x =
    LocalGatewayRoute'
      <$> (x .@? "state")
      <*> (x .@? "localGatewayRouteTableArn")
      <*> (x .@? "ownerId")
      <*> (x .@? "localGatewayRouteTableId")
      <*> (x .@? "type")
      <*> (x .@? "localGatewayVirtualInterfaceGroupId")
      <*> (x .@? "destinationCidrBlock")

instance Hashable LocalGatewayRoute

instance NFData LocalGatewayRoute
