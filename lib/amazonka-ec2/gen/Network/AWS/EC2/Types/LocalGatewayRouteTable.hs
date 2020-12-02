{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTable where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a local gateway route table.
--
--
--
-- /See:/ 'localGatewayRouteTable' smart constructor.
data LocalGatewayRouteTable = LocalGatewayRouteTable'
  { _lgrtState ::
      !(Maybe Text),
    _lgrtLocalGatewayRouteTableARN ::
      !(Maybe Text),
    _lgrtLocalGatewayId :: !(Maybe Text),
    _lgrtOutpostARN :: !(Maybe Text),
    _lgrtOwnerId :: !(Maybe Text),
    _lgrtLocalGatewayRouteTableId ::
      !(Maybe Text),
    _lgrtTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalGatewayRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrtState' - The state of the local gateway route table.
--
-- * 'lgrtLocalGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- * 'lgrtLocalGatewayId' - The ID of the local gateway.
--
-- * 'lgrtOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'lgrtOwnerId' - The AWS account ID that owns the local gateway route table.
--
-- * 'lgrtLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'lgrtTags' - The tags assigned to the local gateway route table.
localGatewayRouteTable ::
  LocalGatewayRouteTable
localGatewayRouteTable =
  LocalGatewayRouteTable'
    { _lgrtState = Nothing,
      _lgrtLocalGatewayRouteTableARN = Nothing,
      _lgrtLocalGatewayId = Nothing,
      _lgrtOutpostARN = Nothing,
      _lgrtOwnerId = Nothing,
      _lgrtLocalGatewayRouteTableId = Nothing,
      _lgrtTags = Nothing
    }

-- | The state of the local gateway route table.
lgrtState :: Lens' LocalGatewayRouteTable (Maybe Text)
lgrtState = lens _lgrtState (\s a -> s {_lgrtState = a})

-- | The Amazon Resource Name (ARN) of the local gateway route table.
lgrtLocalGatewayRouteTableARN :: Lens' LocalGatewayRouteTable (Maybe Text)
lgrtLocalGatewayRouteTableARN = lens _lgrtLocalGatewayRouteTableARN (\s a -> s {_lgrtLocalGatewayRouteTableARN = a})

-- | The ID of the local gateway.
lgrtLocalGatewayId :: Lens' LocalGatewayRouteTable (Maybe Text)
lgrtLocalGatewayId = lens _lgrtLocalGatewayId (\s a -> s {_lgrtLocalGatewayId = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
lgrtOutpostARN :: Lens' LocalGatewayRouteTable (Maybe Text)
lgrtOutpostARN = lens _lgrtOutpostARN (\s a -> s {_lgrtOutpostARN = a})

-- | The AWS account ID that owns the local gateway route table.
lgrtOwnerId :: Lens' LocalGatewayRouteTable (Maybe Text)
lgrtOwnerId = lens _lgrtOwnerId (\s a -> s {_lgrtOwnerId = a})

-- | The ID of the local gateway route table.
lgrtLocalGatewayRouteTableId :: Lens' LocalGatewayRouteTable (Maybe Text)
lgrtLocalGatewayRouteTableId = lens _lgrtLocalGatewayRouteTableId (\s a -> s {_lgrtLocalGatewayRouteTableId = a})

-- | The tags assigned to the local gateway route table.
lgrtTags :: Lens' LocalGatewayRouteTable [Tag]
lgrtTags = lens _lgrtTags (\s a -> s {_lgrtTags = a}) . _Default . _Coerce

instance FromXML LocalGatewayRouteTable where
  parseXML x =
    LocalGatewayRouteTable'
      <$> (x .@? "state")
      <*> (x .@? "localGatewayRouteTableArn")
      <*> (x .@? "localGatewayId")
      <*> (x .@? "outpostArn")
      <*> (x .@? "ownerId")
      <*> (x .@? "localGatewayRouteTableId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable LocalGatewayRouteTable

instance NFData LocalGatewayRouteTable
