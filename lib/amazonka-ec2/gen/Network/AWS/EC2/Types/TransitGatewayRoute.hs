{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRoute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayRouteAttachment
import Network.AWS.EC2.Types.TransitGatewayRouteState
import Network.AWS.EC2.Types.TransitGatewayRouteType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a route for a transit gateway route table.
--
--
--
-- /See:/ 'transitGatewayRoute' smart constructor.
data TransitGatewayRoute = TransitGatewayRoute'
  { _tgrState ::
      !(Maybe TransitGatewayRouteState),
    _tgrPrefixListId :: !(Maybe Text),
    _tgrTransitGatewayAttachments ::
      !(Maybe [TransitGatewayRouteAttachment]),
    _tgrType :: !(Maybe TransitGatewayRouteType),
    _tgrDestinationCidrBlock :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgrState' - The state of the route.
--
-- * 'tgrPrefixListId' - The ID of the prefix list used for destination matches.
--
-- * 'tgrTransitGatewayAttachments' - The attachments.
--
-- * 'tgrType' - The route type.
--
-- * 'tgrDestinationCidrBlock' - The CIDR block used for destination matches.
transitGatewayRoute ::
  TransitGatewayRoute
transitGatewayRoute =
  TransitGatewayRoute'
    { _tgrState = Nothing,
      _tgrPrefixListId = Nothing,
      _tgrTransitGatewayAttachments = Nothing,
      _tgrType = Nothing,
      _tgrDestinationCidrBlock = Nothing
    }

-- | The state of the route.
tgrState :: Lens' TransitGatewayRoute (Maybe TransitGatewayRouteState)
tgrState = lens _tgrState (\s a -> s {_tgrState = a})

-- | The ID of the prefix list used for destination matches.
tgrPrefixListId :: Lens' TransitGatewayRoute (Maybe Text)
tgrPrefixListId = lens _tgrPrefixListId (\s a -> s {_tgrPrefixListId = a})

-- | The attachments.
tgrTransitGatewayAttachments :: Lens' TransitGatewayRoute [TransitGatewayRouteAttachment]
tgrTransitGatewayAttachments = lens _tgrTransitGatewayAttachments (\s a -> s {_tgrTransitGatewayAttachments = a}) . _Default . _Coerce

-- | The route type.
tgrType :: Lens' TransitGatewayRoute (Maybe TransitGatewayRouteType)
tgrType = lens _tgrType (\s a -> s {_tgrType = a})

-- | The CIDR block used for destination matches.
tgrDestinationCidrBlock :: Lens' TransitGatewayRoute (Maybe Text)
tgrDestinationCidrBlock = lens _tgrDestinationCidrBlock (\s a -> s {_tgrDestinationCidrBlock = a})

instance FromXML TransitGatewayRoute where
  parseXML x =
    TransitGatewayRoute'
      <$> (x .@? "state")
      <*> (x .@? "prefixListId")
      <*> ( x .@? "transitGatewayAttachments" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "type")
      <*> (x .@? "destinationCidrBlock")

instance Hashable TransitGatewayRoute

instance NFData TransitGatewayRoute
