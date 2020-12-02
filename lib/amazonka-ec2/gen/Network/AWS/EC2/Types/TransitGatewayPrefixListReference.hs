{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReference where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a prefix list reference.
--
--
--
-- /See:/ 'transitGatewayPrefixListReference' smart constructor.
data TransitGatewayPrefixListReference = TransitGatewayPrefixListReference'
  { _tgplrState ::
      !( Maybe
           TransitGatewayPrefixListReferenceState
       ),
    _tgplrTransitGatewayRouteTableId ::
      !(Maybe Text),
    _tgplrPrefixListOwnerId ::
      !(Maybe Text),
    _tgplrBlackhole ::
      !(Maybe Bool),
    _tgplrPrefixListId ::
      !(Maybe Text),
    _tgplrTransitGatewayAttachment ::
      !( Maybe
           TransitGatewayPrefixListAttachment
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayPrefixListReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgplrState' - The state of the prefix list reference.
--
-- * 'tgplrTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'tgplrPrefixListOwnerId' - The ID of the prefix list owner.
--
-- * 'tgplrBlackhole' - Indicates whether traffic that matches this route is dropped.
--
-- * 'tgplrPrefixListId' - The ID of the prefix list.
--
-- * 'tgplrTransitGatewayAttachment' - Information about the transit gateway attachment.
transitGatewayPrefixListReference ::
  TransitGatewayPrefixListReference
transitGatewayPrefixListReference =
  TransitGatewayPrefixListReference'
    { _tgplrState = Nothing,
      _tgplrTransitGatewayRouteTableId = Nothing,
      _tgplrPrefixListOwnerId = Nothing,
      _tgplrBlackhole = Nothing,
      _tgplrPrefixListId = Nothing,
      _tgplrTransitGatewayAttachment = Nothing
    }

-- | The state of the prefix list reference.
tgplrState :: Lens' TransitGatewayPrefixListReference (Maybe TransitGatewayPrefixListReferenceState)
tgplrState = lens _tgplrState (\s a -> s {_tgplrState = a})

-- | The ID of the transit gateway route table.
tgplrTransitGatewayRouteTableId :: Lens' TransitGatewayPrefixListReference (Maybe Text)
tgplrTransitGatewayRouteTableId = lens _tgplrTransitGatewayRouteTableId (\s a -> s {_tgplrTransitGatewayRouteTableId = a})

-- | The ID of the prefix list owner.
tgplrPrefixListOwnerId :: Lens' TransitGatewayPrefixListReference (Maybe Text)
tgplrPrefixListOwnerId = lens _tgplrPrefixListOwnerId (\s a -> s {_tgplrPrefixListOwnerId = a})

-- | Indicates whether traffic that matches this route is dropped.
tgplrBlackhole :: Lens' TransitGatewayPrefixListReference (Maybe Bool)
tgplrBlackhole = lens _tgplrBlackhole (\s a -> s {_tgplrBlackhole = a})

-- | The ID of the prefix list.
tgplrPrefixListId :: Lens' TransitGatewayPrefixListReference (Maybe Text)
tgplrPrefixListId = lens _tgplrPrefixListId (\s a -> s {_tgplrPrefixListId = a})

-- | Information about the transit gateway attachment.
tgplrTransitGatewayAttachment :: Lens' TransitGatewayPrefixListReference (Maybe TransitGatewayPrefixListAttachment)
tgplrTransitGatewayAttachment = lens _tgplrTransitGatewayAttachment (\s a -> s {_tgplrTransitGatewayAttachment = a})

instance FromXML TransitGatewayPrefixListReference where
  parseXML x =
    TransitGatewayPrefixListReference'
      <$> (x .@? "state")
      <*> (x .@? "transitGatewayRouteTableId")
      <*> (x .@? "prefixListOwnerId")
      <*> (x .@? "blackhole")
      <*> (x .@? "prefixListId")
      <*> (x .@? "transitGatewayAttachment")

instance Hashable TransitGatewayPrefixListReference

instance NFData TransitGatewayPrefixListReference
