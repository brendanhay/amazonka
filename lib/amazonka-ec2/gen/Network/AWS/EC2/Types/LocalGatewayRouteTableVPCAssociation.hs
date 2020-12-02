{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVPCAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTableVPCAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between a local gateway route table and a VPC.
--
--
--
-- /See:/ 'localGatewayRouteTableVPCAssociation' smart constructor.
data LocalGatewayRouteTableVPCAssociation = LocalGatewayRouteTableVPCAssociation'
  { _lgrtvaState ::
      !(Maybe Text),
    _lgrtvaLocalGatewayRouteTableARN ::
      !(Maybe Text),
    _lgrtvaVPCId ::
      !(Maybe Text),
    _lgrtvaLocalGatewayId ::
      !(Maybe Text),
    _lgrtvaLocalGatewayRouteTableVPCAssociationId ::
      !(Maybe Text),
    _lgrtvaOwnerId ::
      !(Maybe Text),
    _lgrtvaLocalGatewayRouteTableId ::
      !(Maybe Text),
    _lgrtvaTags ::
      !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalGatewayRouteTableVPCAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrtvaState' - The state of the association.
--
-- * 'lgrtvaLocalGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table for the association.
--
-- * 'lgrtvaVPCId' - The ID of the VPC.
--
-- * 'lgrtvaLocalGatewayId' - The ID of the local gateway.
--
-- * 'lgrtvaLocalGatewayRouteTableVPCAssociationId' - The ID of the association.
--
-- * 'lgrtvaOwnerId' - The AWS account ID that owns the local gateway route table for the association.
--
-- * 'lgrtvaLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'lgrtvaTags' - The tags assigned to the association.
localGatewayRouteTableVPCAssociation ::
  LocalGatewayRouteTableVPCAssociation
localGatewayRouteTableVPCAssociation =
  LocalGatewayRouteTableVPCAssociation'
    { _lgrtvaState = Nothing,
      _lgrtvaLocalGatewayRouteTableARN = Nothing,
      _lgrtvaVPCId = Nothing,
      _lgrtvaLocalGatewayId = Nothing,
      _lgrtvaLocalGatewayRouteTableVPCAssociationId = Nothing,
      _lgrtvaOwnerId = Nothing,
      _lgrtvaLocalGatewayRouteTableId = Nothing,
      _lgrtvaTags = Nothing
    }

-- | The state of the association.
lgrtvaState :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaState = lens _lgrtvaState (\s a -> s {_lgrtvaState = a})

-- | The Amazon Resource Name (ARN) of the local gateway route table for the association.
lgrtvaLocalGatewayRouteTableARN :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaLocalGatewayRouteTableARN = lens _lgrtvaLocalGatewayRouteTableARN (\s a -> s {_lgrtvaLocalGatewayRouteTableARN = a})

-- | The ID of the VPC.
lgrtvaVPCId :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaVPCId = lens _lgrtvaVPCId (\s a -> s {_lgrtvaVPCId = a})

-- | The ID of the local gateway.
lgrtvaLocalGatewayId :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaLocalGatewayId = lens _lgrtvaLocalGatewayId (\s a -> s {_lgrtvaLocalGatewayId = a})

-- | The ID of the association.
lgrtvaLocalGatewayRouteTableVPCAssociationId :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaLocalGatewayRouteTableVPCAssociationId = lens _lgrtvaLocalGatewayRouteTableVPCAssociationId (\s a -> s {_lgrtvaLocalGatewayRouteTableVPCAssociationId = a})

-- | The AWS account ID that owns the local gateway route table for the association.
lgrtvaOwnerId :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaOwnerId = lens _lgrtvaOwnerId (\s a -> s {_lgrtvaOwnerId = a})

-- | The ID of the local gateway route table.
lgrtvaLocalGatewayRouteTableId :: Lens' LocalGatewayRouteTableVPCAssociation (Maybe Text)
lgrtvaLocalGatewayRouteTableId = lens _lgrtvaLocalGatewayRouteTableId (\s a -> s {_lgrtvaLocalGatewayRouteTableId = a})

-- | The tags assigned to the association.
lgrtvaTags :: Lens' LocalGatewayRouteTableVPCAssociation [Tag]
lgrtvaTags = lens _lgrtvaTags (\s a -> s {_lgrtvaTags = a}) . _Default . _Coerce

instance FromXML LocalGatewayRouteTableVPCAssociation where
  parseXML x =
    LocalGatewayRouteTableVPCAssociation'
      <$> (x .@? "state")
      <*> (x .@? "localGatewayRouteTableArn")
      <*> (x .@? "vpcId")
      <*> (x .@? "localGatewayId")
      <*> (x .@? "localGatewayRouteTableVpcAssociationId")
      <*> (x .@? "ownerId")
      <*> (x .@? "localGatewayRouteTableId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable LocalGatewayRouteTableVPCAssociation

instance NFData LocalGatewayRouteTableVPCAssociation
