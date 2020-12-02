{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CarrierGatewayState
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a carrier gateway.
--
--
--
-- /See:/ 'carrierGateway' smart constructor.
data CarrierGateway = CarrierGateway'
  { _cgState ::
      !(Maybe CarrierGatewayState),
    _cgVPCId :: !(Maybe Text),
    _cgOwnerId :: !(Maybe Text),
    _cgTags :: !(Maybe [Tag]),
    _cgCarrierGatewayId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CarrierGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgState' - The state of the carrier gateway.
--
-- * 'cgVPCId' - The ID of the VPC associated with the carrier gateway.
--
-- * 'cgOwnerId' - The AWS account ID of the owner of the carrier gateway.
--
-- * 'cgTags' - The tags assigned to the carrier gateway.
--
-- * 'cgCarrierGatewayId' - The ID of the carrier gateway.
carrierGateway ::
  CarrierGateway
carrierGateway =
  CarrierGateway'
    { _cgState = Nothing,
      _cgVPCId = Nothing,
      _cgOwnerId = Nothing,
      _cgTags = Nothing,
      _cgCarrierGatewayId = Nothing
    }

-- | The state of the carrier gateway.
cgState :: Lens' CarrierGateway (Maybe CarrierGatewayState)
cgState = lens _cgState (\s a -> s {_cgState = a})

-- | The ID of the VPC associated with the carrier gateway.
cgVPCId :: Lens' CarrierGateway (Maybe Text)
cgVPCId = lens _cgVPCId (\s a -> s {_cgVPCId = a})

-- | The AWS account ID of the owner of the carrier gateway.
cgOwnerId :: Lens' CarrierGateway (Maybe Text)
cgOwnerId = lens _cgOwnerId (\s a -> s {_cgOwnerId = a})

-- | The tags assigned to the carrier gateway.
cgTags :: Lens' CarrierGateway [Tag]
cgTags = lens _cgTags (\s a -> s {_cgTags = a}) . _Default . _Coerce

-- | The ID of the carrier gateway.
cgCarrierGatewayId :: Lens' CarrierGateway (Maybe Text)
cgCarrierGatewayId = lens _cgCarrierGatewayId (\s a -> s {_cgCarrierGatewayId = a})

instance FromXML CarrierGateway where
  parseXML x =
    CarrierGateway'
      <$> (x .@? "state")
      <*> (x .@? "vpcId")
      <*> (x .@? "ownerId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "carrierGatewayId")

instance Hashable CarrierGateway

instance NFData CarrierGateway
