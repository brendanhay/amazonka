{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTableAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between a route table and a resource attachment.
--
--
--
-- /See:/ 'transitGatewayRouteTableAssociation' smart constructor.
data TransitGatewayRouteTableAssociation = TransitGatewayRouteTableAssociation'
  { _tgrtaState ::
      !( Maybe
           TransitGatewayAssociationState
       ),
    _tgrtaResourceId ::
      !(Maybe Text),
    _tgrtaResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tgrtaTransitGatewayAttachmentId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayRouteTableAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgrtaState' - The state of the association.
--
-- * 'tgrtaResourceId' - The ID of the resource.
--
-- * 'tgrtaResourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- * 'tgrtaTransitGatewayAttachmentId' - The ID of the attachment.
transitGatewayRouteTableAssociation ::
  TransitGatewayRouteTableAssociation
transitGatewayRouteTableAssociation =
  TransitGatewayRouteTableAssociation'
    { _tgrtaState = Nothing,
      _tgrtaResourceId = Nothing,
      _tgrtaResourceType = Nothing,
      _tgrtaTransitGatewayAttachmentId = Nothing
    }

-- | The state of the association.
tgrtaState :: Lens' TransitGatewayRouteTableAssociation (Maybe TransitGatewayAssociationState)
tgrtaState = lens _tgrtaState (\s a -> s {_tgrtaState = a})

-- | The ID of the resource.
tgrtaResourceId :: Lens' TransitGatewayRouteTableAssociation (Maybe Text)
tgrtaResourceId = lens _tgrtaResourceId (\s a -> s {_tgrtaResourceId = a})

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
tgrtaResourceType :: Lens' TransitGatewayRouteTableAssociation (Maybe TransitGatewayAttachmentResourceType)
tgrtaResourceType = lens _tgrtaResourceType (\s a -> s {_tgrtaResourceType = a})

-- | The ID of the attachment.
tgrtaTransitGatewayAttachmentId :: Lens' TransitGatewayRouteTableAssociation (Maybe Text)
tgrtaTransitGatewayAttachmentId = lens _tgrtaTransitGatewayAttachmentId (\s a -> s {_tgrtaTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayRouteTableAssociation where
  parseXML x =
    TransitGatewayRouteTableAssociation'
      <$> (x .@? "state")
      <*> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayRouteTableAssociation

instance NFData TransitGatewayRouteTableAssociation
