{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between a resource attachment and a transit gateway route table.
--
--
--
-- /See:/ 'transitGatewayAssociation' smart constructor.
data TransitGatewayAssociation = TransitGatewayAssociation'
  { _traState ::
      !(Maybe TransitGatewayAssociationState),
    _traResourceId :: !(Maybe Text),
    _traResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _traTransitGatewayRouteTableId ::
      !(Maybe Text),
    _traTransitGatewayAttachmentId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'traState' - The state of the association.
--
-- * 'traResourceId' - The ID of the resource.
--
-- * 'traResourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- * 'traTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'traTransitGatewayAttachmentId' - The ID of the attachment.
transitGatewayAssociation ::
  TransitGatewayAssociation
transitGatewayAssociation =
  TransitGatewayAssociation'
    { _traState = Nothing,
      _traResourceId = Nothing,
      _traResourceType = Nothing,
      _traTransitGatewayRouteTableId = Nothing,
      _traTransitGatewayAttachmentId = Nothing
    }

-- | The state of the association.
traState :: Lens' TransitGatewayAssociation (Maybe TransitGatewayAssociationState)
traState = lens _traState (\s a -> s {_traState = a})

-- | The ID of the resource.
traResourceId :: Lens' TransitGatewayAssociation (Maybe Text)
traResourceId = lens _traResourceId (\s a -> s {_traResourceId = a})

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
traResourceType :: Lens' TransitGatewayAssociation (Maybe TransitGatewayAttachmentResourceType)
traResourceType = lens _traResourceType (\s a -> s {_traResourceType = a})

-- | The ID of the transit gateway route table.
traTransitGatewayRouteTableId :: Lens' TransitGatewayAssociation (Maybe Text)
traTransitGatewayRouteTableId = lens _traTransitGatewayRouteTableId (\s a -> s {_traTransitGatewayRouteTableId = a})

-- | The ID of the attachment.
traTransitGatewayAttachmentId :: Lens' TransitGatewayAssociation (Maybe Text)
traTransitGatewayAttachmentId = lens _traTransitGatewayAttachmentId (\s a -> s {_traTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayAssociation where
  parseXML x =
    TransitGatewayAssociation'
      <$> (x .@? "state")
      <*> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "transitGatewayRouteTableId")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayAssociation

instance NFData TransitGatewayAssociation
