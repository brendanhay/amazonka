{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociations where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the multicast domain associations.
--
--
--
-- /See:/ 'transitGatewayMulticastDomainAssociations' smart constructor.
data TransitGatewayMulticastDomainAssociations = TransitGatewayMulticastDomainAssociations'
  { _tResourceId ::
      !( Maybe
           Text
       ),
    _tResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tSubnets ::
      !( Maybe
           [SubnetAssociation]
       ),
    _tTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _tTransitGatewayAttachmentId ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'TransitGatewayMulticastDomainAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tResourceId' - The ID of the resource.
--
-- * 'tResourceType' - The type of resource, for example a VPC attachment.
--
-- * 'tSubnets' - The subnets associated with the multicast domain.
--
-- * 'tTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'tTransitGatewayAttachmentId' - The ID of the transit gateway attachment.
transitGatewayMulticastDomainAssociations ::
  TransitGatewayMulticastDomainAssociations
transitGatewayMulticastDomainAssociations =
  TransitGatewayMulticastDomainAssociations'
    { _tResourceId =
        Nothing,
      _tResourceType = Nothing,
      _tSubnets = Nothing,
      _tTransitGatewayMulticastDomainId = Nothing,
      _tTransitGatewayAttachmentId = Nothing
    }

-- | The ID of the resource.
tResourceId :: Lens' TransitGatewayMulticastDomainAssociations (Maybe Text)
tResourceId = lens _tResourceId (\s a -> s {_tResourceId = a})

-- | The type of resource, for example a VPC attachment.
tResourceType :: Lens' TransitGatewayMulticastDomainAssociations (Maybe TransitGatewayAttachmentResourceType)
tResourceType = lens _tResourceType (\s a -> s {_tResourceType = a})

-- | The subnets associated with the multicast domain.
tSubnets :: Lens' TransitGatewayMulticastDomainAssociations [SubnetAssociation]
tSubnets = lens _tSubnets (\s a -> s {_tSubnets = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
tTransitGatewayMulticastDomainId :: Lens' TransitGatewayMulticastDomainAssociations (Maybe Text)
tTransitGatewayMulticastDomainId = lens _tTransitGatewayMulticastDomainId (\s a -> s {_tTransitGatewayMulticastDomainId = a})

-- | The ID of the transit gateway attachment.
tTransitGatewayAttachmentId :: Lens' TransitGatewayMulticastDomainAssociations (Maybe Text)
tTransitGatewayAttachmentId = lens _tTransitGatewayAttachmentId (\s a -> s {_tTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayMulticastDomainAssociations where
  parseXML x =
    TransitGatewayMulticastDomainAssociations'
      <$> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "subnets" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "transitGatewayMulticastDomainId")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayMulticastDomainAssociations

instance NFData TransitGatewayMulticastDomainAssociations
