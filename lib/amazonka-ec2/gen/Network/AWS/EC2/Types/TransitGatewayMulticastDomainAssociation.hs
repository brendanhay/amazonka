{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the resources associated with the transit gateway multicast domain.
--
--
--
-- /See:/ 'transitGatewayMulticastDomainAssociation' smart constructor.
data TransitGatewayMulticastDomainAssociation = TransitGatewayMulticastDomainAssociation'
  { _tgmdaResourceId ::
      !( Maybe
           Text
       ),
    _tgmdaResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tgmdaSubnet ::
      !( Maybe
           SubnetAssociation
       ),
    _tgmdaTransitGatewayAttachmentId ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayMulticastDomainAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgmdaResourceId' - The ID of the resource.
--
-- * 'tgmdaResourceType' - The type of resource, for example a VPC attachment.
--
-- * 'tgmdaSubnet' - The subnet associated with the transit gateway multicast domain.
--
-- * 'tgmdaTransitGatewayAttachmentId' - The ID of the transit gateway attachment.
transitGatewayMulticastDomainAssociation ::
  TransitGatewayMulticastDomainAssociation
transitGatewayMulticastDomainAssociation =
  TransitGatewayMulticastDomainAssociation'
    { _tgmdaResourceId =
        Nothing,
      _tgmdaResourceType = Nothing,
      _tgmdaSubnet = Nothing,
      _tgmdaTransitGatewayAttachmentId = Nothing
    }

-- | The ID of the resource.
tgmdaResourceId :: Lens' TransitGatewayMulticastDomainAssociation (Maybe Text)
tgmdaResourceId = lens _tgmdaResourceId (\s a -> s {_tgmdaResourceId = a})

-- | The type of resource, for example a VPC attachment.
tgmdaResourceType :: Lens' TransitGatewayMulticastDomainAssociation (Maybe TransitGatewayAttachmentResourceType)
tgmdaResourceType = lens _tgmdaResourceType (\s a -> s {_tgmdaResourceType = a})

-- | The subnet associated with the transit gateway multicast domain.
tgmdaSubnet :: Lens' TransitGatewayMulticastDomainAssociation (Maybe SubnetAssociation)
tgmdaSubnet = lens _tgmdaSubnet (\s a -> s {_tgmdaSubnet = a})

-- | The ID of the transit gateway attachment.
tgmdaTransitGatewayAttachmentId :: Lens' TransitGatewayMulticastDomainAssociation (Maybe Text)
tgmdaTransitGatewayAttachmentId = lens _tgmdaTransitGatewayAttachmentId (\s a -> s {_tgmdaTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayMulticastDomainAssociation where
  parseXML x =
    TransitGatewayMulticastDomainAssociation'
      <$> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "subnet")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayMulticastDomainAssociation

instance NFData TransitGatewayMulticastDomainAssociation
