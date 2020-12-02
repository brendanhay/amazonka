{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a route table propagation.
--
--
--
-- /See:/ 'transitGatewayRouteTablePropagation' smart constructor.
data TransitGatewayRouteTablePropagation = TransitGatewayRouteTablePropagation'
  { _tgrtpState ::
      !( Maybe
           TransitGatewayPropagationState
       ),
    _tgrtpResourceId ::
      !(Maybe Text),
    _tgrtpResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tgrtpTransitGatewayAttachmentId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayRouteTablePropagation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgrtpState' - The state of the resource.
--
-- * 'tgrtpResourceId' - The ID of the resource.
--
-- * 'tgrtpResourceType' - The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
--
-- * 'tgrtpTransitGatewayAttachmentId' - The ID of the attachment.
transitGatewayRouteTablePropagation ::
  TransitGatewayRouteTablePropagation
transitGatewayRouteTablePropagation =
  TransitGatewayRouteTablePropagation'
    { _tgrtpState = Nothing,
      _tgrtpResourceId = Nothing,
      _tgrtpResourceType = Nothing,
      _tgrtpTransitGatewayAttachmentId = Nothing
    }

-- | The state of the resource.
tgrtpState :: Lens' TransitGatewayRouteTablePropagation (Maybe TransitGatewayPropagationState)
tgrtpState = lens _tgrtpState (\s a -> s {_tgrtpState = a})

-- | The ID of the resource.
tgrtpResourceId :: Lens' TransitGatewayRouteTablePropagation (Maybe Text)
tgrtpResourceId = lens _tgrtpResourceId (\s a -> s {_tgrtpResourceId = a})

-- | The type of resource. Note that the @tgw-peering@ resource type has been deprecated.
tgrtpResourceType :: Lens' TransitGatewayRouteTablePropagation (Maybe TransitGatewayAttachmentResourceType)
tgrtpResourceType = lens _tgrtpResourceType (\s a -> s {_tgrtpResourceType = a})

-- | The ID of the attachment.
tgrtpTransitGatewayAttachmentId :: Lens' TransitGatewayRouteTablePropagation (Maybe Text)
tgrtpTransitGatewayAttachmentId = lens _tgrtpTransitGatewayAttachmentId (\s a -> s {_tgrtpTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayRouteTablePropagation where
  parseXML x =
    TransitGatewayRouteTablePropagation'
      <$> (x .@? "state")
      <*> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayRouteTablePropagation

instance NFData TransitGatewayRouteTablePropagation
