{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a route attachment.
--
--
--
-- /See:/ 'transitGatewayRouteAttachment' smart constructor.
data TransitGatewayRouteAttachment = TransitGatewayRouteAttachment'
  { _tgraResourceId ::
      !(Maybe Text),
    _tgraResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tgraTransitGatewayAttachmentId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayRouteAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgraResourceId' - The ID of the resource.
--
-- * 'tgraResourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- * 'tgraTransitGatewayAttachmentId' - The ID of the attachment.
transitGatewayRouteAttachment ::
  TransitGatewayRouteAttachment
transitGatewayRouteAttachment =
  TransitGatewayRouteAttachment'
    { _tgraResourceId = Nothing,
      _tgraResourceType = Nothing,
      _tgraTransitGatewayAttachmentId = Nothing
    }

-- | The ID of the resource.
tgraResourceId :: Lens' TransitGatewayRouteAttachment (Maybe Text)
tgraResourceId = lens _tgraResourceId (\s a -> s {_tgraResourceId = a})

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
tgraResourceType :: Lens' TransitGatewayRouteAttachment (Maybe TransitGatewayAttachmentResourceType)
tgraResourceType = lens _tgraResourceType (\s a -> s {_tgraResourceType = a})

-- | The ID of the attachment.
tgraTransitGatewayAttachmentId :: Lens' TransitGatewayRouteAttachment (Maybe Text)
tgraTransitGatewayAttachmentId = lens _tgraTransitGatewayAttachmentId (\s a -> s {_tgraTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayRouteAttachment where
  parseXML x =
    TransitGatewayRouteAttachment'
      <$> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayRouteAttachment

instance NFData TransitGatewayRouteAttachment
