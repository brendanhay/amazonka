{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a transit gateway prefix list attachment.
--
--
--
-- /See:/ 'transitGatewayPrefixListAttachment' smart constructor.
data TransitGatewayPrefixListAttachment = TransitGatewayPrefixListAttachment'
  { _tgplaResourceId ::
      !(Maybe Text),
    _tgplaResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tgplaTransitGatewayAttachmentId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayPrefixListAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgplaResourceId' - The ID of the resource.
--
-- * 'tgplaResourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- * 'tgplaTransitGatewayAttachmentId' - The ID of the attachment.
transitGatewayPrefixListAttachment ::
  TransitGatewayPrefixListAttachment
transitGatewayPrefixListAttachment =
  TransitGatewayPrefixListAttachment'
    { _tgplaResourceId = Nothing,
      _tgplaResourceType = Nothing,
      _tgplaTransitGatewayAttachmentId = Nothing
    }

-- | The ID of the resource.
tgplaResourceId :: Lens' TransitGatewayPrefixListAttachment (Maybe Text)
tgplaResourceId = lens _tgplaResourceId (\s a -> s {_tgplaResourceId = a})

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
tgplaResourceType :: Lens' TransitGatewayPrefixListAttachment (Maybe TransitGatewayAttachmentResourceType)
tgplaResourceType = lens _tgplaResourceType (\s a -> s {_tgplaResourceType = a})

-- | The ID of the attachment.
tgplaTransitGatewayAttachmentId :: Lens' TransitGatewayPrefixListAttachment (Maybe Text)
tgplaTransitGatewayAttachmentId = lens _tgplaTransitGatewayAttachmentId (\s a -> s {_tgplaTransitGatewayAttachmentId = a})

instance FromXML TransitGatewayPrefixListAttachment where
  parseXML x =
    TransitGatewayPrefixListAttachment'
      <$> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "transitGatewayAttachmentId")

instance Hashable TransitGatewayPrefixListAttachment

instance NFData TransitGatewayPrefixListAttachment
