{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an attachment between a resource and a transit gateway.
--
--
--
-- /See:/ 'transitGatewayAttachment' smart constructor.
data TransitGatewayAttachment = TransitGatewayAttachment'
  { _tgaCreationTime ::
      !(Maybe ISO8601),
    _tgaState ::
      !(Maybe TransitGatewayAttachmentState),
    _tgaResourceId :: !(Maybe Text),
    _tgaResourceType ::
      !( Maybe
           TransitGatewayAttachmentResourceType
       ),
    _tgaTransitGatewayOwnerId ::
      !(Maybe Text),
    _tgaTransitGatewayId :: !(Maybe Text),
    _tgaTransitGatewayAttachmentId ::
      !(Maybe Text),
    _tgaResourceOwnerId :: !(Maybe Text),
    _tgaTags :: !(Maybe [Tag]),
    _tgaAssociation ::
      !( Maybe
           TransitGatewayAttachmentAssociation
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgaCreationTime' - The creation time.
--
-- * 'tgaState' - The attachment state. Note that the @initiating@ state has been deprecated.
--
-- * 'tgaResourceId' - The ID of the resource.
--
-- * 'tgaResourceType' - The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- * 'tgaTransitGatewayOwnerId' - The ID of the AWS account that owns the transit gateway.
--
-- * 'tgaTransitGatewayId' - The ID of the transit gateway.
--
-- * 'tgaTransitGatewayAttachmentId' - The ID of the attachment.
--
-- * 'tgaResourceOwnerId' - The ID of the AWS account that owns the resource.
--
-- * 'tgaTags' - The tags for the attachment.
--
-- * 'tgaAssociation' - The association.
transitGatewayAttachment ::
  TransitGatewayAttachment
transitGatewayAttachment =
  TransitGatewayAttachment'
    { _tgaCreationTime = Nothing,
      _tgaState = Nothing,
      _tgaResourceId = Nothing,
      _tgaResourceType = Nothing,
      _tgaTransitGatewayOwnerId = Nothing,
      _tgaTransitGatewayId = Nothing,
      _tgaTransitGatewayAttachmentId = Nothing,
      _tgaResourceOwnerId = Nothing,
      _tgaTags = Nothing,
      _tgaAssociation = Nothing
    }

-- | The creation time.
tgaCreationTime :: Lens' TransitGatewayAttachment (Maybe UTCTime)
tgaCreationTime = lens _tgaCreationTime (\s a -> s {_tgaCreationTime = a}) . mapping _Time

-- | The attachment state. Note that the @initiating@ state has been deprecated.
tgaState :: Lens' TransitGatewayAttachment (Maybe TransitGatewayAttachmentState)
tgaState = lens _tgaState (\s a -> s {_tgaState = a})

-- | The ID of the resource.
tgaResourceId :: Lens' TransitGatewayAttachment (Maybe Text)
tgaResourceId = lens _tgaResourceId (\s a -> s {_tgaResourceId = a})

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
tgaResourceType :: Lens' TransitGatewayAttachment (Maybe TransitGatewayAttachmentResourceType)
tgaResourceType = lens _tgaResourceType (\s a -> s {_tgaResourceType = a})

-- | The ID of the AWS account that owns the transit gateway.
tgaTransitGatewayOwnerId :: Lens' TransitGatewayAttachment (Maybe Text)
tgaTransitGatewayOwnerId = lens _tgaTransitGatewayOwnerId (\s a -> s {_tgaTransitGatewayOwnerId = a})

-- | The ID of the transit gateway.
tgaTransitGatewayId :: Lens' TransitGatewayAttachment (Maybe Text)
tgaTransitGatewayId = lens _tgaTransitGatewayId (\s a -> s {_tgaTransitGatewayId = a})

-- | The ID of the attachment.
tgaTransitGatewayAttachmentId :: Lens' TransitGatewayAttachment (Maybe Text)
tgaTransitGatewayAttachmentId = lens _tgaTransitGatewayAttachmentId (\s a -> s {_tgaTransitGatewayAttachmentId = a})

-- | The ID of the AWS account that owns the resource.
tgaResourceOwnerId :: Lens' TransitGatewayAttachment (Maybe Text)
tgaResourceOwnerId = lens _tgaResourceOwnerId (\s a -> s {_tgaResourceOwnerId = a})

-- | The tags for the attachment.
tgaTags :: Lens' TransitGatewayAttachment [Tag]
tgaTags = lens _tgaTags (\s a -> s {_tgaTags = a}) . _Default . _Coerce

-- | The association.
tgaAssociation :: Lens' TransitGatewayAttachment (Maybe TransitGatewayAttachmentAssociation)
tgaAssociation = lens _tgaAssociation (\s a -> s {_tgaAssociation = a})

instance FromXML TransitGatewayAttachment where
  parseXML x =
    TransitGatewayAttachment'
      <$> (x .@? "creationTime")
      <*> (x .@? "state")
      <*> (x .@? "resourceId")
      <*> (x .@? "resourceType")
      <*> (x .@? "transitGatewayOwnerId")
      <*> (x .@? "transitGatewayId")
      <*> (x .@? "transitGatewayAttachmentId")
      <*> (x .@? "resourceOwnerId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "association")

instance Hashable TransitGatewayAttachment

instance NFData TransitGatewayAttachment
