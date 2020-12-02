{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPeeringAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PeeringAttachmentStatus
import Network.AWS.EC2.Types.PeeringTgwInfo
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the transit gateway peering attachment.
--
--
--
-- /See:/ 'transitGatewayPeeringAttachment' smart constructor.
data TransitGatewayPeeringAttachment = TransitGatewayPeeringAttachment'
  { _tgpaCreationTime ::
      !(Maybe ISO8601),
    _tgpaRequesterTgwInfo ::
      !(Maybe PeeringTgwInfo),
    _tgpaStatus ::
      !( Maybe
           PeeringAttachmentStatus
       ),
    _tgpaState ::
      !( Maybe
           TransitGatewayAttachmentState
       ),
    _tgpaAccepterTgwInfo ::
      !(Maybe PeeringTgwInfo),
    _tgpaTransitGatewayAttachmentId ::
      !(Maybe Text),
    _tgpaTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgpaCreationTime' - The time the transit gateway peering attachment was created.
--
-- * 'tgpaRequesterTgwInfo' - Information about the requester transit gateway.
--
-- * 'tgpaStatus' - The status of the transit gateway peering attachment.
--
-- * 'tgpaState' - The state of the transit gateway peering attachment. Note that the @initiating@ state has been deprecated.
--
-- * 'tgpaAccepterTgwInfo' - Information about the accepter transit gateway.
--
-- * 'tgpaTransitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
--
-- * 'tgpaTags' - The tags for the transit gateway peering attachment.
transitGatewayPeeringAttachment ::
  TransitGatewayPeeringAttachment
transitGatewayPeeringAttachment =
  TransitGatewayPeeringAttachment'
    { _tgpaCreationTime = Nothing,
      _tgpaRequesterTgwInfo = Nothing,
      _tgpaStatus = Nothing,
      _tgpaState = Nothing,
      _tgpaAccepterTgwInfo = Nothing,
      _tgpaTransitGatewayAttachmentId = Nothing,
      _tgpaTags = Nothing
    }

-- | The time the transit gateway peering attachment was created.
tgpaCreationTime :: Lens' TransitGatewayPeeringAttachment (Maybe UTCTime)
tgpaCreationTime = lens _tgpaCreationTime (\s a -> s {_tgpaCreationTime = a}) . mapping _Time

-- | Information about the requester transit gateway.
tgpaRequesterTgwInfo :: Lens' TransitGatewayPeeringAttachment (Maybe PeeringTgwInfo)
tgpaRequesterTgwInfo = lens _tgpaRequesterTgwInfo (\s a -> s {_tgpaRequesterTgwInfo = a})

-- | The status of the transit gateway peering attachment.
tgpaStatus :: Lens' TransitGatewayPeeringAttachment (Maybe PeeringAttachmentStatus)
tgpaStatus = lens _tgpaStatus (\s a -> s {_tgpaStatus = a})

-- | The state of the transit gateway peering attachment. Note that the @initiating@ state has been deprecated.
tgpaState :: Lens' TransitGatewayPeeringAttachment (Maybe TransitGatewayAttachmentState)
tgpaState = lens _tgpaState (\s a -> s {_tgpaState = a})

-- | Information about the accepter transit gateway.
tgpaAccepterTgwInfo :: Lens' TransitGatewayPeeringAttachment (Maybe PeeringTgwInfo)
tgpaAccepterTgwInfo = lens _tgpaAccepterTgwInfo (\s a -> s {_tgpaAccepterTgwInfo = a})

-- | The ID of the transit gateway peering attachment.
tgpaTransitGatewayAttachmentId :: Lens' TransitGatewayPeeringAttachment (Maybe Text)
tgpaTransitGatewayAttachmentId = lens _tgpaTransitGatewayAttachmentId (\s a -> s {_tgpaTransitGatewayAttachmentId = a})

-- | The tags for the transit gateway peering attachment.
tgpaTags :: Lens' TransitGatewayPeeringAttachment [Tag]
tgpaTags = lens _tgpaTags (\s a -> s {_tgpaTags = a}) . _Default . _Coerce

instance FromXML TransitGatewayPeeringAttachment where
  parseXML x =
    TransitGatewayPeeringAttachment'
      <$> (x .@? "creationTime")
      <*> (x .@? "requesterTgwInfo")
      <*> (x .@? "status")
      <*> (x .@? "state")
      <*> (x .@? "accepterTgwInfo")
      <*> (x .@? "transitGatewayAttachmentId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable TransitGatewayPeeringAttachment

instance NFData TransitGatewayPeeringAttachment
