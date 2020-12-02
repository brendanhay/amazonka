{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayVPCAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a VPC attachment.
--
--
--
-- /See:/ 'transitGatewayVPCAttachment' smart constructor.
data TransitGatewayVPCAttachment = TransitGatewayVPCAttachment'
  { _tgvaCreationTime ::
      !(Maybe ISO8601),
    _tgvaState ::
      !( Maybe
           TransitGatewayAttachmentState
       ),
    _tgvaSubnetIds :: !(Maybe [Text]),
    _tgvaVPCId :: !(Maybe Text),
    _tgvaTransitGatewayId ::
      !(Maybe Text),
    _tgvaOptions ::
      !( Maybe
           TransitGatewayVPCAttachmentOptions
       ),
    _tgvaTransitGatewayAttachmentId ::
      !(Maybe Text),
    _tgvaTags :: !(Maybe [Tag]),
    _tgvaVPCOwnerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgvaCreationTime' - The creation time.
--
-- * 'tgvaState' - The state of the VPC attachment. Note that the @initiating@ state has been deprecated.
--
-- * 'tgvaSubnetIds' - The IDs of the subnets.
--
-- * 'tgvaVPCId' - The ID of the VPC.
--
-- * 'tgvaTransitGatewayId' - The ID of the transit gateway.
--
-- * 'tgvaOptions' - The VPC attachment options.
--
-- * 'tgvaTransitGatewayAttachmentId' - The ID of the attachment.
--
-- * 'tgvaTags' - The tags for the VPC attachment.
--
-- * 'tgvaVPCOwnerId' - The ID of the AWS account that owns the VPC.
transitGatewayVPCAttachment ::
  TransitGatewayVPCAttachment
transitGatewayVPCAttachment =
  TransitGatewayVPCAttachment'
    { _tgvaCreationTime = Nothing,
      _tgvaState = Nothing,
      _tgvaSubnetIds = Nothing,
      _tgvaVPCId = Nothing,
      _tgvaTransitGatewayId = Nothing,
      _tgvaOptions = Nothing,
      _tgvaTransitGatewayAttachmentId = Nothing,
      _tgvaTags = Nothing,
      _tgvaVPCOwnerId = Nothing
    }

-- | The creation time.
tgvaCreationTime :: Lens' TransitGatewayVPCAttachment (Maybe UTCTime)
tgvaCreationTime = lens _tgvaCreationTime (\s a -> s {_tgvaCreationTime = a}) . mapping _Time

-- | The state of the VPC attachment. Note that the @initiating@ state has been deprecated.
tgvaState :: Lens' TransitGatewayVPCAttachment (Maybe TransitGatewayAttachmentState)
tgvaState = lens _tgvaState (\s a -> s {_tgvaState = a})

-- | The IDs of the subnets.
tgvaSubnetIds :: Lens' TransitGatewayVPCAttachment [Text]
tgvaSubnetIds = lens _tgvaSubnetIds (\s a -> s {_tgvaSubnetIds = a}) . _Default . _Coerce

-- | The ID of the VPC.
tgvaVPCId :: Lens' TransitGatewayVPCAttachment (Maybe Text)
tgvaVPCId = lens _tgvaVPCId (\s a -> s {_tgvaVPCId = a})

-- | The ID of the transit gateway.
tgvaTransitGatewayId :: Lens' TransitGatewayVPCAttachment (Maybe Text)
tgvaTransitGatewayId = lens _tgvaTransitGatewayId (\s a -> s {_tgvaTransitGatewayId = a})

-- | The VPC attachment options.
tgvaOptions :: Lens' TransitGatewayVPCAttachment (Maybe TransitGatewayVPCAttachmentOptions)
tgvaOptions = lens _tgvaOptions (\s a -> s {_tgvaOptions = a})

-- | The ID of the attachment.
tgvaTransitGatewayAttachmentId :: Lens' TransitGatewayVPCAttachment (Maybe Text)
tgvaTransitGatewayAttachmentId = lens _tgvaTransitGatewayAttachmentId (\s a -> s {_tgvaTransitGatewayAttachmentId = a})

-- | The tags for the VPC attachment.
tgvaTags :: Lens' TransitGatewayVPCAttachment [Tag]
tgvaTags = lens _tgvaTags (\s a -> s {_tgvaTags = a}) . _Default . _Coerce

-- | The ID of the AWS account that owns the VPC.
tgvaVPCOwnerId :: Lens' TransitGatewayVPCAttachment (Maybe Text)
tgvaVPCOwnerId = lens _tgvaVPCOwnerId (\s a -> s {_tgvaVPCOwnerId = a})

instance FromXML TransitGatewayVPCAttachment where
  parseXML x =
    TransitGatewayVPCAttachment'
      <$> (x .@? "creationTime")
      <*> (x .@? "state")
      <*> (x .@? "subnetIds" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vpcId")
      <*> (x .@? "transitGatewayId")
      <*> (x .@? "options")
      <*> (x .@? "transitGatewayAttachmentId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vpcOwnerId")

instance Hashable TransitGatewayVPCAttachment

instance NFData TransitGatewayVPCAttachment
