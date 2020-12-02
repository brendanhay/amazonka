{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InternetGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an internet gateway.
--
--
--
-- /See:/ 'internetGateway' smart constructor.
data InternetGateway = InternetGateway'
  { _igAttachments ::
      !(Maybe [InternetGatewayAttachment]),
    _igOwnerId :: !(Maybe Text),
    _igTags :: !(Maybe [Tag]),
    _igInternetGatewayId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'igAttachments' - Any VPCs attached to the internet gateway.
--
-- * 'igOwnerId' - The ID of the AWS account that owns the internet gateway.
--
-- * 'igTags' - Any tags assigned to the internet gateway.
--
-- * 'igInternetGatewayId' - The ID of the internet gateway.
internetGateway ::
  -- | 'igInternetGatewayId'
  Text ->
  InternetGateway
internetGateway pInternetGatewayId_ =
  InternetGateway'
    { _igAttachments = Nothing,
      _igOwnerId = Nothing,
      _igTags = Nothing,
      _igInternetGatewayId = pInternetGatewayId_
    }

-- | Any VPCs attached to the internet gateway.
igAttachments :: Lens' InternetGateway [InternetGatewayAttachment]
igAttachments = lens _igAttachments (\s a -> s {_igAttachments = a}) . _Default . _Coerce

-- | The ID of the AWS account that owns the internet gateway.
igOwnerId :: Lens' InternetGateway (Maybe Text)
igOwnerId = lens _igOwnerId (\s a -> s {_igOwnerId = a})

-- | Any tags assigned to the internet gateway.
igTags :: Lens' InternetGateway [Tag]
igTags = lens _igTags (\s a -> s {_igTags = a}) . _Default . _Coerce

-- | The ID of the internet gateway.
igInternetGatewayId :: Lens' InternetGateway Text
igInternetGatewayId = lens _igInternetGatewayId (\s a -> s {_igInternetGatewayId = a})

instance FromXML InternetGateway where
  parseXML x =
    InternetGateway'
      <$> (x .@? "attachmentSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "ownerId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "internetGatewayId")

instance Hashable InternetGateway

instance NFData InternetGateway
