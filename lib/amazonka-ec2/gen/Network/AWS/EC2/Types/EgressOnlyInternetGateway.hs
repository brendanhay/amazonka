{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EgressOnlyInternetGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InternetGatewayAttachment
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an egress-only internet gateway.
--
--
--
-- /See:/ 'egressOnlyInternetGateway' smart constructor.
data EgressOnlyInternetGateway = EgressOnlyInternetGateway'
  { _eoigEgressOnlyInternetGatewayId ::
      !(Maybe Text),
    _eoigAttachments ::
      !(Maybe [InternetGatewayAttachment]),
    _eoigTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoigEgressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
--
-- * 'eoigAttachments' - Information about the attachment of the egress-only internet gateway.
--
-- * 'eoigTags' - The tags assigned to the egress-only internet gateway.
egressOnlyInternetGateway ::
  EgressOnlyInternetGateway
egressOnlyInternetGateway =
  EgressOnlyInternetGateway'
    { _eoigEgressOnlyInternetGatewayId =
        Nothing,
      _eoigAttachments = Nothing,
      _eoigTags = Nothing
    }

-- | The ID of the egress-only internet gateway.
eoigEgressOnlyInternetGatewayId :: Lens' EgressOnlyInternetGateway (Maybe Text)
eoigEgressOnlyInternetGatewayId = lens _eoigEgressOnlyInternetGatewayId (\s a -> s {_eoigEgressOnlyInternetGatewayId = a})

-- | Information about the attachment of the egress-only internet gateway.
eoigAttachments :: Lens' EgressOnlyInternetGateway [InternetGatewayAttachment]
eoigAttachments = lens _eoigAttachments (\s a -> s {_eoigAttachments = a}) . _Default . _Coerce

-- | The tags assigned to the egress-only internet gateway.
eoigTags :: Lens' EgressOnlyInternetGateway [Tag]
eoigTags = lens _eoigTags (\s a -> s {_eoigTags = a}) . _Default . _Coerce

instance FromXML EgressOnlyInternetGateway where
  parseXML x =
    EgressOnlyInternetGateway'
      <$> (x .@? "egressOnlyInternetGatewayId")
      <*> (x .@? "attachmentSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable EgressOnlyInternetGateway

instance NFData EgressOnlyInternetGateway
