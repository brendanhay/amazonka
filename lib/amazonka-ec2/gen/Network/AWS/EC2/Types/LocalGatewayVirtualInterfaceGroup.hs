{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a local gateway virtual interface group.
--
--
--
-- /See:/ 'localGatewayVirtualInterfaceGroup' smart constructor.
data LocalGatewayVirtualInterfaceGroup = LocalGatewayVirtualInterfaceGroup'
  { _lgvigLocalGatewayId ::
      !(Maybe Text),
    _lgvigOwnerId ::
      !(Maybe Text),
    _lgvigLocalGatewayVirtualInterfaceIds ::
      !(Maybe [Text]),
    _lgvigLocalGatewayVirtualInterfaceGroupId ::
      !(Maybe Text),
    _lgvigTags ::
      !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalGatewayVirtualInterfaceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgvigLocalGatewayId' - The ID of the local gateway.
--
-- * 'lgvigOwnerId' - The AWS account ID that owns the local gateway virtual interface group.
--
-- * 'lgvigLocalGatewayVirtualInterfaceIds' - The IDs of the virtual interfaces.
--
-- * 'lgvigLocalGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- * 'lgvigTags' - The tags assigned to the virtual interface group.
localGatewayVirtualInterfaceGroup ::
  LocalGatewayVirtualInterfaceGroup
localGatewayVirtualInterfaceGroup =
  LocalGatewayVirtualInterfaceGroup'
    { _lgvigLocalGatewayId =
        Nothing,
      _lgvigOwnerId = Nothing,
      _lgvigLocalGatewayVirtualInterfaceIds = Nothing,
      _lgvigLocalGatewayVirtualInterfaceGroupId = Nothing,
      _lgvigTags = Nothing
    }

-- | The ID of the local gateway.
lgvigLocalGatewayId :: Lens' LocalGatewayVirtualInterfaceGroup (Maybe Text)
lgvigLocalGatewayId = lens _lgvigLocalGatewayId (\s a -> s {_lgvigLocalGatewayId = a})

-- | The AWS account ID that owns the local gateway virtual interface group.
lgvigOwnerId :: Lens' LocalGatewayVirtualInterfaceGroup (Maybe Text)
lgvigOwnerId = lens _lgvigOwnerId (\s a -> s {_lgvigOwnerId = a})

-- | The IDs of the virtual interfaces.
lgvigLocalGatewayVirtualInterfaceIds :: Lens' LocalGatewayVirtualInterfaceGroup [Text]
lgvigLocalGatewayVirtualInterfaceIds = lens _lgvigLocalGatewayVirtualInterfaceIds (\s a -> s {_lgvigLocalGatewayVirtualInterfaceIds = a}) . _Default . _Coerce

-- | The ID of the virtual interface group.
lgvigLocalGatewayVirtualInterfaceGroupId :: Lens' LocalGatewayVirtualInterfaceGroup (Maybe Text)
lgvigLocalGatewayVirtualInterfaceGroupId = lens _lgvigLocalGatewayVirtualInterfaceGroupId (\s a -> s {_lgvigLocalGatewayVirtualInterfaceGroupId = a})

-- | The tags assigned to the virtual interface group.
lgvigTags :: Lens' LocalGatewayVirtualInterfaceGroup [Tag]
lgvigTags = lens _lgvigTags (\s a -> s {_lgvigTags = a}) . _Default . _Coerce

instance FromXML LocalGatewayVirtualInterfaceGroup where
  parseXML x =
    LocalGatewayVirtualInterfaceGroup'
      <$> (x .@? "localGatewayId")
      <*> (x .@? "ownerId")
      <*> ( x .@? "localGatewayVirtualInterfaceIdSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "localGatewayVirtualInterfaceGroupId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable LocalGatewayVirtualInterfaceGroup

instance NFData LocalGatewayVirtualInterfaceGroup
