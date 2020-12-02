{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayVirtualInterface where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a local gateway virtual interface.
--
--
--
-- /See:/ 'localGatewayVirtualInterface' smart constructor.
data LocalGatewayVirtualInterface = LocalGatewayVirtualInterface'
  { _lgviLocalGatewayVirtualInterfaceId ::
      !(Maybe Text),
    _lgviLocalBGPASN :: !(Maybe Int),
    _lgviVLAN :: !(Maybe Int),
    _lgviLocalGatewayId ::
      !(Maybe Text),
    _lgviLocalAddress ::
      !(Maybe Text),
    _lgviPeerBGPASN :: !(Maybe Int),
    _lgviOwnerId :: !(Maybe Text),
    _lgviPeerAddress :: !(Maybe Text),
    _lgviTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalGatewayVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgviLocalGatewayVirtualInterfaceId' - The ID of the virtual interface.
--
-- * 'lgviLocalBGPASN' - The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the local gateway.
--
-- * 'lgviVLAN' - The ID of the VLAN.
--
-- * 'lgviLocalGatewayId' - The ID of the local gateway.
--
-- * 'lgviLocalAddress' - The local address.
--
-- * 'lgviPeerBGPASN' - The peer BGP ASN.
--
-- * 'lgviOwnerId' - The AWS account ID that owns the local gateway virtual interface.
--
-- * 'lgviPeerAddress' - The peer address.
--
-- * 'lgviTags' - The tags assigned to the virtual interface.
localGatewayVirtualInterface ::
  LocalGatewayVirtualInterface
localGatewayVirtualInterface =
  LocalGatewayVirtualInterface'
    { _lgviLocalGatewayVirtualInterfaceId =
        Nothing,
      _lgviLocalBGPASN = Nothing,
      _lgviVLAN = Nothing,
      _lgviLocalGatewayId = Nothing,
      _lgviLocalAddress = Nothing,
      _lgviPeerBGPASN = Nothing,
      _lgviOwnerId = Nothing,
      _lgviPeerAddress = Nothing,
      _lgviTags = Nothing
    }

-- | The ID of the virtual interface.
lgviLocalGatewayVirtualInterfaceId :: Lens' LocalGatewayVirtualInterface (Maybe Text)
lgviLocalGatewayVirtualInterfaceId = lens _lgviLocalGatewayVirtualInterfaceId (\s a -> s {_lgviLocalGatewayVirtualInterfaceId = a})

-- | The Border Gateway Protocol (BGP) Autonomous System Number (ASN) of the local gateway.
lgviLocalBGPASN :: Lens' LocalGatewayVirtualInterface (Maybe Int)
lgviLocalBGPASN = lens _lgviLocalBGPASN (\s a -> s {_lgviLocalBGPASN = a})

-- | The ID of the VLAN.
lgviVLAN :: Lens' LocalGatewayVirtualInterface (Maybe Int)
lgviVLAN = lens _lgviVLAN (\s a -> s {_lgviVLAN = a})

-- | The ID of the local gateway.
lgviLocalGatewayId :: Lens' LocalGatewayVirtualInterface (Maybe Text)
lgviLocalGatewayId = lens _lgviLocalGatewayId (\s a -> s {_lgviLocalGatewayId = a})

-- | The local address.
lgviLocalAddress :: Lens' LocalGatewayVirtualInterface (Maybe Text)
lgviLocalAddress = lens _lgviLocalAddress (\s a -> s {_lgviLocalAddress = a})

-- | The peer BGP ASN.
lgviPeerBGPASN :: Lens' LocalGatewayVirtualInterface (Maybe Int)
lgviPeerBGPASN = lens _lgviPeerBGPASN (\s a -> s {_lgviPeerBGPASN = a})

-- | The AWS account ID that owns the local gateway virtual interface.
lgviOwnerId :: Lens' LocalGatewayVirtualInterface (Maybe Text)
lgviOwnerId = lens _lgviOwnerId (\s a -> s {_lgviOwnerId = a})

-- | The peer address.
lgviPeerAddress :: Lens' LocalGatewayVirtualInterface (Maybe Text)
lgviPeerAddress = lens _lgviPeerAddress (\s a -> s {_lgviPeerAddress = a})

-- | The tags assigned to the virtual interface.
lgviTags :: Lens' LocalGatewayVirtualInterface [Tag]
lgviTags = lens _lgviTags (\s a -> s {_lgviTags = a}) . _Default . _Coerce

instance FromXML LocalGatewayVirtualInterface where
  parseXML x =
    LocalGatewayVirtualInterface'
      <$> (x .@? "localGatewayVirtualInterfaceId")
      <*> (x .@? "localBgpAsn")
      <*> (x .@? "vlan")
      <*> (x .@? "localGatewayId")
      <*> (x .@? "localAddress")
      <*> (x .@? "peerBgpAsn")
      <*> (x .@? "ownerId")
      <*> (x .@? "peerAddress")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable LocalGatewayVirtualInterface

instance NFData LocalGatewayVirtualInterface
