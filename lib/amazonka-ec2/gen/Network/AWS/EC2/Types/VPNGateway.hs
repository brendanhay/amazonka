{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VPCAttachment
import Network.AWS.EC2.Types.VPNState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a virtual private gateway.
--
--
--
-- /See:/ 'vpnGateway' smart constructor.
data VPNGateway = VPNGateway'
  { _vgState :: !(Maybe VPNState),
    _vgVPCAttachments :: !(Maybe [VPCAttachment]),
    _vgVPNGatewayId :: !(Maybe Text),
    _vgAmazonSideASN :: !(Maybe Integer),
    _vgAvailabilityZone :: !(Maybe Text),
    _vgType :: !(Maybe GatewayType),
    _vgTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPNGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vgState' - The current state of the virtual private gateway.
--
-- * 'vgVPCAttachments' - Any VPCs attached to the virtual private gateway.
--
-- * 'vgVPNGatewayId' - The ID of the virtual private gateway.
--
-- * 'vgAmazonSideASN' - The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
--
-- * 'vgAvailabilityZone' - The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
--
-- * 'vgType' - The type of VPN connection the virtual private gateway supports.
--
-- * 'vgTags' - Any tags assigned to the virtual private gateway.
vpnGateway ::
  VPNGateway
vpnGateway =
  VPNGateway'
    { _vgState = Nothing,
      _vgVPCAttachments = Nothing,
      _vgVPNGatewayId = Nothing,
      _vgAmazonSideASN = Nothing,
      _vgAvailabilityZone = Nothing,
      _vgType = Nothing,
      _vgTags = Nothing
    }

-- | The current state of the virtual private gateway.
vgState :: Lens' VPNGateway (Maybe VPNState)
vgState = lens _vgState (\s a -> s {_vgState = a})

-- | Any VPCs attached to the virtual private gateway.
vgVPCAttachments :: Lens' VPNGateway [VPCAttachment]
vgVPCAttachments = lens _vgVPCAttachments (\s a -> s {_vgVPCAttachments = a}) . _Default . _Coerce

-- | The ID of the virtual private gateway.
vgVPNGatewayId :: Lens' VPNGateway (Maybe Text)
vgVPNGatewayId = lens _vgVPNGatewayId (\s a -> s {_vgVPNGatewayId = a})

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP session.
vgAmazonSideASN :: Lens' VPNGateway (Maybe Integer)
vgAmazonSideASN = lens _vgAmazonSideASN (\s a -> s {_vgAmazonSideASN = a})

-- | The Availability Zone where the virtual private gateway was created, if applicable. This field may be empty or not returned.
vgAvailabilityZone :: Lens' VPNGateway (Maybe Text)
vgAvailabilityZone = lens _vgAvailabilityZone (\s a -> s {_vgAvailabilityZone = a})

-- | The type of VPN connection the virtual private gateway supports.
vgType :: Lens' VPNGateway (Maybe GatewayType)
vgType = lens _vgType (\s a -> s {_vgType = a})

-- | Any tags assigned to the virtual private gateway.
vgTags :: Lens' VPNGateway [Tag]
vgTags = lens _vgTags (\s a -> s {_vgTags = a}) . _Default . _Coerce

instance FromXML VPNGateway where
  parseXML x =
    VPNGateway'
      <$> (x .@? "state")
      <*> (x .@? "attachments" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vpnGatewayId")
      <*> (x .@? "amazonSideAsn")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "type")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable VPNGateway

instance NFData VPNGateway
