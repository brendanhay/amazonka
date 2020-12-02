{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCPeeringConnectionVPCInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CidrBlock
import Network.AWS.EC2.Types.IPv6CidrBlock
import Network.AWS.EC2.Types.VPCPeeringConnectionOptionsDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a VPC in a VPC peering connection.
--
--
--
-- /See:/ 'vpcPeeringConnectionVPCInfo' smart constructor.
data VPCPeeringConnectionVPCInfo = VPCPeeringConnectionVPCInfo'
  { _vpcviCidrBlockSet ::
      !(Maybe [CidrBlock]),
    _vpcviVPCId :: !(Maybe Text),
    _vpcviOwnerId :: !(Maybe Text),
    _vpcviPeeringOptions ::
      !( Maybe
           VPCPeeringConnectionOptionsDescription
       ),
    _vpcviCidrBlock :: !(Maybe Text),
    _vpcviRegion :: !(Maybe Text),
    _vpcviIPv6CidrBlockSet ::
      !(Maybe [IPv6CidrBlock])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCPeeringConnectionVPCInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcviCidrBlockSet' - Information about the IPv4 CIDR blocks for the VPC.
--
-- * 'vpcviVPCId' - The ID of the VPC.
--
-- * 'vpcviOwnerId' - The AWS account ID of the VPC owner.
--
-- * 'vpcviPeeringOptions' - Information about the VPC peering connection options for the accepter or requester VPC.
--
-- * 'vpcviCidrBlock' - The IPv4 CIDR block for the VPC.
--
-- * 'vpcviRegion' - The Region in which the VPC is located.
--
-- * 'vpcviIPv6CidrBlockSet' - The IPv6 CIDR block for the VPC.
vpcPeeringConnectionVPCInfo ::
  VPCPeeringConnectionVPCInfo
vpcPeeringConnectionVPCInfo =
  VPCPeeringConnectionVPCInfo'
    { _vpcviCidrBlockSet = Nothing,
      _vpcviVPCId = Nothing,
      _vpcviOwnerId = Nothing,
      _vpcviPeeringOptions = Nothing,
      _vpcviCidrBlock = Nothing,
      _vpcviRegion = Nothing,
      _vpcviIPv6CidrBlockSet = Nothing
    }

-- | Information about the IPv4 CIDR blocks for the VPC.
vpcviCidrBlockSet :: Lens' VPCPeeringConnectionVPCInfo [CidrBlock]
vpcviCidrBlockSet = lens _vpcviCidrBlockSet (\s a -> s {_vpcviCidrBlockSet = a}) . _Default . _Coerce

-- | The ID of the VPC.
vpcviVPCId :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviVPCId = lens _vpcviVPCId (\s a -> s {_vpcviVPCId = a})

-- | The AWS account ID of the VPC owner.
vpcviOwnerId :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviOwnerId = lens _vpcviOwnerId (\s a -> s {_vpcviOwnerId = a})

-- | Information about the VPC peering connection options for the accepter or requester VPC.
vpcviPeeringOptions :: Lens' VPCPeeringConnectionVPCInfo (Maybe VPCPeeringConnectionOptionsDescription)
vpcviPeeringOptions = lens _vpcviPeeringOptions (\s a -> s {_vpcviPeeringOptions = a})

-- | The IPv4 CIDR block for the VPC.
vpcviCidrBlock :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviCidrBlock = lens _vpcviCidrBlock (\s a -> s {_vpcviCidrBlock = a})

-- | The Region in which the VPC is located.
vpcviRegion :: Lens' VPCPeeringConnectionVPCInfo (Maybe Text)
vpcviRegion = lens _vpcviRegion (\s a -> s {_vpcviRegion = a})

-- | The IPv6 CIDR block for the VPC.
vpcviIPv6CidrBlockSet :: Lens' VPCPeeringConnectionVPCInfo [IPv6CidrBlock]
vpcviIPv6CidrBlockSet = lens _vpcviIPv6CidrBlockSet (\s a -> s {_vpcviIPv6CidrBlockSet = a}) . _Default . _Coerce

instance FromXML VPCPeeringConnectionVPCInfo where
  parseXML x =
    VPCPeeringConnectionVPCInfo'
      <$> (x .@? "cidrBlockSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "vpcId")
      <*> (x .@? "ownerId")
      <*> (x .@? "peeringOptions")
      <*> (x .@? "cidrBlock")
      <*> (x .@? "region")
      <*> (x .@? "ipv6CidrBlockSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable VPCPeeringConnectionVPCInfo

instance NFData VPCPeeringConnectionVPCInfo
