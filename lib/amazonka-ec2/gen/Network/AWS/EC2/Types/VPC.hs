{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPC where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.EC2.Types.VPCCidrBlockAssociation
import Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation
import Network.AWS.EC2.Types.VPCState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a VPC.
--
--
--
-- /See:/ 'vpc' smart constructor.
data VPC = VPC'
  { _vpcIPv6CidrBlockAssociationSet ::
      !(Maybe [VPCIPv6CidrBlockAssociation]),
    _vpcCidrBlockAssociationSet :: !(Maybe [VPCCidrBlockAssociation]),
    _vpcOwnerId :: !(Maybe Text),
    _vpcTags :: !(Maybe [Tag]),
    _vpcIsDefault :: !(Maybe Bool),
    _vpcCidrBlock :: !Text,
    _vpcDHCPOptionsId :: !Text,
    _vpcInstanceTenancy :: !Tenancy,
    _vpcState :: !VPCState,
    _vpcVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcIPv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the VPC.
--
-- * 'vpcCidrBlockAssociationSet' - Information about the IPv4 CIDR blocks associated with the VPC.
--
-- * 'vpcOwnerId' - The ID of the AWS account that owns the VPC.
--
-- * 'vpcTags' - Any tags assigned to the VPC.
--
-- * 'vpcIsDefault' - Indicates whether the VPC is the default VPC.
--
-- * 'vpcCidrBlock' - The primary IPv4 CIDR block for the VPC.
--
-- * 'vpcDHCPOptionsId' - The ID of the set of DHCP options you've associated with the VPC.
--
-- * 'vpcInstanceTenancy' - The allowed tenancy of instances launched into the VPC.
--
-- * 'vpcState' - The current state of the VPC.
--
-- * 'vpcVPCId' - The ID of the VPC.
vpc ::
  -- | 'vpcCidrBlock'
  Text ->
  -- | 'vpcDHCPOptionsId'
  Text ->
  -- | 'vpcInstanceTenancy'
  Tenancy ->
  -- | 'vpcState'
  VPCState ->
  -- | 'vpcVPCId'
  Text ->
  VPC
vpc pCidrBlock_ pDHCPOptionsId_ pInstanceTenancy_ pState_ pVPCId_ =
  VPC'
    { _vpcIPv6CidrBlockAssociationSet = Nothing,
      _vpcCidrBlockAssociationSet = Nothing,
      _vpcOwnerId = Nothing,
      _vpcTags = Nothing,
      _vpcIsDefault = Nothing,
      _vpcCidrBlock = pCidrBlock_,
      _vpcDHCPOptionsId = pDHCPOptionsId_,
      _vpcInstanceTenancy = pInstanceTenancy_,
      _vpcState = pState_,
      _vpcVPCId = pVPCId_
    }

-- | Information about the IPv6 CIDR blocks associated with the VPC.
vpcIPv6CidrBlockAssociationSet :: Lens' VPC [VPCIPv6CidrBlockAssociation]
vpcIPv6CidrBlockAssociationSet = lens _vpcIPv6CidrBlockAssociationSet (\s a -> s {_vpcIPv6CidrBlockAssociationSet = a}) . _Default . _Coerce

-- | Information about the IPv4 CIDR blocks associated with the VPC.
vpcCidrBlockAssociationSet :: Lens' VPC [VPCCidrBlockAssociation]
vpcCidrBlockAssociationSet = lens _vpcCidrBlockAssociationSet (\s a -> s {_vpcCidrBlockAssociationSet = a}) . _Default . _Coerce

-- | The ID of the AWS account that owns the VPC.
vpcOwnerId :: Lens' VPC (Maybe Text)
vpcOwnerId = lens _vpcOwnerId (\s a -> s {_vpcOwnerId = a})

-- | Any tags assigned to the VPC.
vpcTags :: Lens' VPC [Tag]
vpcTags = lens _vpcTags (\s a -> s {_vpcTags = a}) . _Default . _Coerce

-- | Indicates whether the VPC is the default VPC.
vpcIsDefault :: Lens' VPC (Maybe Bool)
vpcIsDefault = lens _vpcIsDefault (\s a -> s {_vpcIsDefault = a})

-- | The primary IPv4 CIDR block for the VPC.
vpcCidrBlock :: Lens' VPC Text
vpcCidrBlock = lens _vpcCidrBlock (\s a -> s {_vpcCidrBlock = a})

-- | The ID of the set of DHCP options you've associated with the VPC.
vpcDHCPOptionsId :: Lens' VPC Text
vpcDHCPOptionsId = lens _vpcDHCPOptionsId (\s a -> s {_vpcDHCPOptionsId = a})

-- | The allowed tenancy of instances launched into the VPC.
vpcInstanceTenancy :: Lens' VPC Tenancy
vpcInstanceTenancy = lens _vpcInstanceTenancy (\s a -> s {_vpcInstanceTenancy = a})

-- | The current state of the VPC.
vpcState :: Lens' VPC VPCState
vpcState = lens _vpcState (\s a -> s {_vpcState = a})

-- | The ID of the VPC.
vpcVPCId :: Lens' VPC Text
vpcVPCId = lens _vpcVPCId (\s a -> s {_vpcVPCId = a})

instance FromXML VPC where
  parseXML x =
    VPC'
      <$> ( x .@? "ipv6CidrBlockAssociationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "cidrBlockAssociationSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "ownerId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "isDefault")
      <*> (x .@ "cidrBlock")
      <*> (x .@ "dhcpOptionsId")
      <*> (x .@ "instanceTenancy")
      <*> (x .@ "state")
      <*> (x .@ "vpcId")

instance Hashable VPC

instance NFData VPC
