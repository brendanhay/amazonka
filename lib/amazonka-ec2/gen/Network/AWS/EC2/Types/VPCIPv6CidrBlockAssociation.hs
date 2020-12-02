{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCIPv6CidrBlockAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VPCCidrBlockState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 CIDR block associated with a VPC.
--
--
--
-- /See:/ 'vpcIPv6CidrBlockAssociation' smart constructor.
data VPCIPv6CidrBlockAssociation = VPCIPv6CidrBlockAssociation'
  { _vicbaAssociationId ::
      !(Maybe Text),
    _vicbaIPv6CidrBlock ::
      !(Maybe Text),
    _vicbaNetworkBorderGroup ::
      !(Maybe Text),
    _vicbaIPv6CidrBlockState ::
      !(Maybe VPCCidrBlockState),
    _vicbaIPv6Pool :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCIPv6CidrBlockAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vicbaAssociationId' - The association ID for the IPv6 CIDR block.
--
-- * 'vicbaIPv6CidrBlock' - The IPv6 CIDR block.
--
-- * 'vicbaNetworkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses, for example, @us-east-1-wl1-bos-wlz-1@ .
--
-- * 'vicbaIPv6CidrBlockState' - Information about the state of the CIDR block.
--
-- * 'vicbaIPv6Pool' - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
vpcIPv6CidrBlockAssociation ::
  VPCIPv6CidrBlockAssociation
vpcIPv6CidrBlockAssociation =
  VPCIPv6CidrBlockAssociation'
    { _vicbaAssociationId = Nothing,
      _vicbaIPv6CidrBlock = Nothing,
      _vicbaNetworkBorderGroup = Nothing,
      _vicbaIPv6CidrBlockState = Nothing,
      _vicbaIPv6Pool = Nothing
    }

-- | The association ID for the IPv6 CIDR block.
vicbaAssociationId :: Lens' VPCIPv6CidrBlockAssociation (Maybe Text)
vicbaAssociationId = lens _vicbaAssociationId (\s a -> s {_vicbaAssociationId = a})

-- | The IPv6 CIDR block.
vicbaIPv6CidrBlock :: Lens' VPCIPv6CidrBlockAssociation (Maybe Text)
vicbaIPv6CidrBlock = lens _vicbaIPv6CidrBlock (\s a -> s {_vicbaIPv6CidrBlock = a})

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses, for example, @us-east-1-wl1-bos-wlz-1@ .
vicbaNetworkBorderGroup :: Lens' VPCIPv6CidrBlockAssociation (Maybe Text)
vicbaNetworkBorderGroup = lens _vicbaNetworkBorderGroup (\s a -> s {_vicbaNetworkBorderGroup = a})

-- | Information about the state of the CIDR block.
vicbaIPv6CidrBlockState :: Lens' VPCIPv6CidrBlockAssociation (Maybe VPCCidrBlockState)
vicbaIPv6CidrBlockState = lens _vicbaIPv6CidrBlockState (\s a -> s {_vicbaIPv6CidrBlockState = a})

-- | The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
vicbaIPv6Pool :: Lens' VPCIPv6CidrBlockAssociation (Maybe Text)
vicbaIPv6Pool = lens _vicbaIPv6Pool (\s a -> s {_vicbaIPv6Pool = a})

instance FromXML VPCIPv6CidrBlockAssociation where
  parseXML x =
    VPCIPv6CidrBlockAssociation'
      <$> (x .@? "associationId")
      <*> (x .@? "ipv6CidrBlock")
      <*> (x .@? "networkBorderGroup")
      <*> (x .@? "ipv6CidrBlockState")
      <*> (x .@? "ipv6Pool")

instance Hashable VPCIPv6CidrBlockAssociation

instance NFData VPCIPv6CidrBlockAssociation
