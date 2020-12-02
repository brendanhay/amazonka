{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet in a specified VPC.
--
--
-- You must specify an IPv4 CIDR block for the subnet. After you create a subnet, you can't change its CIDR block. The allowed block size is between a /16 netmask (65,536 IP addresses) and /28 netmask (16 IP addresses). The CIDR block must not overlap with the CIDR block of an existing subnet in the VPC.
--
-- If you've associated an IPv6 CIDR block with your VPC, you can create a subnet with an IPv6 CIDR block that uses a /64 prefix length.
--
-- /Important:/ AWS reserves both the first four and the last IPv4 address in each subnet's CIDR block. They're not available for use.
--
-- If you add more than one subnet to a VPC, they're set up in a star topology with a logical router in the middle.
--
-- When you stop an instance in a subnet, it retains its private IPv4 address. It's therefore possible to have a subnet with no running instances (they're all stopped), but no remaining IP addresses available.
--
-- For more information about subnets, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateSubnet
  ( -- * Creating a Request
    createSubnet,
    CreateSubnet,

    -- * Request Lenses
    cssIPv6CidrBlock,
    cssAvailabilityZoneId,
    cssOutpostARN,
    cssTagSpecifications,
    cssAvailabilityZone,
    cssDryRun,
    cssCidrBlock,
    cssVPCId,

    -- * Destructuring the Response
    createSubnetResponse,
    CreateSubnetResponse,

    -- * Response Lenses
    crersSubnet,
    crersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSubnet' smart constructor.
data CreateSubnet = CreateSubnet'
  { _cssIPv6CidrBlock ::
      !(Maybe Text),
    _cssAvailabilityZoneId :: !(Maybe Text),
    _cssOutpostARN :: !(Maybe Text),
    _cssTagSpecifications :: !(Maybe [TagSpecification]),
    _cssAvailabilityZone :: !(Maybe Text),
    _cssDryRun :: !(Maybe Bool),
    _cssCidrBlock :: !Text,
    _cssVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSubnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssIPv6CidrBlock' - The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
--
-- * 'cssAvailabilityZoneId' - The AZ ID or the Local Zone ID of the subnet.
--
-- * 'cssOutpostARN' - The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
--
-- * 'cssTagSpecifications' - The tags to assign to the subnet.
--
-- * 'cssAvailabilityZone' - The Availability Zone or Local Zone for the subnet. Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet. To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ . To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
--
-- * 'cssDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cssCidrBlock' - The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- * 'cssVPCId' - The ID of the VPC.
createSubnet ::
  -- | 'cssCidrBlock'
  Text ->
  -- | 'cssVPCId'
  Text ->
  CreateSubnet
createSubnet pCidrBlock_ pVPCId_ =
  CreateSubnet'
    { _cssIPv6CidrBlock = Nothing,
      _cssAvailabilityZoneId = Nothing,
      _cssOutpostARN = Nothing,
      _cssTagSpecifications = Nothing,
      _cssAvailabilityZone = Nothing,
      _cssDryRun = Nothing,
      _cssCidrBlock = pCidrBlock_,
      _cssVPCId = pVPCId_
    }

-- | The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
cssIPv6CidrBlock :: Lens' CreateSubnet (Maybe Text)
cssIPv6CidrBlock = lens _cssIPv6CidrBlock (\s a -> s {_cssIPv6CidrBlock = a})

-- | The AZ ID or the Local Zone ID of the subnet.
cssAvailabilityZoneId :: Lens' CreateSubnet (Maybe Text)
cssAvailabilityZoneId = lens _cssAvailabilityZoneId (\s a -> s {_cssAvailabilityZoneId = a})

-- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
cssOutpostARN :: Lens' CreateSubnet (Maybe Text)
cssOutpostARN = lens _cssOutpostARN (\s a -> s {_cssOutpostARN = a})

-- | The tags to assign to the subnet.
cssTagSpecifications :: Lens' CreateSubnet [TagSpecification]
cssTagSpecifications = lens _cssTagSpecifications (\s a -> s {_cssTagSpecifications = a}) . _Default . _Coerce

-- | The Availability Zone or Local Zone for the subnet. Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet. To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ . To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
cssAvailabilityZone :: Lens' CreateSubnet (Maybe Text)
cssAvailabilityZone = lens _cssAvailabilityZone (\s a -> s {_cssAvailabilityZone = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cssDryRun :: Lens' CreateSubnet (Maybe Bool)
cssDryRun = lens _cssDryRun (\s a -> s {_cssDryRun = a})

-- | The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
cssCidrBlock :: Lens' CreateSubnet Text
cssCidrBlock = lens _cssCidrBlock (\s a -> s {_cssCidrBlock = a})

-- | The ID of the VPC.
cssVPCId :: Lens' CreateSubnet Text
cssVPCId = lens _cssVPCId (\s a -> s {_cssVPCId = a})

instance AWSRequest CreateSubnet where
  type Rs CreateSubnet = CreateSubnetResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateSubnetResponse' <$> (x .@? "subnet") <*> (pure (fromEnum s))
      )

instance Hashable CreateSubnet

instance NFData CreateSubnet

instance ToHeaders CreateSubnet where
  toHeaders = const mempty

instance ToPath CreateSubnet where
  toPath = const "/"

instance ToQuery CreateSubnet where
  toQuery CreateSubnet' {..} =
    mconcat
      [ "Action" =: ("CreateSubnet" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "Ipv6CidrBlock" =: _cssIPv6CidrBlock,
        "AvailabilityZoneId" =: _cssAvailabilityZoneId,
        "OutpostArn" =: _cssOutpostARN,
        toQuery (toQueryList "TagSpecification" <$> _cssTagSpecifications),
        "AvailabilityZone" =: _cssAvailabilityZone,
        "DryRun" =: _cssDryRun,
        "CidrBlock" =: _cssCidrBlock,
        "VpcId" =: _cssVPCId
      ]

-- | /See:/ 'createSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'
  { _crersSubnet ::
      !(Maybe Subnet),
    _crersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSubnetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersSubnet' - Information about the subnet.
--
-- * 'crersResponseStatus' - -- | The response status code.
createSubnetResponse ::
  -- | 'crersResponseStatus'
  Int ->
  CreateSubnetResponse
createSubnetResponse pResponseStatus_ =
  CreateSubnetResponse'
    { _crersSubnet = Nothing,
      _crersResponseStatus = pResponseStatus_
    }

-- | Information about the subnet.
crersSubnet :: Lens' CreateSubnetResponse (Maybe Subnet)
crersSubnet = lens _crersSubnet (\s a -> s {_crersSubnet = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateSubnetResponse Int
crersResponseStatus = lens _crersResponseStatus (\s a -> s {_crersResponseStatus = a})

instance NFData CreateSubnetResponse
