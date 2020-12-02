{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.VPCConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.VPCConfigurationDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the VPC of the Amazon ES destination.
--
--
--
-- /See:/ 'vpcConfigurationDescription' smart constructor.
data VPCConfigurationDescription = VPCConfigurationDescription'
  { _vcdSubnetIds ::
      !(List1 Text),
    _vcdRoleARN :: !Text,
    _vcdSecurityGroupIds ::
      !(List1 Text),
    _vcdVPCId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcdSubnetIds' - The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs. The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
--
-- * 'vcdRoleARN' - The ARN of the IAM role that the delivery stream uses to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:     * @ec2:DescribeVpcs@      * @ec2:DescribeVpcAttribute@      * @ec2:DescribeSubnets@      * @ec2:DescribeSecurityGroups@      * @ec2:DescribeNetworkInterfaces@      * @ec2:CreateNetworkInterface@      * @ec2:CreateNetworkInterfacePermission@      * @ec2:DeleteNetworkInterface@  If you revoke these permissions after you create the delivery stream, Kinesis Data Firehose can't scale out by creating more ENIs when necessary. You might therefore see a degradation in performance.
--
-- * 'vcdSecurityGroupIds' - The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
--
-- * 'vcdVPCId' - The ID of the Amazon ES destination's VPC.
vpcConfigurationDescription ::
  -- | 'vcdSubnetIds'
  NonEmpty Text ->
  -- | 'vcdRoleARN'
  Text ->
  -- | 'vcdSecurityGroupIds'
  NonEmpty Text ->
  -- | 'vcdVPCId'
  Text ->
  VPCConfigurationDescription
vpcConfigurationDescription
  pSubnetIds_
  pRoleARN_
  pSecurityGroupIds_
  pVPCId_ =
    VPCConfigurationDescription'
      { _vcdSubnetIds =
          _List1 # pSubnetIds_,
        _vcdRoleARN = pRoleARN_,
        _vcdSecurityGroupIds = _List1 # pSecurityGroupIds_,
        _vcdVPCId = pVPCId_
      }

-- | The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs. The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
vcdSubnetIds :: Lens' VPCConfigurationDescription (NonEmpty Text)
vcdSubnetIds = lens _vcdSubnetIds (\s a -> s {_vcdSubnetIds = a}) . _List1

-- | The ARN of the IAM role that the delivery stream uses to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:     * @ec2:DescribeVpcs@      * @ec2:DescribeVpcAttribute@      * @ec2:DescribeSubnets@      * @ec2:DescribeSecurityGroups@      * @ec2:DescribeNetworkInterfaces@      * @ec2:CreateNetworkInterface@      * @ec2:CreateNetworkInterfacePermission@      * @ec2:DeleteNetworkInterface@  If you revoke these permissions after you create the delivery stream, Kinesis Data Firehose can't scale out by creating more ENIs when necessary. You might therefore see a degradation in performance.
vcdRoleARN :: Lens' VPCConfigurationDescription Text
vcdRoleARN = lens _vcdRoleARN (\s a -> s {_vcdRoleARN = a})

-- | The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
vcdSecurityGroupIds :: Lens' VPCConfigurationDescription (NonEmpty Text)
vcdSecurityGroupIds = lens _vcdSecurityGroupIds (\s a -> s {_vcdSecurityGroupIds = a}) . _List1

-- | The ID of the Amazon ES destination's VPC.
vcdVPCId :: Lens' VPCConfigurationDescription Text
vcdVPCId = lens _vcdVPCId (\s a -> s {_vcdVPCId = a})

instance FromJSON VPCConfigurationDescription where
  parseJSON =
    withObject
      "VPCConfigurationDescription"
      ( \x ->
          VPCConfigurationDescription'
            <$> (x .: "SubnetIds")
            <*> (x .: "RoleARN")
            <*> (x .: "SecurityGroupIds")
            <*> (x .: "VpcId")
      )

instance Hashable VPCConfigurationDescription

instance NFData VPCConfigurationDescription
