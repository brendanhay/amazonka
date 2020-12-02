{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.VPCConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.VPCConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the VPC of the Amazon ES destination.
--
--
--
-- /See:/ 'vpcConfiguration' smart constructor.
data VPCConfiguration = VPCConfiguration'
  { _vcSubnetIds ::
      !(List1 Text),
    _vcRoleARN :: !Text,
    _vcSecurityGroupIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSubnetIds' - The IDs of the subnets that you want Kinesis Data Firehose to use to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs. The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
--
-- * 'vcRoleARN' - The ARN of the IAM role that you want the delivery stream to use to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:     * @ec2:DescribeVpcs@      * @ec2:DescribeVpcAttribute@      * @ec2:DescribeSubnets@      * @ec2:DescribeSecurityGroups@      * @ec2:DescribeNetworkInterfaces@      * @ec2:CreateNetworkInterface@      * @ec2:CreateNetworkInterfacePermission@      * @ec2:DeleteNetworkInterface@  If you revoke these permissions after you create the delivery stream, Kinesis Data Firehose can't scale out by creating more ENIs when necessary. You might therefore see a degradation in performance.
--
-- * 'vcSecurityGroupIds' - The IDs of the security groups that you want Kinesis Data Firehose to use when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups here, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
vpcConfiguration ::
  -- | 'vcSubnetIds'
  NonEmpty Text ->
  -- | 'vcRoleARN'
  Text ->
  -- | 'vcSecurityGroupIds'
  NonEmpty Text ->
  VPCConfiguration
vpcConfiguration pSubnetIds_ pRoleARN_ pSecurityGroupIds_ =
  VPCConfiguration'
    { _vcSubnetIds = _List1 # pSubnetIds_,
      _vcRoleARN = pRoleARN_,
      _vcSecurityGroupIds = _List1 # pSecurityGroupIds_
    }

-- | The IDs of the subnets that you want Kinesis Data Firehose to use to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs. The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
vcSubnetIds :: Lens' VPCConfiguration (NonEmpty Text)
vcSubnetIds = lens _vcSubnetIds (\s a -> s {_vcSubnetIds = a}) . _List1

-- | The ARN of the IAM role that you want the delivery stream to use to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:     * @ec2:DescribeVpcs@      * @ec2:DescribeVpcAttribute@      * @ec2:DescribeSubnets@      * @ec2:DescribeSecurityGroups@      * @ec2:DescribeNetworkInterfaces@      * @ec2:CreateNetworkInterface@      * @ec2:CreateNetworkInterfacePermission@      * @ec2:DeleteNetworkInterface@  If you revoke these permissions after you create the delivery stream, Kinesis Data Firehose can't scale out by creating more ENIs when necessary. You might therefore see a degradation in performance.
vcRoleARN :: Lens' VPCConfiguration Text
vcRoleARN = lens _vcRoleARN (\s a -> s {_vcRoleARN = a})

-- | The IDs of the security groups that you want Kinesis Data Firehose to use when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups here, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
vcSecurityGroupIds :: Lens' VPCConfiguration (NonEmpty Text)
vcSecurityGroupIds = lens _vcSecurityGroupIds (\s a -> s {_vcSecurityGroupIds = a}) . _List1

instance Hashable VPCConfiguration

instance NFData VPCConfiguration

instance ToJSON VPCConfiguration where
  toJSON VPCConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("SubnetIds" .= _vcSubnetIds),
            Just ("RoleARN" .= _vcRoleARN),
            Just ("SecurityGroupIds" .= _vcSecurityGroupIds)
          ]
      )
