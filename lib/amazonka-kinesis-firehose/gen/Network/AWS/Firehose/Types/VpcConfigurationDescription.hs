{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.VpcConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.VpcConfigurationDescription
  ( VpcConfigurationDescription (..)
  -- * Smart constructor
  , mkVpcConfigurationDescription
  -- * Lenses
  , vcdSubnetIds
  , vcdRoleARN
  , vcdSecurityGroupIds
  , vcdVpcId
  ) where

import qualified Network.AWS.Firehose.Types.NonEmptyStringWithoutWhitespace as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the VPC of the Amazon ES destination.
--
-- /See:/ 'mkVpcConfigurationDescription' smart constructor.
data VpcConfigurationDescription = VpcConfigurationDescription'
  { subnetIds :: Core.NonEmpty Types.NonEmptyStringWithoutWhitespace
    -- ^ The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs.
--
-- The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
  , roleARN :: Types.RoleARN
    -- ^ The ARN of the IAM role that the delivery stream uses to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:
--
--
--     * @ec2:DescribeVpcs@ 
--
--
--     * @ec2:DescribeVpcAttribute@ 
--
--
--     * @ec2:DescribeSubnets@ 
--
--
--     * @ec2:DescribeSecurityGroups@ 
--
--
--     * @ec2:DescribeNetworkInterfaces@ 
--
--
--     * @ec2:CreateNetworkInterface@ 
--
--
--     * @ec2:CreateNetworkInterfacePermission@ 
--
--
--     * @ec2:DeleteNetworkInterface@ 
--
--
-- If you revoke these permissions after you create the delivery stream, Kinesis Data Firehose can't scale out by creating more ENIs when necessary. You might therefore see a degradation in performance.
  , securityGroupIds :: Core.NonEmpty Types.NonEmptyStringWithoutWhitespace
    -- ^ The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
  , vpcId :: Types.VpcId
    -- ^ The ID of the Amazon ES destination's VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcConfigurationDescription' value with any optional fields omitted.
mkVpcConfigurationDescription
    :: Core.NonEmpty Types.NonEmptyStringWithoutWhitespace -- ^ 'subnetIds'
    -> Types.RoleARN -- ^ 'roleARN'
    -> Core.NonEmpty Types.NonEmptyStringWithoutWhitespace -- ^ 'securityGroupIds'
    -> Types.VpcId -- ^ 'vpcId'
    -> VpcConfigurationDescription
mkVpcConfigurationDescription subnetIds roleARN securityGroupIds
  vpcId
  = VpcConfigurationDescription'{subnetIds, roleARN,
                                 securityGroupIds, vpcId}

-- | The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs.
--
-- The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdSubnetIds :: Lens.Lens' VpcConfigurationDescription (Core.NonEmpty Types.NonEmptyStringWithoutWhitespace)
vcdSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE vcdSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The ARN of the IAM role that the delivery stream uses to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:
--
--
--     * @ec2:DescribeVpcs@ 
--
--
--     * @ec2:DescribeVpcAttribute@ 
--
--
--     * @ec2:DescribeSubnets@ 
--
--
--     * @ec2:DescribeSecurityGroups@ 
--
--
--     * @ec2:DescribeNetworkInterfaces@ 
--
--
--     * @ec2:CreateNetworkInterface@ 
--
--
--     * @ec2:CreateNetworkInterfacePermission@ 
--
--
--     * @ec2:DeleteNetworkInterface@ 
--
--
-- If you revoke these permissions after you create the delivery stream, Kinesis Data Firehose can't scale out by creating more ENIs when necessary. You might therefore see a degradation in performance.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdRoleARN :: Lens.Lens' VpcConfigurationDescription Types.RoleARN
vcdRoleARN = Lens.field @"roleARN"
{-# INLINEABLE vcdRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdSecurityGroupIds :: Lens.Lens' VpcConfigurationDescription (Core.NonEmpty Types.NonEmptyStringWithoutWhitespace)
vcdSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcdSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The ID of the Amazon ES destination's VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdVpcId :: Lens.Lens' VpcConfigurationDescription Types.VpcId
vcdVpcId = Lens.field @"vpcId"
{-# INLINEABLE vcdVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON VpcConfigurationDescription where
        parseJSON
          = Core.withObject "VpcConfigurationDescription" Core.$
              \ x ->
                VpcConfigurationDescription' Core.<$>
                  (x Core..: "SubnetIds") Core.<*> x Core..: "RoleARN" Core.<*>
                    x Core..: "SecurityGroupIds"
                    Core.<*> x Core..: "VpcId"
