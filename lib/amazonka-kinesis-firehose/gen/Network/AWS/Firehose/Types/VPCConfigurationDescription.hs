{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.VPCConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.VPCConfigurationDescription
  ( VPCConfigurationDescription (..),

    -- * Smart constructor
    mkVPCConfigurationDescription,

    -- * Lenses
    vcdSecurityGroupIds,
    vcdSubnetIds,
    vcdVPCId,
    vcdRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the VPC of the Amazon ES destination.
--
-- /See:/ 'mkVPCConfigurationDescription' smart constructor.
data VPCConfigurationDescription = VPCConfigurationDescription'
  { -- | The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
    securityGroupIds :: Lude.NonEmpty Lude.Text,
    -- | The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs.
    --
    -- The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
    subnetIds :: Lude.NonEmpty Lude.Text,
    -- | The ID of the Amazon ES destination's VPC.
    vpcId :: Lude.Text,
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
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCConfigurationDescription' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
-- * 'subnetIds' - The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs.
--
-- The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
-- * 'vpcId' - The ID of the Amazon ES destination's VPC.
-- * 'roleARN' - The ARN of the IAM role that the delivery stream uses to create endpoints in the destination VPC. You can use your existing Kinesis Data Firehose delivery role or you can specify a new role. In either case, make sure that the role trusts the Kinesis Data Firehose service principal and that it grants the following permissions:
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
mkVPCConfigurationDescription ::
  -- | 'securityGroupIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'subnetIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  VPCConfigurationDescription
mkVPCConfigurationDescription
  pSecurityGroupIds_
  pSubnetIds_
  pVPCId_
  pRoleARN_ =
    VPCConfigurationDescription'
      { securityGroupIds =
          pSecurityGroupIds_,
        subnetIds = pSubnetIds_,
        vpcId = pVPCId_,
        roleARN = pRoleARN_
      }

-- | The IDs of the security groups that Kinesis Data Firehose uses when it creates ENIs in the VPC of the Amazon ES destination. You can use the same security group that the Amazon ES domain uses or different ones. If you specify different security groups, ensure that they allow outbound HTTPS traffic to the Amazon ES domain's security group. Also ensure that the Amazon ES domain's security group allows HTTPS traffic from the security groups specified here. If you use the same security group for both your delivery stream and the Amazon ES domain, make sure the security group inbound rule allows HTTPS traffic. For more information about security group rules, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules> in the Amazon VPC documentation.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdSecurityGroupIds :: Lens.Lens' VPCConfigurationDescription (Lude.NonEmpty Lude.Text)
vcdSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfigurationDescription -> Lude.NonEmpty Lude.Text) (\s a -> s {securityGroupIds = a} :: VPCConfigurationDescription)
{-# DEPRECATED vcdSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in the VPC of the Amazon ES destination. Make sure that the routing tables and inbound and outbound rules allow traffic to flow from the subnets whose IDs are specified here to the subnets that have the destination Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in each of the subnets that are specified here. Do not delete or modify these ENIs.
--
-- The number of ENIs that Kinesis Data Firehose creates in the subnets specified here scales up and down automatically based on throughput. To enable Kinesis Data Firehose to scale up the number of ENIs to match throughput, ensure that you have sufficient quota. To help you calculate the quota you need, assume that Kinesis Data Firehose can create up to three ENIs for this delivery stream for each of the subnets specified here. For more information about ENI quota, see <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces > in the Amazon VPC Quotas topic.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdSubnetIds :: Lens.Lens' VPCConfigurationDescription (Lude.NonEmpty Lude.Text)
vcdSubnetIds = Lens.lens (subnetIds :: VPCConfigurationDescription -> Lude.NonEmpty Lude.Text) (\s a -> s {subnetIds = a} :: VPCConfigurationDescription)
{-# DEPRECATED vcdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the Amazon ES destination's VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcdVPCId :: Lens.Lens' VPCConfigurationDescription Lude.Text
vcdVPCId = Lens.lens (vpcId :: VPCConfigurationDescription -> Lude.Text) (\s a -> s {vpcId = a} :: VPCConfigurationDescription)
{-# DEPRECATED vcdVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

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
vcdRoleARN :: Lens.Lens' VPCConfigurationDescription Lude.Text
vcdRoleARN = Lens.lens (roleARN :: VPCConfigurationDescription -> Lude.Text) (\s a -> s {roleARN = a} :: VPCConfigurationDescription)
{-# DEPRECATED vcdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON VPCConfigurationDescription where
  parseJSON =
    Lude.withObject
      "VPCConfigurationDescription"
      ( \x ->
          VPCConfigurationDescription'
            Lude.<$> (x Lude..: "SecurityGroupIds")
            Lude.<*> (x Lude..: "SubnetIds")
            Lude.<*> (x Lude..: "VpcId")
            Lude.<*> (x Lude..: "RoleARN")
      )
