{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Firehose.Types.VpcConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the VPC of the Amazon ES destination.
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | The IDs of the subnets that you want Kinesis Data Firehose to use to
    -- create ENIs in the VPC of the Amazon ES destination. Make sure that the
    -- routing tables and inbound and outbound rules allow traffic to flow from
    -- the subnets whose IDs are specified here to the subnets that have the
    -- destination Amazon ES endpoints. Kinesis Data Firehose creates at least
    -- one ENI in each of the subnets that are specified here. Do not delete or
    -- modify these ENIs.
    --
    -- The number of ENIs that Kinesis Data Firehose creates in the subnets
    -- specified here scales up and down automatically based on throughput. To
    -- enable Kinesis Data Firehose to scale up the number of ENIs to match
    -- throughput, ensure that you have sufficient quota. To help you calculate
    -- the quota you need, assume that Kinesis Data Firehose can create up to
    -- three ENIs for this delivery stream for each of the subnets specified
    -- here. For more information about ENI quota, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces>
    -- in the Amazon VPC Quotas topic.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ARN of the IAM role that you want the delivery stream to use to
    -- create endpoints in the destination VPC. You can use your existing
    -- Kinesis Data Firehose delivery role or you can specify a new role. In
    -- either case, make sure that the role trusts the Kinesis Data Firehose
    -- service principal and that it grants the following permissions:
    --
    -- -   @ec2:DescribeVpcs@
    --
    -- -   @ec2:DescribeVpcAttribute@
    --
    -- -   @ec2:DescribeSubnets@
    --
    -- -   @ec2:DescribeSecurityGroups@
    --
    -- -   @ec2:DescribeNetworkInterfaces@
    --
    -- -   @ec2:CreateNetworkInterface@
    --
    -- -   @ec2:CreateNetworkInterfacePermission@
    --
    -- -   @ec2:DeleteNetworkInterface@
    --
    -- If you revoke these permissions after you create the delivery stream,
    -- Kinesis Data Firehose can\'t scale out by creating more ENIs when
    -- necessary. You might therefore see a degradation in performance.
    roleARN :: Prelude.Text,
    -- | The IDs of the security groups that you want Kinesis Data Firehose to
    -- use when it creates ENIs in the VPC of the Amazon ES destination. You
    -- can use the same security group that the Amazon ES domain uses or
    -- different ones. If you specify different security groups here, ensure
    -- that they allow outbound HTTPS traffic to the Amazon ES domain\'s
    -- security group. Also ensure that the Amazon ES domain\'s security group
    -- allows HTTPS traffic from the security groups specified here. If you use
    -- the same security group for both your delivery stream and the Amazon ES
    -- domain, make sure the security group inbound rule allows HTTPS traffic.
    -- For more information about security group rules, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules>
    -- in the Amazon VPC documentation.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIds', 'vpcConfiguration_subnetIds' - The IDs of the subnets that you want Kinesis Data Firehose to use to
-- create ENIs in the VPC of the Amazon ES destination. Make sure that the
-- routing tables and inbound and outbound rules allow traffic to flow from
-- the subnets whose IDs are specified here to the subnets that have the
-- destination Amazon ES endpoints. Kinesis Data Firehose creates at least
-- one ENI in each of the subnets that are specified here. Do not delete or
-- modify these ENIs.
--
-- The number of ENIs that Kinesis Data Firehose creates in the subnets
-- specified here scales up and down automatically based on throughput. To
-- enable Kinesis Data Firehose to scale up the number of ENIs to match
-- throughput, ensure that you have sufficient quota. To help you calculate
-- the quota you need, assume that Kinesis Data Firehose can create up to
-- three ENIs for this delivery stream for each of the subnets specified
-- here. For more information about ENI quota, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces>
-- in the Amazon VPC Quotas topic.
--
-- 'roleARN', 'vpcConfiguration_roleARN' - The ARN of the IAM role that you want the delivery stream to use to
-- create endpoints in the destination VPC. You can use your existing
-- Kinesis Data Firehose delivery role or you can specify a new role. In
-- either case, make sure that the role trusts the Kinesis Data Firehose
-- service principal and that it grants the following permissions:
--
-- -   @ec2:DescribeVpcs@
--
-- -   @ec2:DescribeVpcAttribute@
--
-- -   @ec2:DescribeSubnets@
--
-- -   @ec2:DescribeSecurityGroups@
--
-- -   @ec2:DescribeNetworkInterfaces@
--
-- -   @ec2:CreateNetworkInterface@
--
-- -   @ec2:CreateNetworkInterfacePermission@
--
-- -   @ec2:DeleteNetworkInterface@
--
-- If you revoke these permissions after you create the delivery stream,
-- Kinesis Data Firehose can\'t scale out by creating more ENIs when
-- necessary. You might therefore see a degradation in performance.
--
-- 'securityGroupIds', 'vpcConfiguration_securityGroupIds' - The IDs of the security groups that you want Kinesis Data Firehose to
-- use when it creates ENIs in the VPC of the Amazon ES destination. You
-- can use the same security group that the Amazon ES domain uses or
-- different ones. If you specify different security groups here, ensure
-- that they allow outbound HTTPS traffic to the Amazon ES domain\'s
-- security group. Also ensure that the Amazon ES domain\'s security group
-- allows HTTPS traffic from the security groups specified here. If you use
-- the same security group for both your delivery stream and the Amazon ES
-- domain, make sure the security group inbound rule allows HTTPS traffic.
-- For more information about security group rules, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules>
-- in the Amazon VPC documentation.
newVpcConfiguration ::
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfiguration
newVpcConfiguration
  pSubnetIds_
  pRoleARN_
  pSecurityGroupIds_ =
    VpcConfiguration'
      { subnetIds =
          Lens.coerced Lens.# pSubnetIds_,
        roleARN = pRoleARN_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_
      }

-- | The IDs of the subnets that you want Kinesis Data Firehose to use to
-- create ENIs in the VPC of the Amazon ES destination. Make sure that the
-- routing tables and inbound and outbound rules allow traffic to flow from
-- the subnets whose IDs are specified here to the subnets that have the
-- destination Amazon ES endpoints. Kinesis Data Firehose creates at least
-- one ENI in each of the subnets that are specified here. Do not delete or
-- modify these ENIs.
--
-- The number of ENIs that Kinesis Data Firehose creates in the subnets
-- specified here scales up and down automatically based on throughput. To
-- enable Kinesis Data Firehose to scale up the number of ENIs to match
-- throughput, ensure that you have sufficient quota. To help you calculate
-- the quota you need, assume that Kinesis Data Firehose can create up to
-- three ENIs for this delivery stream for each of the subnets specified
-- here. For more information about ENI quota, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/amazon-vpc-limits.html#vpc-limits-enis Network Interfaces>
-- in the Amazon VPC Quotas topic.
vpcConfiguration_subnetIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_subnetIds = Lens.lens (\VpcConfiguration' {subnetIds} -> subnetIds) (\s@VpcConfiguration' {} a -> s {subnetIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

-- | The ARN of the IAM role that you want the delivery stream to use to
-- create endpoints in the destination VPC. You can use your existing
-- Kinesis Data Firehose delivery role or you can specify a new role. In
-- either case, make sure that the role trusts the Kinesis Data Firehose
-- service principal and that it grants the following permissions:
--
-- -   @ec2:DescribeVpcs@
--
-- -   @ec2:DescribeVpcAttribute@
--
-- -   @ec2:DescribeSubnets@
--
-- -   @ec2:DescribeSecurityGroups@
--
-- -   @ec2:DescribeNetworkInterfaces@
--
-- -   @ec2:CreateNetworkInterface@
--
-- -   @ec2:CreateNetworkInterfacePermission@
--
-- -   @ec2:DeleteNetworkInterface@
--
-- If you revoke these permissions after you create the delivery stream,
-- Kinesis Data Firehose can\'t scale out by creating more ENIs when
-- necessary. You might therefore see a degradation in performance.
vpcConfiguration_roleARN :: Lens.Lens' VpcConfiguration Prelude.Text
vpcConfiguration_roleARN = Lens.lens (\VpcConfiguration' {roleARN} -> roleARN) (\s@VpcConfiguration' {} a -> s {roleARN = a} :: VpcConfiguration)

-- | The IDs of the security groups that you want Kinesis Data Firehose to
-- use when it creates ENIs in the VPC of the Amazon ES destination. You
-- can use the same security group that the Amazon ES domain uses or
-- different ones. If you specify different security groups here, ensure
-- that they allow outbound HTTPS traffic to the Amazon ES domain\'s
-- security group. Also ensure that the Amazon ES domain\'s security group
-- allows HTTPS traffic from the security groups specified here. If you use
-- the same security group for both your delivery stream and the Amazon ES
-- domain, make sure the security group inbound rule allows HTTPS traffic.
-- For more information about security group rules, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules>
-- in the Amazon VPC documentation.
vpcConfiguration_securityGroupIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_securityGroupIds = Lens.lens (\VpcConfiguration' {securityGroupIds} -> securityGroupIds) (\s@VpcConfiguration' {} a -> s {securityGroupIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} =
    Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf securityGroupIds

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just ("RoleARN" Data..= roleARN),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds)
          ]
      )
