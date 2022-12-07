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
-- Module      : Amazonka.Firehose.Types.VpcConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.VpcConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the VPC of the Amazon ES destination.
--
-- /See:/ 'newVpcConfigurationDescription' smart constructor.
data VpcConfigurationDescription = VpcConfigurationDescription'
  { -- | The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in
    -- the VPC of the Amazon ES destination. Make sure that the routing tables
    -- and inbound and outbound rules allow traffic to flow from the subnets
    -- whose IDs are specified here to the subnets that have the destination
    -- Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in
    -- each of the subnets that are specified here. Do not delete or modify
    -- these ENIs.
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
    -- | The ARN of the IAM role that the delivery stream uses to create
    -- endpoints in the destination VPC. You can use your existing Kinesis Data
    -- Firehose delivery role or you can specify a new role. In either case,
    -- make sure that the role trusts the Kinesis Data Firehose service
    -- principal and that it grants the following permissions:
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
    -- | The IDs of the security groups that Kinesis Data Firehose uses when it
    -- creates ENIs in the VPC of the Amazon ES destination. You can use the
    -- same security group that the Amazon ES domain uses or different ones. If
    -- you specify different security groups, ensure that they allow outbound
    -- HTTPS traffic to the Amazon ES domain\'s security group. Also ensure
    -- that the Amazon ES domain\'s security group allows HTTPS traffic from
    -- the security groups specified here. If you use the same security group
    -- for both your delivery stream and the Amazon ES domain, make sure the
    -- security group inbound rule allows HTTPS traffic. For more information
    -- about security group rules, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules>
    -- in the Amazon VPC documentation.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the Amazon ES destination\'s VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIds', 'vpcConfigurationDescription_subnetIds' - The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in
-- the VPC of the Amazon ES destination. Make sure that the routing tables
-- and inbound and outbound rules allow traffic to flow from the subnets
-- whose IDs are specified here to the subnets that have the destination
-- Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in
-- each of the subnets that are specified here. Do not delete or modify
-- these ENIs.
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
-- 'roleARN', 'vpcConfigurationDescription_roleARN' - The ARN of the IAM role that the delivery stream uses to create
-- endpoints in the destination VPC. You can use your existing Kinesis Data
-- Firehose delivery role or you can specify a new role. In either case,
-- make sure that the role trusts the Kinesis Data Firehose service
-- principal and that it grants the following permissions:
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
-- 'securityGroupIds', 'vpcConfigurationDescription_securityGroupIds' - The IDs of the security groups that Kinesis Data Firehose uses when it
-- creates ENIs in the VPC of the Amazon ES destination. You can use the
-- same security group that the Amazon ES domain uses or different ones. If
-- you specify different security groups, ensure that they allow outbound
-- HTTPS traffic to the Amazon ES domain\'s security group. Also ensure
-- that the Amazon ES domain\'s security group allows HTTPS traffic from
-- the security groups specified here. If you use the same security group
-- for both your delivery stream and the Amazon ES domain, make sure the
-- security group inbound rule allows HTTPS traffic. For more information
-- about security group rules, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules>
-- in the Amazon VPC documentation.
--
-- 'vpcId', 'vpcConfigurationDescription_vpcId' - The ID of the Amazon ES destination\'s VPC.
newVpcConfigurationDescription ::
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  VpcConfigurationDescription
newVpcConfigurationDescription
  pSubnetIds_
  pRoleARN_
  pSecurityGroupIds_
  pVpcId_ =
    VpcConfigurationDescription'
      { subnetIds =
          Lens.coerced Lens.# pSubnetIds_,
        roleARN = pRoleARN_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_,
        vpcId = pVpcId_
      }

-- | The IDs of the subnets that Kinesis Data Firehose uses to create ENIs in
-- the VPC of the Amazon ES destination. Make sure that the routing tables
-- and inbound and outbound rules allow traffic to flow from the subnets
-- whose IDs are specified here to the subnets that have the destination
-- Amazon ES endpoints. Kinesis Data Firehose creates at least one ENI in
-- each of the subnets that are specified here. Do not delete or modify
-- these ENIs.
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
vpcConfigurationDescription_subnetIds :: Lens.Lens' VpcConfigurationDescription (Prelude.NonEmpty Prelude.Text)
vpcConfigurationDescription_subnetIds = Lens.lens (\VpcConfigurationDescription' {subnetIds} -> subnetIds) (\s@VpcConfigurationDescription' {} a -> s {subnetIds = a} :: VpcConfigurationDescription) Prelude.. Lens.coerced

-- | The ARN of the IAM role that the delivery stream uses to create
-- endpoints in the destination VPC. You can use your existing Kinesis Data
-- Firehose delivery role or you can specify a new role. In either case,
-- make sure that the role trusts the Kinesis Data Firehose service
-- principal and that it grants the following permissions:
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
vpcConfigurationDescription_roleARN :: Lens.Lens' VpcConfigurationDescription Prelude.Text
vpcConfigurationDescription_roleARN = Lens.lens (\VpcConfigurationDescription' {roleARN} -> roleARN) (\s@VpcConfigurationDescription' {} a -> s {roleARN = a} :: VpcConfigurationDescription)

-- | The IDs of the security groups that Kinesis Data Firehose uses when it
-- creates ENIs in the VPC of the Amazon ES destination. You can use the
-- same security group that the Amazon ES domain uses or different ones. If
-- you specify different security groups, ensure that they allow outbound
-- HTTPS traffic to the Amazon ES domain\'s security group. Also ensure
-- that the Amazon ES domain\'s security group allows HTTPS traffic from
-- the security groups specified here. If you use the same security group
-- for both your delivery stream and the Amazon ES domain, make sure the
-- security group inbound rule allows HTTPS traffic. For more information
-- about security group rules, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#SecurityGroupRules Security group rules>
-- in the Amazon VPC documentation.
vpcConfigurationDescription_securityGroupIds :: Lens.Lens' VpcConfigurationDescription (Prelude.NonEmpty Prelude.Text)
vpcConfigurationDescription_securityGroupIds = Lens.lens (\VpcConfigurationDescription' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigurationDescription' {} a -> s {securityGroupIds = a} :: VpcConfigurationDescription) Prelude.. Lens.coerced

-- | The ID of the Amazon ES destination\'s VPC.
vpcConfigurationDescription_vpcId :: Lens.Lens' VpcConfigurationDescription Prelude.Text
vpcConfigurationDescription_vpcId = Lens.lens (\VpcConfigurationDescription' {vpcId} -> vpcId) (\s@VpcConfigurationDescription' {} a -> s {vpcId = a} :: VpcConfigurationDescription)

instance Data.FromJSON VpcConfigurationDescription where
  parseJSON =
    Data.withObject
      "VpcConfigurationDescription"
      ( \x ->
          VpcConfigurationDescription'
            Prelude.<$> (x Data..: "SubnetIds")
            Prelude.<*> (x Data..: "RoleARN")
            Prelude.<*> (x Data..: "SecurityGroupIds")
            Prelude.<*> (x Data..: "VpcId")
      )

instance Prelude.Hashable VpcConfigurationDescription where
  hashWithSalt _salt VpcConfigurationDescription' {..} =
    _salt `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcConfigurationDescription where
  rnf VpcConfigurationDescription' {..} =
    Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf vpcId
