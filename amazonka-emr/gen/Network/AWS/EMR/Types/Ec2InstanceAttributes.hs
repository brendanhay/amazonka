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
-- Module      : Network.AWS.EMR.Types.Ec2InstanceAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Ec2InstanceAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
--
-- /See:/ 'newEc2InstanceAttributes' smart constructor.
data Ec2InstanceAttributes = Ec2InstanceAttributes'
  { -- | The name of the Amazon EC2 key pair to use when connecting with SSH into
    -- the master node as a user named \"hadoop\".
    ec2KeyName :: Core.Maybe Core.Text,
    -- | A list of additional Amazon EC2 security group IDs for the core and task
    -- nodes.
    additionalSlaveSecurityGroups :: Core.Maybe [Core.Text],
    -- | The Availability Zone in which the cluster will run.
    ec2AvailabilityZone :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon EC2 security group for the core and task
    -- nodes.
    emrManagedSlaveSecurityGroup :: Core.Maybe Core.Text,
    -- | Applies to clusters configured with the instance fleets option.
    -- Specifies one or more Availability Zones in which to launch EC2 cluster
    -- instances when the EC2-Classic network configuration is supported.
    -- Amazon EMR chooses the Availability Zone with the best fit from among
    -- the list of @RequestedEc2AvailabilityZones@, and then launches all
    -- cluster instances within that Availability Zone. If you do not specify
    -- this value, Amazon EMR chooses the Availability Zone for you.
    -- @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be
    -- specified together.
    requestedEc2AvailabilityZones :: Core.Maybe [Core.Text],
    -- | Set this parameter to the identifier of the Amazon VPC subnet where you
    -- want the cluster to launch. If you do not specify this value, and your
    -- account supports EC2-Classic, the cluster launches in EC2-Classic.
    ec2SubnetId :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon EC2 security group for the master node.
    emrManagedMasterSecurityGroup :: Core.Maybe Core.Text,
    -- | The IAM role that was specified when the cluster was launched. The EC2
    -- instances of the cluster assume this role.
    iamInstanceProfile :: Core.Maybe Core.Text,
    -- | A list of additional Amazon EC2 security group IDs for the master node.
    additionalMasterSecurityGroups :: Core.Maybe [Core.Text],
    -- | Applies to clusters configured with the instance fleets option.
    -- Specifies the unique identifier of one or more Amazon EC2 subnets in
    -- which to launch EC2 cluster instances. Subnets must exist within the
    -- same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among
    -- the list of @RequestedEc2SubnetIds@, and then launches all cluster
    -- instances within that Subnet. If this value is not specified, and the
    -- account and Region support EC2-Classic networks, the cluster launches
    -- instances in the EC2-Classic network and uses
    -- @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic
    -- is not supported, and no Subnet is specified, Amazon EMR chooses the
    -- subnet for you. @RequestedEc2SubnetIDs@ and
    -- @RequestedEc2AvailabilityZones@ cannot be specified together.
    requestedEc2SubnetIds :: Core.Maybe [Core.Text],
    -- | The identifier of the Amazon EC2 security group for the Amazon EMR
    -- service to access clusters in VPC private subnets.
    serviceAccessSecurityGroup :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Ec2InstanceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2KeyName', 'ec2InstanceAttributes_ec2KeyName' - The name of the Amazon EC2 key pair to use when connecting with SSH into
-- the master node as a user named \"hadoop\".
--
-- 'additionalSlaveSecurityGroups', 'ec2InstanceAttributes_additionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
--
-- 'ec2AvailabilityZone', 'ec2InstanceAttributes_ec2AvailabilityZone' - The Availability Zone in which the cluster will run.
--
-- 'emrManagedSlaveSecurityGroup', 'ec2InstanceAttributes_emrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task
-- nodes.
--
-- 'requestedEc2AvailabilityZones', 'ec2InstanceAttributes_requestedEc2AvailabilityZones' - Applies to clusters configured with the instance fleets option.
-- Specifies one or more Availability Zones in which to launch EC2 cluster
-- instances when the EC2-Classic network configuration is supported.
-- Amazon EMR chooses the Availability Zone with the best fit from among
-- the list of @RequestedEc2AvailabilityZones@, and then launches all
-- cluster instances within that Availability Zone. If you do not specify
-- this value, Amazon EMR chooses the Availability Zone for you.
-- @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be
-- specified together.
--
-- 'ec2SubnetId', 'ec2InstanceAttributes_ec2SubnetId' - Set this parameter to the identifier of the Amazon VPC subnet where you
-- want the cluster to launch. If you do not specify this value, and your
-- account supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- 'emrManagedMasterSecurityGroup', 'ec2InstanceAttributes_emrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
--
-- 'iamInstanceProfile', 'ec2InstanceAttributes_iamInstanceProfile' - The IAM role that was specified when the cluster was launched. The EC2
-- instances of the cluster assume this role.
--
-- 'additionalMasterSecurityGroups', 'ec2InstanceAttributes_additionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- 'requestedEc2SubnetIds', 'ec2InstanceAttributes_requestedEc2SubnetIds' - Applies to clusters configured with the instance fleets option.
-- Specifies the unique identifier of one or more Amazon EC2 subnets in
-- which to launch EC2 cluster instances. Subnets must exist within the
-- same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among
-- the list of @RequestedEc2SubnetIds@, and then launches all cluster
-- instances within that Subnet. If this value is not specified, and the
-- account and Region support EC2-Classic networks, the cluster launches
-- instances in the EC2-Classic network and uses
-- @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic
-- is not supported, and no Subnet is specified, Amazon EMR chooses the
-- subnet for you. @RequestedEc2SubnetIDs@ and
-- @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- 'serviceAccessSecurityGroup', 'ec2InstanceAttributes_serviceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
newEc2InstanceAttributes ::
  Ec2InstanceAttributes
newEc2InstanceAttributes =
  Ec2InstanceAttributes'
    { ec2KeyName = Core.Nothing,
      additionalSlaveSecurityGroups = Core.Nothing,
      ec2AvailabilityZone = Core.Nothing,
      emrManagedSlaveSecurityGroup = Core.Nothing,
      requestedEc2AvailabilityZones = Core.Nothing,
      ec2SubnetId = Core.Nothing,
      emrManagedMasterSecurityGroup = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      additionalMasterSecurityGroups = Core.Nothing,
      requestedEc2SubnetIds = Core.Nothing,
      serviceAccessSecurityGroup = Core.Nothing
    }

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into
-- the master node as a user named \"hadoop\".
ec2InstanceAttributes_ec2KeyName :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_ec2KeyName = Lens.lens (\Ec2InstanceAttributes' {ec2KeyName} -> ec2KeyName) (\s@Ec2InstanceAttributes' {} a -> s {ec2KeyName = a} :: Ec2InstanceAttributes)

-- | A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
ec2InstanceAttributes_additionalSlaveSecurityGroups :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Core.Text])
ec2InstanceAttributes_additionalSlaveSecurityGroups = Lens.lens (\Ec2InstanceAttributes' {additionalSlaveSecurityGroups} -> additionalSlaveSecurityGroups) (\s@Ec2InstanceAttributes' {} a -> s {additionalSlaveSecurityGroups = a} :: Ec2InstanceAttributes) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone in which the cluster will run.
ec2InstanceAttributes_ec2AvailabilityZone :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_ec2AvailabilityZone = Lens.lens (\Ec2InstanceAttributes' {ec2AvailabilityZone} -> ec2AvailabilityZone) (\s@Ec2InstanceAttributes' {} a -> s {ec2AvailabilityZone = a} :: Ec2InstanceAttributes)

-- | The identifier of the Amazon EC2 security group for the core and task
-- nodes.
ec2InstanceAttributes_emrManagedSlaveSecurityGroup :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_emrManagedSlaveSecurityGroup = Lens.lens (\Ec2InstanceAttributes' {emrManagedSlaveSecurityGroup} -> emrManagedSlaveSecurityGroup) (\s@Ec2InstanceAttributes' {} a -> s {emrManagedSlaveSecurityGroup = a} :: Ec2InstanceAttributes)

-- | Applies to clusters configured with the instance fleets option.
-- Specifies one or more Availability Zones in which to launch EC2 cluster
-- instances when the EC2-Classic network configuration is supported.
-- Amazon EMR chooses the Availability Zone with the best fit from among
-- the list of @RequestedEc2AvailabilityZones@, and then launches all
-- cluster instances within that Availability Zone. If you do not specify
-- this value, Amazon EMR chooses the Availability Zone for you.
-- @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be
-- specified together.
ec2InstanceAttributes_requestedEc2AvailabilityZones :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Core.Text])
ec2InstanceAttributes_requestedEc2AvailabilityZones = Lens.lens (\Ec2InstanceAttributes' {requestedEc2AvailabilityZones} -> requestedEc2AvailabilityZones) (\s@Ec2InstanceAttributes' {} a -> s {requestedEc2AvailabilityZones = a} :: Ec2InstanceAttributes) Core.. Lens.mapping Lens._Coerce

-- | Set this parameter to the identifier of the Amazon VPC subnet where you
-- want the cluster to launch. If you do not specify this value, and your
-- account supports EC2-Classic, the cluster launches in EC2-Classic.
ec2InstanceAttributes_ec2SubnetId :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_ec2SubnetId = Lens.lens (\Ec2InstanceAttributes' {ec2SubnetId} -> ec2SubnetId) (\s@Ec2InstanceAttributes' {} a -> s {ec2SubnetId = a} :: Ec2InstanceAttributes)

-- | The identifier of the Amazon EC2 security group for the master node.
ec2InstanceAttributes_emrManagedMasterSecurityGroup :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_emrManagedMasterSecurityGroup = Lens.lens (\Ec2InstanceAttributes' {emrManagedMasterSecurityGroup} -> emrManagedMasterSecurityGroup) (\s@Ec2InstanceAttributes' {} a -> s {emrManagedMasterSecurityGroup = a} :: Ec2InstanceAttributes)

-- | The IAM role that was specified when the cluster was launched. The EC2
-- instances of the cluster assume this role.
ec2InstanceAttributes_iamInstanceProfile :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_iamInstanceProfile = Lens.lens (\Ec2InstanceAttributes' {iamInstanceProfile} -> iamInstanceProfile) (\s@Ec2InstanceAttributes' {} a -> s {iamInstanceProfile = a} :: Ec2InstanceAttributes)

-- | A list of additional Amazon EC2 security group IDs for the master node.
ec2InstanceAttributes_additionalMasterSecurityGroups :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Core.Text])
ec2InstanceAttributes_additionalMasterSecurityGroups = Lens.lens (\Ec2InstanceAttributes' {additionalMasterSecurityGroups} -> additionalMasterSecurityGroups) (\s@Ec2InstanceAttributes' {} a -> s {additionalMasterSecurityGroups = a} :: Ec2InstanceAttributes) Core.. Lens.mapping Lens._Coerce

-- | Applies to clusters configured with the instance fleets option.
-- Specifies the unique identifier of one or more Amazon EC2 subnets in
-- which to launch EC2 cluster instances. Subnets must exist within the
-- same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among
-- the list of @RequestedEc2SubnetIds@, and then launches all cluster
-- instances within that Subnet. If this value is not specified, and the
-- account and Region support EC2-Classic networks, the cluster launches
-- instances in the EC2-Classic network and uses
-- @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic
-- is not supported, and no Subnet is specified, Amazon EMR chooses the
-- subnet for you. @RequestedEc2SubnetIDs@ and
-- @RequestedEc2AvailabilityZones@ cannot be specified together.
ec2InstanceAttributes_requestedEc2SubnetIds :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe [Core.Text])
ec2InstanceAttributes_requestedEc2SubnetIds = Lens.lens (\Ec2InstanceAttributes' {requestedEc2SubnetIds} -> requestedEc2SubnetIds) (\s@Ec2InstanceAttributes' {} a -> s {requestedEc2SubnetIds = a} :: Ec2InstanceAttributes) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
ec2InstanceAttributes_serviceAccessSecurityGroup :: Lens.Lens' Ec2InstanceAttributes (Core.Maybe Core.Text)
ec2InstanceAttributes_serviceAccessSecurityGroup = Lens.lens (\Ec2InstanceAttributes' {serviceAccessSecurityGroup} -> serviceAccessSecurityGroup) (\s@Ec2InstanceAttributes' {} a -> s {serviceAccessSecurityGroup = a} :: Ec2InstanceAttributes)

instance Core.FromJSON Ec2InstanceAttributes where
  parseJSON =
    Core.withObject
      "Ec2InstanceAttributes"
      ( \x ->
          Ec2InstanceAttributes'
            Core.<$> (x Core..:? "Ec2KeyName")
            Core.<*> ( x Core..:? "AdditionalSlaveSecurityGroups"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Ec2AvailabilityZone")
            Core.<*> (x Core..:? "EmrManagedSlaveSecurityGroup")
            Core.<*> ( x Core..:? "RequestedEc2AvailabilityZones"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Ec2SubnetId")
            Core.<*> (x Core..:? "EmrManagedMasterSecurityGroup")
            Core.<*> (x Core..:? "IamInstanceProfile")
            Core.<*> ( x Core..:? "AdditionalMasterSecurityGroups"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "RequestedEc2SubnetIds"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ServiceAccessSecurityGroup")
      )

instance Core.Hashable Ec2InstanceAttributes

instance Core.NFData Ec2InstanceAttributes
