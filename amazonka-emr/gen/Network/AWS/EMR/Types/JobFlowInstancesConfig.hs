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
-- Module      : Network.AWS.EMR.Types.JobFlowInstancesConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.JobFlowInstancesConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.InstanceFleetConfig
import Network.AWS.EMR.Types.InstanceGroupConfig
import Network.AWS.EMR.Types.PlacementType
import qualified Network.AWS.Lens as Lens

-- | A description of the Amazon EC2 instance on which the cluster (job flow)
-- runs. A valid JobFlowInstancesConfig must contain either InstanceGroups
-- or InstanceFleets. They cannot be used together. You may also have
-- MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must
-- be present), but we don\'t recommend this configuration.
--
-- /See:/ 'newJobFlowInstancesConfig' smart constructor.
data JobFlowInstancesConfig = JobFlowInstancesConfig'
  { -- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
    -- version for the cluster. Valid inputs are \"0.18\" (no longer
    -- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
    -- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
    -- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
    -- set in the RunJobFlow call, in which case the default version of Hadoop
    -- for that AMI version is used.
    hadoopVersion :: Core.Maybe Core.Text,
    -- | The name of the EC2 key pair that can be used to connect to the master
    -- node using SSH as the user called \"hadoop.\"
    ec2KeyName :: Core.Maybe Core.Text,
    -- | The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    --
    -- Describes the EC2 instances and instance configurations for clusters
    -- that use the instance fleet configuration.
    instanceFleets :: Core.Maybe [InstanceFleetConfig],
    -- | Applies to clusters that use the instance fleet configuration. When
    -- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
    -- launches instances in the optimal subnet.
    --
    -- The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    ec2SubnetIds :: Core.Maybe [Core.Text],
    -- | The Availability Zone in which the cluster runs.
    placement :: Core.Maybe PlacementType,
    -- | A list of additional Amazon EC2 security group IDs for the core and task
    -- nodes.
    additionalSlaveSecurityGroups :: Core.Maybe [Core.Text],
    -- | Specifies whether to lock the cluster to prevent the Amazon EC2
    -- instances from being terminated by API call, user intervention, or in
    -- the event of a job-flow error.
    terminationProtected :: Core.Maybe Core.Bool,
    -- | The identifier of the Amazon EC2 security group for the core and task
    -- nodes.
    emrManagedSlaveSecurityGroup :: Core.Maybe Core.Text,
    -- | Configuration for the instance groups in a cluster.
    instanceGroups :: Core.Maybe [InstanceGroupConfig],
    -- | The EC2 instance type of the master node.
    masterInstanceType :: Core.Maybe Core.Text,
    -- | Applies to clusters that use the uniform instance group configuration.
    -- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
    -- this parameter to the identifier of the Amazon VPC subnet where you want
    -- the cluster to launch. If you do not specify this value and your account
    -- supports EC2-Classic, the cluster launches in EC2-Classic.
    ec2SubnetId :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon EC2 security group for the master node.
    emrManagedMasterSecurityGroup :: Core.Maybe Core.Text,
    -- | A list of additional Amazon EC2 security group IDs for the master node.
    additionalMasterSecurityGroups :: Core.Maybe [Core.Text],
    -- | The EC2 instance type of the core and task nodes.
    slaveInstanceType :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon EC2 security group for the Amazon EMR
    -- service to access clusters in VPC private subnets.
    serviceAccessSecurityGroup :: Core.Maybe Core.Text,
    -- | Specifies whether the cluster should remain available after completing
    -- all steps.
    keepJobFlowAliveWhenNoSteps :: Core.Maybe Core.Bool,
    -- | The number of EC2 instances in the cluster.
    instanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobFlowInstancesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hadoopVersion', 'jobFlowInstancesConfig_hadoopVersion' - Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
-- version for the cluster. Valid inputs are \"0.18\" (no longer
-- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
-- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
-- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
-- set in the RunJobFlow call, in which case the default version of Hadoop
-- for that AMI version is used.
--
-- 'ec2KeyName', 'jobFlowInstancesConfig_ec2KeyName' - The name of the EC2 key pair that can be used to connect to the master
-- node using SSH as the user called \"hadoop.\"
--
-- 'instanceFleets', 'jobFlowInstancesConfig_instanceFleets' - The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- Describes the EC2 instances and instance configurations for clusters
-- that use the instance fleet configuration.
--
-- 'ec2SubnetIds', 'jobFlowInstancesConfig_ec2SubnetIds' - Applies to clusters that use the instance fleet configuration. When
-- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
-- launches instances in the optimal subnet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- 'placement', 'jobFlowInstancesConfig_placement' - The Availability Zone in which the cluster runs.
--
-- 'additionalSlaveSecurityGroups', 'jobFlowInstancesConfig_additionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
--
-- 'terminationProtected', 'jobFlowInstancesConfig_terminationProtected' - Specifies whether to lock the cluster to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in
-- the event of a job-flow error.
--
-- 'emrManagedSlaveSecurityGroup', 'jobFlowInstancesConfig_emrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task
-- nodes.
--
-- 'instanceGroups', 'jobFlowInstancesConfig_instanceGroups' - Configuration for the instance groups in a cluster.
--
-- 'masterInstanceType', 'jobFlowInstancesConfig_masterInstanceType' - The EC2 instance type of the master node.
--
-- 'ec2SubnetId', 'jobFlowInstancesConfig_ec2SubnetId' - Applies to clusters that use the uniform instance group configuration.
-- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the cluster to launch. If you do not specify this value and your account
-- supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- 'emrManagedMasterSecurityGroup', 'jobFlowInstancesConfig_emrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
--
-- 'additionalMasterSecurityGroups', 'jobFlowInstancesConfig_additionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- 'slaveInstanceType', 'jobFlowInstancesConfig_slaveInstanceType' - The EC2 instance type of the core and task nodes.
--
-- 'serviceAccessSecurityGroup', 'jobFlowInstancesConfig_serviceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
--
-- 'keepJobFlowAliveWhenNoSteps', 'jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps' - Specifies whether the cluster should remain available after completing
-- all steps.
--
-- 'instanceCount', 'jobFlowInstancesConfig_instanceCount' - The number of EC2 instances in the cluster.
newJobFlowInstancesConfig ::
  JobFlowInstancesConfig
newJobFlowInstancesConfig =
  JobFlowInstancesConfig'
    { hadoopVersion =
        Core.Nothing,
      ec2KeyName = Core.Nothing,
      instanceFleets = Core.Nothing,
      ec2SubnetIds = Core.Nothing,
      placement = Core.Nothing,
      additionalSlaveSecurityGroups = Core.Nothing,
      terminationProtected = Core.Nothing,
      emrManagedSlaveSecurityGroup = Core.Nothing,
      instanceGroups = Core.Nothing,
      masterInstanceType = Core.Nothing,
      ec2SubnetId = Core.Nothing,
      emrManagedMasterSecurityGroup = Core.Nothing,
      additionalMasterSecurityGroups = Core.Nothing,
      slaveInstanceType = Core.Nothing,
      serviceAccessSecurityGroup = Core.Nothing,
      keepJobFlowAliveWhenNoSteps = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
-- version for the cluster. Valid inputs are \"0.18\" (no longer
-- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
-- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
-- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
-- set in the RunJobFlow call, in which case the default version of Hadoop
-- for that AMI version is used.
jobFlowInstancesConfig_hadoopVersion :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_hadoopVersion = Lens.lens (\JobFlowInstancesConfig' {hadoopVersion} -> hadoopVersion) (\s@JobFlowInstancesConfig' {} a -> s {hadoopVersion = a} :: JobFlowInstancesConfig)

-- | The name of the EC2 key pair that can be used to connect to the master
-- node using SSH as the user called \"hadoop.\"
jobFlowInstancesConfig_ec2KeyName :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_ec2KeyName = Lens.lens (\JobFlowInstancesConfig' {ec2KeyName} -> ec2KeyName) (\s@JobFlowInstancesConfig' {} a -> s {ec2KeyName = a} :: JobFlowInstancesConfig)

-- | The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- Describes the EC2 instances and instance configurations for clusters
-- that use the instance fleet configuration.
jobFlowInstancesConfig_instanceFleets :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [InstanceFleetConfig])
jobFlowInstancesConfig_instanceFleets = Lens.lens (\JobFlowInstancesConfig' {instanceFleets} -> instanceFleets) (\s@JobFlowInstancesConfig' {} a -> s {instanceFleets = a} :: JobFlowInstancesConfig) Core.. Lens.mapping Lens._Coerce

-- | Applies to clusters that use the instance fleet configuration. When
-- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
-- launches instances in the optimal subnet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
jobFlowInstancesConfig_ec2SubnetIds :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Core.Text])
jobFlowInstancesConfig_ec2SubnetIds = Lens.lens (\JobFlowInstancesConfig' {ec2SubnetIds} -> ec2SubnetIds) (\s@JobFlowInstancesConfig' {} a -> s {ec2SubnetIds = a} :: JobFlowInstancesConfig) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone in which the cluster runs.
jobFlowInstancesConfig_placement :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe PlacementType)
jobFlowInstancesConfig_placement = Lens.lens (\JobFlowInstancesConfig' {placement} -> placement) (\s@JobFlowInstancesConfig' {} a -> s {placement = a} :: JobFlowInstancesConfig)

-- | A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
jobFlowInstancesConfig_additionalSlaveSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Core.Text])
jobFlowInstancesConfig_additionalSlaveSecurityGroups = Lens.lens (\JobFlowInstancesConfig' {additionalSlaveSecurityGroups} -> additionalSlaveSecurityGroups) (\s@JobFlowInstancesConfig' {} a -> s {additionalSlaveSecurityGroups = a} :: JobFlowInstancesConfig) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether to lock the cluster to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in
-- the event of a job-flow error.
jobFlowInstancesConfig_terminationProtected :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Bool)
jobFlowInstancesConfig_terminationProtected = Lens.lens (\JobFlowInstancesConfig' {terminationProtected} -> terminationProtected) (\s@JobFlowInstancesConfig' {} a -> s {terminationProtected = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the core and task
-- nodes.
jobFlowInstancesConfig_emrManagedSlaveSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_emrManagedSlaveSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {emrManagedSlaveSecurityGroup} -> emrManagedSlaveSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {emrManagedSlaveSecurityGroup = a} :: JobFlowInstancesConfig)

-- | Configuration for the instance groups in a cluster.
jobFlowInstancesConfig_instanceGroups :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [InstanceGroupConfig])
jobFlowInstancesConfig_instanceGroups = Lens.lens (\JobFlowInstancesConfig' {instanceGroups} -> instanceGroups) (\s@JobFlowInstancesConfig' {} a -> s {instanceGroups = a} :: JobFlowInstancesConfig) Core.. Lens.mapping Lens._Coerce

-- | The EC2 instance type of the master node.
jobFlowInstancesConfig_masterInstanceType :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_masterInstanceType = Lens.lens (\JobFlowInstancesConfig' {masterInstanceType} -> masterInstanceType) (\s@JobFlowInstancesConfig' {} a -> s {masterInstanceType = a} :: JobFlowInstancesConfig)

-- | Applies to clusters that use the uniform instance group configuration.
-- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the cluster to launch. If you do not specify this value and your account
-- supports EC2-Classic, the cluster launches in EC2-Classic.
jobFlowInstancesConfig_ec2SubnetId :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_ec2SubnetId = Lens.lens (\JobFlowInstancesConfig' {ec2SubnetId} -> ec2SubnetId) (\s@JobFlowInstancesConfig' {} a -> s {ec2SubnetId = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the master node.
jobFlowInstancesConfig_emrManagedMasterSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_emrManagedMasterSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {emrManagedMasterSecurityGroup} -> emrManagedMasterSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {emrManagedMasterSecurityGroup = a} :: JobFlowInstancesConfig)

-- | A list of additional Amazon EC2 security group IDs for the master node.
jobFlowInstancesConfig_additionalMasterSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Core.Text])
jobFlowInstancesConfig_additionalMasterSecurityGroups = Lens.lens (\JobFlowInstancesConfig' {additionalMasterSecurityGroups} -> additionalMasterSecurityGroups) (\s@JobFlowInstancesConfig' {} a -> s {additionalMasterSecurityGroups = a} :: JobFlowInstancesConfig) Core.. Lens.mapping Lens._Coerce

-- | The EC2 instance type of the core and task nodes.
jobFlowInstancesConfig_slaveInstanceType :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_slaveInstanceType = Lens.lens (\JobFlowInstancesConfig' {slaveInstanceType} -> slaveInstanceType) (\s@JobFlowInstancesConfig' {} a -> s {slaveInstanceType = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
jobFlowInstancesConfig_serviceAccessSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Text)
jobFlowInstancesConfig_serviceAccessSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {serviceAccessSecurityGroup} -> serviceAccessSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {serviceAccessSecurityGroup = a} :: JobFlowInstancesConfig)

-- | Specifies whether the cluster should remain available after completing
-- all steps.
jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Bool)
jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps = Lens.lens (\JobFlowInstancesConfig' {keepJobFlowAliveWhenNoSteps} -> keepJobFlowAliveWhenNoSteps) (\s@JobFlowInstancesConfig' {} a -> s {keepJobFlowAliveWhenNoSteps = a} :: JobFlowInstancesConfig)

-- | The number of EC2 instances in the cluster.
jobFlowInstancesConfig_instanceCount :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Int)
jobFlowInstancesConfig_instanceCount = Lens.lens (\JobFlowInstancesConfig' {instanceCount} -> instanceCount) (\s@JobFlowInstancesConfig' {} a -> s {instanceCount = a} :: JobFlowInstancesConfig)

instance Core.Hashable JobFlowInstancesConfig

instance Core.NFData JobFlowInstancesConfig

instance Core.ToJSON JobFlowInstancesConfig where
  toJSON JobFlowInstancesConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HadoopVersion" Core..=) Core.<$> hadoopVersion,
            ("Ec2KeyName" Core..=) Core.<$> ec2KeyName,
            ("InstanceFleets" Core..=) Core.<$> instanceFleets,
            ("Ec2SubnetIds" Core..=) Core.<$> ec2SubnetIds,
            ("Placement" Core..=) Core.<$> placement,
            ("AdditionalSlaveSecurityGroups" Core..=)
              Core.<$> additionalSlaveSecurityGroups,
            ("TerminationProtected" Core..=)
              Core.<$> terminationProtected,
            ("EmrManagedSlaveSecurityGroup" Core..=)
              Core.<$> emrManagedSlaveSecurityGroup,
            ("InstanceGroups" Core..=) Core.<$> instanceGroups,
            ("MasterInstanceType" Core..=)
              Core.<$> masterInstanceType,
            ("Ec2SubnetId" Core..=) Core.<$> ec2SubnetId,
            ("EmrManagedMasterSecurityGroup" Core..=)
              Core.<$> emrManagedMasterSecurityGroup,
            ("AdditionalMasterSecurityGroups" Core..=)
              Core.<$> additionalMasterSecurityGroups,
            ("SlaveInstanceType" Core..=)
              Core.<$> slaveInstanceType,
            ("ServiceAccessSecurityGroup" Core..=)
              Core.<$> serviceAccessSecurityGroup,
            ("KeepJobFlowAliveWhenNoSteps" Core..=)
              Core.<$> keepJobFlowAliveWhenNoSteps,
            ("InstanceCount" Core..=) Core.<$> instanceCount
          ]
      )
