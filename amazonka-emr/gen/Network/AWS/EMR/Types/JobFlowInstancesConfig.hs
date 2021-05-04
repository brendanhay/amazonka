{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EMR.Types.InstanceFleetConfig
import Network.AWS.EMR.Types.InstanceGroupConfig
import Network.AWS.EMR.Types.PlacementType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    hadoopVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the EC2 key pair that can be used to connect to the master
    -- node using SSH as the user called \"hadoop.\"
    ec2KeyName :: Prelude.Maybe Prelude.Text,
    -- | The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    --
    -- Describes the EC2 instances and instance configurations for clusters
    -- that use the instance fleet configuration.
    instanceFleets :: Prelude.Maybe [InstanceFleetConfig],
    -- | Applies to clusters that use the instance fleet configuration. When
    -- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
    -- launches instances in the optimal subnet.
    --
    -- The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    ec2SubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zone in which the cluster runs.
    placement :: Prelude.Maybe PlacementType,
    -- | A list of additional Amazon EC2 security group IDs for the core and task
    -- nodes.
    additionalSlaveSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether to lock the cluster to prevent the Amazon EC2
    -- instances from being terminated by API call, user intervention, or in
    -- the event of a job-flow error.
    terminationProtected :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the Amazon EC2 security group for the core and task
    -- nodes.
    emrManagedSlaveSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | Configuration for the instance groups in a cluster.
    instanceGroups :: Prelude.Maybe [InstanceGroupConfig],
    -- | The EC2 instance type of the master node.
    masterInstanceType :: Prelude.Maybe Prelude.Text,
    -- | Applies to clusters that use the uniform instance group configuration.
    -- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
    -- this parameter to the identifier of the Amazon VPC subnet where you want
    -- the cluster to launch. If you do not specify this value and your account
    -- supports EC2-Classic, the cluster launches in EC2-Classic.
    ec2SubnetId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon EC2 security group for the master node.
    emrManagedMasterSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | A list of additional Amazon EC2 security group IDs for the master node.
    additionalMasterSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 instance type of the core and task nodes.
    slaveInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon EC2 security group for the Amazon EMR
    -- service to access clusters in VPC private subnets.
    serviceAccessSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the cluster should remain available after completing
    -- all steps.
    keepJobFlowAliveWhenNoSteps :: Prelude.Maybe Prelude.Bool,
    -- | The number of EC2 instances in the cluster.
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      ec2KeyName = Prelude.Nothing,
      instanceFleets = Prelude.Nothing,
      ec2SubnetIds = Prelude.Nothing,
      placement = Prelude.Nothing,
      additionalSlaveSecurityGroups = Prelude.Nothing,
      terminationProtected = Prelude.Nothing,
      emrManagedSlaveSecurityGroup = Prelude.Nothing,
      instanceGroups = Prelude.Nothing,
      masterInstanceType = Prelude.Nothing,
      ec2SubnetId = Prelude.Nothing,
      emrManagedMasterSecurityGroup = Prelude.Nothing,
      additionalMasterSecurityGroups = Prelude.Nothing,
      slaveInstanceType = Prelude.Nothing,
      serviceAccessSecurityGroup = Prelude.Nothing,
      keepJobFlowAliveWhenNoSteps = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
-- version for the cluster. Valid inputs are \"0.18\" (no longer
-- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
-- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
-- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
-- set in the RunJobFlow call, in which case the default version of Hadoop
-- for that AMI version is used.
jobFlowInstancesConfig_hadoopVersion :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_hadoopVersion = Lens.lens (\JobFlowInstancesConfig' {hadoopVersion} -> hadoopVersion) (\s@JobFlowInstancesConfig' {} a -> s {hadoopVersion = a} :: JobFlowInstancesConfig)

-- | The name of the EC2 key pair that can be used to connect to the master
-- node using SSH as the user called \"hadoop.\"
jobFlowInstancesConfig_ec2KeyName :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_ec2KeyName = Lens.lens (\JobFlowInstancesConfig' {ec2KeyName} -> ec2KeyName) (\s@JobFlowInstancesConfig' {} a -> s {ec2KeyName = a} :: JobFlowInstancesConfig)

-- | The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- Describes the EC2 instances and instance configurations for clusters
-- that use the instance fleet configuration.
jobFlowInstancesConfig_instanceFleets :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [InstanceFleetConfig])
jobFlowInstancesConfig_instanceFleets = Lens.lens (\JobFlowInstancesConfig' {instanceFleets} -> instanceFleets) (\s@JobFlowInstancesConfig' {} a -> s {instanceFleets = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Applies to clusters that use the instance fleet configuration. When
-- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
-- launches instances in the optimal subnet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
jobFlowInstancesConfig_ec2SubnetIds :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [Prelude.Text])
jobFlowInstancesConfig_ec2SubnetIds = Lens.lens (\JobFlowInstancesConfig' {ec2SubnetIds} -> ec2SubnetIds) (\s@JobFlowInstancesConfig' {} a -> s {ec2SubnetIds = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone in which the cluster runs.
jobFlowInstancesConfig_placement :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe PlacementType)
jobFlowInstancesConfig_placement = Lens.lens (\JobFlowInstancesConfig' {placement} -> placement) (\s@JobFlowInstancesConfig' {} a -> s {placement = a} :: JobFlowInstancesConfig)

-- | A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
jobFlowInstancesConfig_additionalSlaveSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [Prelude.Text])
jobFlowInstancesConfig_additionalSlaveSecurityGroups = Lens.lens (\JobFlowInstancesConfig' {additionalSlaveSecurityGroups} -> additionalSlaveSecurityGroups) (\s@JobFlowInstancesConfig' {} a -> s {additionalSlaveSecurityGroups = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies whether to lock the cluster to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in
-- the event of a job-flow error.
jobFlowInstancesConfig_terminationProtected :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Bool)
jobFlowInstancesConfig_terminationProtected = Lens.lens (\JobFlowInstancesConfig' {terminationProtected} -> terminationProtected) (\s@JobFlowInstancesConfig' {} a -> s {terminationProtected = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the core and task
-- nodes.
jobFlowInstancesConfig_emrManagedSlaveSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_emrManagedSlaveSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {emrManagedSlaveSecurityGroup} -> emrManagedSlaveSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {emrManagedSlaveSecurityGroup = a} :: JobFlowInstancesConfig)

-- | Configuration for the instance groups in a cluster.
jobFlowInstancesConfig_instanceGroups :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [InstanceGroupConfig])
jobFlowInstancesConfig_instanceGroups = Lens.lens (\JobFlowInstancesConfig' {instanceGroups} -> instanceGroups) (\s@JobFlowInstancesConfig' {} a -> s {instanceGroups = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The EC2 instance type of the master node.
jobFlowInstancesConfig_masterInstanceType :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_masterInstanceType = Lens.lens (\JobFlowInstancesConfig' {masterInstanceType} -> masterInstanceType) (\s@JobFlowInstancesConfig' {} a -> s {masterInstanceType = a} :: JobFlowInstancesConfig)

-- | Applies to clusters that use the uniform instance group configuration.
-- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the cluster to launch. If you do not specify this value and your account
-- supports EC2-Classic, the cluster launches in EC2-Classic.
jobFlowInstancesConfig_ec2SubnetId :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_ec2SubnetId = Lens.lens (\JobFlowInstancesConfig' {ec2SubnetId} -> ec2SubnetId) (\s@JobFlowInstancesConfig' {} a -> s {ec2SubnetId = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the master node.
jobFlowInstancesConfig_emrManagedMasterSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_emrManagedMasterSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {emrManagedMasterSecurityGroup} -> emrManagedMasterSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {emrManagedMasterSecurityGroup = a} :: JobFlowInstancesConfig)

-- | A list of additional Amazon EC2 security group IDs for the master node.
jobFlowInstancesConfig_additionalMasterSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [Prelude.Text])
jobFlowInstancesConfig_additionalMasterSecurityGroups = Lens.lens (\JobFlowInstancesConfig' {additionalMasterSecurityGroups} -> additionalMasterSecurityGroups) (\s@JobFlowInstancesConfig' {} a -> s {additionalMasterSecurityGroups = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The EC2 instance type of the core and task nodes.
jobFlowInstancesConfig_slaveInstanceType :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_slaveInstanceType = Lens.lens (\JobFlowInstancesConfig' {slaveInstanceType} -> slaveInstanceType) (\s@JobFlowInstancesConfig' {} a -> s {slaveInstanceType = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
jobFlowInstancesConfig_serviceAccessSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_serviceAccessSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {serviceAccessSecurityGroup} -> serviceAccessSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {serviceAccessSecurityGroup = a} :: JobFlowInstancesConfig)

-- | Specifies whether the cluster should remain available after completing
-- all steps.
jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Bool)
jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps = Lens.lens (\JobFlowInstancesConfig' {keepJobFlowAliveWhenNoSteps} -> keepJobFlowAliveWhenNoSteps) (\s@JobFlowInstancesConfig' {} a -> s {keepJobFlowAliveWhenNoSteps = a} :: JobFlowInstancesConfig)

-- | The number of EC2 instances in the cluster.
jobFlowInstancesConfig_instanceCount :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Int)
jobFlowInstancesConfig_instanceCount = Lens.lens (\JobFlowInstancesConfig' {instanceCount} -> instanceCount) (\s@JobFlowInstancesConfig' {} a -> s {instanceCount = a} :: JobFlowInstancesConfig)

instance Prelude.Hashable JobFlowInstancesConfig

instance Prelude.NFData JobFlowInstancesConfig

instance Prelude.ToJSON JobFlowInstancesConfig where
  toJSON JobFlowInstancesConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HadoopVersion" Prelude..=)
              Prelude.<$> hadoopVersion,
            ("Ec2KeyName" Prelude..=) Prelude.<$> ec2KeyName,
            ("InstanceFleets" Prelude..=)
              Prelude.<$> instanceFleets,
            ("Ec2SubnetIds" Prelude..=) Prelude.<$> ec2SubnetIds,
            ("Placement" Prelude..=) Prelude.<$> placement,
            ("AdditionalSlaveSecurityGroups" Prelude..=)
              Prelude.<$> additionalSlaveSecurityGroups,
            ("TerminationProtected" Prelude..=)
              Prelude.<$> terminationProtected,
            ("EmrManagedSlaveSecurityGroup" Prelude..=)
              Prelude.<$> emrManagedSlaveSecurityGroup,
            ("InstanceGroups" Prelude..=)
              Prelude.<$> instanceGroups,
            ("MasterInstanceType" Prelude..=)
              Prelude.<$> masterInstanceType,
            ("Ec2SubnetId" Prelude..=) Prelude.<$> ec2SubnetId,
            ("EmrManagedMasterSecurityGroup" Prelude..=)
              Prelude.<$> emrManagedMasterSecurityGroup,
            ("AdditionalMasterSecurityGroups" Prelude..=)
              Prelude.<$> additionalMasterSecurityGroups,
            ("SlaveInstanceType" Prelude..=)
              Prelude.<$> slaveInstanceType,
            ("ServiceAccessSecurityGroup" Prelude..=)
              Prelude.<$> serviceAccessSecurityGroup,
            ("KeepJobFlowAliveWhenNoSteps" Prelude..=)
              Prelude.<$> keepJobFlowAliveWhenNoSteps,
            ("InstanceCount" Prelude..=)
              Prelude.<$> instanceCount
          ]
      )
