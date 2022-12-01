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
-- Module      : Amazonka.EMR.Types.JobFlowInstancesConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.JobFlowInstancesConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.InstanceFleetConfig
import Amazonka.EMR.Types.InstanceGroupConfig
import Amazonka.EMR.Types.PlacementType
import qualified Amazonka.Prelude as Prelude

-- | A description of the Amazon EC2 instance on which the cluster (job flow)
-- runs. A valid JobFlowInstancesConfig must contain either InstanceGroups
-- or InstanceFleets. They cannot be used together. You may also have
-- MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must
-- be present), but we don\'t recommend this configuration.
--
-- /See:/ 'newJobFlowInstancesConfig' smart constructor.
data JobFlowInstancesConfig = JobFlowInstancesConfig'
  { -- | Applies to clusters that use the instance fleet configuration. When
    -- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
    -- launches instances in the optimal subnet.
    --
    -- The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    ec2SubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the EC2 key pair that can be used to connect to the master
    -- node using SSH as the user called \"hadoop.\"
    ec2KeyName :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which the cluster runs.
    placement :: Prelude.Maybe PlacementType,
    -- | Applies to clusters that use the uniform instance group configuration.
    -- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
    -- this parameter to the identifier of the Amazon VPC subnet where you want
    -- the cluster to launch. If you do not specify this value and your account
    -- supports EC2-Classic, the cluster launches in EC2-Classic.
    ec2SubnetId :: Prelude.Maybe Prelude.Text,
    -- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
    -- version for the cluster. Valid inputs are \"0.18\" (no longer
    -- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
    -- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
    -- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
    -- set in the RunJobFlow call, in which case the default version of Hadoop
    -- for that AMI version is used.
    hadoopVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon EC2 security group for the master node. If
    -- you specify @EmrManagedMasterSecurityGroup@, you must also specify
    -- @EmrManagedSlaveSecurityGroup@.
    emrManagedMasterSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | A list of additional Amazon EC2 security group IDs for the master node.
    additionalMasterSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the Amazon EC2 security group for the Amazon EMR
    -- service to access clusters in VPC private subnets.
    serviceAccessSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to lock the cluster to prevent the Amazon EC2
    -- instances from being terminated by API call, user intervention, or in
    -- the event of a job-flow error.
    terminationProtected :: Prelude.Maybe Prelude.Bool,
    -- | The EC2 instance type of the core and task nodes.
    slaveInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The number of EC2 instances in the cluster.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    --
    -- Describes the EC2 instances and instance configurations for clusters
    -- that use the instance fleet configuration.
    instanceFleets :: Prelude.Maybe [InstanceFleetConfig],
    -- | A list of additional Amazon EC2 security group IDs for the core and task
    -- nodes.
    additionalSlaveSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Configuration for the instance groups in a cluster.
    instanceGroups :: Prelude.Maybe [InstanceGroupConfig],
    -- | Specifies whether the cluster should remain available after completing
    -- all steps. Defaults to @true@. For more information about configuring
    -- cluster termination, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-termination.html Control Cluster Termination>
    -- in the /EMR Management Guide/.
    keepJobFlowAliveWhenNoSteps :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the Amazon EC2 security group for the core and task
    -- nodes. If you specify @EmrManagedSlaveSecurityGroup@, you must also
    -- specify @EmrManagedMasterSecurityGroup@.
    emrManagedSlaveSecurityGroup :: Prelude.Maybe Prelude.Text,
    -- | The EC2 instance type of the master node.
    masterInstanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobFlowInstancesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2SubnetIds', 'jobFlowInstancesConfig_ec2SubnetIds' - Applies to clusters that use the instance fleet configuration. When
-- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
-- launches instances in the optimal subnet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- 'ec2KeyName', 'jobFlowInstancesConfig_ec2KeyName' - The name of the EC2 key pair that can be used to connect to the master
-- node using SSH as the user called \"hadoop.\"
--
-- 'placement', 'jobFlowInstancesConfig_placement' - The Availability Zone in which the cluster runs.
--
-- 'ec2SubnetId', 'jobFlowInstancesConfig_ec2SubnetId' - Applies to clusters that use the uniform instance group configuration.
-- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the cluster to launch. If you do not specify this value and your account
-- supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- 'hadoopVersion', 'jobFlowInstancesConfig_hadoopVersion' - Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
-- version for the cluster. Valid inputs are \"0.18\" (no longer
-- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
-- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
-- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
-- set in the RunJobFlow call, in which case the default version of Hadoop
-- for that AMI version is used.
--
-- 'emrManagedMasterSecurityGroup', 'jobFlowInstancesConfig_emrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node. If
-- you specify @EmrManagedMasterSecurityGroup@, you must also specify
-- @EmrManagedSlaveSecurityGroup@.
--
-- 'additionalMasterSecurityGroups', 'jobFlowInstancesConfig_additionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- 'serviceAccessSecurityGroup', 'jobFlowInstancesConfig_serviceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
--
-- 'terminationProtected', 'jobFlowInstancesConfig_terminationProtected' - Specifies whether to lock the cluster to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in
-- the event of a job-flow error.
--
-- 'slaveInstanceType', 'jobFlowInstancesConfig_slaveInstanceType' - The EC2 instance type of the core and task nodes.
--
-- 'instanceCount', 'jobFlowInstancesConfig_instanceCount' - The number of EC2 instances in the cluster.
--
-- 'instanceFleets', 'jobFlowInstancesConfig_instanceFleets' - The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- Describes the EC2 instances and instance configurations for clusters
-- that use the instance fleet configuration.
--
-- 'additionalSlaveSecurityGroups', 'jobFlowInstancesConfig_additionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
--
-- 'instanceGroups', 'jobFlowInstancesConfig_instanceGroups' - Configuration for the instance groups in a cluster.
--
-- 'keepJobFlowAliveWhenNoSteps', 'jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps' - Specifies whether the cluster should remain available after completing
-- all steps. Defaults to @true@. For more information about configuring
-- cluster termination, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-termination.html Control Cluster Termination>
-- in the /EMR Management Guide/.
--
-- 'emrManagedSlaveSecurityGroup', 'jobFlowInstancesConfig_emrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task
-- nodes. If you specify @EmrManagedSlaveSecurityGroup@, you must also
-- specify @EmrManagedMasterSecurityGroup@.
--
-- 'masterInstanceType', 'jobFlowInstancesConfig_masterInstanceType' - The EC2 instance type of the master node.
newJobFlowInstancesConfig ::
  JobFlowInstancesConfig
newJobFlowInstancesConfig =
  JobFlowInstancesConfig'
    { ec2SubnetIds =
        Prelude.Nothing,
      ec2KeyName = Prelude.Nothing,
      placement = Prelude.Nothing,
      ec2SubnetId = Prelude.Nothing,
      hadoopVersion = Prelude.Nothing,
      emrManagedMasterSecurityGroup = Prelude.Nothing,
      additionalMasterSecurityGroups = Prelude.Nothing,
      serviceAccessSecurityGroup = Prelude.Nothing,
      terminationProtected = Prelude.Nothing,
      slaveInstanceType = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceFleets = Prelude.Nothing,
      additionalSlaveSecurityGroups = Prelude.Nothing,
      instanceGroups = Prelude.Nothing,
      keepJobFlowAliveWhenNoSteps = Prelude.Nothing,
      emrManagedSlaveSecurityGroup = Prelude.Nothing,
      masterInstanceType = Prelude.Nothing
    }

-- | Applies to clusters that use the instance fleet configuration. When
-- multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and
-- launches instances in the optimal subnet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
jobFlowInstancesConfig_ec2SubnetIds :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [Prelude.Text])
jobFlowInstancesConfig_ec2SubnetIds = Lens.lens (\JobFlowInstancesConfig' {ec2SubnetIds} -> ec2SubnetIds) (\s@JobFlowInstancesConfig' {} a -> s {ec2SubnetIds = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the EC2 key pair that can be used to connect to the master
-- node using SSH as the user called \"hadoop.\"
jobFlowInstancesConfig_ec2KeyName :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_ec2KeyName = Lens.lens (\JobFlowInstancesConfig' {ec2KeyName} -> ec2KeyName) (\s@JobFlowInstancesConfig' {} a -> s {ec2KeyName = a} :: JobFlowInstancesConfig)

-- | The Availability Zone in which the cluster runs.
jobFlowInstancesConfig_placement :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe PlacementType)
jobFlowInstancesConfig_placement = Lens.lens (\JobFlowInstancesConfig' {placement} -> placement) (\s@JobFlowInstancesConfig' {} a -> s {placement = a} :: JobFlowInstancesConfig)

-- | Applies to clusters that use the uniform instance group configuration.
-- To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set
-- this parameter to the identifier of the Amazon VPC subnet where you want
-- the cluster to launch. If you do not specify this value and your account
-- supports EC2-Classic, the cluster launches in EC2-Classic.
jobFlowInstancesConfig_ec2SubnetId :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_ec2SubnetId = Lens.lens (\JobFlowInstancesConfig' {ec2SubnetId} -> ec2SubnetId) (\s@JobFlowInstancesConfig' {} a -> s {ec2SubnetId = a} :: JobFlowInstancesConfig)

-- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop
-- version for the cluster. Valid inputs are \"0.18\" (no longer
-- maintained), \"0.20\" (no longer maintained), \"0.20.205\" (no longer
-- maintained), \"1.0.3\", \"2.2.0\", or \"2.4.0\". If you do not set this
-- value, the default of 0.18 is used, unless the @AmiVersion@ parameter is
-- set in the RunJobFlow call, in which case the default version of Hadoop
-- for that AMI version is used.
jobFlowInstancesConfig_hadoopVersion :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_hadoopVersion = Lens.lens (\JobFlowInstancesConfig' {hadoopVersion} -> hadoopVersion) (\s@JobFlowInstancesConfig' {} a -> s {hadoopVersion = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the master node. If
-- you specify @EmrManagedMasterSecurityGroup@, you must also specify
-- @EmrManagedSlaveSecurityGroup@.
jobFlowInstancesConfig_emrManagedMasterSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_emrManagedMasterSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {emrManagedMasterSecurityGroup} -> emrManagedMasterSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {emrManagedMasterSecurityGroup = a} :: JobFlowInstancesConfig)

-- | A list of additional Amazon EC2 security group IDs for the master node.
jobFlowInstancesConfig_additionalMasterSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [Prelude.Text])
jobFlowInstancesConfig_additionalMasterSecurityGroups = Lens.lens (\JobFlowInstancesConfig' {additionalMasterSecurityGroups} -> additionalMasterSecurityGroups) (\s@JobFlowInstancesConfig' {} a -> s {additionalMasterSecurityGroups = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon EC2 security group for the Amazon EMR
-- service to access clusters in VPC private subnets.
jobFlowInstancesConfig_serviceAccessSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_serviceAccessSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {serviceAccessSecurityGroup} -> serviceAccessSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {serviceAccessSecurityGroup = a} :: JobFlowInstancesConfig)

-- | Specifies whether to lock the cluster to prevent the Amazon EC2
-- instances from being terminated by API call, user intervention, or in
-- the event of a job-flow error.
jobFlowInstancesConfig_terminationProtected :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Bool)
jobFlowInstancesConfig_terminationProtected = Lens.lens (\JobFlowInstancesConfig' {terminationProtected} -> terminationProtected) (\s@JobFlowInstancesConfig' {} a -> s {terminationProtected = a} :: JobFlowInstancesConfig)

-- | The EC2 instance type of the core and task nodes.
jobFlowInstancesConfig_slaveInstanceType :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_slaveInstanceType = Lens.lens (\JobFlowInstancesConfig' {slaveInstanceType} -> slaveInstanceType) (\s@JobFlowInstancesConfig' {} a -> s {slaveInstanceType = a} :: JobFlowInstancesConfig)

-- | The number of EC2 instances in the cluster.
jobFlowInstancesConfig_instanceCount :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Int)
jobFlowInstancesConfig_instanceCount = Lens.lens (\JobFlowInstancesConfig' {instanceCount} -> instanceCount) (\s@JobFlowInstancesConfig' {} a -> s {instanceCount = a} :: JobFlowInstancesConfig)

-- | The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- Describes the EC2 instances and instance configurations for clusters
-- that use the instance fleet configuration.
jobFlowInstancesConfig_instanceFleets :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [InstanceFleetConfig])
jobFlowInstancesConfig_instanceFleets = Lens.lens (\JobFlowInstancesConfig' {instanceFleets} -> instanceFleets) (\s@JobFlowInstancesConfig' {} a -> s {instanceFleets = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Lens.coerced

-- | A list of additional Amazon EC2 security group IDs for the core and task
-- nodes.
jobFlowInstancesConfig_additionalSlaveSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [Prelude.Text])
jobFlowInstancesConfig_additionalSlaveSecurityGroups = Lens.lens (\JobFlowInstancesConfig' {additionalSlaveSecurityGroups} -> additionalSlaveSecurityGroups) (\s@JobFlowInstancesConfig' {} a -> s {additionalSlaveSecurityGroups = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for the instance groups in a cluster.
jobFlowInstancesConfig_instanceGroups :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe [InstanceGroupConfig])
jobFlowInstancesConfig_instanceGroups = Lens.lens (\JobFlowInstancesConfig' {instanceGroups} -> instanceGroups) (\s@JobFlowInstancesConfig' {} a -> s {instanceGroups = a} :: JobFlowInstancesConfig) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the cluster should remain available after completing
-- all steps. Defaults to @true@. For more information about configuring
-- cluster termination, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-termination.html Control Cluster Termination>
-- in the /EMR Management Guide/.
jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Bool)
jobFlowInstancesConfig_keepJobFlowAliveWhenNoSteps = Lens.lens (\JobFlowInstancesConfig' {keepJobFlowAliveWhenNoSteps} -> keepJobFlowAliveWhenNoSteps) (\s@JobFlowInstancesConfig' {} a -> s {keepJobFlowAliveWhenNoSteps = a} :: JobFlowInstancesConfig)

-- | The identifier of the Amazon EC2 security group for the core and task
-- nodes. If you specify @EmrManagedSlaveSecurityGroup@, you must also
-- specify @EmrManagedMasterSecurityGroup@.
jobFlowInstancesConfig_emrManagedSlaveSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_emrManagedSlaveSecurityGroup = Lens.lens (\JobFlowInstancesConfig' {emrManagedSlaveSecurityGroup} -> emrManagedSlaveSecurityGroup) (\s@JobFlowInstancesConfig' {} a -> s {emrManagedSlaveSecurityGroup = a} :: JobFlowInstancesConfig)

-- | The EC2 instance type of the master node.
jobFlowInstancesConfig_masterInstanceType :: Lens.Lens' JobFlowInstancesConfig (Prelude.Maybe Prelude.Text)
jobFlowInstancesConfig_masterInstanceType = Lens.lens (\JobFlowInstancesConfig' {masterInstanceType} -> masterInstanceType) (\s@JobFlowInstancesConfig' {} a -> s {masterInstanceType = a} :: JobFlowInstancesConfig)

instance Prelude.Hashable JobFlowInstancesConfig where
  hashWithSalt _salt JobFlowInstancesConfig' {..} =
    _salt `Prelude.hashWithSalt` ec2SubnetIds
      `Prelude.hashWithSalt` ec2KeyName
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` ec2SubnetId
      `Prelude.hashWithSalt` hadoopVersion
      `Prelude.hashWithSalt` emrManagedMasterSecurityGroup
      `Prelude.hashWithSalt` additionalMasterSecurityGroups
      `Prelude.hashWithSalt` serviceAccessSecurityGroup
      `Prelude.hashWithSalt` terminationProtected
      `Prelude.hashWithSalt` slaveInstanceType
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceFleets
      `Prelude.hashWithSalt` additionalSlaveSecurityGroups
      `Prelude.hashWithSalt` instanceGroups
      `Prelude.hashWithSalt` keepJobFlowAliveWhenNoSteps
      `Prelude.hashWithSalt` emrManagedSlaveSecurityGroup
      `Prelude.hashWithSalt` masterInstanceType

instance Prelude.NFData JobFlowInstancesConfig where
  rnf JobFlowInstancesConfig' {..} =
    Prelude.rnf ec2SubnetIds
      `Prelude.seq` Prelude.rnf ec2KeyName
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf ec2SubnetId
      `Prelude.seq` Prelude.rnf hadoopVersion
      `Prelude.seq` Prelude.rnf emrManagedMasterSecurityGroup
      `Prelude.seq` Prelude.rnf additionalMasterSecurityGroups
      `Prelude.seq` Prelude.rnf serviceAccessSecurityGroup
      `Prelude.seq` Prelude.rnf terminationProtected
      `Prelude.seq` Prelude.rnf slaveInstanceType
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceFleets
      `Prelude.seq` Prelude.rnf additionalSlaveSecurityGroups
      `Prelude.seq` Prelude.rnf instanceGroups
      `Prelude.seq` Prelude.rnf keepJobFlowAliveWhenNoSteps
      `Prelude.seq` Prelude.rnf
        emrManagedSlaveSecurityGroup
      `Prelude.seq` Prelude.rnf masterInstanceType

instance Core.ToJSON JobFlowInstancesConfig where
  toJSON JobFlowInstancesConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Ec2SubnetIds" Core..=) Prelude.<$> ec2SubnetIds,
            ("Ec2KeyName" Core..=) Prelude.<$> ec2KeyName,
            ("Placement" Core..=) Prelude.<$> placement,
            ("Ec2SubnetId" Core..=) Prelude.<$> ec2SubnetId,
            ("HadoopVersion" Core..=) Prelude.<$> hadoopVersion,
            ("EmrManagedMasterSecurityGroup" Core..=)
              Prelude.<$> emrManagedMasterSecurityGroup,
            ("AdditionalMasterSecurityGroups" Core..=)
              Prelude.<$> additionalMasterSecurityGroups,
            ("ServiceAccessSecurityGroup" Core..=)
              Prelude.<$> serviceAccessSecurityGroup,
            ("TerminationProtected" Core..=)
              Prelude.<$> terminationProtected,
            ("SlaveInstanceType" Core..=)
              Prelude.<$> slaveInstanceType,
            ("InstanceCount" Core..=) Prelude.<$> instanceCount,
            ("InstanceFleets" Core..=)
              Prelude.<$> instanceFleets,
            ("AdditionalSlaveSecurityGroups" Core..=)
              Prelude.<$> additionalSlaveSecurityGroups,
            ("InstanceGroups" Core..=)
              Prelude.<$> instanceGroups,
            ("KeepJobFlowAliveWhenNoSteps" Core..=)
              Prelude.<$> keepJobFlowAliveWhenNoSteps,
            ("EmrManagedSlaveSecurityGroup" Core..=)
              Prelude.<$> emrManagedSlaveSecurityGroup,
            ("MasterInstanceType" Core..=)
              Prelude.<$> masterInstanceType
          ]
      )
