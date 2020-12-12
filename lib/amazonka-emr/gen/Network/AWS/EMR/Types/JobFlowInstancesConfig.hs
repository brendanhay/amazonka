{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.JobFlowInstancesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.JobFlowInstancesConfig
  ( JobFlowInstancesConfig (..),

    -- * Smart constructor
    mkJobFlowInstancesConfig,

    -- * Lenses
    jficInstanceFleets,
    jficEC2KeyName,
    jficSlaveInstanceType,
    jficInstanceCount,
    jficEmrManagedSlaveSecurityGroup,
    jficAdditionalSlaveSecurityGroups,
    jficEC2SubnetIds,
    jficHadoopVersion,
    jficAdditionalMasterSecurityGroups,
    jficEmrManagedMasterSecurityGroup,
    jficEC2SubnetId,
    jficMasterInstanceType,
    jficInstanceGroups,
    jficKeepJobFlowAliveWhenNoSteps,
    jficServiceAccessSecurityGroup,
    jficTerminationProtected,
    jficPlacement,
  )
where

import Network.AWS.EMR.Types.InstanceFleetConfig
import Network.AWS.EMR.Types.InstanceGroupConfig
import Network.AWS.EMR.Types.PlacementType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of the Amazon EC2 instance on which the cluster (job flow) runs. A valid JobFlowInstancesConfig must contain either InstanceGroups or InstanceFleets. They cannot be used together. You may also have MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must be present), but we don't recommend this configuration.
--
-- /See:/ 'mkJobFlowInstancesConfig' smart constructor.
data JobFlowInstancesConfig = JobFlowInstancesConfig'
  { instanceFleets ::
      Lude.Maybe [InstanceFleetConfig],
    ec2KeyName :: Lude.Maybe Lude.Text,
    slaveInstanceType :: Lude.Maybe Lude.Text,
    instanceCount :: Lude.Maybe Lude.Int,
    emrManagedSlaveSecurityGroup ::
      Lude.Maybe Lude.Text,
    additionalSlaveSecurityGroups ::
      Lude.Maybe [Lude.Text],
    ec2SubnetIds :: Lude.Maybe [Lude.Text],
    hadoopVersion :: Lude.Maybe Lude.Text,
    additionalMasterSecurityGroups ::
      Lude.Maybe [Lude.Text],
    emrManagedMasterSecurityGroup ::
      Lude.Maybe Lude.Text,
    ec2SubnetId :: Lude.Maybe Lude.Text,
    masterInstanceType :: Lude.Maybe Lude.Text,
    instanceGroups ::
      Lude.Maybe [InstanceGroupConfig],
    keepJobFlowAliveWhenNoSteps ::
      Lude.Maybe Lude.Bool,
    serviceAccessSecurityGroup ::
      Lude.Maybe Lude.Text,
    terminationProtected :: Lude.Maybe Lude.Bool,
    placement :: Lude.Maybe PlacementType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobFlowInstancesConfig' with the minimum fields required to make a request.
--
-- * 'additionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
-- * 'additionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task nodes.
-- * 'ec2KeyName' - The name of the EC2 key pair that can be used to connect to the master node using SSH as the user called "hadoop."
-- * 'ec2SubnetId' - Applies to clusters that use the uniform instance group configuration. To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value and your account supports EC2-Classic, the cluster launches in EC2-Classic.
-- * 'ec2SubnetIds' - Applies to clusters that use the instance fleet configuration. When multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and launches instances in the optimal subnet.
-- * 'emrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
-- * 'emrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task nodes.
-- * 'hadoopVersion' - Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop version for the cluster. Valid inputs are "0.18" (no longer maintained), "0.20" (no longer maintained), "0.20.205" (no longer maintained), "1.0.3", "2.2.0", or "2.4.0". If you do not set this value, the default of 0.18 is used, unless the @AmiVersion@ parameter is set in the RunJobFlow call, in which case the default version of Hadoop for that AMI version is used.
-- * 'instanceCount' - The number of EC2 instances in the cluster.
-- * 'instanceFleets' - Describes the EC2 instances and instance configurations for clusters that use the instance fleet configuration.
-- * 'instanceGroups' - Configuration for the instance groups in a cluster.
-- * 'keepJobFlowAliveWhenNoSteps' - Specifies whether the cluster should remain available after completing all steps.
-- * 'masterInstanceType' - The EC2 instance type of the master node.
-- * 'placement' - The Availability Zone in which the cluster runs.
-- * 'serviceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
-- * 'slaveInstanceType' - The EC2 instance type of the core and task nodes.
-- * 'terminationProtected' - Specifies whether to lock the cluster to prevent the Amazon EC2 instances from being terminated by API call, user intervention, or in the event of a job-flow error.
mkJobFlowInstancesConfig ::
  JobFlowInstancesConfig
mkJobFlowInstancesConfig =
  JobFlowInstancesConfig'
    { instanceFleets = Lude.Nothing,
      ec2KeyName = Lude.Nothing,
      slaveInstanceType = Lude.Nothing,
      instanceCount = Lude.Nothing,
      emrManagedSlaveSecurityGroup = Lude.Nothing,
      additionalSlaveSecurityGroups = Lude.Nothing,
      ec2SubnetIds = Lude.Nothing,
      hadoopVersion = Lude.Nothing,
      additionalMasterSecurityGroups = Lude.Nothing,
      emrManagedMasterSecurityGroup = Lude.Nothing,
      ec2SubnetId = Lude.Nothing,
      masterInstanceType = Lude.Nothing,
      instanceGroups = Lude.Nothing,
      keepJobFlowAliveWhenNoSteps = Lude.Nothing,
      serviceAccessSecurityGroup = Lude.Nothing,
      terminationProtected = Lude.Nothing,
      placement = Lude.Nothing
    }

-- | Describes the EC2 instances and instance configurations for clusters that use the instance fleet configuration.
--
-- /Note:/ Consider using 'instanceFleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficInstanceFleets :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe [InstanceFleetConfig])
jficInstanceFleets = Lens.lens (instanceFleets :: JobFlowInstancesConfig -> Lude.Maybe [InstanceFleetConfig]) (\s a -> s {instanceFleets = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficInstanceFleets "Use generic-lens or generic-optics with 'instanceFleets' instead." #-}

-- | The name of the EC2 key pair that can be used to connect to the master node using SSH as the user called "hadoop."
--
-- /Note:/ Consider using 'ec2KeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEC2KeyName :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficEC2KeyName = Lens.lens (ec2KeyName :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {ec2KeyName = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficEC2KeyName "Use generic-lens or generic-optics with 'ec2KeyName' instead." #-}

-- | The EC2 instance type of the core and task nodes.
--
-- /Note:/ Consider using 'slaveInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficSlaveInstanceType :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficSlaveInstanceType = Lens.lens (slaveInstanceType :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {slaveInstanceType = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficSlaveInstanceType "Use generic-lens or generic-optics with 'slaveInstanceType' instead." #-}

-- | The number of EC2 instances in the cluster.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficInstanceCount :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Int)
jficInstanceCount = Lens.lens (instanceCount :: JobFlowInstancesConfig -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The identifier of the Amazon EC2 security group for the core and task nodes.
--
-- /Note:/ Consider using 'emrManagedSlaveSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEmrManagedSlaveSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficEmrManagedSlaveSecurityGroup = Lens.lens (emrManagedSlaveSecurityGroup :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {emrManagedSlaveSecurityGroup = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficEmrManagedSlaveSecurityGroup "Use generic-lens or generic-optics with 'emrManagedSlaveSecurityGroup' instead." #-}

-- | A list of additional Amazon EC2 security group IDs for the core and task nodes.
--
-- /Note:/ Consider using 'additionalSlaveSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficAdditionalSlaveSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe [Lude.Text])
jficAdditionalSlaveSecurityGroups = Lens.lens (additionalSlaveSecurityGroups :: JobFlowInstancesConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalSlaveSecurityGroups = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficAdditionalSlaveSecurityGroups "Use generic-lens or generic-optics with 'additionalSlaveSecurityGroups' instead." #-}

-- | Applies to clusters that use the instance fleet configuration. When multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and launches instances in the optimal subnet.
--
-- /Note:/ Consider using 'ec2SubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEC2SubnetIds :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe [Lude.Text])
jficEC2SubnetIds = Lens.lens (ec2SubnetIds :: JobFlowInstancesConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {ec2SubnetIds = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficEC2SubnetIds "Use generic-lens or generic-optics with 'ec2SubnetIds' instead." #-}

-- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop version for the cluster. Valid inputs are "0.18" (no longer maintained), "0.20" (no longer maintained), "0.20.205" (no longer maintained), "1.0.3", "2.2.0", or "2.4.0". If you do not set this value, the default of 0.18 is used, unless the @AmiVersion@ parameter is set in the RunJobFlow call, in which case the default version of Hadoop for that AMI version is used.
--
-- /Note:/ Consider using 'hadoopVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficHadoopVersion :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficHadoopVersion = Lens.lens (hadoopVersion :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {hadoopVersion = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficHadoopVersion "Use generic-lens or generic-optics with 'hadoopVersion' instead." #-}

-- | A list of additional Amazon EC2 security group IDs for the master node.
--
-- /Note:/ Consider using 'additionalMasterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficAdditionalMasterSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe [Lude.Text])
jficAdditionalMasterSecurityGroups = Lens.lens (additionalMasterSecurityGroups :: JobFlowInstancesConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMasterSecurityGroups = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficAdditionalMasterSecurityGroups "Use generic-lens or generic-optics with 'additionalMasterSecurityGroups' instead." #-}

-- | The identifier of the Amazon EC2 security group for the master node.
--
-- /Note:/ Consider using 'emrManagedMasterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEmrManagedMasterSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficEmrManagedMasterSecurityGroup = Lens.lens (emrManagedMasterSecurityGroup :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {emrManagedMasterSecurityGroup = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficEmrManagedMasterSecurityGroup "Use generic-lens or generic-optics with 'emrManagedMasterSecurityGroup' instead." #-}

-- | Applies to clusters that use the uniform instance group configuration. To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value and your account supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- /Note:/ Consider using 'ec2SubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEC2SubnetId :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficEC2SubnetId = Lens.lens (ec2SubnetId :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {ec2SubnetId = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficEC2SubnetId "Use generic-lens or generic-optics with 'ec2SubnetId' instead." #-}

-- | The EC2 instance type of the master node.
--
-- /Note:/ Consider using 'masterInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficMasterInstanceType :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficMasterInstanceType = Lens.lens (masterInstanceType :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {masterInstanceType = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficMasterInstanceType "Use generic-lens or generic-optics with 'masterInstanceType' instead." #-}

-- | Configuration for the instance groups in a cluster.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficInstanceGroups :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe [InstanceGroupConfig])
jficInstanceGroups = Lens.lens (instanceGroups :: JobFlowInstancesConfig -> Lude.Maybe [InstanceGroupConfig]) (\s a -> s {instanceGroups = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficInstanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead." #-}

-- | Specifies whether the cluster should remain available after completing all steps.
--
-- /Note:/ Consider using 'keepJobFlowAliveWhenNoSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficKeepJobFlowAliveWhenNoSteps :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Bool)
jficKeepJobFlowAliveWhenNoSteps = Lens.lens (keepJobFlowAliveWhenNoSteps :: JobFlowInstancesConfig -> Lude.Maybe Lude.Bool) (\s a -> s {keepJobFlowAliveWhenNoSteps = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficKeepJobFlowAliveWhenNoSteps "Use generic-lens or generic-optics with 'keepJobFlowAliveWhenNoSteps' instead." #-}

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- /Note:/ Consider using 'serviceAccessSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficServiceAccessSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Text)
jficServiceAccessSecurityGroup = Lens.lens (serviceAccessSecurityGroup :: JobFlowInstancesConfig -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessSecurityGroup = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficServiceAccessSecurityGroup "Use generic-lens or generic-optics with 'serviceAccessSecurityGroup' instead." #-}

-- | Specifies whether to lock the cluster to prevent the Amazon EC2 instances from being terminated by API call, user intervention, or in the event of a job-flow error.
--
-- /Note:/ Consider using 'terminationProtected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficTerminationProtected :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe Lude.Bool)
jficTerminationProtected = Lens.lens (terminationProtected :: JobFlowInstancesConfig -> Lude.Maybe Lude.Bool) (\s a -> s {terminationProtected = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficTerminationProtected "Use generic-lens or generic-optics with 'terminationProtected' instead." #-}

-- | The Availability Zone in which the cluster runs.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficPlacement :: Lens.Lens' JobFlowInstancesConfig (Lude.Maybe PlacementType)
jficPlacement = Lens.lens (placement :: JobFlowInstancesConfig -> Lude.Maybe PlacementType) (\s a -> s {placement = a} :: JobFlowInstancesConfig)
{-# DEPRECATED jficPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.ToJSON JobFlowInstancesConfig where
  toJSON JobFlowInstancesConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceFleets" Lude..=) Lude.<$> instanceFleets,
            ("Ec2KeyName" Lude..=) Lude.<$> ec2KeyName,
            ("SlaveInstanceType" Lude..=) Lude.<$> slaveInstanceType,
            ("InstanceCount" Lude..=) Lude.<$> instanceCount,
            ("EmrManagedSlaveSecurityGroup" Lude..=)
              Lude.<$> emrManagedSlaveSecurityGroup,
            ("AdditionalSlaveSecurityGroups" Lude..=)
              Lude.<$> additionalSlaveSecurityGroups,
            ("Ec2SubnetIds" Lude..=) Lude.<$> ec2SubnetIds,
            ("HadoopVersion" Lude..=) Lude.<$> hadoopVersion,
            ("AdditionalMasterSecurityGroups" Lude..=)
              Lude.<$> additionalMasterSecurityGroups,
            ("EmrManagedMasterSecurityGroup" Lude..=)
              Lude.<$> emrManagedMasterSecurityGroup,
            ("Ec2SubnetId" Lude..=) Lude.<$> ec2SubnetId,
            ("MasterInstanceType" Lude..=) Lude.<$> masterInstanceType,
            ("InstanceGroups" Lude..=) Lude.<$> instanceGroups,
            ("KeepJobFlowAliveWhenNoSteps" Lude..=)
              Lude.<$> keepJobFlowAliveWhenNoSteps,
            ("ServiceAccessSecurityGroup" Lude..=)
              Lude.<$> serviceAccessSecurityGroup,
            ("TerminationProtected" Lude..=) Lude.<$> terminationProtected,
            ("Placement" Lude..=) Lude.<$> placement
          ]
      )
