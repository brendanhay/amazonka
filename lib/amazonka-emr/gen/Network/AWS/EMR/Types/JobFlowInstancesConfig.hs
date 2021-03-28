{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.JobFlowInstancesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.JobFlowInstancesConfig
  ( JobFlowInstancesConfig (..)
  -- * Smart constructor
  , mkJobFlowInstancesConfig
  -- * Lenses
  , jficAdditionalMasterSecurityGroups
  , jficAdditionalSlaveSecurityGroups
  , jficEc2KeyName
  , jficEc2SubnetId
  , jficEc2SubnetIds
  , jficEmrManagedMasterSecurityGroup
  , jficEmrManagedSlaveSecurityGroup
  , jficHadoopVersion
  , jficInstanceCount
  , jficInstanceFleets
  , jficInstanceGroups
  , jficKeepJobFlowAliveWhenNoSteps
  , jficMasterInstanceType
  , jficPlacement
  , jficServiceAccessSecurityGroup
  , jficSlaveInstanceType
  , jficTerminationProtected
  ) where

import qualified Network.AWS.EMR.Types.InstanceFleetConfig as Types
import qualified Network.AWS.EMR.Types.InstanceGroupConfig as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.EMR.Types.PlacementType as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A description of the Amazon EC2 instance on which the cluster (job flow) runs. A valid JobFlowInstancesConfig must contain either InstanceGroups or InstanceFleets. They cannot be used together. You may also have MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must be present), but we don't recommend this configuration.
--
-- /See:/ 'mkJobFlowInstancesConfig' smart constructor.
data JobFlowInstancesConfig = JobFlowInstancesConfig'
  { additionalMasterSecurityGroups :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ A list of additional Amazon EC2 security group IDs for the master node.
  , additionalSlaveSecurityGroups :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ A list of additional Amazon EC2 security group IDs for the core and task nodes.
  , ec2KeyName :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The name of the EC2 key pair that can be used to connect to the master node using SSH as the user called "hadoop."
  , ec2SubnetId :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ Applies to clusters that use the uniform instance group configuration. To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value and your account supports EC2-Classic, the cluster launches in EC2-Classic.
  , ec2SubnetIds :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ Applies to clusters that use the instance fleet configuration. When multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and launches instances in the optimal subnet.
  , emrManagedMasterSecurityGroup :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The identifier of the Amazon EC2 security group for the master node.
  , emrManagedSlaveSecurityGroup :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The identifier of the Amazon EC2 security group for the core and task nodes.
  , hadoopVersion :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop version for the cluster. Valid inputs are "0.18" (no longer maintained), "0.20" (no longer maintained), "0.20.205" (no longer maintained), "1.0.3", "2.2.0", or "2.4.0". If you do not set this value, the default of 0.18 is used, unless the @AmiVersion@ parameter is set in the RunJobFlow call, in which case the default version of Hadoop for that AMI version is used.
  , instanceCount :: Core.Maybe Core.Int
    -- ^ The number of EC2 instances in the cluster.
  , instanceFleets :: Core.Maybe [Types.InstanceFleetConfig]
    -- ^ Describes the EC2 instances and instance configurations for clusters that use the instance fleet configuration.
  , instanceGroups :: Core.Maybe [Types.InstanceGroupConfig]
    -- ^ Configuration for the instance groups in a cluster.
  , keepJobFlowAliveWhenNoSteps :: Core.Maybe Core.Bool
    -- ^ Specifies whether the cluster should remain available after completing all steps.
  , masterInstanceType :: Core.Maybe Types.InstanceType
    -- ^ The EC2 instance type of the master node.
  , placement :: Core.Maybe Types.PlacementType
    -- ^ The Availability Zone in which the cluster runs.
  , serviceAccessSecurityGroup :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
  , slaveInstanceType :: Core.Maybe Types.InstanceType
    -- ^ The EC2 instance type of the core and task nodes.
  , terminationProtected :: Core.Maybe Core.Bool
    -- ^ Specifies whether to lock the cluster to prevent the Amazon EC2 instances from being terminated by API call, user intervention, or in the event of a job-flow error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobFlowInstancesConfig' value with any optional fields omitted.
mkJobFlowInstancesConfig
    :: JobFlowInstancesConfig
mkJobFlowInstancesConfig
  = JobFlowInstancesConfig'{additionalMasterSecurityGroups =
                              Core.Nothing,
                            additionalSlaveSecurityGroups = Core.Nothing,
                            ec2KeyName = Core.Nothing, ec2SubnetId = Core.Nothing,
                            ec2SubnetIds = Core.Nothing,
                            emrManagedMasterSecurityGroup = Core.Nothing,
                            emrManagedSlaveSecurityGroup = Core.Nothing,
                            hadoopVersion = Core.Nothing, instanceCount = Core.Nothing,
                            instanceFleets = Core.Nothing, instanceGroups = Core.Nothing,
                            keepJobFlowAliveWhenNoSteps = Core.Nothing,
                            masterInstanceType = Core.Nothing, placement = Core.Nothing,
                            serviceAccessSecurityGroup = Core.Nothing,
                            slaveInstanceType = Core.Nothing,
                            terminationProtected = Core.Nothing}

-- | A list of additional Amazon EC2 security group IDs for the master node.
--
-- /Note:/ Consider using 'additionalMasterSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficAdditionalMasterSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Types.XmlStringMaxLen256])
jficAdditionalMasterSecurityGroups = Lens.field @"additionalMasterSecurityGroups"
{-# INLINEABLE jficAdditionalMasterSecurityGroups #-}
{-# DEPRECATED additionalMasterSecurityGroups "Use generic-lens or generic-optics with 'additionalMasterSecurityGroups' instead"  #-}

-- | A list of additional Amazon EC2 security group IDs for the core and task nodes.
--
-- /Note:/ Consider using 'additionalSlaveSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficAdditionalSlaveSecurityGroups :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Types.XmlStringMaxLen256])
jficAdditionalSlaveSecurityGroups = Lens.field @"additionalSlaveSecurityGroups"
{-# INLINEABLE jficAdditionalSlaveSecurityGroups #-}
{-# DEPRECATED additionalSlaveSecurityGroups "Use generic-lens or generic-optics with 'additionalSlaveSecurityGroups' instead"  #-}

-- | The name of the EC2 key pair that can be used to connect to the master node using SSH as the user called "hadoop."
--
-- /Note:/ Consider using 'ec2KeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEc2KeyName :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.XmlStringMaxLen256)
jficEc2KeyName = Lens.field @"ec2KeyName"
{-# INLINEABLE jficEc2KeyName #-}
{-# DEPRECATED ec2KeyName "Use generic-lens or generic-optics with 'ec2KeyName' instead"  #-}

-- | Applies to clusters that use the uniform instance group configuration. To launch the cluster in Amazon Virtual Private Cloud (Amazon VPC), set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value and your account supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- /Note:/ Consider using 'ec2SubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEc2SubnetId :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.XmlStringMaxLen256)
jficEc2SubnetId = Lens.field @"ec2SubnetId"
{-# INLINEABLE jficEc2SubnetId #-}
{-# DEPRECATED ec2SubnetId "Use generic-lens or generic-optics with 'ec2SubnetId' instead"  #-}

-- | Applies to clusters that use the instance fleet configuration. When multiple EC2 subnet IDs are specified, Amazon EMR evaluates them and launches instances in the optimal subnet.
--
-- /Note:/ Consider using 'ec2SubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEc2SubnetIds :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Types.XmlStringMaxLen256])
jficEc2SubnetIds = Lens.field @"ec2SubnetIds"
{-# INLINEABLE jficEc2SubnetIds #-}
{-# DEPRECATED ec2SubnetIds "Use generic-lens or generic-optics with 'ec2SubnetIds' instead"  #-}

-- | The identifier of the Amazon EC2 security group for the master node.
--
-- /Note:/ Consider using 'emrManagedMasterSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEmrManagedMasterSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.XmlStringMaxLen256)
jficEmrManagedMasterSecurityGroup = Lens.field @"emrManagedMasterSecurityGroup"
{-# INLINEABLE jficEmrManagedMasterSecurityGroup #-}
{-# DEPRECATED emrManagedMasterSecurityGroup "Use generic-lens or generic-optics with 'emrManagedMasterSecurityGroup' instead"  #-}

-- | The identifier of the Amazon EC2 security group for the core and task nodes.
--
-- /Note:/ Consider using 'emrManagedSlaveSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficEmrManagedSlaveSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.XmlStringMaxLen256)
jficEmrManagedSlaveSecurityGroup = Lens.field @"emrManagedSlaveSecurityGroup"
{-# INLINEABLE jficEmrManagedSlaveSecurityGroup #-}
{-# DEPRECATED emrManagedSlaveSecurityGroup "Use generic-lens or generic-optics with 'emrManagedSlaveSecurityGroup' instead"  #-}

-- | Applies only to Amazon EMR release versions earlier than 4.0. The Hadoop version for the cluster. Valid inputs are "0.18" (no longer maintained), "0.20" (no longer maintained), "0.20.205" (no longer maintained), "1.0.3", "2.2.0", or "2.4.0". If you do not set this value, the default of 0.18 is used, unless the @AmiVersion@ parameter is set in the RunJobFlow call, in which case the default version of Hadoop for that AMI version is used.
--
-- /Note:/ Consider using 'hadoopVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficHadoopVersion :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.XmlStringMaxLen256)
jficHadoopVersion = Lens.field @"hadoopVersion"
{-# INLINEABLE jficHadoopVersion #-}
{-# DEPRECATED hadoopVersion "Use generic-lens or generic-optics with 'hadoopVersion' instead"  #-}

-- | The number of EC2 instances in the cluster.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficInstanceCount :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Int)
jficInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE jficInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | Describes the EC2 instances and instance configurations for clusters that use the instance fleet configuration.
--
-- /Note:/ Consider using 'instanceFleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficInstanceFleets :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Types.InstanceFleetConfig])
jficInstanceFleets = Lens.field @"instanceFleets"
{-# INLINEABLE jficInstanceFleets #-}
{-# DEPRECATED instanceFleets "Use generic-lens or generic-optics with 'instanceFleets' instead"  #-}

-- | Configuration for the instance groups in a cluster.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficInstanceGroups :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe [Types.InstanceGroupConfig])
jficInstanceGroups = Lens.field @"instanceGroups"
{-# INLINEABLE jficInstanceGroups #-}
{-# DEPRECATED instanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead"  #-}

-- | Specifies whether the cluster should remain available after completing all steps.
--
-- /Note:/ Consider using 'keepJobFlowAliveWhenNoSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficKeepJobFlowAliveWhenNoSteps :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Bool)
jficKeepJobFlowAliveWhenNoSteps = Lens.field @"keepJobFlowAliveWhenNoSteps"
{-# INLINEABLE jficKeepJobFlowAliveWhenNoSteps #-}
{-# DEPRECATED keepJobFlowAliveWhenNoSteps "Use generic-lens or generic-optics with 'keepJobFlowAliveWhenNoSteps' instead"  #-}

-- | The EC2 instance type of the master node.
--
-- /Note:/ Consider using 'masterInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficMasterInstanceType :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.InstanceType)
jficMasterInstanceType = Lens.field @"masterInstanceType"
{-# INLINEABLE jficMasterInstanceType #-}
{-# DEPRECATED masterInstanceType "Use generic-lens or generic-optics with 'masterInstanceType' instead"  #-}

-- | The Availability Zone in which the cluster runs.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficPlacement :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.PlacementType)
jficPlacement = Lens.field @"placement"
{-# INLINEABLE jficPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- /Note:/ Consider using 'serviceAccessSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficServiceAccessSecurityGroup :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.XmlStringMaxLen256)
jficServiceAccessSecurityGroup = Lens.field @"serviceAccessSecurityGroup"
{-# INLINEABLE jficServiceAccessSecurityGroup #-}
{-# DEPRECATED serviceAccessSecurityGroup "Use generic-lens or generic-optics with 'serviceAccessSecurityGroup' instead"  #-}

-- | The EC2 instance type of the core and task nodes.
--
-- /Note:/ Consider using 'slaveInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficSlaveInstanceType :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Types.InstanceType)
jficSlaveInstanceType = Lens.field @"slaveInstanceType"
{-# INLINEABLE jficSlaveInstanceType #-}
{-# DEPRECATED slaveInstanceType "Use generic-lens or generic-optics with 'slaveInstanceType' instead"  #-}

-- | Specifies whether to lock the cluster to prevent the Amazon EC2 instances from being terminated by API call, user intervention, or in the event of a job-flow error.
--
-- /Note:/ Consider using 'terminationProtected' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jficTerminationProtected :: Lens.Lens' JobFlowInstancesConfig (Core.Maybe Core.Bool)
jficTerminationProtected = Lens.field @"terminationProtected"
{-# INLINEABLE jficTerminationProtected #-}
{-# DEPRECATED terminationProtected "Use generic-lens or generic-optics with 'terminationProtected' instead"  #-}

instance Core.FromJSON JobFlowInstancesConfig where
        toJSON JobFlowInstancesConfig{..}
          = Core.object
              (Core.catMaybes
                 [("AdditionalMasterSecurityGroups" Core..=) Core.<$>
                    additionalMasterSecurityGroups,
                  ("AdditionalSlaveSecurityGroups" Core..=) Core.<$>
                    additionalSlaveSecurityGroups,
                  ("Ec2KeyName" Core..=) Core.<$> ec2KeyName,
                  ("Ec2SubnetId" Core..=) Core.<$> ec2SubnetId,
                  ("Ec2SubnetIds" Core..=) Core.<$> ec2SubnetIds,
                  ("EmrManagedMasterSecurityGroup" Core..=) Core.<$>
                    emrManagedMasterSecurityGroup,
                  ("EmrManagedSlaveSecurityGroup" Core..=) Core.<$>
                    emrManagedSlaveSecurityGroup,
                  ("HadoopVersion" Core..=) Core.<$> hadoopVersion,
                  ("InstanceCount" Core..=) Core.<$> instanceCount,
                  ("InstanceFleets" Core..=) Core.<$> instanceFleets,
                  ("InstanceGroups" Core..=) Core.<$> instanceGroups,
                  ("KeepJobFlowAliveWhenNoSteps" Core..=) Core.<$>
                    keepJobFlowAliveWhenNoSteps,
                  ("MasterInstanceType" Core..=) Core.<$> masterInstanceType,
                  ("Placement" Core..=) Core.<$> placement,
                  ("ServiceAccessSecurityGroup" Core..=) Core.<$>
                    serviceAccessSecurityGroup,
                  ("SlaveInstanceType" Core..=) Core.<$> slaveInstanceType,
                  ("TerminationProtected" Core..=) Core.<$> terminationProtected])
