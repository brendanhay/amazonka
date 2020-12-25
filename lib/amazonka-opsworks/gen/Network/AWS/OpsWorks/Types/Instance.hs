{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iAgentVersion,
    iAmiId,
    iArchitecture,
    iArn,
    iAutoScalingType,
    iAvailabilityZone,
    iBlockDeviceMappings,
    iCreatedAt,
    iEbsOptimized,
    iEc2InstanceId,
    iEcsClusterArn,
    iEcsContainerInstanceArn,
    iElasticIp,
    iHostname,
    iInfrastructureClass,
    iInstallUpdatesOnBoot,
    iInstanceId,
    iInstanceProfileArn,
    iInstanceType,
    iLastServiceErrorId,
    iLayerIds,
    iOs,
    iPlatform,
    iPrivateDns,
    iPrivateIp,
    iPublicDns,
    iPublicIp,
    iRegisteredBy,
    iReportedAgentVersion,
    iReportedOs,
    iRootDeviceType,
    iRootDeviceVolumeId,
    iSecurityGroupIds,
    iSshHostDsaKeyFingerprint,
    iSshHostRsaKeyFingerprint,
    iSshKeyName,
    iStackId,
    iStatus,
    iSubnetId,
    iTenancy,
    iVirtualizationType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.Architecture as Types
import qualified Network.AWS.OpsWorks.Types.AutoScalingType as Types
import qualified Network.AWS.OpsWorks.Types.BlockDeviceMapping as Types
import qualified Network.AWS.OpsWorks.Types.DateTime as Types
import qualified Network.AWS.OpsWorks.Types.ReportedOs as Types
import qualified Network.AWS.OpsWorks.Types.RootDeviceType as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.OpsWorks.Types.VirtualizationType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
    agentVersion :: Core.Maybe Types.String,
    -- | A custom AMI ID to be used to create the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
    amiId :: Core.Maybe Types.String,
    -- | The instance architecture: "i386" or "x86_64".
    architecture :: Core.Maybe Types.Architecture,
    -- | The instance's Amazon Resource Number (ARN).
    arn :: Core.Maybe Types.String,
    -- | For load-based or time-based instances, the type.
    autoScalingType :: Core.Maybe Types.AutoScalingType,
    -- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    availabilityZone :: Core.Maybe Types.String,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | The time that the instance was created.
    createdAt :: Core.Maybe Types.DateTime,
    -- | Whether this is an Amazon EBS-optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The ID of the associated Amazon EC2 instance.
    ec2InstanceId :: Core.Maybe Types.String,
    -- | For container instances, the Amazon ECS cluster's ARN.
    ecsClusterArn :: Core.Maybe Types.String,
    -- | For container instances, the instance's ARN.
    ecsContainerInstanceArn :: Core.Maybe Types.String,
    -- | The instance <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
    elasticIp :: Core.Maybe Types.String,
    -- | The instance host name.
    hostname :: Core.Maybe Types.String,
    -- | For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
    infrastructureClass :: Core.Maybe Types.String,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The instance ID.
    instanceId :: Core.Maybe Types.String,
    -- | The ARN of the instance's IAM profile. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    instanceProfileArn :: Core.Maybe Types.String,
    -- | The instance type, such as @t2.micro@ .
    instanceType :: Core.Maybe Types.String,
    -- | The ID of the last service error. For more information, call 'DescribeServiceErrors' .
    lastServiceErrorId :: Core.Maybe Types.String,
    -- | An array containing the instance layer IDs.
    layerIds :: Core.Maybe [Types.String],
    -- | The instance's operating system.
    os :: Core.Maybe Types.String,
    -- | The instance's platform.
    platform :: Core.Maybe Types.String,
    -- | The instance's private DNS name.
    privateDns :: Core.Maybe Types.String,
    -- | The instance's private IP address.
    privateIp :: Core.Maybe Types.String,
    -- | The instance public DNS name.
    publicDns :: Core.Maybe Types.String,
    -- | The instance public IP address.
    publicIp :: Core.Maybe Types.String,
    -- | For registered instances, who performed the registration.
    registeredBy :: Core.Maybe Types.String,
    -- | The instance's reported AWS OpsWorks Stacks agent version.
    reportedAgentVersion :: Core.Maybe Types.String,
    -- | For registered instances, the reported operating system.
    reportedOs :: Core.Maybe Types.ReportedOs,
    -- | The instance's root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    rootDeviceType :: Core.Maybe Types.RootDeviceType,
    -- | The root device volume ID.
    rootDeviceVolumeId :: Core.Maybe Types.String,
    -- | An array containing the instance security group IDs.
    securityGroupIds :: Core.Maybe [Types.String],
    -- | The SSH key's Deep Security Agent (DSA) fingerprint.
    sshHostDsaKeyFingerprint :: Core.Maybe Types.String,
    -- | The SSH key's RSA fingerprint.
    sshHostRsaKeyFingerprint :: Core.Maybe Types.String,
    -- | The instance's Amazon EC2 key-pair name.
    sshKeyName :: Core.Maybe Types.String,
    -- | The stack ID.
    stackId :: Core.Maybe Types.String,
    -- | The instance status:
    --
    --
    --     * @booting@
    --
    --
    --     * @connection_lost@
    --
    --
    --     * @online@
    --
    --
    --     * @pending@
    --
    --
    --     * @rebooting@
    --
    --
    --     * @requested@
    --
    --
    --     * @running_setup@
    --
    --
    --     * @setup_failed@
    --
    --
    --     * @shutting_down@
    --
    --
    --     * @start_failed@
    --
    --
    --     * @stop_failed@
    --
    --
    --     * @stopped@
    --
    --
    --     * @stopping@
    --
    --
    --     * @terminated@
    --
    --
    --     * @terminating@
    status :: Core.Maybe Types.String,
    -- | The instance's subnet ID; applicable only if the stack is running in a VPC.
    subnetId :: Core.Maybe Types.String,
    -- | The instance's tenancy option, such as @dedicated@ or @host@ .
    tenancy :: Core.Maybe Types.String,
    -- | The instance's virtualization type: @paravirtual@ or @hvm@ .
    virtualizationType :: Core.Maybe Types.VirtualizationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { agentVersion = Core.Nothing,
      amiId = Core.Nothing,
      architecture = Core.Nothing,
      arn = Core.Nothing,
      autoScalingType = Core.Nothing,
      availabilityZone = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      createdAt = Core.Nothing,
      ebsOptimized = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      ecsClusterArn = Core.Nothing,
      ecsContainerInstanceArn = Core.Nothing,
      elasticIp = Core.Nothing,
      hostname = Core.Nothing,
      infrastructureClass = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      instanceId = Core.Nothing,
      instanceProfileArn = Core.Nothing,
      instanceType = Core.Nothing,
      lastServiceErrorId = Core.Nothing,
      layerIds = Core.Nothing,
      os = Core.Nothing,
      platform = Core.Nothing,
      privateDns = Core.Nothing,
      privateIp = Core.Nothing,
      publicDns = Core.Nothing,
      publicIp = Core.Nothing,
      registeredBy = Core.Nothing,
      reportedAgentVersion = Core.Nothing,
      reportedOs = Core.Nothing,
      rootDeviceType = Core.Nothing,
      rootDeviceVolumeId = Core.Nothing,
      securityGroupIds = Core.Nothing,
      sshHostDsaKeyFingerprint = Core.Nothing,
      sshHostRsaKeyFingerprint = Core.Nothing,
      sshKeyName = Core.Nothing,
      stackId = Core.Nothing,
      status = Core.Nothing,
      subnetId = Core.Nothing,
      tenancy = Core.Nothing,
      virtualizationType = Core.Nothing
    }

-- | The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAgentVersion :: Lens.Lens' Instance (Core.Maybe Types.String)
iAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED iAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A custom AMI ID to be used to create the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAmiId :: Lens.Lens' Instance (Core.Maybe Types.String)
iAmiId = Lens.field @"amiId"
{-# DEPRECATED iAmiId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The instance architecture: "i386" or "x86_64".
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArchitecture :: Lens.Lens' Instance (Core.Maybe Types.Architecture)
iArchitecture = Lens.field @"architecture"
{-# DEPRECATED iArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The instance's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArn :: Lens.Lens' Instance (Core.Maybe Types.String)
iArn = Lens.field @"arn"
{-# DEPRECATED iArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | For load-based or time-based instances, the type.
--
-- /Note:/ Consider using 'autoScalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAutoScalingType :: Lens.Lens' Instance (Core.Maybe Types.AutoScalingType)
iAutoScalingType = Lens.field @"autoScalingType"
{-# DEPRECATED iAutoScalingType "Use generic-lens or generic-optics with 'autoScalingType' instead." #-}

-- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAvailabilityZone :: Lens.Lens' Instance (Core.Maybe Types.String)
iAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED iAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlockDeviceMappings :: Lens.Lens' Instance (Core.Maybe [Types.BlockDeviceMapping])
iBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED iBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The time that the instance was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedAt :: Lens.Lens' Instance (Core.Maybe Types.DateTime)
iCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED iCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Whether this is an Amazon EBS-optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEbsOptimized :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED iEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The ID of the associated Amazon EC2 instance.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEc2InstanceId :: Lens.Lens' Instance (Core.Maybe Types.String)
iEc2InstanceId = Lens.field @"ec2InstanceId"
{-# DEPRECATED iEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | For container instances, the Amazon ECS cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEcsClusterArn :: Lens.Lens' Instance (Core.Maybe Types.String)
iEcsClusterArn = Lens.field @"ecsClusterArn"
{-# DEPRECATED iEcsClusterArn "Use generic-lens or generic-optics with 'ecsClusterArn' instead." #-}

-- | For container instances, the instance's ARN.
--
-- /Note:/ Consider using 'ecsContainerInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEcsContainerInstanceArn :: Lens.Lens' Instance (Core.Maybe Types.String)
iEcsContainerInstanceArn = Lens.field @"ecsContainerInstanceArn"
{-# DEPRECATED iEcsContainerInstanceArn "Use generic-lens or generic-optics with 'ecsContainerInstanceArn' instead." #-}

-- | The instance <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iElasticIp :: Lens.Lens' Instance (Core.Maybe Types.String)
iElasticIp = Lens.field @"elasticIp"
{-# DEPRECATED iElasticIp "Use generic-lens or generic-optics with 'elasticIp' instead." #-}

-- | The instance host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHostname :: Lens.Lens' Instance (Core.Maybe Types.String)
iHostname = Lens.field @"hostname"
{-# DEPRECATED iHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
--
-- /Note:/ Consider using 'infrastructureClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInfrastructureClass :: Lens.Lens' Instance (Core.Maybe Types.String)
iInfrastructureClass = Lens.field @"infrastructureClass"
{-# DEPRECATED iInfrastructureClass "Use generic-lens or generic-optics with 'infrastructureClass' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstallUpdatesOnBoot :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# DEPRECATED iInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance (Core.Maybe Types.String)
iInstanceId = Lens.field @"instanceId"
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'instanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceProfileArn :: Lens.Lens' Instance (Core.Maybe Types.String)
iInstanceProfileArn = Lens.field @"instanceProfileArn"
{-# DEPRECATED iInstanceProfileArn "Use generic-lens or generic-optics with 'instanceProfileArn' instead." #-}

-- | The instance type, such as @t2.micro@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance (Core.Maybe Types.String)
iInstanceType = Lens.field @"instanceType"
{-# DEPRECATED iInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the last service error. For more information, call 'DescribeServiceErrors' .
--
-- /Note:/ Consider using 'lastServiceErrorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLastServiceErrorId :: Lens.Lens' Instance (Core.Maybe Types.String)
iLastServiceErrorId = Lens.field @"lastServiceErrorId"
{-# DEPRECATED iLastServiceErrorId "Use generic-lens or generic-optics with 'lastServiceErrorId' instead." #-}

-- | An array containing the instance layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLayerIds :: Lens.Lens' Instance (Core.Maybe [Types.String])
iLayerIds = Lens.field @"layerIds"
{-# DEPRECATED iLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The instance's operating system.
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOs :: Lens.Lens' Instance (Core.Maybe Types.String)
iOs = Lens.field @"os"
{-# DEPRECATED iOs "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The instance's platform.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlatform :: Lens.Lens' Instance (Core.Maybe Types.String)
iPlatform = Lens.field @"platform"
{-# DEPRECATED iPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The instance's private DNS name.
--
-- /Note:/ Consider using 'privateDns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDns :: Lens.Lens' Instance (Core.Maybe Types.String)
iPrivateDns = Lens.field @"privateDns"
{-# DEPRECATED iPrivateDns "Use generic-lens or generic-optics with 'privateDns' instead." #-}

-- | The instance's private IP address.
--
-- /Note:/ Consider using 'privateIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIp :: Lens.Lens' Instance (Core.Maybe Types.String)
iPrivateIp = Lens.field @"privateIp"
{-# DEPRECATED iPrivateIp "Use generic-lens or generic-optics with 'privateIp' instead." #-}

-- | The instance public DNS name.
--
-- /Note:/ Consider using 'publicDns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDns :: Lens.Lens' Instance (Core.Maybe Types.String)
iPublicDns = Lens.field @"publicDns"
{-# DEPRECATED iPublicDns "Use generic-lens or generic-optics with 'publicDns' instead." #-}

-- | The instance public IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIp :: Lens.Lens' Instance (Core.Maybe Types.String)
iPublicIp = Lens.field @"publicIp"
{-# DEPRECATED iPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | For registered instances, who performed the registration.
--
-- /Note:/ Consider using 'registeredBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegisteredBy :: Lens.Lens' Instance (Core.Maybe Types.String)
iRegisteredBy = Lens.field @"registeredBy"
{-# DEPRECATED iRegisteredBy "Use generic-lens or generic-optics with 'registeredBy' instead." #-}

-- | The instance's reported AWS OpsWorks Stacks agent version.
--
-- /Note:/ Consider using 'reportedAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iReportedAgentVersion :: Lens.Lens' Instance (Core.Maybe Types.String)
iReportedAgentVersion = Lens.field @"reportedAgentVersion"
{-# DEPRECATED iReportedAgentVersion "Use generic-lens or generic-optics with 'reportedAgentVersion' instead." #-}

-- | For registered instances, the reported operating system.
--
-- /Note:/ Consider using 'reportedOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iReportedOs :: Lens.Lens' Instance (Core.Maybe Types.ReportedOs)
iReportedOs = Lens.field @"reportedOs"
{-# DEPRECATED iReportedOs "Use generic-lens or generic-optics with 'reportedOs' instead." #-}

-- | The instance's root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceType :: Lens.Lens' Instance (Core.Maybe Types.RootDeviceType)
iRootDeviceType = Lens.field @"rootDeviceType"
{-# DEPRECATED iRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | The root device volume ID.
--
-- /Note:/ Consider using 'rootDeviceVolumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceVolumeId :: Lens.Lens' Instance (Core.Maybe Types.String)
iRootDeviceVolumeId = Lens.field @"rootDeviceVolumeId"
{-# DEPRECATED iRootDeviceVolumeId "Use generic-lens or generic-optics with 'rootDeviceVolumeId' instead." #-}

-- | An array containing the instance security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSecurityGroupIds :: Lens.Lens' Instance (Core.Maybe [Types.String])
iSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED iSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The SSH key's Deep Security Agent (DSA) fingerprint.
--
-- /Note:/ Consider using 'sshHostDsaKeyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSshHostDsaKeyFingerprint :: Lens.Lens' Instance (Core.Maybe Types.String)
iSshHostDsaKeyFingerprint = Lens.field @"sshHostDsaKeyFingerprint"
{-# DEPRECATED iSshHostDsaKeyFingerprint "Use generic-lens or generic-optics with 'sshHostDsaKeyFingerprint' instead." #-}

-- | The SSH key's RSA fingerprint.
--
-- /Note:/ Consider using 'sshHostRsaKeyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSshHostRsaKeyFingerprint :: Lens.Lens' Instance (Core.Maybe Types.String)
iSshHostRsaKeyFingerprint = Lens.field @"sshHostRsaKeyFingerprint"
{-# DEPRECATED iSshHostRsaKeyFingerprint "Use generic-lens or generic-optics with 'sshHostRsaKeyFingerprint' instead." #-}

-- | The instance's Amazon EC2 key-pair name.
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSshKeyName :: Lens.Lens' Instance (Core.Maybe Types.String)
iSshKeyName = Lens.field @"sshKeyName"
{-# DEPRECATED iSshKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStackId :: Lens.Lens' Instance (Core.Maybe Types.String)
iStackId = Lens.field @"stackId"
{-# DEPRECATED iStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The instance status:
--
--
--     * @booting@
--
--
--     * @connection_lost@
--
--
--     * @online@
--
--
--     * @pending@
--
--
--     * @rebooting@
--
--
--     * @requested@
--
--
--     * @running_setup@
--
--
--     * @setup_failed@
--
--
--     * @shutting_down@
--
--
--     * @start_failed@
--
--
--     * @stop_failed@
--
--
--     * @stopped@
--
--
--     * @stopping@
--
--
--     * @terminated@
--
--
--     * @terminating@
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Instance (Core.Maybe Types.String)
iStatus = Lens.field @"status"
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The instance's subnet ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSubnetId :: Lens.Lens' Instance (Core.Maybe Types.String)
iSubnetId = Lens.field @"subnetId"
{-# DEPRECATED iSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance's tenancy option, such as @dedicated@ or @host@ .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTenancy :: Lens.Lens' Instance (Core.Maybe Types.String)
iTenancy = Lens.field @"tenancy"
{-# DEPRECATED iTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The instance's virtualization type: @paravirtual@ or @hvm@ .
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVirtualizationType :: Lens.Lens' Instance (Core.Maybe Types.VirtualizationType)
iVirtualizationType = Lens.field @"virtualizationType"
{-# DEPRECATED iVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject "Instance" Core.$
      \x ->
        Instance'
          Core.<$> (x Core..:? "AgentVersion")
          Core.<*> (x Core..:? "AmiId")
          Core.<*> (x Core..:? "Architecture")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "AutoScalingType")
          Core.<*> (x Core..:? "AvailabilityZone")
          Core.<*> (x Core..:? "BlockDeviceMappings")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "EbsOptimized")
          Core.<*> (x Core..:? "Ec2InstanceId")
          Core.<*> (x Core..:? "EcsClusterArn")
          Core.<*> (x Core..:? "EcsContainerInstanceArn")
          Core.<*> (x Core..:? "ElasticIp")
          Core.<*> (x Core..:? "Hostname")
          Core.<*> (x Core..:? "InfrastructureClass")
          Core.<*> (x Core..:? "InstallUpdatesOnBoot")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "InstanceProfileArn")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "LastServiceErrorId")
          Core.<*> (x Core..:? "LayerIds")
          Core.<*> (x Core..:? "Os")
          Core.<*> (x Core..:? "Platform")
          Core.<*> (x Core..:? "PrivateDns")
          Core.<*> (x Core..:? "PrivateIp")
          Core.<*> (x Core..:? "PublicDns")
          Core.<*> (x Core..:? "PublicIp")
          Core.<*> (x Core..:? "RegisteredBy")
          Core.<*> (x Core..:? "ReportedAgentVersion")
          Core.<*> (x Core..:? "ReportedOs")
          Core.<*> (x Core..:? "RootDeviceType")
          Core.<*> (x Core..:? "RootDeviceVolumeId")
          Core.<*> (x Core..:? "SecurityGroupIds")
          Core.<*> (x Core..:? "SshHostDsaKeyFingerprint")
          Core.<*> (x Core..:? "SshHostRsaKeyFingerprint")
          Core.<*> (x Core..:? "SshKeyName")
          Core.<*> (x Core..:? "StackId")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "SubnetId")
          Core.<*> (x Core..:? "Tenancy")
          Core.<*> (x Core..:? "VirtualizationType")
