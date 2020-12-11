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
    iPrivateDNS,
    iReportedAgentVersion,
    iInstanceId,
    iStatus,
    iPrivateIP,
    iInstallUpdatesOnBoot,
    iVirtualizationType,
    iInstanceProfileARN,
    iPlatform,
    iHostname,
    iSSHHostRsaKeyFingerprint,
    iSecurityGroupIds,
    iEcsClusterARN,
    iARN,
    iCreatedAt,
    iEC2InstanceId,
    iSSHKeyName,
    iAgentVersion,
    iRootDeviceVolumeId,
    iSubnetId,
    iInfrastructureClass,
    iSSHHostDsaKeyFingerprint,
    iInstanceType,
    iEBSOptimized,
    iElasticIP,
    iOS,
    iAvailabilityZone,
    iLastServiceErrorId,
    iTenancy,
    iAutoScalingType,
    iLayerIds,
    iArchitecture,
    iPublicDNS,
    iAMIId,
    iPublicIP,
    iReportedOS,
    iRegisteredBy,
    iStackId,
    iRootDeviceType,
    iEcsContainerInstanceARN,
    iBlockDeviceMappings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.Architecture
import Network.AWS.OpsWorks.Types.AutoScalingType
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ReportedOS
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.VirtualizationType
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { privateDNS :: Lude.Maybe Lude.Text,
    reportedAgentVersion :: Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    privateIP :: Lude.Maybe Lude.Text,
    installUpdatesOnBoot :: Lude.Maybe Lude.Bool,
    virtualizationType :: Lude.Maybe VirtualizationType,
    instanceProfileARN :: Lude.Maybe Lude.Text,
    platform :: Lude.Maybe Lude.Text,
    hostname :: Lude.Maybe Lude.Text,
    sshHostRsaKeyFingerprint :: Lude.Maybe Lude.Text,
    securityGroupIds :: Lude.Maybe [Lude.Text],
    ecsClusterARN :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    ec2InstanceId :: Lude.Maybe Lude.Text,
    sshKeyName :: Lude.Maybe Lude.Text,
    agentVersion :: Lude.Maybe Lude.Text,
    rootDeviceVolumeId :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    infrastructureClass :: Lude.Maybe Lude.Text,
    sshHostDsaKeyFingerprint :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    ebsOptimized :: Lude.Maybe Lude.Bool,
    elasticIP :: Lude.Maybe Lude.Text,
    os :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    lastServiceErrorId :: Lude.Maybe Lude.Text,
    tenancy :: Lude.Maybe Lude.Text,
    autoScalingType :: Lude.Maybe AutoScalingType,
    layerIds :: Lude.Maybe [Lude.Text],
    architecture :: Lude.Maybe Architecture,
    publicDNS :: Lude.Maybe Lude.Text,
    amiId :: Lude.Maybe Lude.Text,
    publicIP :: Lude.Maybe Lude.Text,
    reportedOS :: Lude.Maybe ReportedOS,
    registeredBy :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    rootDeviceType :: Lude.Maybe RootDeviceType,
    ecsContainerInstanceARN :: Lude.Maybe Lude.Text,
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'agentVersion' - The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
-- * 'amiId' - A custom AMI ID to be used to create the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
-- * 'architecture' - The instance architecture: "i386" or "x86_64".
-- * 'arn' - The instance's Amazon Resource Number (ARN).
-- * 'autoScalingType' - For load-based or time-based instances, the type.
-- * 'availabilityZone' - The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'blockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
-- * 'createdAt' - The time that the instance was created.
-- * 'ebsOptimized' - Whether this is an Amazon EBS-optimized instance.
-- * 'ec2InstanceId' - The ID of the associated Amazon EC2 instance.
-- * 'ecsClusterARN' - For container instances, the Amazon ECS cluster's ARN.
-- * 'ecsContainerInstanceARN' - For container instances, the instance's ARN.
-- * 'elasticIP' - The instance <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
-- * 'hostname' - The instance host name.
-- * 'infrastructureClass' - For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
-- * 'installUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
-- * 'instanceId' - The instance ID.
-- * 'instanceProfileARN' - The ARN of the instance's IAM profile. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'instanceType' - The instance type, such as @t2.micro@ .
-- * 'lastServiceErrorId' - The ID of the last service error. For more information, call 'DescribeServiceErrors' .
-- * 'layerIds' - An array containing the instance layer IDs.
-- * 'os' - The instance's operating system.
-- * 'platform' - The instance's platform.
-- * 'privateDNS' - The instance's private DNS name.
-- * 'privateIP' - The instance's private IP address.
-- * 'publicDNS' - The instance public DNS name.
-- * 'publicIP' - The instance public IP address.
-- * 'registeredBy' - For registered instances, who performed the registration.
-- * 'reportedAgentVersion' - The instance's reported AWS OpsWorks Stacks agent version.
-- * 'reportedOS' - For registered instances, the reported operating system.
-- * 'rootDeviceType' - The instance's root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
-- * 'rootDeviceVolumeId' - The root device volume ID.
-- * 'securityGroupIds' - An array containing the instance security group IDs.
-- * 'sshHostDsaKeyFingerprint' - The SSH key's Deep Security Agent (DSA) fingerprint.
-- * 'sshHostRsaKeyFingerprint' - The SSH key's RSA fingerprint.
-- * 'sshKeyName' - The instance's Amazon EC2 key-pair name.
-- * 'stackId' - The stack ID.
-- * 'status' - The instance status:
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
-- * 'subnetId' - The instance's subnet ID; applicable only if the stack is running in a VPC.
-- * 'tenancy' - The instance's tenancy option, such as @dedicated@ or @host@ .
-- * 'virtualizationType' - The instance's virtualization type: @paravirtual@ or @hvm@ .
mkInstance ::
  Instance
mkInstance =
  Instance'
    { privateDNS = Lude.Nothing,
      reportedAgentVersion = Lude.Nothing,
      instanceId = Lude.Nothing,
      status = Lude.Nothing,
      privateIP = Lude.Nothing,
      installUpdatesOnBoot = Lude.Nothing,
      virtualizationType = Lude.Nothing,
      instanceProfileARN = Lude.Nothing,
      platform = Lude.Nothing,
      hostname = Lude.Nothing,
      sshHostRsaKeyFingerprint = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      ecsClusterARN = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      sshKeyName = Lude.Nothing,
      agentVersion = Lude.Nothing,
      rootDeviceVolumeId = Lude.Nothing,
      subnetId = Lude.Nothing,
      infrastructureClass = Lude.Nothing,
      sshHostDsaKeyFingerprint = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      elasticIP = Lude.Nothing,
      os = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      lastServiceErrorId = Lude.Nothing,
      tenancy = Lude.Nothing,
      autoScalingType = Lude.Nothing,
      layerIds = Lude.Nothing,
      architecture = Lude.Nothing,
      publicDNS = Lude.Nothing,
      amiId = Lude.Nothing,
      publicIP = Lude.Nothing,
      reportedOS = Lude.Nothing,
      registeredBy = Lude.Nothing,
      stackId = Lude.Nothing,
      rootDeviceType = Lude.Nothing,
      ecsContainerInstanceARN = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing
    }

-- | The instance's private DNS name.
--
-- /Note:/ Consider using 'privateDNS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDNS :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPrivateDNS = Lens.lens (privateDNS :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateDNS = a} :: Instance)
{-# DEPRECATED iPrivateDNS "Use generic-lens or generic-optics with 'privateDNS' instead." #-}

-- | The instance's reported AWS OpsWorks Stacks agent version.
--
-- /Note:/ Consider using 'reportedAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iReportedAgentVersion :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iReportedAgentVersion = Lens.lens (reportedAgentVersion :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {reportedAgentVersion = a} :: Instance)
{-# DEPRECATED iReportedAgentVersion "Use generic-lens or generic-optics with 'reportedAgentVersion' instead." #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceId = Lens.lens (instanceId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Instance)
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

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
iStatus :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iStatus = Lens.lens (status :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Instance)
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The instance's private IP address.
--
-- /Note:/ Consider using 'privateIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIP :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPrivateIP = Lens.lens (privateIP :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateIP = a} :: Instance)
{-# DEPRECATED iPrivateIP "Use generic-lens or generic-optics with 'privateIP' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstallUpdatesOnBoot :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
iInstallUpdatesOnBoot = Lens.lens (installUpdatesOnBoot :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {installUpdatesOnBoot = a} :: Instance)
{-# DEPRECATED iInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The instance's virtualization type: @paravirtual@ or @hvm@ .
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iVirtualizationType :: Lens.Lens' Instance (Lude.Maybe VirtualizationType)
iVirtualizationType = Lens.lens (virtualizationType :: Instance -> Lude.Maybe VirtualizationType) (\s a -> s {virtualizationType = a} :: Instance)
{-# DEPRECATED iVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'instanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceProfileARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceProfileARN = Lens.lens (instanceProfileARN :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceProfileARN = a} :: Instance)
{-# DEPRECATED iInstanceProfileARN "Use generic-lens or generic-optics with 'instanceProfileARN' instead." #-}

-- | The instance's platform.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPlatform :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPlatform = Lens.lens (platform :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: Instance)
{-# DEPRECATED iPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The instance host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHostname :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iHostname = Lens.lens (hostname :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: Instance)
{-# DEPRECATED iHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The SSH key's RSA fingerprint.
--
-- /Note:/ Consider using 'sshHostRsaKeyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSSHHostRsaKeyFingerprint :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iSSHHostRsaKeyFingerprint = Lens.lens (sshHostRsaKeyFingerprint :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {sshHostRsaKeyFingerprint = a} :: Instance)
{-# DEPRECATED iSSHHostRsaKeyFingerprint "Use generic-lens or generic-optics with 'sshHostRsaKeyFingerprint' instead." #-}

-- | An array containing the instance security group IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSecurityGroupIds :: Lens.Lens' Instance (Lude.Maybe [Lude.Text])
iSecurityGroupIds = Lens.lens (securityGroupIds :: Instance -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: Instance)
{-# DEPRECATED iSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | For container instances, the Amazon ECS cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEcsClusterARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iEcsClusterARN = Lens.lens (ecsClusterARN :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ecsClusterARN = a} :: Instance)
{-# DEPRECATED iEcsClusterARN "Use generic-lens or generic-optics with 'ecsClusterARN' instead." #-}

-- | The instance's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iARN = Lens.lens (arn :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Instance)
{-# DEPRECATED iARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time that the instance was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedAt :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iCreatedAt = Lens.lens (createdAt :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Instance)
{-# DEPRECATED iCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the associated Amazon EC2 instance.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEC2InstanceId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iEC2InstanceId = Lens.lens (ec2InstanceId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: Instance)
{-# DEPRECATED iEC2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The instance's Amazon EC2 key-pair name.
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSSHKeyName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iSSHKeyName = Lens.lens (sshKeyName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {sshKeyName = a} :: Instance)
{-# DEPRECATED iSSHKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

-- | The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAgentVersion :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iAgentVersion = Lens.lens (agentVersion :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: Instance)
{-# DEPRECATED iAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The root device volume ID.
--
-- /Note:/ Consider using 'rootDeviceVolumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceVolumeId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iRootDeviceVolumeId = Lens.lens (rootDeviceVolumeId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {rootDeviceVolumeId = a} :: Instance)
{-# DEPRECATED iRootDeviceVolumeId "Use generic-lens or generic-optics with 'rootDeviceVolumeId' instead." #-}

-- | The instance's subnet ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSubnetId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iSubnetId = Lens.lens (subnetId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: Instance)
{-# DEPRECATED iSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
--
-- /Note:/ Consider using 'infrastructureClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInfrastructureClass :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInfrastructureClass = Lens.lens (infrastructureClass :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {infrastructureClass = a} :: Instance)
{-# DEPRECATED iInfrastructureClass "Use generic-lens or generic-optics with 'infrastructureClass' instead." #-}

-- | The SSH key's Deep Security Agent (DSA) fingerprint.
--
-- /Note:/ Consider using 'sshHostDsaKeyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSSHHostDsaKeyFingerprint :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iSSHHostDsaKeyFingerprint = Lens.lens (sshHostDsaKeyFingerprint :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {sshHostDsaKeyFingerprint = a} :: Instance)
{-# DEPRECATED iSSHHostDsaKeyFingerprint "Use generic-lens or generic-optics with 'sshHostDsaKeyFingerprint' instead." #-}

-- | The instance type, such as @t2.micro@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceType = Lens.lens (instanceType :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: Instance)
{-# DEPRECATED iInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Whether this is an Amazon EBS-optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEBSOptimized :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
iEBSOptimized = Lens.lens (ebsOptimized :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: Instance)
{-# DEPRECATED iEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The instance <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iElasticIP :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iElasticIP = Lens.lens (elasticIP :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {elasticIP = a} :: Instance)
{-# DEPRECATED iElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The instance's operating system.
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOS :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iOS = Lens.lens (os :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {os = a} :: Instance)
{-# DEPRECATED iOS "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAvailabilityZone :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iAvailabilityZone = Lens.lens (availabilityZone :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Instance)
{-# DEPRECATED iAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the last service error. For more information, call 'DescribeServiceErrors' .
--
-- /Note:/ Consider using 'lastServiceErrorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLastServiceErrorId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iLastServiceErrorId = Lens.lens (lastServiceErrorId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {lastServiceErrorId = a} :: Instance)
{-# DEPRECATED iLastServiceErrorId "Use generic-lens or generic-optics with 'lastServiceErrorId' instead." #-}

-- | The instance's tenancy option, such as @dedicated@ or @host@ .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTenancy :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iTenancy = Lens.lens (tenancy :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {tenancy = a} :: Instance)
{-# DEPRECATED iTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | For load-based or time-based instances, the type.
--
-- /Note:/ Consider using 'autoScalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAutoScalingType :: Lens.Lens' Instance (Lude.Maybe AutoScalingType)
iAutoScalingType = Lens.lens (autoScalingType :: Instance -> Lude.Maybe AutoScalingType) (\s a -> s {autoScalingType = a} :: Instance)
{-# DEPRECATED iAutoScalingType "Use generic-lens or generic-optics with 'autoScalingType' instead." #-}

-- | An array containing the instance layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLayerIds :: Lens.Lens' Instance (Lude.Maybe [Lude.Text])
iLayerIds = Lens.lens (layerIds :: Instance -> Lude.Maybe [Lude.Text]) (\s a -> s {layerIds = a} :: Instance)
{-# DEPRECATED iLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The instance architecture: "i386" or "x86_64".
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArchitecture :: Lens.Lens' Instance (Lude.Maybe Architecture)
iArchitecture = Lens.lens (architecture :: Instance -> Lude.Maybe Architecture) (\s a -> s {architecture = a} :: Instance)
{-# DEPRECATED iArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The instance public DNS name.
--
-- /Note:/ Consider using 'publicDNS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDNS :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPublicDNS = Lens.lens (publicDNS :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicDNS = a} :: Instance)
{-# DEPRECATED iPublicDNS "Use generic-lens or generic-optics with 'publicDNS' instead." #-}

-- | A custom AMI ID to be used to create the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAMIId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iAMIId = Lens.lens (amiId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {amiId = a} :: Instance)
{-# DEPRECATED iAMIId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The instance public IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIP :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPublicIP = Lens.lens (publicIP :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: Instance)
{-# DEPRECATED iPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | For registered instances, the reported operating system.
--
-- /Note:/ Consider using 'reportedOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iReportedOS :: Lens.Lens' Instance (Lude.Maybe ReportedOS)
iReportedOS = Lens.lens (reportedOS :: Instance -> Lude.Maybe ReportedOS) (\s a -> s {reportedOS = a} :: Instance)
{-# DEPRECATED iReportedOS "Use generic-lens or generic-optics with 'reportedOS' instead." #-}

-- | For registered instances, who performed the registration.
--
-- /Note:/ Consider using 'registeredBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegisteredBy :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iRegisteredBy = Lens.lens (registeredBy :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {registeredBy = a} :: Instance)
{-# DEPRECATED iRegisteredBy "Use generic-lens or generic-optics with 'registeredBy' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStackId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iStackId = Lens.lens (stackId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Instance)
{-# DEPRECATED iStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The instance's root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootDeviceType :: Lens.Lens' Instance (Lude.Maybe RootDeviceType)
iRootDeviceType = Lens.lens (rootDeviceType :: Instance -> Lude.Maybe RootDeviceType) (\s a -> s {rootDeviceType = a} :: Instance)
{-# DEPRECATED iRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | For container instances, the instance's ARN.
--
-- /Note:/ Consider using 'ecsContainerInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEcsContainerInstanceARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iEcsContainerInstanceARN = Lens.lens (ecsContainerInstanceARN :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ecsContainerInstanceARN = a} :: Instance)
{-# DEPRECATED iEcsContainerInstanceARN "Use generic-lens or generic-optics with 'ecsContainerInstanceARN' instead." #-}

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlockDeviceMappings :: Lens.Lens' Instance (Lude.Maybe [BlockDeviceMapping])
iBlockDeviceMappings = Lens.lens (blockDeviceMappings :: Instance -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: Instance)
{-# DEPRECATED iBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

instance Lude.FromJSON Instance where
  parseJSON =
    Lude.withObject
      "Instance"
      ( \x ->
          Instance'
            Lude.<$> (x Lude..:? "PrivateDns")
            Lude.<*> (x Lude..:? "ReportedAgentVersion")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PrivateIp")
            Lude.<*> (x Lude..:? "InstallUpdatesOnBoot")
            Lude.<*> (x Lude..:? "VirtualizationType")
            Lude.<*> (x Lude..:? "InstanceProfileArn")
            Lude.<*> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "Hostname")
            Lude.<*> (x Lude..:? "SshHostRsaKeyFingerprint")
            Lude.<*> (x Lude..:? "SecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EcsClusterArn")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Ec2InstanceId")
            Lude.<*> (x Lude..:? "SshKeyName")
            Lude.<*> (x Lude..:? "AgentVersion")
            Lude.<*> (x Lude..:? "RootDeviceVolumeId")
            Lude.<*> (x Lude..:? "SubnetId")
            Lude.<*> (x Lude..:? "InfrastructureClass")
            Lude.<*> (x Lude..:? "SshHostDsaKeyFingerprint")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "EbsOptimized")
            Lude.<*> (x Lude..:? "ElasticIp")
            Lude.<*> (x Lude..:? "Os")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "LastServiceErrorId")
            Lude.<*> (x Lude..:? "Tenancy")
            Lude.<*> (x Lude..:? "AutoScalingType")
            Lude.<*> (x Lude..:? "LayerIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Architecture")
            Lude.<*> (x Lude..:? "PublicDns")
            Lude.<*> (x Lude..:? "AmiId")
            Lude.<*> (x Lude..:? "PublicIp")
            Lude.<*> (x Lude..:? "ReportedOs")
            Lude.<*> (x Lude..:? "RegisteredBy")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "RootDeviceType")
            Lude.<*> (x Lude..:? "EcsContainerInstanceArn")
            Lude.<*> (x Lude..:? "BlockDeviceMappings" Lude..!= Lude.mempty)
      )
