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
-- Module      : Network.AWS.OpsWorks.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Instance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.Architecture
import Network.AWS.OpsWorks.Types.AutoScalingType
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ReportedOs
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.VirtualizationType

-- | Describes an instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The instance host name.
    hostname :: Core.Maybe Core.Text,
    -- | The instance\'s platform.
    platform :: Core.Maybe Core.Text,
    -- | An array containing the instance security group IDs.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The SSH key\'s RSA fingerprint.
    sshHostRsaKeyFingerprint :: Core.Maybe Core.Text,
    -- | The ARN of the instance\'s IAM profile. For more information about IAM
    -- ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    instanceProfileArn :: Core.Maybe Core.Text,
    -- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
    virtualizationType :: Core.Maybe VirtualizationType,
    -- | The instance\'s private DNS name.
    privateDns :: Core.Maybe Core.Text,
    -- | The instance
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- .
    elasticIp :: Core.Maybe Core.Text,
    -- | The instance status:
    --
    -- -   @booting@
    --
    -- -   @connection_lost@
    --
    -- -   @online@
    --
    -- -   @pending@
    --
    -- -   @rebooting@
    --
    -- -   @requested@
    --
    -- -   @running_setup@
    --
    -- -   @setup_failed@
    --
    -- -   @shutting_down@
    --
    -- -   @start_failed@
    --
    -- -   @stop_failed@
    --
    -- -   @stopped@
    --
    -- -   @stopping@
    --
    -- -   @terminated@
    --
    -- -   @terminating@
    status :: Core.Maybe Core.Text,
    -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. If this value is set to
    -- @false@, you must then update your instances manually by using
    -- CreateDeployment to run the @update_dependencies@ stack command or by
    -- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
    -- instances.
    --
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The instance ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The instance\'s reported AWS OpsWorks Stacks agent version.
    reportedAgentVersion :: Core.Maybe Core.Text,
    -- | The instance type, such as @t2.micro@.
    instanceType :: Core.Maybe Core.Text,
    -- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
    sshHostDsaKeyFingerprint :: Core.Maybe Core.Text,
    -- | Whether this is an Amazon EBS-optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The instance\'s root device type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    rootDeviceType :: Core.Maybe RootDeviceType,
    -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | The agent version. This parameter is set to @INHERIT@ if the instance
    -- inherits the default stack setting or to a a version number for a fixed
    -- agent version.
    agentVersion :: Core.Maybe Core.Text,
    -- | The root device volume ID.
    rootDeviceVolumeId :: Core.Maybe Core.Text,
    -- | The instance\'s Amazon EC2 key-pair name.
    sshKeyName :: Core.Maybe Core.Text,
    -- | The instance public DNS name.
    publicDns :: Core.Maybe Core.Text,
    -- | A custom AMI ID to be used to create the instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
    amiId :: Core.Maybe Core.Text,
    -- | The instance\'s Amazon Resource Number (ARN).
    arn :: Core.Maybe Core.Text,
    -- | The time that the instance was created.
    createdAt :: Core.Maybe Core.Text,
    -- | An array containing the instance layer IDs.
    layerIds :: Core.Maybe [Core.Text],
    -- | The instance architecture: \"i386\" or \"x86_64\".
    architecture :: Core.Maybe Architecture,
    -- | The instance\'s tenancy option, such as @dedicated@ or @host@.
    tenancy :: Core.Maybe Core.Text,
    -- | For load-based or time-based instances, the type.
    autoScalingType :: Core.Maybe AutoScalingType,
    -- | The instance Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The instance\'s operating system.
    os :: Core.Maybe Core.Text,
    -- | The instance\'s private IP address.
    privateIp :: Core.Maybe Core.Text,
    -- | For registered instances, the infrastructure class: @ec2@ or
    -- @on-premises@.
    infrastructureClass :: Core.Maybe Core.Text,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
    -- block device mappings.
    blockDeviceMappings :: Core.Maybe [BlockDeviceMapping],
    -- | The instance\'s subnet ID; applicable only if the stack is running in a
    -- VPC.
    subnetId :: Core.Maybe Core.Text,
    -- | For container instances, the instance\'s ARN.
    ecsContainerInstanceArn :: Core.Maybe Core.Text,
    -- | For registered instances, who performed the registration.
    registeredBy :: Core.Maybe Core.Text,
    -- | For registered instances, the reported operating system.
    reportedOs :: Core.Maybe ReportedOs,
    -- | The instance public IP address.
    publicIp :: Core.Maybe Core.Text,
    -- | The ID of the associated Amazon EC2 instance.
    ec2InstanceId :: Core.Maybe Core.Text,
    -- | For container instances, the Amazon ECS cluster\'s ARN.
    ecsClusterArn :: Core.Maybe Core.Text,
    -- | The ID of the last service error. For more information, call
    -- DescribeServiceErrors.
    lastServiceErrorId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'instance_hostname' - The instance host name.
--
-- 'platform', 'instance_platform' - The instance\'s platform.
--
-- 'securityGroupIds', 'instance_securityGroupIds' - An array containing the instance security group IDs.
--
-- 'sshHostRsaKeyFingerprint', 'instance_sshHostRsaKeyFingerprint' - The SSH key\'s RSA fingerprint.
--
-- 'instanceProfileArn', 'instance_instanceProfileArn' - The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'virtualizationType', 'instance_virtualizationType' - The instance\'s virtualization type: @paravirtual@ or @hvm@.
--
-- 'privateDns', 'instance_privateDns' - The instance\'s private DNS name.
--
-- 'elasticIp', 'instance_elasticIp' - The instance
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- .
--
-- 'status', 'instance_status' - The instance status:
--
-- -   @booting@
--
-- -   @connection_lost@
--
-- -   @online@
--
-- -   @pending@
--
-- -   @rebooting@
--
-- -   @requested@
--
-- -   @running_setup@
--
-- -   @setup_failed@
--
-- -   @shutting_down@
--
-- -   @start_failed@
--
-- -   @stop_failed@
--
-- -   @stopped@
--
-- -   @stopping@
--
-- -   @terminated@
--
-- -   @terminating@
--
-- 'installUpdatesOnBoot', 'instance_installUpdatesOnBoot' - Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or by
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
--
-- 'instanceId', 'instance_instanceId' - The instance ID.
--
-- 'reportedAgentVersion', 'instance_reportedAgentVersion' - The instance\'s reported AWS OpsWorks Stacks agent version.
--
-- 'instanceType', 'instance_instanceType' - The instance type, such as @t2.micro@.
--
-- 'sshHostDsaKeyFingerprint', 'instance_sshHostDsaKeyFingerprint' - The SSH key\'s Deep Security Agent (DSA) fingerprint.
--
-- 'ebsOptimized', 'instance_ebsOptimized' - Whether this is an Amazon EBS-optimized instance.
--
-- 'rootDeviceType', 'instance_rootDeviceType' - The instance\'s root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'stackId', 'instance_stackId' - The stack ID.
--
-- 'agentVersion', 'instance_agentVersion' - The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
--
-- 'rootDeviceVolumeId', 'instance_rootDeviceVolumeId' - The root device volume ID.
--
-- 'sshKeyName', 'instance_sshKeyName' - The instance\'s Amazon EC2 key-pair name.
--
-- 'publicDns', 'instance_publicDns' - The instance public DNS name.
--
-- 'amiId', 'instance_amiId' - A custom AMI ID to be used to create the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- 'arn', 'instance_arn' - The instance\'s Amazon Resource Number (ARN).
--
-- 'createdAt', 'instance_createdAt' - The time that the instance was created.
--
-- 'layerIds', 'instance_layerIds' - An array containing the instance layer IDs.
--
-- 'architecture', 'instance_architecture' - The instance architecture: \"i386\" or \"x86_64\".
--
-- 'tenancy', 'instance_tenancy' - The instance\'s tenancy option, such as @dedicated@ or @host@.
--
-- 'autoScalingType', 'instance_autoScalingType' - For load-based or time-based instances, the type.
--
-- 'availabilityZone', 'instance_availabilityZone' - The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'os', 'instance_os' - The instance\'s operating system.
--
-- 'privateIp', 'instance_privateIp' - The instance\'s private IP address.
--
-- 'infrastructureClass', 'instance_infrastructureClass' - For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
--
-- 'blockDeviceMappings', 'instance_blockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
--
-- 'subnetId', 'instance_subnetId' - The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
--
-- 'ecsContainerInstanceArn', 'instance_ecsContainerInstanceArn' - For container instances, the instance\'s ARN.
--
-- 'registeredBy', 'instance_registeredBy' - For registered instances, who performed the registration.
--
-- 'reportedOs', 'instance_reportedOs' - For registered instances, the reported operating system.
--
-- 'publicIp', 'instance_publicIp' - The instance public IP address.
--
-- 'ec2InstanceId', 'instance_ec2InstanceId' - The ID of the associated Amazon EC2 instance.
--
-- 'ecsClusterArn', 'instance_ecsClusterArn' - For container instances, the Amazon ECS cluster\'s ARN.
--
-- 'lastServiceErrorId', 'instance_lastServiceErrorId' - The ID of the last service error. For more information, call
-- DescribeServiceErrors.
newInstance ::
  Instance
newInstance =
  Instance'
    { hostname = Core.Nothing,
      platform = Core.Nothing,
      securityGroupIds = Core.Nothing,
      sshHostRsaKeyFingerprint = Core.Nothing,
      instanceProfileArn = Core.Nothing,
      virtualizationType = Core.Nothing,
      privateDns = Core.Nothing,
      elasticIp = Core.Nothing,
      status = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      instanceId = Core.Nothing,
      reportedAgentVersion = Core.Nothing,
      instanceType = Core.Nothing,
      sshHostDsaKeyFingerprint = Core.Nothing,
      ebsOptimized = Core.Nothing,
      rootDeviceType = Core.Nothing,
      stackId = Core.Nothing,
      agentVersion = Core.Nothing,
      rootDeviceVolumeId = Core.Nothing,
      sshKeyName = Core.Nothing,
      publicDns = Core.Nothing,
      amiId = Core.Nothing,
      arn = Core.Nothing,
      createdAt = Core.Nothing,
      layerIds = Core.Nothing,
      architecture = Core.Nothing,
      tenancy = Core.Nothing,
      autoScalingType = Core.Nothing,
      availabilityZone = Core.Nothing,
      os = Core.Nothing,
      privateIp = Core.Nothing,
      infrastructureClass = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      subnetId = Core.Nothing,
      ecsContainerInstanceArn = Core.Nothing,
      registeredBy = Core.Nothing,
      reportedOs = Core.Nothing,
      publicIp = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      ecsClusterArn = Core.Nothing,
      lastServiceErrorId = Core.Nothing
    }

-- | The instance host name.
instance_hostname :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_hostname = Lens.lens (\Instance' {hostname} -> hostname) (\s@Instance' {} a -> s {hostname = a} :: Instance)

-- | The instance\'s platform.
instance_platform :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_platform = Lens.lens (\Instance' {platform} -> platform) (\s@Instance' {} a -> s {platform = a} :: Instance)

-- | An array containing the instance security group IDs.
instance_securityGroupIds :: Lens.Lens' Instance (Core.Maybe [Core.Text])
instance_securityGroupIds = Lens.lens (\Instance' {securityGroupIds} -> securityGroupIds) (\s@Instance' {} a -> s {securityGroupIds = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | The SSH key\'s RSA fingerprint.
instance_sshHostRsaKeyFingerprint :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_sshHostRsaKeyFingerprint = Lens.lens (\Instance' {sshHostRsaKeyFingerprint} -> sshHostRsaKeyFingerprint) (\s@Instance' {} a -> s {sshHostRsaKeyFingerprint = a} :: Instance)

-- | The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
instance_instanceProfileArn :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_instanceProfileArn = Lens.lens (\Instance' {instanceProfileArn} -> instanceProfileArn) (\s@Instance' {} a -> s {instanceProfileArn = a} :: Instance)

-- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
instance_virtualizationType :: Lens.Lens' Instance (Core.Maybe VirtualizationType)
instance_virtualizationType = Lens.lens (\Instance' {virtualizationType} -> virtualizationType) (\s@Instance' {} a -> s {virtualizationType = a} :: Instance)

-- | The instance\'s private DNS name.
instance_privateDns :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_privateDns = Lens.lens (\Instance' {privateDns} -> privateDns) (\s@Instance' {} a -> s {privateDns = a} :: Instance)

-- | The instance
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- .
instance_elasticIp :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_elasticIp = Lens.lens (\Instance' {elasticIp} -> elasticIp) (\s@Instance' {} a -> s {elasticIp = a} :: Instance)

-- | The instance status:
--
-- -   @booting@
--
-- -   @connection_lost@
--
-- -   @online@
--
-- -   @pending@
--
-- -   @rebooting@
--
-- -   @requested@
--
-- -   @running_setup@
--
-- -   @setup_failed@
--
-- -   @shutting_down@
--
-- -   @start_failed@
--
-- -   @stop_failed@
--
-- -   @stopped@
--
-- -   @stopping@
--
-- -   @terminated@
--
-- -   @terminating@
instance_status :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or by
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
instance_installUpdatesOnBoot :: Lens.Lens' Instance (Core.Maybe Core.Bool)
instance_installUpdatesOnBoot = Lens.lens (\Instance' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@Instance' {} a -> s {installUpdatesOnBoot = a} :: Instance)

-- | The instance ID.
instance_instanceId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | The instance\'s reported AWS OpsWorks Stacks agent version.
instance_reportedAgentVersion :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_reportedAgentVersion = Lens.lens (\Instance' {reportedAgentVersion} -> reportedAgentVersion) (\s@Instance' {} a -> s {reportedAgentVersion = a} :: Instance)

-- | The instance type, such as @t2.micro@.
instance_instanceType :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
instance_sshHostDsaKeyFingerprint :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_sshHostDsaKeyFingerprint = Lens.lens (\Instance' {sshHostDsaKeyFingerprint} -> sshHostDsaKeyFingerprint) (\s@Instance' {} a -> s {sshHostDsaKeyFingerprint = a} :: Instance)

-- | Whether this is an Amazon EBS-optimized instance.
instance_ebsOptimized :: Lens.Lens' Instance (Core.Maybe Core.Bool)
instance_ebsOptimized = Lens.lens (\Instance' {ebsOptimized} -> ebsOptimized) (\s@Instance' {} a -> s {ebsOptimized = a} :: Instance)

-- | The instance\'s root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
instance_rootDeviceType :: Lens.Lens' Instance (Core.Maybe RootDeviceType)
instance_rootDeviceType = Lens.lens (\Instance' {rootDeviceType} -> rootDeviceType) (\s@Instance' {} a -> s {rootDeviceType = a} :: Instance)

-- | The stack ID.
instance_stackId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_stackId = Lens.lens (\Instance' {stackId} -> stackId) (\s@Instance' {} a -> s {stackId = a} :: Instance)

-- | The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
instance_agentVersion :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_agentVersion = Lens.lens (\Instance' {agentVersion} -> agentVersion) (\s@Instance' {} a -> s {agentVersion = a} :: Instance)

-- | The root device volume ID.
instance_rootDeviceVolumeId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_rootDeviceVolumeId = Lens.lens (\Instance' {rootDeviceVolumeId} -> rootDeviceVolumeId) (\s@Instance' {} a -> s {rootDeviceVolumeId = a} :: Instance)

-- | The instance\'s Amazon EC2 key-pair name.
instance_sshKeyName :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | The instance public DNS name.
instance_publicDns :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_publicDns = Lens.lens (\Instance' {publicDns} -> publicDns) (\s@Instance' {} a -> s {publicDns = a} :: Instance)

-- | A custom AMI ID to be used to create the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
instance_amiId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_amiId = Lens.lens (\Instance' {amiId} -> amiId) (\s@Instance' {} a -> s {amiId = a} :: Instance)

-- | The instance\'s Amazon Resource Number (ARN).
instance_arn :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The time that the instance was created.
instance_createdAt :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance)

-- | An array containing the instance layer IDs.
instance_layerIds :: Lens.Lens' Instance (Core.Maybe [Core.Text])
instance_layerIds = Lens.lens (\Instance' {layerIds} -> layerIds) (\s@Instance' {} a -> s {layerIds = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | The instance architecture: \"i386\" or \"x86_64\".
instance_architecture :: Lens.Lens' Instance (Core.Maybe Architecture)
instance_architecture = Lens.lens (\Instance' {architecture} -> architecture) (\s@Instance' {} a -> s {architecture = a} :: Instance)

-- | The instance\'s tenancy option, such as @dedicated@ or @host@.
instance_tenancy :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_tenancy = Lens.lens (\Instance' {tenancy} -> tenancy) (\s@Instance' {} a -> s {tenancy = a} :: Instance)

-- | For load-based or time-based instances, the type.
instance_autoScalingType :: Lens.Lens' Instance (Core.Maybe AutoScalingType)
instance_autoScalingType = Lens.lens (\Instance' {autoScalingType} -> autoScalingType) (\s@Instance' {} a -> s {autoScalingType = a} :: Instance)

-- | The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
instance_availabilityZone :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_availabilityZone = Lens.lens (\Instance' {availabilityZone} -> availabilityZone) (\s@Instance' {} a -> s {availabilityZone = a} :: Instance)

-- | The instance\'s operating system.
instance_os :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_os = Lens.lens (\Instance' {os} -> os) (\s@Instance' {} a -> s {os = a} :: Instance)

-- | The instance\'s private IP address.
instance_privateIp :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_privateIp = Lens.lens (\Instance' {privateIp} -> privateIp) (\s@Instance' {} a -> s {privateIp = a} :: Instance)

-- | For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
instance_infrastructureClass :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_infrastructureClass = Lens.lens (\Instance' {infrastructureClass} -> infrastructureClass) (\s@Instance' {} a -> s {infrastructureClass = a} :: Instance)

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
instance_blockDeviceMappings :: Lens.Lens' Instance (Core.Maybe [BlockDeviceMapping])
instance_blockDeviceMappings = Lens.lens (\Instance' {blockDeviceMappings} -> blockDeviceMappings) (\s@Instance' {} a -> s {blockDeviceMappings = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
instance_subnetId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_subnetId = Lens.lens (\Instance' {subnetId} -> subnetId) (\s@Instance' {} a -> s {subnetId = a} :: Instance)

-- | For container instances, the instance\'s ARN.
instance_ecsContainerInstanceArn :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_ecsContainerInstanceArn = Lens.lens (\Instance' {ecsContainerInstanceArn} -> ecsContainerInstanceArn) (\s@Instance' {} a -> s {ecsContainerInstanceArn = a} :: Instance)

-- | For registered instances, who performed the registration.
instance_registeredBy :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_registeredBy = Lens.lens (\Instance' {registeredBy} -> registeredBy) (\s@Instance' {} a -> s {registeredBy = a} :: Instance)

-- | For registered instances, the reported operating system.
instance_reportedOs :: Lens.Lens' Instance (Core.Maybe ReportedOs)
instance_reportedOs = Lens.lens (\Instance' {reportedOs} -> reportedOs) (\s@Instance' {} a -> s {reportedOs = a} :: Instance)

-- | The instance public IP address.
instance_publicIp :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_publicIp = Lens.lens (\Instance' {publicIp} -> publicIp) (\s@Instance' {} a -> s {publicIp = a} :: Instance)

-- | The ID of the associated Amazon EC2 instance.
instance_ec2InstanceId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_ec2InstanceId = Lens.lens (\Instance' {ec2InstanceId} -> ec2InstanceId) (\s@Instance' {} a -> s {ec2InstanceId = a} :: Instance)

-- | For container instances, the Amazon ECS cluster\'s ARN.
instance_ecsClusterArn :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_ecsClusterArn = Lens.lens (\Instance' {ecsClusterArn} -> ecsClusterArn) (\s@Instance' {} a -> s {ecsClusterArn = a} :: Instance)

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
instance_lastServiceErrorId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_lastServiceErrorId = Lens.lens (\Instance' {lastServiceErrorId} -> lastServiceErrorId) (\s@Instance' {} a -> s {lastServiceErrorId = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Core.<$> (x Core..:? "Hostname")
            Core.<*> (x Core..:? "Platform")
            Core.<*> (x Core..:? "SecurityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SshHostRsaKeyFingerprint")
            Core.<*> (x Core..:? "InstanceProfileArn")
            Core.<*> (x Core..:? "VirtualizationType")
            Core.<*> (x Core..:? "PrivateDns")
            Core.<*> (x Core..:? "ElasticIp")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "InstallUpdatesOnBoot")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "ReportedAgentVersion")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "SshHostDsaKeyFingerprint")
            Core.<*> (x Core..:? "EbsOptimized")
            Core.<*> (x Core..:? "RootDeviceType")
            Core.<*> (x Core..:? "StackId")
            Core.<*> (x Core..:? "AgentVersion")
            Core.<*> (x Core..:? "RootDeviceVolumeId")
            Core.<*> (x Core..:? "SshKeyName")
            Core.<*> (x Core..:? "PublicDns")
            Core.<*> (x Core..:? "AmiId")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "LayerIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Architecture")
            Core.<*> (x Core..:? "Tenancy")
            Core.<*> (x Core..:? "AutoScalingType")
            Core.<*> (x Core..:? "AvailabilityZone")
            Core.<*> (x Core..:? "Os")
            Core.<*> (x Core..:? "PrivateIp")
            Core.<*> (x Core..:? "InfrastructureClass")
            Core.<*> ( x Core..:? "BlockDeviceMappings"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "SubnetId")
            Core.<*> (x Core..:? "EcsContainerInstanceArn")
            Core.<*> (x Core..:? "RegisteredBy")
            Core.<*> (x Core..:? "ReportedOs")
            Core.<*> (x Core..:? "PublicIp")
            Core.<*> (x Core..:? "Ec2InstanceId")
            Core.<*> (x Core..:? "EcsClusterArn")
            Core.<*> (x Core..:? "LastServiceErrorId")
      )

instance Core.Hashable Instance

instance Core.NFData Instance
