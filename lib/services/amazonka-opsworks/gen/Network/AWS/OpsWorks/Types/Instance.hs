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
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The instance\'s private DNS name.
    privateDns :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s reported AWS OpsWorks Stacks agent version.
    reportedAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s private IP address.
    privateIp :: Prelude.Maybe Prelude.Text,
    -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. If this value is set to
    -- @false@, you must then update your instances manually by using
    -- CreateDeployment to run the @update_dependencies@ stack command or by
    -- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
    -- instances.
    --
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Prelude.Maybe Prelude.Bool,
    -- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
    virtualizationType :: Prelude.Maybe VirtualizationType,
    -- | The ARN of the instance\'s IAM profile. For more information about IAM
    -- ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    instanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s platform.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The instance host name.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The SSH key\'s RSA fingerprint.
    sshHostRsaKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | An array containing the instance security group IDs.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | For container instances, the Amazon ECS cluster\'s ARN.
    ecsClusterArn :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s Amazon Resource Number (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that the instance was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the associated Amazon EC2 instance.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s Amazon EC2 key-pair name.
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The agent version. This parameter is set to @INHERIT@ if the instance
    -- inherits the default stack setting or to a a version number for a fixed
    -- agent version.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The root device volume ID.
    rootDeviceVolumeId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s subnet ID; applicable only if the stack is running in a
    -- VPC.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | For registered instances, the infrastructure class: @ec2@ or
    -- @on-premises@.
    infrastructureClass :: Prelude.Maybe Prelude.Text,
    -- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
    sshHostDsaKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The instance type, such as @t2.micro@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Whether this is an Amazon EBS-optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The instance
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- .
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s operating system.
    os :: Prelude.Maybe Prelude.Text,
    -- | The instance Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the last service error. For more information, call
    -- DescribeServiceErrors.
    lastServiceErrorId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s tenancy option, such as @dedicated@ or @host@.
    tenancy :: Prelude.Maybe Prelude.Text,
    -- | For load-based or time-based instances, the type.
    autoScalingType :: Prelude.Maybe AutoScalingType,
    -- | An array containing the instance layer IDs.
    layerIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance architecture: \"i386\" or \"x86_64\".
    architecture :: Prelude.Maybe Architecture,
    -- | The instance public DNS name.
    publicDns :: Prelude.Maybe Prelude.Text,
    -- | A custom AMI ID to be used to create the instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The instance public IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | For registered instances, the reported operating system.
    reportedOs :: Prelude.Maybe ReportedOs,
    -- | For registered instances, who performed the registration.
    registeredBy :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s root device type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    rootDeviceType :: Prelude.Maybe RootDeviceType,
    -- | For container instances, the instance\'s ARN.
    ecsContainerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
    -- block device mappings.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateDns', 'instance_privateDns' - The instance\'s private DNS name.
--
-- 'reportedAgentVersion', 'instance_reportedAgentVersion' - The instance\'s reported AWS OpsWorks Stacks agent version.
--
-- 'instanceId', 'instance_instanceId' - The instance ID.
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
-- 'privateIp', 'instance_privateIp' - The instance\'s private IP address.
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
-- 'virtualizationType', 'instance_virtualizationType' - The instance\'s virtualization type: @paravirtual@ or @hvm@.
--
-- 'instanceProfileArn', 'instance_instanceProfileArn' - The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'platform', 'instance_platform' - The instance\'s platform.
--
-- 'hostname', 'instance_hostname' - The instance host name.
--
-- 'sshHostRsaKeyFingerprint', 'instance_sshHostRsaKeyFingerprint' - The SSH key\'s RSA fingerprint.
--
-- 'securityGroupIds', 'instance_securityGroupIds' - An array containing the instance security group IDs.
--
-- 'ecsClusterArn', 'instance_ecsClusterArn' - For container instances, the Amazon ECS cluster\'s ARN.
--
-- 'arn', 'instance_arn' - The instance\'s Amazon Resource Number (ARN).
--
-- 'createdAt', 'instance_createdAt' - The time that the instance was created.
--
-- 'ec2InstanceId', 'instance_ec2InstanceId' - The ID of the associated Amazon EC2 instance.
--
-- 'sshKeyName', 'instance_sshKeyName' - The instance\'s Amazon EC2 key-pair name.
--
-- 'agentVersion', 'instance_agentVersion' - The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
--
-- 'rootDeviceVolumeId', 'instance_rootDeviceVolumeId' - The root device volume ID.
--
-- 'subnetId', 'instance_subnetId' - The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
--
-- 'infrastructureClass', 'instance_infrastructureClass' - For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
--
-- 'sshHostDsaKeyFingerprint', 'instance_sshHostDsaKeyFingerprint' - The SSH key\'s Deep Security Agent (DSA) fingerprint.
--
-- 'instanceType', 'instance_instanceType' - The instance type, such as @t2.micro@.
--
-- 'ebsOptimized', 'instance_ebsOptimized' - Whether this is an Amazon EBS-optimized instance.
--
-- 'elasticIp', 'instance_elasticIp' - The instance
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- .
--
-- 'os', 'instance_os' - The instance\'s operating system.
--
-- 'availabilityZone', 'instance_availabilityZone' - The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'lastServiceErrorId', 'instance_lastServiceErrorId' - The ID of the last service error. For more information, call
-- DescribeServiceErrors.
--
-- 'tenancy', 'instance_tenancy' - The instance\'s tenancy option, such as @dedicated@ or @host@.
--
-- 'autoScalingType', 'instance_autoScalingType' - For load-based or time-based instances, the type.
--
-- 'layerIds', 'instance_layerIds' - An array containing the instance layer IDs.
--
-- 'architecture', 'instance_architecture' - The instance architecture: \"i386\" or \"x86_64\".
--
-- 'publicDns', 'instance_publicDns' - The instance public DNS name.
--
-- 'amiId', 'instance_amiId' - A custom AMI ID to be used to create the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- 'publicIp', 'instance_publicIp' - The instance public IP address.
--
-- 'reportedOs', 'instance_reportedOs' - For registered instances, the reported operating system.
--
-- 'registeredBy', 'instance_registeredBy' - For registered instances, who performed the registration.
--
-- 'stackId', 'instance_stackId' - The stack ID.
--
-- 'rootDeviceType', 'instance_rootDeviceType' - The instance\'s root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'ecsContainerInstanceArn', 'instance_ecsContainerInstanceArn' - For container instances, the instance\'s ARN.
--
-- 'blockDeviceMappings', 'instance_blockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
newInstance ::
  Instance
newInstance =
  Instance'
    { privateDns = Prelude.Nothing,
      reportedAgentVersion = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      status = Prelude.Nothing,
      privateIp = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      virtualizationType = Prelude.Nothing,
      instanceProfileArn = Prelude.Nothing,
      platform = Prelude.Nothing,
      hostname = Prelude.Nothing,
      sshHostRsaKeyFingerprint = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      ecsClusterArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      ec2InstanceId = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      rootDeviceVolumeId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      infrastructureClass = Prelude.Nothing,
      sshHostDsaKeyFingerprint = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      elasticIp = Prelude.Nothing,
      os = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      lastServiceErrorId = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      autoScalingType = Prelude.Nothing,
      layerIds = Prelude.Nothing,
      architecture = Prelude.Nothing,
      publicDns = Prelude.Nothing,
      amiId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      reportedOs = Prelude.Nothing,
      registeredBy = Prelude.Nothing,
      stackId = Prelude.Nothing,
      rootDeviceType = Prelude.Nothing,
      ecsContainerInstanceArn = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing
    }

-- | The instance\'s private DNS name.
instance_privateDns :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateDns = Lens.lens (\Instance' {privateDns} -> privateDns) (\s@Instance' {} a -> s {privateDns = a} :: Instance)

-- | The instance\'s reported AWS OpsWorks Stacks agent version.
instance_reportedAgentVersion :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_reportedAgentVersion = Lens.lens (\Instance' {reportedAgentVersion} -> reportedAgentVersion) (\s@Instance' {} a -> s {reportedAgentVersion = a} :: Instance)

-- | The instance ID.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

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
instance_status :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

-- | The instance\'s private IP address.
instance_privateIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIp = Lens.lens (\Instance' {privateIp} -> privateIp) (\s@Instance' {} a -> s {privateIp = a} :: Instance)

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or by
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
instance_installUpdatesOnBoot :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_installUpdatesOnBoot = Lens.lens (\Instance' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@Instance' {} a -> s {installUpdatesOnBoot = a} :: Instance)

-- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
instance_virtualizationType :: Lens.Lens' Instance (Prelude.Maybe VirtualizationType)
instance_virtualizationType = Lens.lens (\Instance' {virtualizationType} -> virtualizationType) (\s@Instance' {} a -> s {virtualizationType = a} :: Instance)

-- | The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
instance_instanceProfileArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceProfileArn = Lens.lens (\Instance' {instanceProfileArn} -> instanceProfileArn) (\s@Instance' {} a -> s {instanceProfileArn = a} :: Instance)

-- | The instance\'s platform.
instance_platform :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_platform = Lens.lens (\Instance' {platform} -> platform) (\s@Instance' {} a -> s {platform = a} :: Instance)

-- | The instance host name.
instance_hostname :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_hostname = Lens.lens (\Instance' {hostname} -> hostname) (\s@Instance' {} a -> s {hostname = a} :: Instance)

-- | The SSH key\'s RSA fingerprint.
instance_sshHostRsaKeyFingerprint :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshHostRsaKeyFingerprint = Lens.lens (\Instance' {sshHostRsaKeyFingerprint} -> sshHostRsaKeyFingerprint) (\s@Instance' {} a -> s {sshHostRsaKeyFingerprint = a} :: Instance)

-- | An array containing the instance security group IDs.
instance_securityGroupIds :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_securityGroupIds = Lens.lens (\Instance' {securityGroupIds} -> securityGroupIds) (\s@Instance' {} a -> s {securityGroupIds = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | For container instances, the Amazon ECS cluster\'s ARN.
instance_ecsClusterArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ecsClusterArn = Lens.lens (\Instance' {ecsClusterArn} -> ecsClusterArn) (\s@Instance' {} a -> s {ecsClusterArn = a} :: Instance)

-- | The instance\'s Amazon Resource Number (ARN).
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The time that the instance was created.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance)

-- | The ID of the associated Amazon EC2 instance.
instance_ec2InstanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ec2InstanceId = Lens.lens (\Instance' {ec2InstanceId} -> ec2InstanceId) (\s@Instance' {} a -> s {ec2InstanceId = a} :: Instance)

-- | The instance\'s Amazon EC2 key-pair name.
instance_sshKeyName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
instance_agentVersion :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_agentVersion = Lens.lens (\Instance' {agentVersion} -> agentVersion) (\s@Instance' {} a -> s {agentVersion = a} :: Instance)

-- | The root device volume ID.
instance_rootDeviceVolumeId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_rootDeviceVolumeId = Lens.lens (\Instance' {rootDeviceVolumeId} -> rootDeviceVolumeId) (\s@Instance' {} a -> s {rootDeviceVolumeId = a} :: Instance)

-- | The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
instance_subnetId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_subnetId = Lens.lens (\Instance' {subnetId} -> subnetId) (\s@Instance' {} a -> s {subnetId = a} :: Instance)

-- | For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
instance_infrastructureClass :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_infrastructureClass = Lens.lens (\Instance' {infrastructureClass} -> infrastructureClass) (\s@Instance' {} a -> s {infrastructureClass = a} :: Instance)

-- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
instance_sshHostDsaKeyFingerprint :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshHostDsaKeyFingerprint = Lens.lens (\Instance' {sshHostDsaKeyFingerprint} -> sshHostDsaKeyFingerprint) (\s@Instance' {} a -> s {sshHostDsaKeyFingerprint = a} :: Instance)

-- | The instance type, such as @t2.micro@.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | Whether this is an Amazon EBS-optimized instance.
instance_ebsOptimized :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_ebsOptimized = Lens.lens (\Instance' {ebsOptimized} -> ebsOptimized) (\s@Instance' {} a -> s {ebsOptimized = a} :: Instance)

-- | The instance
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- .
instance_elasticIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_elasticIp = Lens.lens (\Instance' {elasticIp} -> elasticIp) (\s@Instance' {} a -> s {elasticIp = a} :: Instance)

-- | The instance\'s operating system.
instance_os :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_os = Lens.lens (\Instance' {os} -> os) (\s@Instance' {} a -> s {os = a} :: Instance)

-- | The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
instance_availabilityZone :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_availabilityZone = Lens.lens (\Instance' {availabilityZone} -> availabilityZone) (\s@Instance' {} a -> s {availabilityZone = a} :: Instance)

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
instance_lastServiceErrorId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_lastServiceErrorId = Lens.lens (\Instance' {lastServiceErrorId} -> lastServiceErrorId) (\s@Instance' {} a -> s {lastServiceErrorId = a} :: Instance)

-- | The instance\'s tenancy option, such as @dedicated@ or @host@.
instance_tenancy :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_tenancy = Lens.lens (\Instance' {tenancy} -> tenancy) (\s@Instance' {} a -> s {tenancy = a} :: Instance)

-- | For load-based or time-based instances, the type.
instance_autoScalingType :: Lens.Lens' Instance (Prelude.Maybe AutoScalingType)
instance_autoScalingType = Lens.lens (\Instance' {autoScalingType} -> autoScalingType) (\s@Instance' {} a -> s {autoScalingType = a} :: Instance)

-- | An array containing the instance layer IDs.
instance_layerIds :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_layerIds = Lens.lens (\Instance' {layerIds} -> layerIds) (\s@Instance' {} a -> s {layerIds = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The instance architecture: \"i386\" or \"x86_64\".
instance_architecture :: Lens.Lens' Instance (Prelude.Maybe Architecture)
instance_architecture = Lens.lens (\Instance' {architecture} -> architecture) (\s@Instance' {} a -> s {architecture = a} :: Instance)

-- | The instance public DNS name.
instance_publicDns :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicDns = Lens.lens (\Instance' {publicDns} -> publicDns) (\s@Instance' {} a -> s {publicDns = a} :: Instance)

-- | A custom AMI ID to be used to create the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
instance_amiId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_amiId = Lens.lens (\Instance' {amiId} -> amiId) (\s@Instance' {} a -> s {amiId = a} :: Instance)

-- | The instance public IP address.
instance_publicIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIp = Lens.lens (\Instance' {publicIp} -> publicIp) (\s@Instance' {} a -> s {publicIp = a} :: Instance)

-- | For registered instances, the reported operating system.
instance_reportedOs :: Lens.Lens' Instance (Prelude.Maybe ReportedOs)
instance_reportedOs = Lens.lens (\Instance' {reportedOs} -> reportedOs) (\s@Instance' {} a -> s {reportedOs = a} :: Instance)

-- | For registered instances, who performed the registration.
instance_registeredBy :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_registeredBy = Lens.lens (\Instance' {registeredBy} -> registeredBy) (\s@Instance' {} a -> s {registeredBy = a} :: Instance)

-- | The stack ID.
instance_stackId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_stackId = Lens.lens (\Instance' {stackId} -> stackId) (\s@Instance' {} a -> s {stackId = a} :: Instance)

-- | The instance\'s root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
instance_rootDeviceType :: Lens.Lens' Instance (Prelude.Maybe RootDeviceType)
instance_rootDeviceType = Lens.lens (\Instance' {rootDeviceType} -> rootDeviceType) (\s@Instance' {} a -> s {rootDeviceType = a} :: Instance)

-- | For container instances, the instance\'s ARN.
instance_ecsContainerInstanceArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ecsContainerInstanceArn = Lens.lens (\Instance' {ecsContainerInstanceArn} -> ecsContainerInstanceArn) (\s@Instance' {} a -> s {ecsContainerInstanceArn = a} :: Instance)

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
instance_blockDeviceMappings :: Lens.Lens' Instance (Prelude.Maybe [BlockDeviceMapping])
instance_blockDeviceMappings = Lens.lens (\Instance' {blockDeviceMappings} -> blockDeviceMappings) (\s@Instance' {} a -> s {blockDeviceMappings = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Core..:? "PrivateDns")
            Prelude.<*> (x Core..:? "ReportedAgentVersion")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "PrivateIp")
            Prelude.<*> (x Core..:? "InstallUpdatesOnBoot")
            Prelude.<*> (x Core..:? "VirtualizationType")
            Prelude.<*> (x Core..:? "InstanceProfileArn")
            Prelude.<*> (x Core..:? "Platform")
            Prelude.<*> (x Core..:? "Hostname")
            Prelude.<*> (x Core..:? "SshHostRsaKeyFingerprint")
            Prelude.<*> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EcsClusterArn")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Ec2InstanceId")
            Prelude.<*> (x Core..:? "SshKeyName")
            Prelude.<*> (x Core..:? "AgentVersion")
            Prelude.<*> (x Core..:? "RootDeviceVolumeId")
            Prelude.<*> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "InfrastructureClass")
            Prelude.<*> (x Core..:? "SshHostDsaKeyFingerprint")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "EbsOptimized")
            Prelude.<*> (x Core..:? "ElasticIp")
            Prelude.<*> (x Core..:? "Os")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "LastServiceErrorId")
            Prelude.<*> (x Core..:? "Tenancy")
            Prelude.<*> (x Core..:? "AutoScalingType")
            Prelude.<*> (x Core..:? "LayerIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Architecture")
            Prelude.<*> (x Core..:? "PublicDns")
            Prelude.<*> (x Core..:? "AmiId")
            Prelude.<*> (x Core..:? "PublicIp")
            Prelude.<*> (x Core..:? "ReportedOs")
            Prelude.<*> (x Core..:? "RegisteredBy")
            Prelude.<*> (x Core..:? "StackId")
            Prelude.<*> (x Core..:? "RootDeviceType")
            Prelude.<*> (x Core..:? "EcsContainerInstanceArn")
            Prelude.<*> ( x Core..:? "BlockDeviceMappings"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
