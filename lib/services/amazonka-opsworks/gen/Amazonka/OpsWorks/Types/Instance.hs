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
-- Module      : Amazonka.OpsWorks.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.Architecture
import Amazonka.OpsWorks.Types.AutoScalingType
import Amazonka.OpsWorks.Types.BlockDeviceMapping
import Amazonka.OpsWorks.Types.ReportedOs
import Amazonka.OpsWorks.Types.RootDeviceType
import Amazonka.OpsWorks.Types.VirtualizationType
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The agent version. This parameter is set to @INHERIT@ if the instance
    -- inherits the default stack setting or to a a version number for a fixed
    -- agent version.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | A custom AMI ID to be used to create the instance. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The instance architecture: \"i386\" or \"x86_64\".
    architecture :: Prelude.Maybe Architecture,
    -- | The instance\'s Amazon Resource Number (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | For load-based or time-based instances, the type.
    autoScalingType :: Prelude.Maybe AutoScalingType,
    -- | The instance Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
    -- block device mappings.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The time that the instance was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | Whether this is an Amazon EBS-optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the associated Amazon EC2 instance.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    -- | For container instances, the Amazon ECS cluster\'s ARN.
    ecsClusterArn :: Prelude.Maybe Prelude.Text,
    -- | For container instances, the instance\'s ARN.
    ecsContainerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The instance
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
    -- .
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The instance host name.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | For registered instances, the infrastructure class: @ec2@ or
    -- @on-premises@.
    infrastructureClass :: Prelude.Maybe Prelude.Text,
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
    -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the instance\'s IAM profile. For more information about IAM
    -- ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    instanceProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The instance type, such as @t2.micro@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the last service error. For more information, call
    -- DescribeServiceErrors.
    lastServiceErrorId :: Prelude.Maybe Prelude.Text,
    -- | An array containing the instance layer IDs.
    layerIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance\'s operating system.
    os :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s platform.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s private DNS name.
    privateDns :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s private IP address.
    privateIp :: Prelude.Maybe Prelude.Text,
    -- | The instance public DNS name.
    publicDns :: Prelude.Maybe Prelude.Text,
    -- | The instance public IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | For registered instances, who performed the registration.
    registeredBy :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s reported AWS OpsWorks Stacks agent version.
    reportedAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | For registered instances, the reported operating system.
    reportedOs :: Prelude.Maybe ReportedOs,
    -- | The instance\'s root device type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    rootDeviceType :: Prelude.Maybe RootDeviceType,
    -- | The root device volume ID.
    rootDeviceVolumeId :: Prelude.Maybe Prelude.Text,
    -- | An array containing the instance security group IDs.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
    sshHostDsaKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The SSH key\'s RSA fingerprint.
    sshHostRsaKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s Amazon EC2 key-pair name.
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
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
    -- | The instance\'s subnet ID; applicable only if the stack is running in a
    -- VPC.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s tenancy option, such as @dedicated@ or @host@.
    tenancy :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
    virtualizationType :: Prelude.Maybe VirtualizationType
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
-- 'agentVersion', 'instance_agentVersion' - The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
--
-- 'amiId', 'instance_amiId' - A custom AMI ID to be used to create the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- 'architecture', 'instance_architecture' - The instance architecture: \"i386\" or \"x86_64\".
--
-- 'arn', 'instance_arn' - The instance\'s Amazon Resource Number (ARN).
--
-- 'autoScalingType', 'instance_autoScalingType' - For load-based or time-based instances, the type.
--
-- 'availabilityZone', 'instance_availabilityZone' - The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'blockDeviceMappings', 'instance_blockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
--
-- 'createdAt', 'instance_createdAt' - The time that the instance was created.
--
-- 'ebsOptimized', 'instance_ebsOptimized' - Whether this is an Amazon EBS-optimized instance.
--
-- 'ec2InstanceId', 'instance_ec2InstanceId' - The ID of the associated Amazon EC2 instance.
--
-- 'ecsClusterArn', 'instance_ecsClusterArn' - For container instances, the Amazon ECS cluster\'s ARN.
--
-- 'ecsContainerInstanceArn', 'instance_ecsContainerInstanceArn' - For container instances, the instance\'s ARN.
--
-- 'elasticIp', 'instance_elasticIp' - The instance
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- .
--
-- 'hostname', 'instance_hostname' - The instance host name.
--
-- 'infrastructureClass', 'instance_infrastructureClass' - For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
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
-- 'instanceProfileArn', 'instance_instanceProfileArn' - The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'instanceType', 'instance_instanceType' - The instance type, such as @t2.micro@.
--
-- 'lastServiceErrorId', 'instance_lastServiceErrorId' - The ID of the last service error. For more information, call
-- DescribeServiceErrors.
--
-- 'layerIds', 'instance_layerIds' - An array containing the instance layer IDs.
--
-- 'os', 'instance_os' - The instance\'s operating system.
--
-- 'platform', 'instance_platform' - The instance\'s platform.
--
-- 'privateDns', 'instance_privateDns' - The instance\'s private DNS name.
--
-- 'privateIp', 'instance_privateIp' - The instance\'s private IP address.
--
-- 'publicDns', 'instance_publicDns' - The instance public DNS name.
--
-- 'publicIp', 'instance_publicIp' - The instance public IP address.
--
-- 'registeredBy', 'instance_registeredBy' - For registered instances, who performed the registration.
--
-- 'reportedAgentVersion', 'instance_reportedAgentVersion' - The instance\'s reported AWS OpsWorks Stacks agent version.
--
-- 'reportedOs', 'instance_reportedOs' - For registered instances, the reported operating system.
--
-- 'rootDeviceType', 'instance_rootDeviceType' - The instance\'s root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'rootDeviceVolumeId', 'instance_rootDeviceVolumeId' - The root device volume ID.
--
-- 'securityGroupIds', 'instance_securityGroupIds' - An array containing the instance security group IDs.
--
-- 'sshHostDsaKeyFingerprint', 'instance_sshHostDsaKeyFingerprint' - The SSH key\'s Deep Security Agent (DSA) fingerprint.
--
-- 'sshHostRsaKeyFingerprint', 'instance_sshHostRsaKeyFingerprint' - The SSH key\'s RSA fingerprint.
--
-- 'sshKeyName', 'instance_sshKeyName' - The instance\'s Amazon EC2 key-pair name.
--
-- 'stackId', 'instance_stackId' - The stack ID.
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
-- 'subnetId', 'instance_subnetId' - The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
--
-- 'tenancy', 'instance_tenancy' - The instance\'s tenancy option, such as @dedicated@ or @host@.
--
-- 'virtualizationType', 'instance_virtualizationType' - The instance\'s virtualization type: @paravirtual@ or @hvm@.
newInstance ::
  Instance
newInstance =
  Instance'
    { agentVersion = Prelude.Nothing,
      amiId = Prelude.Nothing,
      architecture = Prelude.Nothing,
      arn = Prelude.Nothing,
      autoScalingType = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      ec2InstanceId = Prelude.Nothing,
      ecsClusterArn = Prelude.Nothing,
      ecsContainerInstanceArn = Prelude.Nothing,
      elasticIp = Prelude.Nothing,
      hostname = Prelude.Nothing,
      infrastructureClass = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceProfileArn = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      lastServiceErrorId = Prelude.Nothing,
      layerIds = Prelude.Nothing,
      os = Prelude.Nothing,
      platform = Prelude.Nothing,
      privateDns = Prelude.Nothing,
      privateIp = Prelude.Nothing,
      publicDns = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      registeredBy = Prelude.Nothing,
      reportedAgentVersion = Prelude.Nothing,
      reportedOs = Prelude.Nothing,
      rootDeviceType = Prelude.Nothing,
      rootDeviceVolumeId = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      sshHostDsaKeyFingerprint = Prelude.Nothing,
      sshHostRsaKeyFingerprint = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      stackId = Prelude.Nothing,
      status = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      virtualizationType = Prelude.Nothing
    }

-- | The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
instance_agentVersion :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_agentVersion = Lens.lens (\Instance' {agentVersion} -> agentVersion) (\s@Instance' {} a -> s {agentVersion = a} :: Instance)

-- | A custom AMI ID to be used to create the instance. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
instance_amiId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_amiId = Lens.lens (\Instance' {amiId} -> amiId) (\s@Instance' {} a -> s {amiId = a} :: Instance)

-- | The instance architecture: \"i386\" or \"x86_64\".
instance_architecture :: Lens.Lens' Instance (Prelude.Maybe Architecture)
instance_architecture = Lens.lens (\Instance' {architecture} -> architecture) (\s@Instance' {} a -> s {architecture = a} :: Instance)

-- | The instance\'s Amazon Resource Number (ARN).
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | For load-based or time-based instances, the type.
instance_autoScalingType :: Lens.Lens' Instance (Prelude.Maybe AutoScalingType)
instance_autoScalingType = Lens.lens (\Instance' {autoScalingType} -> autoScalingType) (\s@Instance' {} a -> s {autoScalingType = a} :: Instance)

-- | The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
instance_availabilityZone :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_availabilityZone = Lens.lens (\Instance' {availabilityZone} -> availabilityZone) (\s@Instance' {} a -> s {availabilityZone = a} :: Instance)

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
instance_blockDeviceMappings :: Lens.Lens' Instance (Prelude.Maybe [BlockDeviceMapping])
instance_blockDeviceMappings = Lens.lens (\Instance' {blockDeviceMappings} -> blockDeviceMappings) (\s@Instance' {} a -> s {blockDeviceMappings = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The time that the instance was created.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance)

-- | Whether this is an Amazon EBS-optimized instance.
instance_ebsOptimized :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_ebsOptimized = Lens.lens (\Instance' {ebsOptimized} -> ebsOptimized) (\s@Instance' {} a -> s {ebsOptimized = a} :: Instance)

-- | The ID of the associated Amazon EC2 instance.
instance_ec2InstanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ec2InstanceId = Lens.lens (\Instance' {ec2InstanceId} -> ec2InstanceId) (\s@Instance' {} a -> s {ec2InstanceId = a} :: Instance)

-- | For container instances, the Amazon ECS cluster\'s ARN.
instance_ecsClusterArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ecsClusterArn = Lens.lens (\Instance' {ecsClusterArn} -> ecsClusterArn) (\s@Instance' {} a -> s {ecsClusterArn = a} :: Instance)

-- | For container instances, the instance\'s ARN.
instance_ecsContainerInstanceArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ecsContainerInstanceArn = Lens.lens (\Instance' {ecsContainerInstanceArn} -> ecsContainerInstanceArn) (\s@Instance' {} a -> s {ecsContainerInstanceArn = a} :: Instance)

-- | The instance
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- .
instance_elasticIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_elasticIp = Lens.lens (\Instance' {elasticIp} -> elasticIp) (\s@Instance' {} a -> s {elasticIp = a} :: Instance)

-- | The instance host name.
instance_hostname :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_hostname = Lens.lens (\Instance' {hostname} -> hostname) (\s@Instance' {} a -> s {hostname = a} :: Instance)

-- | For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
instance_infrastructureClass :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_infrastructureClass = Lens.lens (\Instance' {infrastructureClass} -> infrastructureClass) (\s@Instance' {} a -> s {infrastructureClass = a} :: Instance)

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

-- | The instance ID.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
instance_instanceProfileArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceProfileArn = Lens.lens (\Instance' {instanceProfileArn} -> instanceProfileArn) (\s@Instance' {} a -> s {instanceProfileArn = a} :: Instance)

-- | The instance type, such as @t2.micro@.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
instance_lastServiceErrorId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_lastServiceErrorId = Lens.lens (\Instance' {lastServiceErrorId} -> lastServiceErrorId) (\s@Instance' {} a -> s {lastServiceErrorId = a} :: Instance)

-- | An array containing the instance layer IDs.
instance_layerIds :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_layerIds = Lens.lens (\Instance' {layerIds} -> layerIds) (\s@Instance' {} a -> s {layerIds = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The instance\'s operating system.
instance_os :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_os = Lens.lens (\Instance' {os} -> os) (\s@Instance' {} a -> s {os = a} :: Instance)

-- | The instance\'s platform.
instance_platform :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_platform = Lens.lens (\Instance' {platform} -> platform) (\s@Instance' {} a -> s {platform = a} :: Instance)

-- | The instance\'s private DNS name.
instance_privateDns :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateDns = Lens.lens (\Instance' {privateDns} -> privateDns) (\s@Instance' {} a -> s {privateDns = a} :: Instance)

-- | The instance\'s private IP address.
instance_privateIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIp = Lens.lens (\Instance' {privateIp} -> privateIp) (\s@Instance' {} a -> s {privateIp = a} :: Instance)

-- | The instance public DNS name.
instance_publicDns :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicDns = Lens.lens (\Instance' {publicDns} -> publicDns) (\s@Instance' {} a -> s {publicDns = a} :: Instance)

-- | The instance public IP address.
instance_publicIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIp = Lens.lens (\Instance' {publicIp} -> publicIp) (\s@Instance' {} a -> s {publicIp = a} :: Instance)

-- | For registered instances, who performed the registration.
instance_registeredBy :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_registeredBy = Lens.lens (\Instance' {registeredBy} -> registeredBy) (\s@Instance' {} a -> s {registeredBy = a} :: Instance)

-- | The instance\'s reported AWS OpsWorks Stacks agent version.
instance_reportedAgentVersion :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_reportedAgentVersion = Lens.lens (\Instance' {reportedAgentVersion} -> reportedAgentVersion) (\s@Instance' {} a -> s {reportedAgentVersion = a} :: Instance)

-- | For registered instances, the reported operating system.
instance_reportedOs :: Lens.Lens' Instance (Prelude.Maybe ReportedOs)
instance_reportedOs = Lens.lens (\Instance' {reportedOs} -> reportedOs) (\s@Instance' {} a -> s {reportedOs = a} :: Instance)

-- | The instance\'s root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
instance_rootDeviceType :: Lens.Lens' Instance (Prelude.Maybe RootDeviceType)
instance_rootDeviceType = Lens.lens (\Instance' {rootDeviceType} -> rootDeviceType) (\s@Instance' {} a -> s {rootDeviceType = a} :: Instance)

-- | The root device volume ID.
instance_rootDeviceVolumeId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_rootDeviceVolumeId = Lens.lens (\Instance' {rootDeviceVolumeId} -> rootDeviceVolumeId) (\s@Instance' {} a -> s {rootDeviceVolumeId = a} :: Instance)

-- | An array containing the instance security group IDs.
instance_securityGroupIds :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_securityGroupIds = Lens.lens (\Instance' {securityGroupIds} -> securityGroupIds) (\s@Instance' {} a -> s {securityGroupIds = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
instance_sshHostDsaKeyFingerprint :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshHostDsaKeyFingerprint = Lens.lens (\Instance' {sshHostDsaKeyFingerprint} -> sshHostDsaKeyFingerprint) (\s@Instance' {} a -> s {sshHostDsaKeyFingerprint = a} :: Instance)

-- | The SSH key\'s RSA fingerprint.
instance_sshHostRsaKeyFingerprint :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshHostRsaKeyFingerprint = Lens.lens (\Instance' {sshHostRsaKeyFingerprint} -> sshHostRsaKeyFingerprint) (\s@Instance' {} a -> s {sshHostRsaKeyFingerprint = a} :: Instance)

-- | The instance\'s Amazon EC2 key-pair name.
instance_sshKeyName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | The stack ID.
instance_stackId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_stackId = Lens.lens (\Instance' {stackId} -> stackId) (\s@Instance' {} a -> s {stackId = a} :: Instance)

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

-- | The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
instance_subnetId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_subnetId = Lens.lens (\Instance' {subnetId} -> subnetId) (\s@Instance' {} a -> s {subnetId = a} :: Instance)

-- | The instance\'s tenancy option, such as @dedicated@ or @host@.
instance_tenancy :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_tenancy = Lens.lens (\Instance' {tenancy} -> tenancy) (\s@Instance' {} a -> s {tenancy = a} :: Instance)

-- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
instance_virtualizationType :: Lens.Lens' Instance (Prelude.Maybe VirtualizationType)
instance_virtualizationType = Lens.lens (\Instance' {virtualizationType} -> virtualizationType) (\s@Instance' {} a -> s {virtualizationType = a} :: Instance)

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "AgentVersion")
            Prelude.<*> (x Data..:? "AmiId")
            Prelude.<*> (x Data..:? "Architecture")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AutoScalingType")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> ( x
                            Data..:? "BlockDeviceMappings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "EbsOptimized")
            Prelude.<*> (x Data..:? "Ec2InstanceId")
            Prelude.<*> (x Data..:? "EcsClusterArn")
            Prelude.<*> (x Data..:? "EcsContainerInstanceArn")
            Prelude.<*> (x Data..:? "ElasticIp")
            Prelude.<*> (x Data..:? "Hostname")
            Prelude.<*> (x Data..:? "InfrastructureClass")
            Prelude.<*> (x Data..:? "InstallUpdatesOnBoot")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "InstanceProfileArn")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "LastServiceErrorId")
            Prelude.<*> (x Data..:? "LayerIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Os")
            Prelude.<*> (x Data..:? "Platform")
            Prelude.<*> (x Data..:? "PrivateDns")
            Prelude.<*> (x Data..:? "PrivateIp")
            Prelude.<*> (x Data..:? "PublicDns")
            Prelude.<*> (x Data..:? "PublicIp")
            Prelude.<*> (x Data..:? "RegisteredBy")
            Prelude.<*> (x Data..:? "ReportedAgentVersion")
            Prelude.<*> (x Data..:? "ReportedOs")
            Prelude.<*> (x Data..:? "RootDeviceType")
            Prelude.<*> (x Data..:? "RootDeviceVolumeId")
            Prelude.<*> ( x
                            Data..:? "SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SshHostDsaKeyFingerprint")
            Prelude.<*> (x Data..:? "SshHostRsaKeyFingerprint")
            Prelude.<*> (x Data..:? "SshKeyName")
            Prelude.<*> (x Data..:? "StackId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..:? "Tenancy")
            Prelude.<*> (x Data..:? "VirtualizationType")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` autoScalingType
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` ec2InstanceId
      `Prelude.hashWithSalt` ecsClusterArn
      `Prelude.hashWithSalt` ecsContainerInstanceArn
      `Prelude.hashWithSalt` elasticIp
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` infrastructureClass
      `Prelude.hashWithSalt` installUpdatesOnBoot
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceProfileArn
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` lastServiceErrorId
      `Prelude.hashWithSalt` layerIds
      `Prelude.hashWithSalt` os
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` privateDns
      `Prelude.hashWithSalt` privateIp
      `Prelude.hashWithSalt` publicDns
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` registeredBy
      `Prelude.hashWithSalt` reportedAgentVersion
      `Prelude.hashWithSalt` reportedOs
      `Prelude.hashWithSalt` rootDeviceType
      `Prelude.hashWithSalt` rootDeviceVolumeId
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` sshHostDsaKeyFingerprint
      `Prelude.hashWithSalt` sshHostRsaKeyFingerprint
      `Prelude.hashWithSalt` sshKeyName
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` virtualizationType

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf autoScalingType
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf ec2InstanceId
      `Prelude.seq` Prelude.rnf ecsClusterArn
      `Prelude.seq` Prelude.rnf ecsContainerInstanceArn
      `Prelude.seq` Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf infrastructureClass
      `Prelude.seq` Prelude.rnf installUpdatesOnBoot
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceProfileArn
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf lastServiceErrorId
      `Prelude.seq` Prelude.rnf layerIds
      `Prelude.seq` Prelude.rnf os
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf privateDns
      `Prelude.seq` Prelude.rnf
        privateIp
      `Prelude.seq` Prelude.rnf
        publicDns
      `Prelude.seq` Prelude.rnf
        publicIp
      `Prelude.seq` Prelude.rnf
        registeredBy
      `Prelude.seq` Prelude.rnf
        reportedAgentVersion
      `Prelude.seq` Prelude.rnf
        reportedOs
      `Prelude.seq` Prelude.rnf
        rootDeviceType
      `Prelude.seq` Prelude.rnf
        rootDeviceVolumeId
      `Prelude.seq` Prelude.rnf
        securityGroupIds
      `Prelude.seq` Prelude.rnf
        sshHostDsaKeyFingerprint
      `Prelude.seq` Prelude.rnf
        sshHostRsaKeyFingerprint
      `Prelude.seq` Prelude.rnf
        sshKeyName
      `Prelude.seq` Prelude.rnf
        stackId
      `Prelude.seq` Prelude.rnf
        status
      `Prelude.seq` Prelude.rnf
        subnetId
      `Prelude.seq` Prelude.rnf
        tenancy
      `Prelude.seq` Prelude.rnf
        virtualizationType
