{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an instance in a specified stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html Adding an Instance to a Layer>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.CreateInstance
  ( -- * Creating a Request
    CreateInstance (..),
    newCreateInstance,

    -- * Request Lenses
    createInstance_agentVersion,
    createInstance_amiId,
    createInstance_architecture,
    createInstance_autoScalingType,
    createInstance_availabilityZone,
    createInstance_blockDeviceMappings,
    createInstance_ebsOptimized,
    createInstance_hostname,
    createInstance_installUpdatesOnBoot,
    createInstance_os,
    createInstance_rootDeviceType,
    createInstance_sshKeyName,
    createInstance_subnetId,
    createInstance_tenancy,
    createInstance_virtualizationType,
    createInstance_stackId,
    createInstance_layerIds,
    createInstance_instanceType,

    -- * Destructuring the Response
    CreateInstanceResponse (..),
    newCreateInstanceResponse,

    -- * Response Lenses
    createInstanceResponse_instanceId,
    createInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { -- | The default AWS OpsWorks Stacks agent version. You have the following
    -- options:
    --
    -- -   @INHERIT@ - Use the stack\'s default agent version setting.
    --
    -- -   /version_number/ - Use the specified agent version. This value
    --     overrides the stack\'s default setting. To update the agent version,
    --     edit the instance configuration and specify a new version. AWS
    --     OpsWorks Stacks then automatically installs that version on the
    --     instance.
    --
    -- The default setting is @INHERIT@. To specify an agent version, you must
    -- use the complete version number, not the abbreviated number shown on the
    -- console. For a list of available agent version numbers, call
    -- DescribeAgentVersions. AgentVersion cannot be set to Chef 12.2.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | A custom AMI ID to be used to create the instance. The AMI should be
    -- based on one of the supported operating systems. For more information,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
    --
    -- If you specify a custom AMI, you must set @Os@ to @Custom@.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The instance architecture. The default option is @x86_64@. Instance
    -- types do not necessarily support both architectures. For a list of the
    -- architectures that are supported by the different instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
    architecture :: Prelude.Maybe Architecture,
    -- | For load-based or time-based instances, the type. Windows stacks can use
    -- only time-based instances.
    autoScalingType :: Prelude.Maybe AutoScalingType,
    -- | The instance Availability Zone. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
    -- block devices. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>.
    -- Note that block device mappings are not supported for custom AMIs.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | Whether to create an Amazon EBS-optimized instance.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The instance host name.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. To control when updates are
    -- installed, set this value to @false@. You must then update your
    -- instances manually by using CreateDeployment to run the
    -- @update_dependencies@ stack command or by manually running @yum@ (Amazon
    -- Linux) or @apt-get@ (Ubuntu) on the instances.
    --
    -- We strongly recommend using the default value of @true@ to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Prelude.Maybe Prelude.Bool,
    -- | The instance\'s operating system, which must be set to one of the
    -- following.
    --
    -- -   A supported Linux operating system: An Amazon Linux version, such as
    --     @Amazon Linux 2018.03@, @Amazon Linux 2017.09@,
    --     @Amazon Linux 2017.03@, @Amazon Linux 2016.09@,
    --     @Amazon Linux 2016.03@, @Amazon Linux 2015.09@, or
    --     @Amazon Linux 2015.03@.
    --
    -- -   A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@,
    --     @Ubuntu 14.04 LTS@, or @Ubuntu 12.04 LTS@.
    --
    -- -   @CentOS Linux 7@
    --
    -- -   @Red Hat Enterprise Linux 7@
    --
    -- -   A supported Windows operating system, such as
    --     @Microsoft Windows Server 2012 R2 Base@,
    --     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
    --     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
    --     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
    --
    -- -   A custom AMI: @Custom@.
    --
    -- For more information about the supported operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
    --
    -- The default option is the current Amazon Linux version. If you set this
    -- parameter to @Custom@, you must use the CreateInstance action\'s AmiId
    -- parameter to specify the custom AMI that you want to use. Block device
    -- mappings are not supported if the value is @Custom@. For more
    -- information about supported operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>For
    -- more information about how to use custom AMIs with AWS OpsWorks Stacks,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
    os :: Prelude.Maybe Prelude.Text,
    -- | The instance root device type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
    rootDeviceType :: Prelude.Maybe RootDeviceType,
    -- | The instance\'s Amazon EC2 key-pair name.
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance\'s subnet. If the stack is running in a VPC, you
    -- can use this parameter to override the stack\'s default subnet ID value
    -- and direct AWS OpsWorks Stacks to launch the instance in a different
    -- subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s tenancy option. The default option is no tenancy, or if
    -- the instance is running in a VPC, inherit tenancy settings from the VPC.
    -- The following are valid values for this parameter: @dedicated@,
    -- @default@, or @host@. Because there are costs associated with changes in
    -- tenancy options, we recommend that you research tenancy options before
    -- choosing them for your instances. For more information about dedicated
    -- hosts, see
    -- <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview>
    -- and
    -- <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts>.
    -- For more information about dedicated instances, see
    -- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances>
    -- and
    -- <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances>.
    tenancy :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s virtualization type, @paravirtual@ or @hvm@.
    virtualizationType :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Text,
    -- | An array that contains the instance\'s layer IDs.
    layerIds :: [Prelude.Text],
    -- | The instance type, such as @t2.micro@. For a list of supported instance
    -- types, open the stack in the console, choose __Instances__, and choose
    -- __+ Instance__. The __Size__ list contains the currently supported
    -- types. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
    -- The parameter values that you use to specify the various types are in
    -- the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentVersion', 'createInstance_agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following
-- options:
--
-- -   @INHERIT@ - Use the stack\'s default agent version setting.
--
-- -   /version_number/ - Use the specified agent version. This value
--     overrides the stack\'s default setting. To update the agent version,
--     edit the instance configuration and specify a new version. AWS
--     OpsWorks Stacks then automatically installs that version on the
--     instance.
--
-- The default setting is @INHERIT@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions. AgentVersion cannot be set to Chef 12.2.
--
-- 'amiId', 'createInstance_amiId' - A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the supported operating systems. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
--
-- 'architecture', 'createInstance_architecture' - The instance architecture. The default option is @x86_64@. Instance
-- types do not necessarily support both architectures. For a list of the
-- architectures that are supported by the different instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
--
-- 'autoScalingType', 'createInstance_autoScalingType' - For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
--
-- 'availabilityZone', 'createInstance_availabilityZone' - The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- 'blockDeviceMappings', 'createInstance_blockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block devices. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>.
-- Note that block device mappings are not supported for custom AMIs.
--
-- 'ebsOptimized', 'createInstance_ebsOptimized' - Whether to create an Amazon EBS-optimized instance.
--
-- 'hostname', 'createInstance_hostname' - The instance host name.
--
-- 'installUpdatesOnBoot', 'createInstance_installUpdatesOnBoot' - Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@ to ensure that
-- your instances have the latest security updates.
--
-- 'os', 'createInstance_os' - The instance\'s operating system, which must be set to one of the
-- following.
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2018.03@, @Amazon Linux 2017.09@,
--     @Amazon Linux 2017.03@, @Amazon Linux 2016.09@,
--     @Amazon Linux 2016.03@, @Amazon Linux 2015.09@, or
--     @Amazon Linux 2015.03@.
--
-- -   A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@,
--     @Ubuntu 14.04 LTS@, or @Ubuntu 12.04 LTS@.
--
-- -   @CentOS Linux 7@
--
-- -   @Red Hat Enterprise Linux 7@
--
-- -   A supported Windows operating system, such as
--     @Microsoft Windows Server 2012 R2 Base@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
--     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
--
-- -   A custom AMI: @Custom@.
--
-- For more information about the supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- The default option is the current Amazon Linux version. If you set this
-- parameter to @Custom@, you must use the CreateInstance action\'s AmiId
-- parameter to specify the custom AMI that you want to use. Block device
-- mappings are not supported if the value is @Custom@. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>For
-- more information about how to use custom AMIs with AWS OpsWorks Stacks,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- 'rootDeviceType', 'createInstance_rootDeviceType' - The instance root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
--
-- 'sshKeyName', 'createInstance_sshKeyName' - The instance\'s Amazon EC2 key-pair name.
--
-- 'subnetId', 'createInstance_subnetId' - The ID of the instance\'s subnet. If the stack is running in a VPC, you
-- can use this parameter to override the stack\'s default subnet ID value
-- and direct AWS OpsWorks Stacks to launch the instance in a different
-- subnet.
--
-- 'tenancy', 'createInstance_tenancy' - The instance\'s tenancy option. The default option is no tenancy, or if
-- the instance is running in a VPC, inherit tenancy settings from the VPC.
-- The following are valid values for this parameter: @dedicated@,
-- @default@, or @host@. Because there are costs associated with changes in
-- tenancy options, we recommend that you research tenancy options before
-- choosing them for your instances. For more information about dedicated
-- hosts, see
-- <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview>
-- and
-- <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts>.
-- For more information about dedicated instances, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances>
-- and
-- <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances>.
--
-- 'virtualizationType', 'createInstance_virtualizationType' - The instance\'s virtualization type, @paravirtual@ or @hvm@.
--
-- 'stackId', 'createInstance_stackId' - The stack ID.
--
-- 'layerIds', 'createInstance_layerIds' - An array that contains the instance\'s layer IDs.
--
-- 'instanceType', 'createInstance_instanceType' - The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
newCreateInstance ::
  -- | 'stackId'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  CreateInstance
newCreateInstance pStackId_ pInstanceType_ =
  CreateInstance'
    { agentVersion = Prelude.Nothing,
      amiId = Prelude.Nothing,
      architecture = Prelude.Nothing,
      autoScalingType = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      hostname = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      os = Prelude.Nothing,
      rootDeviceType = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      virtualizationType = Prelude.Nothing,
      stackId = pStackId_,
      layerIds = Prelude.mempty,
      instanceType = pInstanceType_
    }

-- | The default AWS OpsWorks Stacks agent version. You have the following
-- options:
--
-- -   @INHERIT@ - Use the stack\'s default agent version setting.
--
-- -   /version_number/ - Use the specified agent version. This value
--     overrides the stack\'s default setting. To update the agent version,
--     edit the instance configuration and specify a new version. AWS
--     OpsWorks Stacks then automatically installs that version on the
--     instance.
--
-- The default setting is @INHERIT@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions. AgentVersion cannot be set to Chef 12.2.
createInstance_agentVersion :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_agentVersion = Lens.lens (\CreateInstance' {agentVersion} -> agentVersion) (\s@CreateInstance' {} a -> s {agentVersion = a} :: CreateInstance)

-- | A custom AMI ID to be used to create the instance. The AMI should be
-- based on one of the supported operating systems. For more information,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- If you specify a custom AMI, you must set @Os@ to @Custom@.
createInstance_amiId :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_amiId = Lens.lens (\CreateInstance' {amiId} -> amiId) (\s@CreateInstance' {} a -> s {amiId = a} :: CreateInstance)

-- | The instance architecture. The default option is @x86_64@. Instance
-- types do not necessarily support both architectures. For a list of the
-- architectures that are supported by the different instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
createInstance_architecture :: Lens.Lens' CreateInstance (Prelude.Maybe Architecture)
createInstance_architecture = Lens.lens (\CreateInstance' {architecture} -> architecture) (\s@CreateInstance' {} a -> s {architecture = a} :: CreateInstance)

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
createInstance_autoScalingType :: Lens.Lens' CreateInstance (Prelude.Maybe AutoScalingType)
createInstance_autoScalingType = Lens.lens (\CreateInstance' {autoScalingType} -> autoScalingType) (\s@CreateInstance' {} a -> s {autoScalingType = a} :: CreateInstance)

-- | The instance Availability Zone. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
createInstance_availabilityZone :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_availabilityZone = Lens.lens (\CreateInstance' {availabilityZone} -> availabilityZone) (\s@CreateInstance' {} a -> s {availabilityZone = a} :: CreateInstance)

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block devices. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>.
-- Note that block device mappings are not supported for custom AMIs.
createInstance_blockDeviceMappings :: Lens.Lens' CreateInstance (Prelude.Maybe [BlockDeviceMapping])
createInstance_blockDeviceMappings = Lens.lens (\CreateInstance' {blockDeviceMappings} -> blockDeviceMappings) (\s@CreateInstance' {} a -> s {blockDeviceMappings = a} :: CreateInstance) Prelude.. Lens.mapping Lens.coerced

-- | Whether to create an Amazon EBS-optimized instance.
createInstance_ebsOptimized :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Bool)
createInstance_ebsOptimized = Lens.lens (\CreateInstance' {ebsOptimized} -> ebsOptimized) (\s@CreateInstance' {} a -> s {ebsOptimized = a} :: CreateInstance)

-- | The instance host name.
createInstance_hostname :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_hostname = Lens.lens (\CreateInstance' {hostname} -> hostname) (\s@CreateInstance' {} a -> s {hostname = a} :: CreateInstance)

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@ to ensure that
-- your instances have the latest security updates.
createInstance_installUpdatesOnBoot :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Bool)
createInstance_installUpdatesOnBoot = Lens.lens (\CreateInstance' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@CreateInstance' {} a -> s {installUpdatesOnBoot = a} :: CreateInstance)

-- | The instance\'s operating system, which must be set to one of the
-- following.
--
-- -   A supported Linux operating system: An Amazon Linux version, such as
--     @Amazon Linux 2018.03@, @Amazon Linux 2017.09@,
--     @Amazon Linux 2017.03@, @Amazon Linux 2016.09@,
--     @Amazon Linux 2016.03@, @Amazon Linux 2015.09@, or
--     @Amazon Linux 2015.03@.
--
-- -   A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@,
--     @Ubuntu 14.04 LTS@, or @Ubuntu 12.04 LTS@.
--
-- -   @CentOS Linux 7@
--
-- -   @Red Hat Enterprise Linux 7@
--
-- -   A supported Windows operating system, such as
--     @Microsoft Windows Server 2012 R2 Base@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Express@,
--     @Microsoft Windows Server 2012 R2 with SQL Server Standard@, or
--     @Microsoft Windows Server 2012 R2 with SQL Server Web@.
--
-- -   A custom AMI: @Custom@.
--
-- For more information about the supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- The default option is the current Amazon Linux version. If you set this
-- parameter to @Custom@, you must use the CreateInstance action\'s AmiId
-- parameter to specify the custom AMI that you want to use. Block device
-- mappings are not supported if the value is @Custom@. For more
-- information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>For
-- more information about how to use custom AMIs with AWS OpsWorks Stacks,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
createInstance_os :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_os = Lens.lens (\CreateInstance' {os} -> os) (\s@CreateInstance' {} a -> s {os = a} :: CreateInstance)

-- | The instance root device type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
createInstance_rootDeviceType :: Lens.Lens' CreateInstance (Prelude.Maybe RootDeviceType)
createInstance_rootDeviceType = Lens.lens (\CreateInstance' {rootDeviceType} -> rootDeviceType) (\s@CreateInstance' {} a -> s {rootDeviceType = a} :: CreateInstance)

-- | The instance\'s Amazon EC2 key-pair name.
createInstance_sshKeyName :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_sshKeyName = Lens.lens (\CreateInstance' {sshKeyName} -> sshKeyName) (\s@CreateInstance' {} a -> s {sshKeyName = a} :: CreateInstance)

-- | The ID of the instance\'s subnet. If the stack is running in a VPC, you
-- can use this parameter to override the stack\'s default subnet ID value
-- and direct AWS OpsWorks Stacks to launch the instance in a different
-- subnet.
createInstance_subnetId :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_subnetId = Lens.lens (\CreateInstance' {subnetId} -> subnetId) (\s@CreateInstance' {} a -> s {subnetId = a} :: CreateInstance)

-- | The instance\'s tenancy option. The default option is no tenancy, or if
-- the instance is running in a VPC, inherit tenancy settings from the VPC.
-- The following are valid values for this parameter: @dedicated@,
-- @default@, or @host@. Because there are costs associated with changes in
-- tenancy options, we recommend that you research tenancy options before
-- choosing them for your instances. For more information about dedicated
-- hosts, see
-- <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview>
-- and
-- <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts>.
-- For more information about dedicated instances, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances>
-- and
-- <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances>.
createInstance_tenancy :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_tenancy = Lens.lens (\CreateInstance' {tenancy} -> tenancy) (\s@CreateInstance' {} a -> s {tenancy = a} :: CreateInstance)

-- | The instance\'s virtualization type, @paravirtual@ or @hvm@.
createInstance_virtualizationType :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_virtualizationType = Lens.lens (\CreateInstance' {virtualizationType} -> virtualizationType) (\s@CreateInstance' {} a -> s {virtualizationType = a} :: CreateInstance)

-- | The stack ID.
createInstance_stackId :: Lens.Lens' CreateInstance Prelude.Text
createInstance_stackId = Lens.lens (\CreateInstance' {stackId} -> stackId) (\s@CreateInstance' {} a -> s {stackId = a} :: CreateInstance)

-- | An array that contains the instance\'s layer IDs.
createInstance_layerIds :: Lens.Lens' CreateInstance [Prelude.Text]
createInstance_layerIds = Lens.lens (\CreateInstance' {layerIds} -> layerIds) (\s@CreateInstance' {} a -> s {layerIds = a} :: CreateInstance) Prelude.. Lens.coerced

-- | The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
createInstance_instanceType :: Lens.Lens' CreateInstance Prelude.Text
createInstance_instanceType = Lens.lens (\CreateInstance' {instanceType} -> instanceType) (\s@CreateInstance' {} a -> s {instanceType = a} :: CreateInstance)

instance Core.AWSRequest CreateInstance where
  type
    AWSResponse CreateInstance =
      CreateInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Prelude.<$> (x Data..?> "InstanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstance where
  hashWithSalt _salt CreateInstance' {..} =
    _salt
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` autoScalingType
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` installUpdatesOnBoot
      `Prelude.hashWithSalt` os
      `Prelude.hashWithSalt` rootDeviceType
      `Prelude.hashWithSalt` sshKeyName
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` virtualizationType
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` layerIds
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData CreateInstance where
  rnf CreateInstance' {..} =
    Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf amiId
      `Prelude.seq` Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf autoScalingType
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf installUpdatesOnBoot
      `Prelude.seq` Prelude.rnf os
      `Prelude.seq` Prelude.rnf rootDeviceType
      `Prelude.seq` Prelude.rnf sshKeyName
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf virtualizationType
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf layerIds
      `Prelude.seq` Prelude.rnf instanceType

instance Data.ToHeaders CreateInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.CreateInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateInstance where
  toJSON CreateInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentVersion" Data..=) Prelude.<$> agentVersion,
            ("AmiId" Data..=) Prelude.<$> amiId,
            ("Architecture" Data..=) Prelude.<$> architecture,
            ("AutoScalingType" Data..=)
              Prelude.<$> autoScalingType,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("BlockDeviceMappings" Data..=)
              Prelude.<$> blockDeviceMappings,
            ("EbsOptimized" Data..=) Prelude.<$> ebsOptimized,
            ("Hostname" Data..=) Prelude.<$> hostname,
            ("InstallUpdatesOnBoot" Data..=)
              Prelude.<$> installUpdatesOnBoot,
            ("Os" Data..=) Prelude.<$> os,
            ("RootDeviceType" Data..=)
              Prelude.<$> rootDeviceType,
            ("SshKeyName" Data..=) Prelude.<$> sshKeyName,
            ("SubnetId" Data..=) Prelude.<$> subnetId,
            ("Tenancy" Data..=) Prelude.<$> tenancy,
            ("VirtualizationType" Data..=)
              Prelude.<$> virtualizationType,
            Prelude.Just ("StackId" Data..= stackId),
            Prelude.Just ("LayerIds" Data..= layerIds),
            Prelude.Just ("InstanceType" Data..= instanceType)
          ]
      )

instance Data.ToPath CreateInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInstance where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @CreateInstance@ request.
--
-- /See:/ 'newCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'createInstanceResponse_instanceId' - The instance ID.
--
-- 'httpStatus', 'createInstanceResponse_httpStatus' - The response's http status code.
newCreateInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInstanceResponse
newCreateInstanceResponse pHttpStatus_ =
  CreateInstanceResponse'
    { instanceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance ID.
createInstanceResponse_instanceId :: Lens.Lens' CreateInstanceResponse (Prelude.Maybe Prelude.Text)
createInstanceResponse_instanceId = Lens.lens (\CreateInstanceResponse' {instanceId} -> instanceId) (\s@CreateInstanceResponse' {} a -> s {instanceId = a} :: CreateInstanceResponse)

-- | The response's http status code.
createInstanceResponse_httpStatus :: Lens.Lens' CreateInstanceResponse Prelude.Int
createInstanceResponse_httpStatus = Lens.lens (\CreateInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceResponse' {} a -> s {httpStatus = a} :: CreateInstanceResponse)

instance Prelude.NFData CreateInstanceResponse where
  rnf CreateInstanceResponse' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf httpStatus
