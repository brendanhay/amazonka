{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an instance in a specified stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html Adding an Instance to a Layer> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateInstance
  ( -- * Creating a request
    CreateInstance (..),
    mkCreateInstance,

    -- ** Request lenses
    ciInstallUpdatesOnBoot,
    ciVirtualizationType,
    ciHostname,
    ciSSHKeyName,
    ciAgentVersion,
    ciSubnetId,
    ciInstanceType,
    ciEBSOptimized,
    ciOS,
    ciAvailabilityZone,
    ciTenancy,
    ciAutoScalingType,
    ciLayerIds,
    ciArchitecture,
    ciAMIId,
    ciStackId,
    ciRootDeviceType,
    ciBlockDeviceMappings,

    -- * Destructuring the response
    CreateInstanceResponse (..),
    mkCreateInstanceResponse,

    -- ** Response lenses
    cirsInstanceId,
    cirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Lude.Maybe Lude.Bool,
    -- | The instance's virtualization type, @paravirtual@ or @hvm@ .
    virtualizationType :: Lude.Maybe Lude.Text,
    -- | The instance host name.
    hostname :: Lude.Maybe Lude.Text,
    -- | The instance's Amazon EC2 key-pair name.
    sshKeyName :: Lude.Maybe Lude.Text,
    -- | The default AWS OpsWorks Stacks agent version. You have the following options:
    --
    --
    --     * @INHERIT@ - Use the stack's default agent version setting.
    --
    --
    --     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance.
    --
    --
    -- The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
    agentVersion :: Lude.Maybe Lude.Text,
    -- | The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Lude.Text,
    -- | Whether to create an Amazon EBS-optimized instance.
    ebsOptimized :: Lude.Maybe Lude.Bool,
    -- | The instance's operating system, which must be set to one of the following.
    --
    --
    --     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2018.03@ , @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .
    --
    --
    --     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .
    --
    --
    --     * @CentOS Linux 7@
    --
    --
    --     * @Red Hat Enterprise Linux 7@
    --
    --
    --     * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
    --
    --
    --     * A custom AMI: @Custom@ .
    --
    --
    -- For more information about the supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
    -- The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the 'CreateInstance' action's AmiId parameter to specify the custom AMI that you want to use. Block device mappings are not supported if the value is @Custom@ . For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> For more information about how to use custom AMIs with AWS OpsWorks Stacks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
    os :: Lude.Maybe Lude.Text,
    -- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
    tenancy :: Lude.Maybe Lude.Text,
    -- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
    autoScalingType :: Lude.Maybe AutoScalingType,
    -- | An array that contains the instance's layer IDs.
    layerIds :: [Lude.Text],
    -- | The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
    architecture :: Lude.Maybe Architecture,
    -- | A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
    amiId :: Lude.Maybe Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Text,
    -- | The instance root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    rootDeviceType :: Lude.Maybe RootDeviceType,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstance' with the minimum fields required to make a request.
--
-- * 'installUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
-- * 'virtualizationType' - The instance's virtualization type, @paravirtual@ or @hvm@ .
-- * 'hostname' - The instance host name.
-- * 'sshKeyName' - The instance's Amazon EC2 key-pair name.
-- * 'agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * @INHERIT@ - Use the stack's default agent version setting.
--
--
--     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance.
--
--
-- The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
-- * 'subnetId' - The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
-- * 'instanceType' - The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
-- * 'ebsOptimized' - Whether to create an Amazon EBS-optimized instance.
-- * 'os' - The instance's operating system, which must be set to one of the following.
--
--
--     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2018.03@ , @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .
--
--
--     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .
--
--
--     * @CentOS Linux 7@
--
--
--     * @Red Hat Enterprise Linux 7@
--
--
--     * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
--
--
--     * A custom AMI: @Custom@ .
--
--
-- For more information about the supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
-- The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the 'CreateInstance' action's AmiId parameter to specify the custom AMI that you want to use. Block device mappings are not supported if the value is @Custom@ . For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> For more information about how to use custom AMIs with AWS OpsWorks Stacks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
-- * 'availabilityZone' - The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'tenancy' - The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
-- * 'autoScalingType' - For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
-- * 'layerIds' - An array that contains the instance's layer IDs.
-- * 'architecture' - The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
-- * 'amiId' - A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
-- * 'stackId' - The stack ID.
-- * 'rootDeviceType' - The instance root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
-- * 'blockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
mkCreateInstance ::
  -- | 'instanceType'
  Lude.Text ->
  -- | 'stackId'
  Lude.Text ->
  CreateInstance
mkCreateInstance pInstanceType_ pStackId_ =
  CreateInstance'
    { installUpdatesOnBoot = Lude.Nothing,
      virtualizationType = Lude.Nothing,
      hostname = Lude.Nothing,
      sshKeyName = Lude.Nothing,
      agentVersion = Lude.Nothing,
      subnetId = Lude.Nothing,
      instanceType = pInstanceType_,
      ebsOptimized = Lude.Nothing,
      os = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      tenancy = Lude.Nothing,
      autoScalingType = Lude.Nothing,
      layerIds = Lude.mempty,
      architecture = Lude.Nothing,
      amiId = Lude.Nothing,
      stackId = pStackId_,
      rootDeviceType = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing
    }

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstallUpdatesOnBoot :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Bool)
ciInstallUpdatesOnBoot = Lens.lens (installUpdatesOnBoot :: CreateInstance -> Lude.Maybe Lude.Bool) (\s a -> s {installUpdatesOnBoot = a} :: CreateInstance)
{-# DEPRECATED ciInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The instance's virtualization type, @paravirtual@ or @hvm@ .
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVirtualizationType :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciVirtualizationType = Lens.lens (virtualizationType :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {virtualizationType = a} :: CreateInstance)
{-# DEPRECATED ciVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | The instance host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciHostname :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciHostname = Lens.lens (hostname :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: CreateInstance)
{-# DEPRECATED ciHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The instance's Amazon EC2 key-pair name.
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSSHKeyName :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciSSHKeyName = Lens.lens (sshKeyName :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {sshKeyName = a} :: CreateInstance)
{-# DEPRECATED ciSSHKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

-- | The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * @INHERIT@ - Use the stack's default agent version setting.
--
--
--     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance.
--
--
-- The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAgentVersion :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciAgentVersion = Lens.lens (agentVersion :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: CreateInstance)
{-# DEPRECATED ciAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSubnetId :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciSubnetId = Lens.lens (subnetId :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: CreateInstance)
{-# DEPRECATED ciSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceType :: Lens.Lens' CreateInstance Lude.Text
ciInstanceType = Lens.lens (instanceType :: CreateInstance -> Lude.Text) (\s a -> s {instanceType = a} :: CreateInstance)
{-# DEPRECATED ciInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Whether to create an Amazon EBS-optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciEBSOptimized :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Bool)
ciEBSOptimized = Lens.lens (ebsOptimized :: CreateInstance -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: CreateInstance)
{-# DEPRECATED ciEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The instance's operating system, which must be set to one of the following.
--
--
--     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2018.03@ , @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .
--
--
--     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .
--
--
--     * @CentOS Linux 7@
--
--
--     * @Red Hat Enterprise Linux 7@
--
--
--     * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
--
--
--     * A custom AMI: @Custom@ .
--
--
-- For more information about the supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
-- The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the 'CreateInstance' action's AmiId parameter to specify the custom AMI that you want to use. Block device mappings are not supported if the value is @Custom@ . For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> For more information about how to use custom AMIs with AWS OpsWorks Stacks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOS :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciOS = Lens.lens (os :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {os = a} :: CreateInstance)
{-# DEPRECATED ciOS "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAvailabilityZone :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciAvailabilityZone = Lens.lens (availabilityZone :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateInstance)
{-# DEPRECATED ciAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTenancy :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciTenancy = Lens.lens (tenancy :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {tenancy = a} :: CreateInstance)
{-# DEPRECATED ciTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
--
-- /Note:/ Consider using 'autoScalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAutoScalingType :: Lens.Lens' CreateInstance (Lude.Maybe AutoScalingType)
ciAutoScalingType = Lens.lens (autoScalingType :: CreateInstance -> Lude.Maybe AutoScalingType) (\s a -> s {autoScalingType = a} :: CreateInstance)
{-# DEPRECATED ciAutoScalingType "Use generic-lens or generic-optics with 'autoScalingType' instead." #-}

-- | An array that contains the instance's layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLayerIds :: Lens.Lens' CreateInstance [Lude.Text]
ciLayerIds = Lens.lens (layerIds :: CreateInstance -> [Lude.Text]) (\s a -> s {layerIds = a} :: CreateInstance)
{-# DEPRECATED ciLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciArchitecture :: Lens.Lens' CreateInstance (Lude.Maybe Architecture)
ciArchitecture = Lens.lens (architecture :: CreateInstance -> Lude.Maybe Architecture) (\s a -> s {architecture = a} :: CreateInstance)
{-# DEPRECATED ciArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAMIId :: Lens.Lens' CreateInstance (Lude.Maybe Lude.Text)
ciAMIId = Lens.lens (amiId :: CreateInstance -> Lude.Maybe Lude.Text) (\s a -> s {amiId = a} :: CreateInstance)
{-# DEPRECATED ciAMIId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStackId :: Lens.Lens' CreateInstance Lude.Text
ciStackId = Lens.lens (stackId :: CreateInstance -> Lude.Text) (\s a -> s {stackId = a} :: CreateInstance)
{-# DEPRECATED ciStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The instance root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRootDeviceType :: Lens.Lens' CreateInstance (Lude.Maybe RootDeviceType)
ciRootDeviceType = Lens.lens (rootDeviceType :: CreateInstance -> Lude.Maybe RootDeviceType) (\s a -> s {rootDeviceType = a} :: CreateInstance)
{-# DEPRECATED ciRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBlockDeviceMappings :: Lens.Lens' CreateInstance (Lude.Maybe [BlockDeviceMapping])
ciBlockDeviceMappings = Lens.lens (blockDeviceMappings :: CreateInstance -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: CreateInstance)
{-# DEPRECATED ciBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

instance Lude.AWSRequest CreateInstance where
  type Rs CreateInstance = CreateInstanceResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Lude.<$> (x Lude..?> "InstanceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CreateInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInstance where
  toJSON CreateInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstallUpdatesOnBoot" Lude..=) Lude.<$> installUpdatesOnBoot,
            ("VirtualizationType" Lude..=) Lude.<$> virtualizationType,
            ("Hostname" Lude..=) Lude.<$> hostname,
            ("SshKeyName" Lude..=) Lude.<$> sshKeyName,
            ("AgentVersion" Lude..=) Lude.<$> agentVersion,
            ("SubnetId" Lude..=) Lude.<$> subnetId,
            Lude.Just ("InstanceType" Lude..= instanceType),
            ("EbsOptimized" Lude..=) Lude.<$> ebsOptimized,
            ("Os" Lude..=) Lude.<$> os,
            ("AvailabilityZone" Lude..=) Lude.<$> availabilityZone,
            ("Tenancy" Lude..=) Lude.<$> tenancy,
            ("AutoScalingType" Lude..=) Lude.<$> autoScalingType,
            Lude.Just ("LayerIds" Lude..= layerIds),
            ("Architecture" Lude..=) Lude.<$> architecture,
            ("AmiId" Lude..=) Lude.<$> amiId,
            Lude.Just ("StackId" Lude..= stackId),
            ("RootDeviceType" Lude..=) Lude.<$> rootDeviceType,
            ("BlockDeviceMappings" Lude..=) Lude.<$> blockDeviceMappings
          ]
      )

instance Lude.ToPath CreateInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstance where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CreateInstance@ request.
--
-- /See:/ 'mkCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { -- | The instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
-- * 'responseStatus' - The response status code.
mkCreateInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstanceResponse
mkCreateInstanceResponse pResponseStatus_ =
  CreateInstanceResponse'
    { instanceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsInstanceId :: Lens.Lens' CreateInstanceResponse (Lude.Maybe Lude.Text)
cirsInstanceId = Lens.lens (instanceId :: CreateInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CreateInstanceResponse)
{-# DEPRECATED cirsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateInstanceResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstanceResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
