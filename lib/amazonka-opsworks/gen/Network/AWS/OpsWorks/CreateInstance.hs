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
    ciStackId,
    ciLayerIds,
    ciInstanceType,
    ciAgentVersion,
    ciAmiId,
    ciArchitecture,
    ciAutoScalingType,
    ciAvailabilityZone,
    ciBlockDeviceMappings,
    ciEbsOptimized,
    ciHostname,
    ciInstallUpdatesOnBoot,
    ciOs,
    ciRootDeviceType,
    ciSshKeyName,
    ciSubnetId,
    ciTenancy,
    ciVirtualizationType,

    -- * Destructuring the response
    CreateInstanceResponse (..),
    mkCreateInstanceResponse,

    -- ** Response lenses
    cirrsInstanceId,
    cirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { -- | The stack ID.
    stackId :: Types.StackId,
    -- | An array that contains the instance's layer IDs.
    layerIds :: [Types.String],
    -- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Types.InstanceType,
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
    agentVersion :: Core.Maybe Types.String,
    -- | A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
    amiId :: Core.Maybe Types.AmiId,
    -- | The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
    architecture :: Core.Maybe Types.Architecture,
    -- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
    autoScalingType :: Core.Maybe Types.AutoScalingType,
    -- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    availabilityZone :: Core.Maybe Types.AvailabilityZone,
    -- | An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | Whether to create an Amazon EBS-optimized instance.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The instance host name.
    hostname :: Core.Maybe Types.Hostname,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
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
    os :: Core.Maybe Types.Os,
    -- | The instance root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    rootDeviceType :: Core.Maybe Types.RootDeviceType,
    -- | The instance's Amazon EC2 key-pair name.
    sshKeyName :: Core.Maybe Types.SshKeyName,
    -- | The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
    tenancy :: Core.Maybe Types.Tenancy,
    -- | The instance's virtualization type, @paravirtual@ or @hvm@ .
    virtualizationType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstance' value with any optional fields omitted.
mkCreateInstance ::
  -- | 'stackId'
  Types.StackId ->
  -- | 'instanceType'
  Types.InstanceType ->
  CreateInstance
mkCreateInstance stackId instanceType =
  CreateInstance'
    { stackId,
      layerIds = Core.mempty,
      instanceType,
      agentVersion = Core.Nothing,
      amiId = Core.Nothing,
      architecture = Core.Nothing,
      autoScalingType = Core.Nothing,
      availabilityZone = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      ebsOptimized = Core.Nothing,
      hostname = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      os = Core.Nothing,
      rootDeviceType = Core.Nothing,
      sshKeyName = Core.Nothing,
      subnetId = Core.Nothing,
      tenancy = Core.Nothing,
      virtualizationType = Core.Nothing
    }

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStackId :: Lens.Lens' CreateInstance Types.StackId
ciStackId = Lens.field @"stackId"
{-# DEPRECATED ciStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | An array that contains the instance's layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLayerIds :: Lens.Lens' CreateInstance [Types.String]
ciLayerIds = Lens.field @"layerIds"
{-# DEPRECATED ciLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceType :: Lens.Lens' CreateInstance Types.InstanceType
ciInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ciInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

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
ciAgentVersion :: Lens.Lens' CreateInstance (Core.Maybe Types.String)
ciAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED ciAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A custom AMI ID to be used to create the instance. The AMI should be based on one of the supported operating systems. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAmiId :: Lens.Lens' CreateInstance (Core.Maybe Types.AmiId)
ciAmiId = Lens.field @"amiId"
{-# DEPRECATED ciAmiId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The instance architecture. The default option is @x86_64@ . Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciArchitecture :: Lens.Lens' CreateInstance (Core.Maybe Types.Architecture)
ciArchitecture = Lens.field @"architecture"
{-# DEPRECATED ciArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
--
-- /Note:/ Consider using 'autoScalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAutoScalingType :: Lens.Lens' CreateInstance (Core.Maybe Types.AutoScalingType)
ciAutoScalingType = Lens.field @"autoScalingType"
{-# DEPRECATED ciAutoScalingType "Use generic-lens or generic-optics with 'autoScalingType' instead." #-}

-- | The instance Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAvailabilityZone :: Lens.Lens' CreateInstance (Core.Maybe Types.AvailabilityZone)
ciAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ciAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block devices. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping> . Note that block device mappings are not supported for custom AMIs.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBlockDeviceMappings :: Lens.Lens' CreateInstance (Core.Maybe [Types.BlockDeviceMapping])
ciBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED ciBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Whether to create an Amazon EBS-optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciEbsOptimized :: Lens.Lens' CreateInstance (Core.Maybe Core.Bool)
ciEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED ciEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The instance host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciHostname :: Lens.Lens' CreateInstance (Core.Maybe Types.Hostname)
ciHostname = Lens.field @"hostname"
{-# DEPRECATED ciHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstallUpdatesOnBoot :: Lens.Lens' CreateInstance (Core.Maybe Core.Bool)
ciInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# DEPRECATED ciInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

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
ciOs :: Lens.Lens' CreateInstance (Core.Maybe Types.Os)
ciOs = Lens.field @"os"
{-# DEPRECATED ciOs "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The instance root device type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRootDeviceType :: Lens.Lens' CreateInstance (Core.Maybe Types.RootDeviceType)
ciRootDeviceType = Lens.field @"rootDeviceType"
{-# DEPRECATED ciRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | The instance's Amazon EC2 key-pair name.
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSshKeyName :: Lens.Lens' CreateInstance (Core.Maybe Types.SshKeyName)
ciSshKeyName = Lens.field @"sshKeyName"
{-# DEPRECATED ciSshKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

-- | The ID of the instance's subnet. If the stack is running in a VPC, you can use this parameter to override the stack's default subnet ID value and direct AWS OpsWorks Stacks to launch the instance in a different subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSubnetId :: Lens.Lens' CreateInstance (Core.Maybe Types.SubnetId)
ciSubnetId = Lens.field @"subnetId"
{-# DEPRECATED ciSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance's tenancy option. The default option is no tenancy, or if the instance is running in a VPC, inherit tenancy settings from the VPC. The following are valid values for this parameter: @dedicated@ , @default@ , or @host@ . Because there are costs associated with changes in tenancy options, we recommend that you research tenancy options before choosing them for your instances. For more information about dedicated hosts, see <http://aws.amazon.com/ec2/dedicated-hosts/ Dedicated Hosts Overview> and <http://aws.amazon.com/ec2/dedicated-hosts/ Amazon EC2 Dedicated Hosts> . For more information about dedicated instances, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/dedicated-instance.html Dedicated Instances> and <http://aws.amazon.com/ec2/purchasing-options/dedicated-instances/ Amazon EC2 Dedicated Instances> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTenancy :: Lens.Lens' CreateInstance (Core.Maybe Types.Tenancy)
ciTenancy = Lens.field @"tenancy"
{-# DEPRECATED ciTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The instance's virtualization type, @paravirtual@ or @hvm@ .
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVirtualizationType :: Lens.Lens' CreateInstance (Core.Maybe Types.String)
ciVirtualizationType = Lens.field @"virtualizationType"
{-# DEPRECATED ciVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

instance Core.FromJSON CreateInstance where
  toJSON CreateInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackId" Core..= stackId),
            Core.Just ("LayerIds" Core..= layerIds),
            Core.Just ("InstanceType" Core..= instanceType),
            ("AgentVersion" Core..=) Core.<$> agentVersion,
            ("AmiId" Core..=) Core.<$> amiId,
            ("Architecture" Core..=) Core.<$> architecture,
            ("AutoScalingType" Core..=) Core.<$> autoScalingType,
            ("AvailabilityZone" Core..=) Core.<$> availabilityZone,
            ("BlockDeviceMappings" Core..=) Core.<$> blockDeviceMappings,
            ("EbsOptimized" Core..=) Core.<$> ebsOptimized,
            ("Hostname" Core..=) Core.<$> hostname,
            ("InstallUpdatesOnBoot" Core..=) Core.<$> installUpdatesOnBoot,
            ("Os" Core..=) Core.<$> os,
            ("RootDeviceType" Core..=) Core.<$> rootDeviceType,
            ("SshKeyName" Core..=) Core.<$> sshKeyName,
            ("SubnetId" Core..=) Core.<$> subnetId,
            ("Tenancy" Core..=) Core.<$> tenancy,
            ("VirtualizationType" Core..=) Core.<$> virtualizationType
          ]
      )

instance Core.AWSRequest CreateInstance where
  type Rs CreateInstance = CreateInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Core.<$> (x Core..:? "InstanceId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @CreateInstance@ request.
--
-- /See:/ 'mkCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { -- | The instance ID.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceResponse' value with any optional fields omitted.
mkCreateInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInstanceResponse
mkCreateInstanceResponse responseStatus =
  CreateInstanceResponse'
    { instanceId = Core.Nothing,
      responseStatus
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsInstanceId :: Lens.Lens' CreateInstanceResponse (Core.Maybe Types.InstanceId)
cirrsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED cirrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateInstanceResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
