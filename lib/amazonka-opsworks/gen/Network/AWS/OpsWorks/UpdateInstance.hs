{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified instance.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateInstance
  ( -- * Creating a request
    UpdateInstance (..),
    mkUpdateInstance,

    -- ** Request lenses
    uiInstanceId,
    uiAgentVersion,
    uiAmiId,
    uiArchitecture,
    uiAutoScalingType,
    uiEbsOptimized,
    uiHostname,
    uiInstallUpdatesOnBoot,
    uiInstanceType,
    uiLayerIds,
    uiOs,
    uiSshKeyName,

    -- * Destructuring the response
    UpdateInstanceResponse (..),
    mkUpdateInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateInstance' smart constructor.
data UpdateInstance = UpdateInstance'
  { -- | The instance ID.
    instanceId :: Types.String,
    -- | The default AWS OpsWorks Stacks agent version. You have the following options:
    --
    --
    --     * @INHERIT@ - Use the stack's default agent version setting.
    --
    --
    --     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, you must edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance.
    --
    --
    -- The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' .
    -- AgentVersion cannot be set to Chef 12.2.
    agentVersion :: Core.Maybe Types.String,
    -- | The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
    amiId :: Core.Maybe Types.String,
    -- | The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
    architecture :: Core.Maybe Types.Architecture,
    -- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
    autoScalingType :: Core.Maybe Types.AutoScalingType,
    -- | This property cannot be updated.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The instance host name.
    hostname :: Core.Maybe Types.String,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Core.Maybe Types.String,
    -- | The instance's layer IDs.
    layerIds :: Core.Maybe [Types.String],
    -- | The instance's operating system, which must be set to one of the following. You cannot update an instance that is using a custom AMI.
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
    -- For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
    -- The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the AmiId parameter to specify the custom AMI that you want to use. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> . For more information about how to use custom AMIs with OpsWorks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
    os :: Core.Maybe Types.String,
    -- | The instance's Amazon EC2 key name.
    sshKeyName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstance' value with any optional fields omitted.
mkUpdateInstance ::
  -- | 'instanceId'
  Types.String ->
  UpdateInstance
mkUpdateInstance instanceId =
  UpdateInstance'
    { instanceId,
      agentVersion = Core.Nothing,
      amiId = Core.Nothing,
      architecture = Core.Nothing,
      autoScalingType = Core.Nothing,
      ebsOptimized = Core.Nothing,
      hostname = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      instanceType = Core.Nothing,
      layerIds = Core.Nothing,
      os = Core.Nothing,
      sshKeyName = Core.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInstanceId :: Lens.Lens' UpdateInstance Types.String
uiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * @INHERIT@ - Use the stack's default agent version setting.
--
--
--     * /version_number/ - Use the specified agent version. This value overrides the stack's default setting. To update the agent version, you must edit the instance configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the instance.
--
--
-- The default setting is @INHERIT@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' .
-- AgentVersion cannot be set to Chef 12.2.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiAgentVersion :: Lens.Lens' UpdateInstance (Core.Maybe Types.String)
uiAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED uiAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiAmiId :: Lens.Lens' UpdateInstance (Core.Maybe Types.String)
uiAmiId = Lens.field @"amiId"
{-# DEPRECATED uiAmiId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiArchitecture :: Lens.Lens' UpdateInstance (Core.Maybe Types.Architecture)
uiArchitecture = Lens.field @"architecture"
{-# DEPRECATED uiArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
--
-- /Note:/ Consider using 'autoScalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiAutoScalingType :: Lens.Lens' UpdateInstance (Core.Maybe Types.AutoScalingType)
uiAutoScalingType = Lens.field @"autoScalingType"
{-# DEPRECATED uiAutoScalingType "Use generic-lens or generic-optics with 'autoScalingType' instead." #-}

-- | This property cannot be updated.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiEbsOptimized :: Lens.Lens' UpdateInstance (Core.Maybe Core.Bool)
uiEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED uiEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The instance host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiHostname :: Lens.Lens' UpdateInstance (Core.Maybe Types.String)
uiHostname = Lens.field @"hostname"
{-# DEPRECATED uiHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInstallUpdatesOnBoot :: Lens.Lens' UpdateInstance (Core.Maybe Core.Bool)
uiInstallUpdatesOnBoot = Lens.field @"installUpdatesOnBoot"
{-# DEPRECATED uiInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInstanceType :: Lens.Lens' UpdateInstance (Core.Maybe Types.String)
uiInstanceType = Lens.field @"instanceType"
{-# DEPRECATED uiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The instance's layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiLayerIds :: Lens.Lens' UpdateInstance (Core.Maybe [Types.String])
uiLayerIds = Lens.field @"layerIds"
{-# DEPRECATED uiLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The instance's operating system, which must be set to one of the following. You cannot update an instance that is using a custom AMI.
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
-- For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
-- The default option is the current Amazon Linux version. If you set this parameter to @Custom@ , you must use the AmiId parameter to specify the custom AMI that you want to use. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems> . For more information about how to use custom AMIs with OpsWorks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
-- /Note:/ Consider using 'os' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiOs :: Lens.Lens' UpdateInstance (Core.Maybe Types.String)
uiOs = Lens.field @"os"
{-# DEPRECATED uiOs "Use generic-lens or generic-optics with 'os' instead." #-}

-- | The instance's Amazon EC2 key name.
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiSshKeyName :: Lens.Lens' UpdateInstance (Core.Maybe Types.String)
uiSshKeyName = Lens.field @"sshKeyName"
{-# DEPRECATED uiSshKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

instance Core.FromJSON UpdateInstance where
  toJSON UpdateInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            ("AgentVersion" Core..=) Core.<$> agentVersion,
            ("AmiId" Core..=) Core.<$> amiId,
            ("Architecture" Core..=) Core.<$> architecture,
            ("AutoScalingType" Core..=) Core.<$> autoScalingType,
            ("EbsOptimized" Core..=) Core.<$> ebsOptimized,
            ("Hostname" Core..=) Core.<$> hostname,
            ("InstallUpdatesOnBoot" Core..=) Core.<$> installUpdatesOnBoot,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("LayerIds" Core..=) Core.<$> layerIds,
            ("Os" Core..=) Core.<$> os,
            ("SshKeyName" Core..=) Core.<$> sshKeyName
          ]
      )

instance Core.AWSRequest UpdateInstance where
  type Rs UpdateInstance = UpdateInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateInstanceResponse'

-- | /See:/ 'mkUpdateInstanceResponse' smart constructor.
data UpdateInstanceResponse = UpdateInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstanceResponse' value with any optional fields omitted.
mkUpdateInstanceResponse ::
  UpdateInstanceResponse
mkUpdateInstanceResponse = UpdateInstanceResponse'
