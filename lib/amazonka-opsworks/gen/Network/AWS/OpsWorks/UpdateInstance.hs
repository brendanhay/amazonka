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
    uInstanceId,
    uInstallUpdatesOnBoot,
    uHostname,
    uSSHKeyName,
    uAgentVersion,
    uInstanceType,
    uEBSOptimized,
    uOS,
    uAutoScalingType,
    uLayerIds,
    uArchitecture,
    uAMIId,

    -- * Destructuring the response
    UpdateInstanceResponse (..),
    mkUpdateInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateInstance' smart constructor.
data UpdateInstance = UpdateInstance'
  { -- | The instance ID.
    instanceId :: Lude.Text,
    -- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
    installUpdatesOnBoot :: Lude.Maybe Lude.Bool,
    -- | The instance host name.
    hostname :: Lude.Maybe Lude.Text,
    -- | The instance's Amazon EC2 key name.
    sshKeyName :: Lude.Maybe Lude.Text,
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
    agentVersion :: Lude.Maybe Lude.Text,
    -- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Lude.Maybe Lude.Text,
    -- | This property cannot be updated.
    ebsOptimized :: Lude.Maybe Lude.Bool,
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
    os :: Lude.Maybe Lude.Text,
    -- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
    autoScalingType :: Lude.Maybe AutoScalingType,
    -- | The instance's layer IDs.
    layerIds :: Lude.Maybe [Lude.Text],
    -- | The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
    architecture :: Lude.Maybe Architecture,
    -- | The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
    amiId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
-- * 'installUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
-- * 'hostname' - The instance host name.
-- * 'sshKeyName' - The instance's Amazon EC2 key name.
-- * 'agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:
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
-- * 'instanceType' - The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
-- * 'ebsOptimized' - This property cannot be updated.
-- * 'os' - The instance's operating system, which must be set to one of the following. You cannot update an instance that is using a custom AMI.
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
-- * 'autoScalingType' - For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
-- * 'layerIds' - The instance's layer IDs.
-- * 'architecture' - The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
-- * 'amiId' - The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
mkUpdateInstance ::
  -- | 'instanceId'
  Lude.Text ->
  UpdateInstance
mkUpdateInstance pInstanceId_ =
  UpdateInstance'
    { instanceId = pInstanceId_,
      installUpdatesOnBoot = Lude.Nothing,
      hostname = Lude.Nothing,
      sshKeyName = Lude.Nothing,
      agentVersion = Lude.Nothing,
      instanceType = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      os = Lude.Nothing,
      autoScalingType = Lude.Nothing,
      layerIds = Lude.Nothing,
      architecture = Lude.Nothing,
      amiId = Lude.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uInstanceId :: Lens.Lens' UpdateInstance Lude.Text
uInstanceId = Lens.lens (instanceId :: UpdateInstance -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateInstance)
{-# DEPRECATED uInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- /Note:/ Consider using 'installUpdatesOnBoot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uInstallUpdatesOnBoot :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Bool)
uInstallUpdatesOnBoot = Lens.lens (installUpdatesOnBoot :: UpdateInstance -> Lude.Maybe Lude.Bool) (\s a -> s {installUpdatesOnBoot = a} :: UpdateInstance)
{-# DEPRECATED uInstallUpdatesOnBoot "Use generic-lens or generic-optics with 'installUpdatesOnBoot' instead." #-}

-- | The instance host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHostname :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Text)
uHostname = Lens.lens (hostname :: UpdateInstance -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: UpdateInstance)
{-# DEPRECATED uHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The instance's Amazon EC2 key name.
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSSHKeyName :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Text)
uSSHKeyName = Lens.lens (sshKeyName :: UpdateInstance -> Lude.Maybe Lude.Text) (\s a -> s {sshKeyName = a} :: UpdateInstance)
{-# DEPRECATED uSSHKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

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
uAgentVersion :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Text)
uAgentVersion = Lens.lens (agentVersion :: UpdateInstance -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: UpdateInstance)
{-# DEPRECATED uAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The instance type, such as @t2.micro@ . For a list of supported instance types, open the stack in the console, choose __Instances__ , and choose __+ Instance__ . The __Size__ list contains the currently supported types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> . The parameter values that you use to specify the various types are in the __API Name__ column of the __Available Instance Types__ table.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uInstanceType :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Text)
uInstanceType = Lens.lens (instanceType :: UpdateInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: UpdateInstance)
{-# DEPRECATED uInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | This property cannot be updated.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEBSOptimized :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Bool)
uEBSOptimized = Lens.lens (ebsOptimized :: UpdateInstance -> Lude.Maybe Lude.Bool) (\s a -> s {ebsOptimized = a} :: UpdateInstance)
{-# DEPRECATED uEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

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
uOS :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Text)
uOS = Lens.lens (os :: UpdateInstance -> Lude.Maybe Lude.Text) (\s a -> s {os = a} :: UpdateInstance)
{-# DEPRECATED uOS "Use generic-lens or generic-optics with 'os' instead." #-}

-- | For load-based or time-based instances, the type. Windows stacks can use only time-based instances.
--
-- /Note:/ Consider using 'autoScalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAutoScalingType :: Lens.Lens' UpdateInstance (Lude.Maybe AutoScalingType)
uAutoScalingType = Lens.lens (autoScalingType :: UpdateInstance -> Lude.Maybe AutoScalingType) (\s a -> s {autoScalingType = a} :: UpdateInstance)
{-# DEPRECATED uAutoScalingType "Use generic-lens or generic-optics with 'autoScalingType' instead." #-}

-- | The instance's layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLayerIds :: Lens.Lens' UpdateInstance (Lude.Maybe [Lude.Text])
uLayerIds = Lens.lens (layerIds :: UpdateInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {layerIds = a} :: UpdateInstance)
{-# DEPRECATED uLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The instance architecture. Instance types do not necessarily support both architectures. For a list of the architectures that are supported by the different instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types> .
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArchitecture :: Lens.Lens' UpdateInstance (Lude.Maybe Architecture)
uArchitecture = Lens.lens (architecture :: UpdateInstance -> Lude.Maybe Architecture) (\s a -> s {architecture = a} :: UpdateInstance)
{-# DEPRECATED uArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The ID of the AMI that was used to create the instance. The value of this parameter must be the same AMI ID that the instance is already using. You cannot apply a new AMI to an instance by running UpdateInstance. UpdateInstance does not work on instances that are using custom AMIs.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAMIId :: Lens.Lens' UpdateInstance (Lude.Maybe Lude.Text)
uAMIId = Lens.lens (amiId :: UpdateInstance -> Lude.Maybe Lude.Text) (\s a -> s {amiId = a} :: UpdateInstance)
{-# DEPRECATED uAMIId "Use generic-lens or generic-optics with 'amiId' instead." #-}

instance Lude.AWSRequest UpdateInstance where
  type Rs UpdateInstance = UpdateInstanceResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateInstanceResponse'

instance Lude.ToHeaders UpdateInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInstance where
  toJSON UpdateInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            ("InstallUpdatesOnBoot" Lude..=) Lude.<$> installUpdatesOnBoot,
            ("Hostname" Lude..=) Lude.<$> hostname,
            ("SshKeyName" Lude..=) Lude.<$> sshKeyName,
            ("AgentVersion" Lude..=) Lude.<$> agentVersion,
            ("InstanceType" Lude..=) Lude.<$> instanceType,
            ("EbsOptimized" Lude..=) Lude.<$> ebsOptimized,
            ("Os" Lude..=) Lude.<$> os,
            ("AutoScalingType" Lude..=) Lude.<$> autoScalingType,
            ("LayerIds" Lude..=) Lude.<$> layerIds,
            ("Architecture" Lude..=) Lude.<$> architecture,
            ("AmiId" Lude..=) Lude.<$> amiId
          ]
      )

instance Lude.ToPath UpdateInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateInstanceResponse' smart constructor.
data UpdateInstanceResponse = UpdateInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceResponse' with the minimum fields required to make a request.
mkUpdateInstanceResponse ::
  UpdateInstanceResponse
mkUpdateInstanceResponse = UpdateInstanceResponse'
