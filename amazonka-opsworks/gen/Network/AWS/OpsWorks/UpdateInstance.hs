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
-- Module      : Network.AWS.OpsWorks.UpdateInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified instance.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UpdateInstance
  ( -- * Creating a Request
    UpdateInstance (..),
    newUpdateInstance,

    -- * Request Lenses
    updateInstance_hostname,
    updateInstance_installUpdatesOnBoot,
    updateInstance_instanceType,
    updateInstance_ebsOptimized,
    updateInstance_agentVersion,
    updateInstance_sshKeyName,
    updateInstance_amiId,
    updateInstance_layerIds,
    updateInstance_architecture,
    updateInstance_autoScalingType,
    updateInstance_os,
    updateInstance_instanceId,

    -- * Destructuring the Response
    UpdateInstanceResponse (..),
    newUpdateInstanceResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateInstance' smart constructor.
data UpdateInstance = UpdateInstance'
  { -- | The instance host name.
    hostname :: Core.Maybe Core.Text,
    -- | Whether to install operating system and package updates when the
    -- instance boots. The default value is @true@. To control when updates are
    -- installed, set this value to @false@. You must then update your
    -- instances manually by using CreateDeployment to run the
    -- @update_dependencies@ stack command or by manually running @yum@ (Amazon
    -- Linux) or @apt-get@ (Ubuntu) on the instances.
    --
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Core.Maybe Core.Bool,
    -- | The instance type, such as @t2.micro@. For a list of supported instance
    -- types, open the stack in the console, choose __Instances__, and choose
    -- __+ Instance__. The __Size__ list contains the currently supported
    -- types. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
    -- The parameter values that you use to specify the various types are in
    -- the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Core.Maybe Core.Text,
    -- | This property cannot be updated.
    ebsOptimized :: Core.Maybe Core.Bool,
    -- | The default AWS OpsWorks Stacks agent version. You have the following
    -- options:
    --
    -- -   @INHERIT@ - Use the stack\'s default agent version setting.
    --
    -- -   /version_number/ - Use the specified agent version. This value
    --     overrides the stack\'s default setting. To update the agent version,
    --     you must edit the instance configuration and specify a new version.
    --     AWS OpsWorks Stacks then automatically installs that version on the
    --     instance.
    --
    -- The default setting is @INHERIT@. To specify an agent version, you must
    -- use the complete version number, not the abbreviated number shown on the
    -- console. For a list of available agent version numbers, call
    -- DescribeAgentVersions.
    --
    -- AgentVersion cannot be set to Chef 12.2.
    agentVersion :: Core.Maybe Core.Text,
    -- | The instance\'s Amazon EC2 key name.
    sshKeyName :: Core.Maybe Core.Text,
    -- | The ID of the AMI that was used to create the instance. The value of
    -- this parameter must be the same AMI ID that the instance is already
    -- using. You cannot apply a new AMI to an instance by running
    -- UpdateInstance. UpdateInstance does not work on instances that are using
    -- custom AMIs.
    amiId :: Core.Maybe Core.Text,
    -- | The instance\'s layer IDs.
    layerIds :: Core.Maybe [Core.Text],
    -- | The instance architecture. Instance types do not necessarily support
    -- both architectures. For a list of the architectures that are supported
    -- by the different instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
    architecture :: Core.Maybe Architecture,
    -- | For load-based or time-based instances, the type. Windows stacks can use
    -- only time-based instances.
    autoScalingType :: Core.Maybe AutoScalingType,
    -- | The instance\'s operating system, which must be set to one of the
    -- following. You cannot update an instance that is using a custom AMI.
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
    -- For more information about supported operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
    --
    -- The default option is the current Amazon Linux version. If you set this
    -- parameter to @Custom@, you must use the AmiId parameter to specify the
    -- custom AMI that you want to use. For more information about supported
    -- operating systems, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>.
    -- For more information about how to use custom AMIs with OpsWorks, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
    --
    -- You can specify a different Linux operating system for the updated
    -- stack, but you cannot change from Linux to Windows or Windows to Linux.
    os :: Core.Maybe Core.Text,
    -- | The instance ID.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'updateInstance_hostname' - The instance host name.
--
-- 'installUpdatesOnBoot', 'updateInstance_installUpdatesOnBoot' - Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
--
-- 'instanceType', 'updateInstance_instanceType' - The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
--
-- 'ebsOptimized', 'updateInstance_ebsOptimized' - This property cannot be updated.
--
-- 'agentVersion', 'updateInstance_agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following
-- options:
--
-- -   @INHERIT@ - Use the stack\'s default agent version setting.
--
-- -   /version_number/ - Use the specified agent version. This value
--     overrides the stack\'s default setting. To update the agent version,
--     you must edit the instance configuration and specify a new version.
--     AWS OpsWorks Stacks then automatically installs that version on the
--     instance.
--
-- The default setting is @INHERIT@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions.
--
-- AgentVersion cannot be set to Chef 12.2.
--
-- 'sshKeyName', 'updateInstance_sshKeyName' - The instance\'s Amazon EC2 key name.
--
-- 'amiId', 'updateInstance_amiId' - The ID of the AMI that was used to create the instance. The value of
-- this parameter must be the same AMI ID that the instance is already
-- using. You cannot apply a new AMI to an instance by running
-- UpdateInstance. UpdateInstance does not work on instances that are using
-- custom AMIs.
--
-- 'layerIds', 'updateInstance_layerIds' - The instance\'s layer IDs.
--
-- 'architecture', 'updateInstance_architecture' - The instance architecture. Instance types do not necessarily support
-- both architectures. For a list of the architectures that are supported
-- by the different instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
--
-- 'autoScalingType', 'updateInstance_autoScalingType' - For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
--
-- 'os', 'updateInstance_os' - The instance\'s operating system, which must be set to one of the
-- following. You cannot update an instance that is using a custom AMI.
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
-- For more information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- The default option is the current Amazon Linux version. If you set this
-- parameter to @Custom@, you must use the AmiId parameter to specify the
-- custom AMI that you want to use. For more information about supported
-- operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>.
-- For more information about how to use custom AMIs with OpsWorks, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- You can specify a different Linux operating system for the updated
-- stack, but you cannot change from Linux to Windows or Windows to Linux.
--
-- 'instanceId', 'updateInstance_instanceId' - The instance ID.
newUpdateInstance ::
  -- | 'instanceId'
  Core.Text ->
  UpdateInstance
newUpdateInstance pInstanceId_ =
  UpdateInstance'
    { hostname = Core.Nothing,
      installUpdatesOnBoot = Core.Nothing,
      instanceType = Core.Nothing,
      ebsOptimized = Core.Nothing,
      agentVersion = Core.Nothing,
      sshKeyName = Core.Nothing,
      amiId = Core.Nothing,
      layerIds = Core.Nothing,
      architecture = Core.Nothing,
      autoScalingType = Core.Nothing,
      os = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The instance host name.
updateInstance_hostname :: Lens.Lens' UpdateInstance (Core.Maybe Core.Text)
updateInstance_hostname = Lens.lens (\UpdateInstance' {hostname} -> hostname) (\s@UpdateInstance' {} a -> s {hostname = a} :: UpdateInstance)

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or by manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
updateInstance_installUpdatesOnBoot :: Lens.Lens' UpdateInstance (Core.Maybe Core.Bool)
updateInstance_installUpdatesOnBoot = Lens.lens (\UpdateInstance' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@UpdateInstance' {} a -> s {installUpdatesOnBoot = a} :: UpdateInstance)

-- | The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
updateInstance_instanceType :: Lens.Lens' UpdateInstance (Core.Maybe Core.Text)
updateInstance_instanceType = Lens.lens (\UpdateInstance' {instanceType} -> instanceType) (\s@UpdateInstance' {} a -> s {instanceType = a} :: UpdateInstance)

-- | This property cannot be updated.
updateInstance_ebsOptimized :: Lens.Lens' UpdateInstance (Core.Maybe Core.Bool)
updateInstance_ebsOptimized = Lens.lens (\UpdateInstance' {ebsOptimized} -> ebsOptimized) (\s@UpdateInstance' {} a -> s {ebsOptimized = a} :: UpdateInstance)

-- | The default AWS OpsWorks Stacks agent version. You have the following
-- options:
--
-- -   @INHERIT@ - Use the stack\'s default agent version setting.
--
-- -   /version_number/ - Use the specified agent version. This value
--     overrides the stack\'s default setting. To update the agent version,
--     you must edit the instance configuration and specify a new version.
--     AWS OpsWorks Stacks then automatically installs that version on the
--     instance.
--
-- The default setting is @INHERIT@. To specify an agent version, you must
-- use the complete version number, not the abbreviated number shown on the
-- console. For a list of available agent version numbers, call
-- DescribeAgentVersions.
--
-- AgentVersion cannot be set to Chef 12.2.
updateInstance_agentVersion :: Lens.Lens' UpdateInstance (Core.Maybe Core.Text)
updateInstance_agentVersion = Lens.lens (\UpdateInstance' {agentVersion} -> agentVersion) (\s@UpdateInstance' {} a -> s {agentVersion = a} :: UpdateInstance)

-- | The instance\'s Amazon EC2 key name.
updateInstance_sshKeyName :: Lens.Lens' UpdateInstance (Core.Maybe Core.Text)
updateInstance_sshKeyName = Lens.lens (\UpdateInstance' {sshKeyName} -> sshKeyName) (\s@UpdateInstance' {} a -> s {sshKeyName = a} :: UpdateInstance)

-- | The ID of the AMI that was used to create the instance. The value of
-- this parameter must be the same AMI ID that the instance is already
-- using. You cannot apply a new AMI to an instance by running
-- UpdateInstance. UpdateInstance does not work on instances that are using
-- custom AMIs.
updateInstance_amiId :: Lens.Lens' UpdateInstance (Core.Maybe Core.Text)
updateInstance_amiId = Lens.lens (\UpdateInstance' {amiId} -> amiId) (\s@UpdateInstance' {} a -> s {amiId = a} :: UpdateInstance)

-- | The instance\'s layer IDs.
updateInstance_layerIds :: Lens.Lens' UpdateInstance (Core.Maybe [Core.Text])
updateInstance_layerIds = Lens.lens (\UpdateInstance' {layerIds} -> layerIds) (\s@UpdateInstance' {} a -> s {layerIds = a} :: UpdateInstance) Core.. Lens.mapping Lens._Coerce

-- | The instance architecture. Instance types do not necessarily support
-- both architectures. For a list of the architectures that are supported
-- by the different instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
updateInstance_architecture :: Lens.Lens' UpdateInstance (Core.Maybe Architecture)
updateInstance_architecture = Lens.lens (\UpdateInstance' {architecture} -> architecture) (\s@UpdateInstance' {} a -> s {architecture = a} :: UpdateInstance)

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
updateInstance_autoScalingType :: Lens.Lens' UpdateInstance (Core.Maybe AutoScalingType)
updateInstance_autoScalingType = Lens.lens (\UpdateInstance' {autoScalingType} -> autoScalingType) (\s@UpdateInstance' {} a -> s {autoScalingType = a} :: UpdateInstance)

-- | The instance\'s operating system, which must be set to one of the
-- following. You cannot update an instance that is using a custom AMI.
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
-- For more information about supported operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems>.
--
-- The default option is the current Amazon Linux version. If you set this
-- parameter to @Custom@, you must use the AmiId parameter to specify the
-- custom AMI that you want to use. For more information about supported
-- operating systems, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html Operating Systems>.
-- For more information about how to use custom AMIs with OpsWorks, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs>.
--
-- You can specify a different Linux operating system for the updated
-- stack, but you cannot change from Linux to Windows or Windows to Linux.
updateInstance_os :: Lens.Lens' UpdateInstance (Core.Maybe Core.Text)
updateInstance_os = Lens.lens (\UpdateInstance' {os} -> os) (\s@UpdateInstance' {} a -> s {os = a} :: UpdateInstance)

-- | The instance ID.
updateInstance_instanceId :: Lens.Lens' UpdateInstance Core.Text
updateInstance_instanceId = Lens.lens (\UpdateInstance' {instanceId} -> instanceId) (\s@UpdateInstance' {} a -> s {instanceId = a} :: UpdateInstance)

instance Core.AWSRequest UpdateInstance where
  type
    AWSResponse UpdateInstance =
      UpdateInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateInstanceResponse'

instance Core.Hashable UpdateInstance

instance Core.NFData UpdateInstance

instance Core.ToHeaders UpdateInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UpdateInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateInstance where
  toJSON UpdateInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Hostname" Core..=) Core.<$> hostname,
            ("InstallUpdatesOnBoot" Core..=)
              Core.<$> installUpdatesOnBoot,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("EbsOptimized" Core..=) Core.<$> ebsOptimized,
            ("AgentVersion" Core..=) Core.<$> agentVersion,
            ("SshKeyName" Core..=) Core.<$> sshKeyName,
            ("AmiId" Core..=) Core.<$> amiId,
            ("LayerIds" Core..=) Core.<$> layerIds,
            ("Architecture" Core..=) Core.<$> architecture,
            ("AutoScalingType" Core..=) Core.<$> autoScalingType,
            ("Os" Core..=) Core.<$> os,
            Core.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath UpdateInstance where
  toPath = Core.const "/"

instance Core.ToQuery UpdateInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateInstanceResponse' smart constructor.
data UpdateInstanceResponse = UpdateInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInstanceResponse ::
  UpdateInstanceResponse
newUpdateInstanceResponse = UpdateInstanceResponse'

instance Core.NFData UpdateInstanceResponse
