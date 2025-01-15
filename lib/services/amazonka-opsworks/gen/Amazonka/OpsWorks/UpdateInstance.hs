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
-- Module      : Amazonka.OpsWorks.UpdateInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.OpsWorks.UpdateInstance
  ( -- * Creating a Request
    UpdateInstance (..),
    newUpdateInstance,

    -- * Request Lenses
    updateInstance_agentVersion,
    updateInstance_amiId,
    updateInstance_architecture,
    updateInstance_autoScalingType,
    updateInstance_ebsOptimized,
    updateInstance_hostname,
    updateInstance_installUpdatesOnBoot,
    updateInstance_instanceType,
    updateInstance_layerIds,
    updateInstance_os,
    updateInstance_sshKeyName,
    updateInstance_instanceId,

    -- * Destructuring the Response
    UpdateInstanceResponse (..),
    newUpdateInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInstance' smart constructor.
data UpdateInstance = UpdateInstance'
  { -- | The default AWS OpsWorks Stacks agent version. You have the following
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
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AMI that was used to create the instance. The value of
    -- this parameter must be the same AMI ID that the instance is already
    -- using. You cannot apply a new AMI to an instance by running
    -- UpdateInstance. UpdateInstance does not work on instances that are using
    -- custom AMIs.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The instance architecture. Instance types do not necessarily support
    -- both architectures. For a list of the architectures that are supported
    -- by the different instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
    architecture :: Prelude.Maybe Architecture,
    -- | For load-based or time-based instances, the type. Windows stacks can use
    -- only time-based instances.
    autoScalingType :: Prelude.Maybe AutoScalingType,
    -- | This property cannot be updated.
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
    -- We strongly recommend using the default value of @true@, to ensure that
    -- your instances have the latest security updates.
    installUpdatesOnBoot :: Prelude.Maybe Prelude.Bool,
    -- | The instance type, such as @t2.micro@. For a list of supported instance
    -- types, open the stack in the console, choose __Instances__, and choose
    -- __+ Instance__. The __Size__ list contains the currently supported
    -- types. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
    -- The parameter values that you use to specify the various types are in
    -- the __API Name__ column of the __Available Instance Types__ table.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s layer IDs.
    layerIds :: Prelude.Maybe [Prelude.Text],
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
    os :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s Amazon EC2 key name.
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'amiId', 'updateInstance_amiId' - The ID of the AMI that was used to create the instance. The value of
-- this parameter must be the same AMI ID that the instance is already
-- using. You cannot apply a new AMI to an instance by running
-- UpdateInstance. UpdateInstance does not work on instances that are using
-- custom AMIs.
--
-- 'architecture', 'updateInstance_architecture' - The instance architecture. Instance types do not necessarily support
-- both architectures. For a list of the architectures that are supported
-- by the different instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
--
-- 'autoScalingType', 'updateInstance_autoScalingType' - For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
--
-- 'ebsOptimized', 'updateInstance_ebsOptimized' - This property cannot be updated.
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
-- 'layerIds', 'updateInstance_layerIds' - The instance\'s layer IDs.
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
-- 'sshKeyName', 'updateInstance_sshKeyName' - The instance\'s Amazon EC2 key name.
--
-- 'instanceId', 'updateInstance_instanceId' - The instance ID.
newUpdateInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  UpdateInstance
newUpdateInstance pInstanceId_ =
  UpdateInstance'
    { agentVersion = Prelude.Nothing,
      amiId = Prelude.Nothing,
      architecture = Prelude.Nothing,
      autoScalingType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      hostname = Prelude.Nothing,
      installUpdatesOnBoot = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      layerIds = Prelude.Nothing,
      os = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      instanceId = pInstanceId_
    }

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
updateInstance_agentVersion :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Text)
updateInstance_agentVersion = Lens.lens (\UpdateInstance' {agentVersion} -> agentVersion) (\s@UpdateInstance' {} a -> s {agentVersion = a} :: UpdateInstance)

-- | The ID of the AMI that was used to create the instance. The value of
-- this parameter must be the same AMI ID that the instance is already
-- using. You cannot apply a new AMI to an instance by running
-- UpdateInstance. UpdateInstance does not work on instances that are using
-- custom AMIs.
updateInstance_amiId :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Text)
updateInstance_amiId = Lens.lens (\UpdateInstance' {amiId} -> amiId) (\s@UpdateInstance' {} a -> s {amiId = a} :: UpdateInstance)

-- | The instance architecture. Instance types do not necessarily support
-- both architectures. For a list of the architectures that are supported
-- by the different instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
updateInstance_architecture :: Lens.Lens' UpdateInstance (Prelude.Maybe Architecture)
updateInstance_architecture = Lens.lens (\UpdateInstance' {architecture} -> architecture) (\s@UpdateInstance' {} a -> s {architecture = a} :: UpdateInstance)

-- | For load-based or time-based instances, the type. Windows stacks can use
-- only time-based instances.
updateInstance_autoScalingType :: Lens.Lens' UpdateInstance (Prelude.Maybe AutoScalingType)
updateInstance_autoScalingType = Lens.lens (\UpdateInstance' {autoScalingType} -> autoScalingType) (\s@UpdateInstance' {} a -> s {autoScalingType = a} :: UpdateInstance)

-- | This property cannot be updated.
updateInstance_ebsOptimized :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Bool)
updateInstance_ebsOptimized = Lens.lens (\UpdateInstance' {ebsOptimized} -> ebsOptimized) (\s@UpdateInstance' {} a -> s {ebsOptimized = a} :: UpdateInstance)

-- | The instance host name.
updateInstance_hostname :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Text)
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
updateInstance_installUpdatesOnBoot :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Bool)
updateInstance_installUpdatesOnBoot = Lens.lens (\UpdateInstance' {installUpdatesOnBoot} -> installUpdatesOnBoot) (\s@UpdateInstance' {} a -> s {installUpdatesOnBoot = a} :: UpdateInstance)

-- | The instance type, such as @t2.micro@. For a list of supported instance
-- types, open the stack in the console, choose __Instances__, and choose
-- __+ Instance__. The __Size__ list contains the currently supported
-- types. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Families and Types>.
-- The parameter values that you use to specify the various types are in
-- the __API Name__ column of the __Available Instance Types__ table.
updateInstance_instanceType :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Text)
updateInstance_instanceType = Lens.lens (\UpdateInstance' {instanceType} -> instanceType) (\s@UpdateInstance' {} a -> s {instanceType = a} :: UpdateInstance)

-- | The instance\'s layer IDs.
updateInstance_layerIds :: Lens.Lens' UpdateInstance (Prelude.Maybe [Prelude.Text])
updateInstance_layerIds = Lens.lens (\UpdateInstance' {layerIds} -> layerIds) (\s@UpdateInstance' {} a -> s {layerIds = a} :: UpdateInstance) Prelude.. Lens.mapping Lens.coerced

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
updateInstance_os :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Text)
updateInstance_os = Lens.lens (\UpdateInstance' {os} -> os) (\s@UpdateInstance' {} a -> s {os = a} :: UpdateInstance)

-- | The instance\'s Amazon EC2 key name.
updateInstance_sshKeyName :: Lens.Lens' UpdateInstance (Prelude.Maybe Prelude.Text)
updateInstance_sshKeyName = Lens.lens (\UpdateInstance' {sshKeyName} -> sshKeyName) (\s@UpdateInstance' {} a -> s {sshKeyName = a} :: UpdateInstance)

-- | The instance ID.
updateInstance_instanceId :: Lens.Lens' UpdateInstance Prelude.Text
updateInstance_instanceId = Lens.lens (\UpdateInstance' {instanceId} -> instanceId) (\s@UpdateInstance' {} a -> s {instanceId = a} :: UpdateInstance)

instance Core.AWSRequest UpdateInstance where
  type
    AWSResponse UpdateInstance =
      UpdateInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateInstanceResponse'

instance Prelude.Hashable UpdateInstance where
  hashWithSalt _salt UpdateInstance' {..} =
    _salt
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` autoScalingType
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` installUpdatesOnBoot
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` layerIds
      `Prelude.hashWithSalt` os
      `Prelude.hashWithSalt` sshKeyName
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateInstance where
  rnf UpdateInstance' {..} =
    Prelude.rnf agentVersion `Prelude.seq`
      Prelude.rnf amiId `Prelude.seq`
        Prelude.rnf architecture `Prelude.seq`
          Prelude.rnf autoScalingType `Prelude.seq`
            Prelude.rnf ebsOptimized `Prelude.seq`
              Prelude.rnf hostname `Prelude.seq`
                Prelude.rnf installUpdatesOnBoot `Prelude.seq`
                  Prelude.rnf instanceType `Prelude.seq`
                    Prelude.rnf layerIds `Prelude.seq`
                      Prelude.rnf os `Prelude.seq`
                        Prelude.rnf sshKeyName `Prelude.seq`
                          Prelude.rnf instanceId

instance Data.ToHeaders UpdateInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UpdateInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInstance where
  toJSON UpdateInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentVersion" Data..=) Prelude.<$> agentVersion,
            ("AmiId" Data..=) Prelude.<$> amiId,
            ("Architecture" Data..=) Prelude.<$> architecture,
            ("AutoScalingType" Data..=)
              Prelude.<$> autoScalingType,
            ("EbsOptimized" Data..=) Prelude.<$> ebsOptimized,
            ("Hostname" Data..=) Prelude.<$> hostname,
            ("InstallUpdatesOnBoot" Data..=)
              Prelude.<$> installUpdatesOnBoot,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("LayerIds" Data..=) Prelude.<$> layerIds,
            ("Os" Data..=) Prelude.<$> os,
            ("SshKeyName" Data..=) Prelude.<$> sshKeyName,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath UpdateInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInstanceResponse' smart constructor.
data UpdateInstanceResponse = UpdateInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInstanceResponse ::
  UpdateInstanceResponse
newUpdateInstanceResponse = UpdateInstanceResponse'

instance Prelude.NFData UpdateInstanceResponse where
  rnf _ = ()
