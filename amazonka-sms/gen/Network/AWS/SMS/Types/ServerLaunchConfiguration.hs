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
-- Module      : Network.AWS.SMS.Types.ServerLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerLaunchConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.UserData

-- | Launch configuration for a server.
--
-- /See:/ 'newServerLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { configureScript :: Core.Maybe S3Location,
    -- | The name of the Amazon EC2 SSH key to be used for connecting to the
    -- launched server.
    ec2KeyName :: Core.Maybe Core.Text,
    -- | The instance type to use when launching the server.
    instanceType :: Core.Maybe Core.Text,
    -- | Location of the user-data script to be executed when launching the
    -- server.
    userData :: Core.Maybe UserData,
    -- | The logical ID of the server in the AWS CloudFormation template.
    logicalId :: Core.Maybe Core.Text,
    -- | The ID of the subnet the server should be launched into.
    subnet :: Core.Maybe Core.Text,
    -- | The name of the IAM instance profile.
    iamInstanceProfileName :: Core.Maybe Core.Text,
    -- | The ID of the server with which the launch configuration is associated.
    server :: Core.Maybe Server,
    -- | Indicates whether a publicly accessible IP address is created when
    -- launching the server.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | The type of configuration script.
    configureScriptType :: Core.Maybe ScriptType,
    -- | The ID of the security group that applies to the launched server.
    securityGroup :: Core.Maybe Core.Text,
    -- | The ID of the VPC into which the server should be launched.
    vpc :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configureScript', 'serverLaunchConfiguration_configureScript' - Undocumented member.
--
-- 'ec2KeyName', 'serverLaunchConfiguration_ec2KeyName' - The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
--
-- 'instanceType', 'serverLaunchConfiguration_instanceType' - The instance type to use when launching the server.
--
-- 'userData', 'serverLaunchConfiguration_userData' - Location of the user-data script to be executed when launching the
-- server.
--
-- 'logicalId', 'serverLaunchConfiguration_logicalId' - The logical ID of the server in the AWS CloudFormation template.
--
-- 'subnet', 'serverLaunchConfiguration_subnet' - The ID of the subnet the server should be launched into.
--
-- 'iamInstanceProfileName', 'serverLaunchConfiguration_iamInstanceProfileName' - The name of the IAM instance profile.
--
-- 'server', 'serverLaunchConfiguration_server' - The ID of the server with which the launch configuration is associated.
--
-- 'associatePublicIpAddress', 'serverLaunchConfiguration_associatePublicIpAddress' - Indicates whether a publicly accessible IP address is created when
-- launching the server.
--
-- 'configureScriptType', 'serverLaunchConfiguration_configureScriptType' - The type of configuration script.
--
-- 'securityGroup', 'serverLaunchConfiguration_securityGroup' - The ID of the security group that applies to the launched server.
--
-- 'vpc', 'serverLaunchConfiguration_vpc' - The ID of the VPC into which the server should be launched.
newServerLaunchConfiguration ::
  ServerLaunchConfiguration
newServerLaunchConfiguration =
  ServerLaunchConfiguration'
    { configureScript =
        Core.Nothing,
      ec2KeyName = Core.Nothing,
      instanceType = Core.Nothing,
      userData = Core.Nothing,
      logicalId = Core.Nothing,
      subnet = Core.Nothing,
      iamInstanceProfileName = Core.Nothing,
      server = Core.Nothing,
      associatePublicIpAddress = Core.Nothing,
      configureScriptType = Core.Nothing,
      securityGroup = Core.Nothing,
      vpc = Core.Nothing
    }

-- | Undocumented member.
serverLaunchConfiguration_configureScript :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe S3Location)
serverLaunchConfiguration_configureScript = Lens.lens (\ServerLaunchConfiguration' {configureScript} -> configureScript) (\s@ServerLaunchConfiguration' {} a -> s {configureScript = a} :: ServerLaunchConfiguration)

-- | The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
serverLaunchConfiguration_ec2KeyName :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_ec2KeyName = Lens.lens (\ServerLaunchConfiguration' {ec2KeyName} -> ec2KeyName) (\s@ServerLaunchConfiguration' {} a -> s {ec2KeyName = a} :: ServerLaunchConfiguration)

-- | The instance type to use when launching the server.
serverLaunchConfiguration_instanceType :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_instanceType = Lens.lens (\ServerLaunchConfiguration' {instanceType} -> instanceType) (\s@ServerLaunchConfiguration' {} a -> s {instanceType = a} :: ServerLaunchConfiguration)

-- | Location of the user-data script to be executed when launching the
-- server.
serverLaunchConfiguration_userData :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe UserData)
serverLaunchConfiguration_userData = Lens.lens (\ServerLaunchConfiguration' {userData} -> userData) (\s@ServerLaunchConfiguration' {} a -> s {userData = a} :: ServerLaunchConfiguration)

-- | The logical ID of the server in the AWS CloudFormation template.
serverLaunchConfiguration_logicalId :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_logicalId = Lens.lens (\ServerLaunchConfiguration' {logicalId} -> logicalId) (\s@ServerLaunchConfiguration' {} a -> s {logicalId = a} :: ServerLaunchConfiguration)

-- | The ID of the subnet the server should be launched into.
serverLaunchConfiguration_subnet :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_subnet = Lens.lens (\ServerLaunchConfiguration' {subnet} -> subnet) (\s@ServerLaunchConfiguration' {} a -> s {subnet = a} :: ServerLaunchConfiguration)

-- | The name of the IAM instance profile.
serverLaunchConfiguration_iamInstanceProfileName :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_iamInstanceProfileName = Lens.lens (\ServerLaunchConfiguration' {iamInstanceProfileName} -> iamInstanceProfileName) (\s@ServerLaunchConfiguration' {} a -> s {iamInstanceProfileName = a} :: ServerLaunchConfiguration)

-- | The ID of the server with which the launch configuration is associated.
serverLaunchConfiguration_server :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Server)
serverLaunchConfiguration_server = Lens.lens (\ServerLaunchConfiguration' {server} -> server) (\s@ServerLaunchConfiguration' {} a -> s {server = a} :: ServerLaunchConfiguration)

-- | Indicates whether a publicly accessible IP address is created when
-- launching the server.
serverLaunchConfiguration_associatePublicIpAddress :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Bool)
serverLaunchConfiguration_associatePublicIpAddress = Lens.lens (\ServerLaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ServerLaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: ServerLaunchConfiguration)

-- | The type of configuration script.
serverLaunchConfiguration_configureScriptType :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe ScriptType)
serverLaunchConfiguration_configureScriptType = Lens.lens (\ServerLaunchConfiguration' {configureScriptType} -> configureScriptType) (\s@ServerLaunchConfiguration' {} a -> s {configureScriptType = a} :: ServerLaunchConfiguration)

-- | The ID of the security group that applies to the launched server.
serverLaunchConfiguration_securityGroup :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_securityGroup = Lens.lens (\ServerLaunchConfiguration' {securityGroup} -> securityGroup) (\s@ServerLaunchConfiguration' {} a -> s {securityGroup = a} :: ServerLaunchConfiguration)

-- | The ID of the VPC into which the server should be launched.
serverLaunchConfiguration_vpc :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Text)
serverLaunchConfiguration_vpc = Lens.lens (\ServerLaunchConfiguration' {vpc} -> vpc) (\s@ServerLaunchConfiguration' {} a -> s {vpc = a} :: ServerLaunchConfiguration)

instance Core.FromJSON ServerLaunchConfiguration where
  parseJSON =
    Core.withObject
      "ServerLaunchConfiguration"
      ( \x ->
          ServerLaunchConfiguration'
            Core.<$> (x Core..:? "configureScript")
            Core.<*> (x Core..:? "ec2KeyName")
            Core.<*> (x Core..:? "instanceType")
            Core.<*> (x Core..:? "userData")
            Core.<*> (x Core..:? "logicalId")
            Core.<*> (x Core..:? "subnet")
            Core.<*> (x Core..:? "iamInstanceProfileName")
            Core.<*> (x Core..:? "server")
            Core.<*> (x Core..:? "associatePublicIpAddress")
            Core.<*> (x Core..:? "configureScriptType")
            Core.<*> (x Core..:? "securityGroup")
            Core.<*> (x Core..:? "vpc")
      )

instance Core.Hashable ServerLaunchConfiguration

instance Core.NFData ServerLaunchConfiguration

instance Core.ToJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("configureScript" Core..=)
              Core.<$> configureScript,
            ("ec2KeyName" Core..=) Core.<$> ec2KeyName,
            ("instanceType" Core..=) Core.<$> instanceType,
            ("userData" Core..=) Core.<$> userData,
            ("logicalId" Core..=) Core.<$> logicalId,
            ("subnet" Core..=) Core.<$> subnet,
            ("iamInstanceProfileName" Core..=)
              Core.<$> iamInstanceProfileName,
            ("server" Core..=) Core.<$> server,
            ("associatePublicIpAddress" Core..=)
              Core.<$> associatePublicIpAddress,
            ("configureScriptType" Core..=)
              Core.<$> configureScriptType,
            ("securityGroup" Core..=) Core.<$> securityGroup,
            ("vpc" Core..=) Core.<$> vpc
          ]
      )
