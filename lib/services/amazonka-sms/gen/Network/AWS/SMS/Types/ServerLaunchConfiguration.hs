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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.UserData

-- | Launch configuration for a server.
--
-- /See:/ 'newServerLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { configureScript :: Prelude.Maybe S3Location,
    -- | The name of the Amazon EC2 SSH key to be used for connecting to the
    -- launched server.
    ec2KeyName :: Prelude.Maybe Prelude.Text,
    -- | Location of the user-data script to be executed when launching the
    -- server.
    userData :: Prelude.Maybe UserData,
    -- | The instance type to use when launching the server.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The logical ID of the server in the AWS CloudFormation template.
    logicalId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet the server should be launched into.
    subnet :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM instance profile.
    iamInstanceProfileName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the server with which the launch configuration is associated.
    server :: Prelude.Maybe Server,
    -- | Indicates whether a publicly accessible IP address is created when
    -- launching the server.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The type of configuration script.
    configureScriptType :: Prelude.Maybe ScriptType,
    -- | The ID of the security group that applies to the launched server.
    securityGroup :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC into which the server should be launched.
    vpc :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'userData', 'serverLaunchConfiguration_userData' - Location of the user-data script to be executed when launching the
-- server.
--
-- 'instanceType', 'serverLaunchConfiguration_instanceType' - The instance type to use when launching the server.
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
        Prelude.Nothing,
      ec2KeyName = Prelude.Nothing,
      userData = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      logicalId = Prelude.Nothing,
      subnet = Prelude.Nothing,
      iamInstanceProfileName = Prelude.Nothing,
      server = Prelude.Nothing,
      associatePublicIpAddress = Prelude.Nothing,
      configureScriptType = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | Undocumented member.
serverLaunchConfiguration_configureScript :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe S3Location)
serverLaunchConfiguration_configureScript = Lens.lens (\ServerLaunchConfiguration' {configureScript} -> configureScript) (\s@ServerLaunchConfiguration' {} a -> s {configureScript = a} :: ServerLaunchConfiguration)

-- | The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
serverLaunchConfiguration_ec2KeyName :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_ec2KeyName = Lens.lens (\ServerLaunchConfiguration' {ec2KeyName} -> ec2KeyName) (\s@ServerLaunchConfiguration' {} a -> s {ec2KeyName = a} :: ServerLaunchConfiguration)

-- | Location of the user-data script to be executed when launching the
-- server.
serverLaunchConfiguration_userData :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe UserData)
serverLaunchConfiguration_userData = Lens.lens (\ServerLaunchConfiguration' {userData} -> userData) (\s@ServerLaunchConfiguration' {} a -> s {userData = a} :: ServerLaunchConfiguration)

-- | The instance type to use when launching the server.
serverLaunchConfiguration_instanceType :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_instanceType = Lens.lens (\ServerLaunchConfiguration' {instanceType} -> instanceType) (\s@ServerLaunchConfiguration' {} a -> s {instanceType = a} :: ServerLaunchConfiguration)

-- | The logical ID of the server in the AWS CloudFormation template.
serverLaunchConfiguration_logicalId :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_logicalId = Lens.lens (\ServerLaunchConfiguration' {logicalId} -> logicalId) (\s@ServerLaunchConfiguration' {} a -> s {logicalId = a} :: ServerLaunchConfiguration)

-- | The ID of the subnet the server should be launched into.
serverLaunchConfiguration_subnet :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_subnet = Lens.lens (\ServerLaunchConfiguration' {subnet} -> subnet) (\s@ServerLaunchConfiguration' {} a -> s {subnet = a} :: ServerLaunchConfiguration)

-- | The name of the IAM instance profile.
serverLaunchConfiguration_iamInstanceProfileName :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_iamInstanceProfileName = Lens.lens (\ServerLaunchConfiguration' {iamInstanceProfileName} -> iamInstanceProfileName) (\s@ServerLaunchConfiguration' {} a -> s {iamInstanceProfileName = a} :: ServerLaunchConfiguration)

-- | The ID of the server with which the launch configuration is associated.
serverLaunchConfiguration_server :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Server)
serverLaunchConfiguration_server = Lens.lens (\ServerLaunchConfiguration' {server} -> server) (\s@ServerLaunchConfiguration' {} a -> s {server = a} :: ServerLaunchConfiguration)

-- | Indicates whether a publicly accessible IP address is created when
-- launching the server.
serverLaunchConfiguration_associatePublicIpAddress :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Bool)
serverLaunchConfiguration_associatePublicIpAddress = Lens.lens (\ServerLaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ServerLaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: ServerLaunchConfiguration)

-- | The type of configuration script.
serverLaunchConfiguration_configureScriptType :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe ScriptType)
serverLaunchConfiguration_configureScriptType = Lens.lens (\ServerLaunchConfiguration' {configureScriptType} -> configureScriptType) (\s@ServerLaunchConfiguration' {} a -> s {configureScriptType = a} :: ServerLaunchConfiguration)

-- | The ID of the security group that applies to the launched server.
serverLaunchConfiguration_securityGroup :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_securityGroup = Lens.lens (\ServerLaunchConfiguration' {securityGroup} -> securityGroup) (\s@ServerLaunchConfiguration' {} a -> s {securityGroup = a} :: ServerLaunchConfiguration)

-- | The ID of the VPC into which the server should be launched.
serverLaunchConfiguration_vpc :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_vpc = Lens.lens (\ServerLaunchConfiguration' {vpc} -> vpc) (\s@ServerLaunchConfiguration' {} a -> s {vpc = a} :: ServerLaunchConfiguration)

instance Core.FromJSON ServerLaunchConfiguration where
  parseJSON =
    Core.withObject
      "ServerLaunchConfiguration"
      ( \x ->
          ServerLaunchConfiguration'
            Prelude.<$> (x Core..:? "configureScript")
            Prelude.<*> (x Core..:? "ec2KeyName")
            Prelude.<*> (x Core..:? "userData")
            Prelude.<*> (x Core..:? "instanceType")
            Prelude.<*> (x Core..:? "logicalId")
            Prelude.<*> (x Core..:? "subnet")
            Prelude.<*> (x Core..:? "iamInstanceProfileName")
            Prelude.<*> (x Core..:? "server")
            Prelude.<*> (x Core..:? "associatePublicIpAddress")
            Prelude.<*> (x Core..:? "configureScriptType")
            Prelude.<*> (x Core..:? "securityGroup")
            Prelude.<*> (x Core..:? "vpc")
      )

instance Prelude.Hashable ServerLaunchConfiguration

instance Prelude.NFData ServerLaunchConfiguration

instance Core.ToJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("configureScript" Core..=)
              Prelude.<$> configureScript,
            ("ec2KeyName" Core..=) Prelude.<$> ec2KeyName,
            ("userData" Core..=) Prelude.<$> userData,
            ("instanceType" Core..=) Prelude.<$> instanceType,
            ("logicalId" Core..=) Prelude.<$> logicalId,
            ("subnet" Core..=) Prelude.<$> subnet,
            ("iamInstanceProfileName" Core..=)
              Prelude.<$> iamInstanceProfileName,
            ("server" Core..=) Prelude.<$> server,
            ("associatePublicIpAddress" Core..=)
              Prelude.<$> associatePublicIpAddress,
            ("configureScriptType" Core..=)
              Prelude.<$> configureScriptType,
            ("securityGroup" Core..=) Prelude.<$> securityGroup,
            ("vpc" Core..=) Prelude.<$> vpc
          ]
      )
