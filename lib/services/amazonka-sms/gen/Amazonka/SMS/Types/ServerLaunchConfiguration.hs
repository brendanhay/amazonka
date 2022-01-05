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
-- Module      : Amazonka.SMS.Types.ServerLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerLaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.S3Location
import Amazonka.SMS.Types.ScriptType
import Amazonka.SMS.Types.Server
import Amazonka.SMS.Types.UserData

-- | Launch configuration for a server.
--
-- /See:/ 'newServerLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { -- | The name of the Amazon EC2 SSH key to be used for connecting to the
    -- launched server.
    ec2KeyName :: Prelude.Maybe Prelude.Text,
    -- | The type of configuration script.
    configureScriptType :: Prelude.Maybe ScriptType,
    -- | Indicates whether a publicly accessible IP address is created when
    -- launching the server.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The name of the IAM instance profile.
    iamInstanceProfileName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet the server should be launched into.
    subnet :: Prelude.Maybe Prelude.Text,
    -- | The logical ID of the server in the AWS CloudFormation template.
    logicalId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group that applies to the launched server.
    securityGroup :: Prelude.Maybe Prelude.Text,
    -- | Location of the user-data script to be executed when launching the
    -- server.
    userData :: Prelude.Maybe UserData,
    -- | The instance type to use when launching the server.
    instanceType :: Prelude.Maybe Prelude.Text,
    configureScript :: Prelude.Maybe S3Location,
    -- | The ID of the server with which the launch configuration is associated.
    server :: Prelude.Maybe Server,
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
-- 'ec2KeyName', 'serverLaunchConfiguration_ec2KeyName' - The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
--
-- 'configureScriptType', 'serverLaunchConfiguration_configureScriptType' - The type of configuration script.
--
-- 'associatePublicIpAddress', 'serverLaunchConfiguration_associatePublicIpAddress' - Indicates whether a publicly accessible IP address is created when
-- launching the server.
--
-- 'iamInstanceProfileName', 'serverLaunchConfiguration_iamInstanceProfileName' - The name of the IAM instance profile.
--
-- 'subnet', 'serverLaunchConfiguration_subnet' - The ID of the subnet the server should be launched into.
--
-- 'logicalId', 'serverLaunchConfiguration_logicalId' - The logical ID of the server in the AWS CloudFormation template.
--
-- 'securityGroup', 'serverLaunchConfiguration_securityGroup' - The ID of the security group that applies to the launched server.
--
-- 'userData', 'serverLaunchConfiguration_userData' - Location of the user-data script to be executed when launching the
-- server.
--
-- 'instanceType', 'serverLaunchConfiguration_instanceType' - The instance type to use when launching the server.
--
-- 'configureScript', 'serverLaunchConfiguration_configureScript' - Undocumented member.
--
-- 'server', 'serverLaunchConfiguration_server' - The ID of the server with which the launch configuration is associated.
--
-- 'vpc', 'serverLaunchConfiguration_vpc' - The ID of the VPC into which the server should be launched.
newServerLaunchConfiguration ::
  ServerLaunchConfiguration
newServerLaunchConfiguration =
  ServerLaunchConfiguration'
    { ec2KeyName =
        Prelude.Nothing,
      configureScriptType = Prelude.Nothing,
      associatePublicIpAddress = Prelude.Nothing,
      iamInstanceProfileName = Prelude.Nothing,
      subnet = Prelude.Nothing,
      logicalId = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      userData = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      configureScript = Prelude.Nothing,
      server = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
serverLaunchConfiguration_ec2KeyName :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_ec2KeyName = Lens.lens (\ServerLaunchConfiguration' {ec2KeyName} -> ec2KeyName) (\s@ServerLaunchConfiguration' {} a -> s {ec2KeyName = a} :: ServerLaunchConfiguration)

-- | The type of configuration script.
serverLaunchConfiguration_configureScriptType :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe ScriptType)
serverLaunchConfiguration_configureScriptType = Lens.lens (\ServerLaunchConfiguration' {configureScriptType} -> configureScriptType) (\s@ServerLaunchConfiguration' {} a -> s {configureScriptType = a} :: ServerLaunchConfiguration)

-- | Indicates whether a publicly accessible IP address is created when
-- launching the server.
serverLaunchConfiguration_associatePublicIpAddress :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Bool)
serverLaunchConfiguration_associatePublicIpAddress = Lens.lens (\ServerLaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ServerLaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: ServerLaunchConfiguration)

-- | The name of the IAM instance profile.
serverLaunchConfiguration_iamInstanceProfileName :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_iamInstanceProfileName = Lens.lens (\ServerLaunchConfiguration' {iamInstanceProfileName} -> iamInstanceProfileName) (\s@ServerLaunchConfiguration' {} a -> s {iamInstanceProfileName = a} :: ServerLaunchConfiguration)

-- | The ID of the subnet the server should be launched into.
serverLaunchConfiguration_subnet :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_subnet = Lens.lens (\ServerLaunchConfiguration' {subnet} -> subnet) (\s@ServerLaunchConfiguration' {} a -> s {subnet = a} :: ServerLaunchConfiguration)

-- | The logical ID of the server in the AWS CloudFormation template.
serverLaunchConfiguration_logicalId :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_logicalId = Lens.lens (\ServerLaunchConfiguration' {logicalId} -> logicalId) (\s@ServerLaunchConfiguration' {} a -> s {logicalId = a} :: ServerLaunchConfiguration)

-- | The ID of the security group that applies to the launched server.
serverLaunchConfiguration_securityGroup :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_securityGroup = Lens.lens (\ServerLaunchConfiguration' {securityGroup} -> securityGroup) (\s@ServerLaunchConfiguration' {} a -> s {securityGroup = a} :: ServerLaunchConfiguration)

-- | Location of the user-data script to be executed when launching the
-- server.
serverLaunchConfiguration_userData :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe UserData)
serverLaunchConfiguration_userData = Lens.lens (\ServerLaunchConfiguration' {userData} -> userData) (\s@ServerLaunchConfiguration' {} a -> s {userData = a} :: ServerLaunchConfiguration)

-- | The instance type to use when launching the server.
serverLaunchConfiguration_instanceType :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_instanceType = Lens.lens (\ServerLaunchConfiguration' {instanceType} -> instanceType) (\s@ServerLaunchConfiguration' {} a -> s {instanceType = a} :: ServerLaunchConfiguration)

-- | Undocumented member.
serverLaunchConfiguration_configureScript :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe S3Location)
serverLaunchConfiguration_configureScript = Lens.lens (\ServerLaunchConfiguration' {configureScript} -> configureScript) (\s@ServerLaunchConfiguration' {} a -> s {configureScript = a} :: ServerLaunchConfiguration)

-- | The ID of the server with which the launch configuration is associated.
serverLaunchConfiguration_server :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Server)
serverLaunchConfiguration_server = Lens.lens (\ServerLaunchConfiguration' {server} -> server) (\s@ServerLaunchConfiguration' {} a -> s {server = a} :: ServerLaunchConfiguration)

-- | The ID of the VPC into which the server should be launched.
serverLaunchConfiguration_vpc :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_vpc = Lens.lens (\ServerLaunchConfiguration' {vpc} -> vpc) (\s@ServerLaunchConfiguration' {} a -> s {vpc = a} :: ServerLaunchConfiguration)

instance Core.FromJSON ServerLaunchConfiguration where
  parseJSON =
    Core.withObject
      "ServerLaunchConfiguration"
      ( \x ->
          ServerLaunchConfiguration'
            Prelude.<$> (x Core..:? "ec2KeyName")
            Prelude.<*> (x Core..:? "configureScriptType")
            Prelude.<*> (x Core..:? "associatePublicIpAddress")
            Prelude.<*> (x Core..:? "iamInstanceProfileName")
            Prelude.<*> (x Core..:? "subnet")
            Prelude.<*> (x Core..:? "logicalId")
            Prelude.<*> (x Core..:? "securityGroup")
            Prelude.<*> (x Core..:? "userData")
            Prelude.<*> (x Core..:? "instanceType")
            Prelude.<*> (x Core..:? "configureScript")
            Prelude.<*> (x Core..:? "server")
            Prelude.<*> (x Core..:? "vpc")
      )

instance Prelude.Hashable ServerLaunchConfiguration where
  hashWithSalt _salt ServerLaunchConfiguration' {..} =
    _salt `Prelude.hashWithSalt` ec2KeyName
      `Prelude.hashWithSalt` configureScriptType
      `Prelude.hashWithSalt` associatePublicIpAddress
      `Prelude.hashWithSalt` iamInstanceProfileName
      `Prelude.hashWithSalt` subnet
      `Prelude.hashWithSalt` logicalId
      `Prelude.hashWithSalt` securityGroup
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` configureScript
      `Prelude.hashWithSalt` server
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData ServerLaunchConfiguration where
  rnf ServerLaunchConfiguration' {..} =
    Prelude.rnf ec2KeyName
      `Prelude.seq` Prelude.rnf configureScriptType
      `Prelude.seq` Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf iamInstanceProfileName
      `Prelude.seq` Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf logicalId
      `Prelude.seq` Prelude.rnf securityGroup
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf configureScript
      `Prelude.seq` Prelude.rnf server
      `Prelude.seq` Prelude.rnf vpc

instance Core.ToJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ec2KeyName" Core..=) Prelude.<$> ec2KeyName,
            ("configureScriptType" Core..=)
              Prelude.<$> configureScriptType,
            ("associatePublicIpAddress" Core..=)
              Prelude.<$> associatePublicIpAddress,
            ("iamInstanceProfileName" Core..=)
              Prelude.<$> iamInstanceProfileName,
            ("subnet" Core..=) Prelude.<$> subnet,
            ("logicalId" Core..=) Prelude.<$> logicalId,
            ("securityGroup" Core..=) Prelude.<$> securityGroup,
            ("userData" Core..=) Prelude.<$> userData,
            ("instanceType" Core..=) Prelude.<$> instanceType,
            ("configureScript" Core..=)
              Prelude.<$> configureScript,
            ("server" Core..=) Prelude.<$> server,
            ("vpc" Core..=) Prelude.<$> vpc
          ]
      )
