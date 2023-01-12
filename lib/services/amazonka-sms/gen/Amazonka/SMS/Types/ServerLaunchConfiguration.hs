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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerLaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.S3Location
import Amazonka.SMS.Types.ScriptType
import Amazonka.SMS.Types.Server
import Amazonka.SMS.Types.UserData

-- | Launch configuration for a server.
--
-- /See:/ 'newServerLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { -- | Indicates whether a publicly accessible IP address is created when
    -- launching the server.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    configureScript :: Prelude.Maybe S3Location,
    -- | The type of configuration script.
    configureScriptType :: Prelude.Maybe ScriptType,
    -- | The name of the Amazon EC2 SSH key to be used for connecting to the
    -- launched server.
    ec2KeyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM instance profile.
    iamInstanceProfileName :: Prelude.Maybe Prelude.Text,
    -- | The instance type to use when launching the server.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The logical ID of the server in the CloudFormation template.
    logicalId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group that applies to the launched server.
    securityGroup :: Prelude.Maybe Prelude.Text,
    -- | The ID of the server with which the launch configuration is associated.
    server :: Prelude.Maybe Server,
    -- | The ID of the subnet the server should be launched into.
    subnet :: Prelude.Maybe Prelude.Text,
    -- | Location of the user-data script to be executed when launching the
    -- server.
    userData :: Prelude.Maybe UserData,
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
-- 'associatePublicIpAddress', 'serverLaunchConfiguration_associatePublicIpAddress' - Indicates whether a publicly accessible IP address is created when
-- launching the server.
--
-- 'configureScript', 'serverLaunchConfiguration_configureScript' - Undocumented member.
--
-- 'configureScriptType', 'serverLaunchConfiguration_configureScriptType' - The type of configuration script.
--
-- 'ec2KeyName', 'serverLaunchConfiguration_ec2KeyName' - The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
--
-- 'iamInstanceProfileName', 'serverLaunchConfiguration_iamInstanceProfileName' - The name of the IAM instance profile.
--
-- 'instanceType', 'serverLaunchConfiguration_instanceType' - The instance type to use when launching the server.
--
-- 'logicalId', 'serverLaunchConfiguration_logicalId' - The logical ID of the server in the CloudFormation template.
--
-- 'securityGroup', 'serverLaunchConfiguration_securityGroup' - The ID of the security group that applies to the launched server.
--
-- 'server', 'serverLaunchConfiguration_server' - The ID of the server with which the launch configuration is associated.
--
-- 'subnet', 'serverLaunchConfiguration_subnet' - The ID of the subnet the server should be launched into.
--
-- 'userData', 'serverLaunchConfiguration_userData' - Location of the user-data script to be executed when launching the
-- server.
--
-- 'vpc', 'serverLaunchConfiguration_vpc' - The ID of the VPC into which the server should be launched.
newServerLaunchConfiguration ::
  ServerLaunchConfiguration
newServerLaunchConfiguration =
  ServerLaunchConfiguration'
    { associatePublicIpAddress =
        Prelude.Nothing,
      configureScript = Prelude.Nothing,
      configureScriptType = Prelude.Nothing,
      ec2KeyName = Prelude.Nothing,
      iamInstanceProfileName = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      logicalId = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      server = Prelude.Nothing,
      subnet = Prelude.Nothing,
      userData = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | Indicates whether a publicly accessible IP address is created when
-- launching the server.
serverLaunchConfiguration_associatePublicIpAddress :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Bool)
serverLaunchConfiguration_associatePublicIpAddress = Lens.lens (\ServerLaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ServerLaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: ServerLaunchConfiguration)

-- | Undocumented member.
serverLaunchConfiguration_configureScript :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe S3Location)
serverLaunchConfiguration_configureScript = Lens.lens (\ServerLaunchConfiguration' {configureScript} -> configureScript) (\s@ServerLaunchConfiguration' {} a -> s {configureScript = a} :: ServerLaunchConfiguration)

-- | The type of configuration script.
serverLaunchConfiguration_configureScriptType :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe ScriptType)
serverLaunchConfiguration_configureScriptType = Lens.lens (\ServerLaunchConfiguration' {configureScriptType} -> configureScriptType) (\s@ServerLaunchConfiguration' {} a -> s {configureScriptType = a} :: ServerLaunchConfiguration)

-- | The name of the Amazon EC2 SSH key to be used for connecting to the
-- launched server.
serverLaunchConfiguration_ec2KeyName :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_ec2KeyName = Lens.lens (\ServerLaunchConfiguration' {ec2KeyName} -> ec2KeyName) (\s@ServerLaunchConfiguration' {} a -> s {ec2KeyName = a} :: ServerLaunchConfiguration)

-- | The name of the IAM instance profile.
serverLaunchConfiguration_iamInstanceProfileName :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_iamInstanceProfileName = Lens.lens (\ServerLaunchConfiguration' {iamInstanceProfileName} -> iamInstanceProfileName) (\s@ServerLaunchConfiguration' {} a -> s {iamInstanceProfileName = a} :: ServerLaunchConfiguration)

-- | The instance type to use when launching the server.
serverLaunchConfiguration_instanceType :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_instanceType = Lens.lens (\ServerLaunchConfiguration' {instanceType} -> instanceType) (\s@ServerLaunchConfiguration' {} a -> s {instanceType = a} :: ServerLaunchConfiguration)

-- | The logical ID of the server in the CloudFormation template.
serverLaunchConfiguration_logicalId :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_logicalId = Lens.lens (\ServerLaunchConfiguration' {logicalId} -> logicalId) (\s@ServerLaunchConfiguration' {} a -> s {logicalId = a} :: ServerLaunchConfiguration)

-- | The ID of the security group that applies to the launched server.
serverLaunchConfiguration_securityGroup :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_securityGroup = Lens.lens (\ServerLaunchConfiguration' {securityGroup} -> securityGroup) (\s@ServerLaunchConfiguration' {} a -> s {securityGroup = a} :: ServerLaunchConfiguration)

-- | The ID of the server with which the launch configuration is associated.
serverLaunchConfiguration_server :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Server)
serverLaunchConfiguration_server = Lens.lens (\ServerLaunchConfiguration' {server} -> server) (\s@ServerLaunchConfiguration' {} a -> s {server = a} :: ServerLaunchConfiguration)

-- | The ID of the subnet the server should be launched into.
serverLaunchConfiguration_subnet :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_subnet = Lens.lens (\ServerLaunchConfiguration' {subnet} -> subnet) (\s@ServerLaunchConfiguration' {} a -> s {subnet = a} :: ServerLaunchConfiguration)

-- | Location of the user-data script to be executed when launching the
-- server.
serverLaunchConfiguration_userData :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe UserData)
serverLaunchConfiguration_userData = Lens.lens (\ServerLaunchConfiguration' {userData} -> userData) (\s@ServerLaunchConfiguration' {} a -> s {userData = a} :: ServerLaunchConfiguration)

-- | The ID of the VPC into which the server should be launched.
serverLaunchConfiguration_vpc :: Lens.Lens' ServerLaunchConfiguration (Prelude.Maybe Prelude.Text)
serverLaunchConfiguration_vpc = Lens.lens (\ServerLaunchConfiguration' {vpc} -> vpc) (\s@ServerLaunchConfiguration' {} a -> s {vpc = a} :: ServerLaunchConfiguration)

instance Data.FromJSON ServerLaunchConfiguration where
  parseJSON =
    Data.withObject
      "ServerLaunchConfiguration"
      ( \x ->
          ServerLaunchConfiguration'
            Prelude.<$> (x Data..:? "associatePublicIpAddress")
            Prelude.<*> (x Data..:? "configureScript")
            Prelude.<*> (x Data..:? "configureScriptType")
            Prelude.<*> (x Data..:? "ec2KeyName")
            Prelude.<*> (x Data..:? "iamInstanceProfileName")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "logicalId")
            Prelude.<*> (x Data..:? "securityGroup")
            Prelude.<*> (x Data..:? "server")
            Prelude.<*> (x Data..:? "subnet")
            Prelude.<*> (x Data..:? "userData")
            Prelude.<*> (x Data..:? "vpc")
      )

instance Prelude.Hashable ServerLaunchConfiguration where
  hashWithSalt _salt ServerLaunchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` associatePublicIpAddress
      `Prelude.hashWithSalt` configureScript
      `Prelude.hashWithSalt` configureScriptType
      `Prelude.hashWithSalt` ec2KeyName
      `Prelude.hashWithSalt` iamInstanceProfileName
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` logicalId
      `Prelude.hashWithSalt` securityGroup
      `Prelude.hashWithSalt` server
      `Prelude.hashWithSalt` subnet
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData ServerLaunchConfiguration where
  rnf ServerLaunchConfiguration' {..} =
    Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf configureScript
      `Prelude.seq` Prelude.rnf configureScriptType
      `Prelude.seq` Prelude.rnf ec2KeyName
      `Prelude.seq` Prelude.rnf iamInstanceProfileName
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf logicalId
      `Prelude.seq` Prelude.rnf securityGroup
      `Prelude.seq` Prelude.rnf server
      `Prelude.seq` Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf vpc

instance Data.ToJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associatePublicIpAddress" Data..=)
              Prelude.<$> associatePublicIpAddress,
            ("configureScript" Data..=)
              Prelude.<$> configureScript,
            ("configureScriptType" Data..=)
              Prelude.<$> configureScriptType,
            ("ec2KeyName" Data..=) Prelude.<$> ec2KeyName,
            ("iamInstanceProfileName" Data..=)
              Prelude.<$> iamInstanceProfileName,
            ("instanceType" Data..=) Prelude.<$> instanceType,
            ("logicalId" Data..=) Prelude.<$> logicalId,
            ("securityGroup" Data..=) Prelude.<$> securityGroup,
            ("server" Data..=) Prelude.<$> server,
            ("subnet" Data..=) Prelude.<$> subnet,
            ("userData" Data..=) Prelude.<$> userData,
            ("vpc" Data..=) Prelude.<$> vpc
          ]
      )
