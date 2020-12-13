{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerLaunchConfiguration
  ( ServerLaunchConfiguration (..),

    -- * Smart constructor
    mkServerLaunchConfiguration,

    -- * Lenses
    slcEc2KeyName,
    slcConfigureScriptType,
    slcAssociatePublicIPAddress,
    slcIamInstanceProfileName,
    slcSubnet,
    slcLogicalId,
    slcSecurityGroup,
    slcUserData,
    slcInstanceType,
    slcConfigureScript,
    slcServer,
    slcVpc,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.UserData

-- | Launch configuration for a server.
--
-- /See:/ 'mkServerLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { -- | The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
    ec2KeyName :: Lude.Maybe Lude.Text,
    -- | The type of configuration script.
    configureScriptType :: Lude.Maybe ScriptType,
    -- | Indicates whether a publicly accessible IP address is created when launching the server.
    associatePublicIPAddress :: Lude.Maybe Lude.Bool,
    -- | The name of the IAM instance profile.
    iamInstanceProfileName :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet the server should be launched into.
    subnet :: Lude.Maybe Lude.Text,
    -- | The logical ID of the server in the AWS CloudFormation template.
    logicalId :: Lude.Maybe Lude.Text,
    -- | The ID of the security group that applies to the launched server.
    securityGroup :: Lude.Maybe Lude.Text,
    -- | Location of the user-data script to be executed when launching the server.
    userData :: Lude.Maybe UserData,
    -- | The instance type to use when launching the server.
    instanceType :: Lude.Maybe Lude.Text,
    configureScript :: Lude.Maybe S3Location,
    -- | The ID of the server with which the launch configuration is associated.
    server :: Lude.Maybe Server,
    -- | The ID of the VPC into which the server should be launched.
    vpc :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'ec2KeyName' - The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
-- * 'configureScriptType' - The type of configuration script.
-- * 'associatePublicIPAddress' - Indicates whether a publicly accessible IP address is created when launching the server.
-- * 'iamInstanceProfileName' - The name of the IAM instance profile.
-- * 'subnet' - The ID of the subnet the server should be launched into.
-- * 'logicalId' - The logical ID of the server in the AWS CloudFormation template.
-- * 'securityGroup' - The ID of the security group that applies to the launched server.
-- * 'userData' - Location of the user-data script to be executed when launching the server.
-- * 'instanceType' - The instance type to use when launching the server.
-- * 'configureScript' -
-- * 'server' - The ID of the server with which the launch configuration is associated.
-- * 'vpc' - The ID of the VPC into which the server should be launched.
mkServerLaunchConfiguration ::
  ServerLaunchConfiguration
mkServerLaunchConfiguration =
  ServerLaunchConfiguration'
    { ec2KeyName = Lude.Nothing,
      configureScriptType = Lude.Nothing,
      associatePublicIPAddress = Lude.Nothing,
      iamInstanceProfileName = Lude.Nothing,
      subnet = Lude.Nothing,
      logicalId = Lude.Nothing,
      securityGroup = Lude.Nothing,
      userData = Lude.Nothing,
      instanceType = Lude.Nothing,
      configureScript = Lude.Nothing,
      server = Lude.Nothing,
      vpc = Lude.Nothing
    }

-- | The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
--
-- /Note:/ Consider using 'ec2KeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEc2KeyName :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcEc2KeyName = Lens.lens (ec2KeyName :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {ec2KeyName = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcEc2KeyName "Use generic-lens or generic-optics with 'ec2KeyName' instead." #-}

-- | The type of configuration script.
--
-- /Note:/ Consider using 'configureScriptType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcConfigureScriptType :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe ScriptType)
slcConfigureScriptType = Lens.lens (configureScriptType :: ServerLaunchConfiguration -> Lude.Maybe ScriptType) (\s a -> s {configureScriptType = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcConfigureScriptType "Use generic-lens or generic-optics with 'configureScriptType' instead." #-}

-- | Indicates whether a publicly accessible IP address is created when launching the server.
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcAssociatePublicIPAddress :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Bool)
slcAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: ServerLaunchConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | The name of the IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcIamInstanceProfileName :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcIamInstanceProfileName = Lens.lens (iamInstanceProfileName :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {iamInstanceProfileName = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcIamInstanceProfileName "Use generic-lens or generic-optics with 'iamInstanceProfileName' instead." #-}

-- | The ID of the subnet the server should be launched into.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcSubnet :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcSubnet = Lens.lens (subnet :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {subnet = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | The logical ID of the server in the AWS CloudFormation template.
--
-- /Note:/ Consider using 'logicalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcLogicalId :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcLogicalId = Lens.lens (logicalId :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {logicalId = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcLogicalId "Use generic-lens or generic-optics with 'logicalId' instead." #-}

-- | The ID of the security group that applies to the launched server.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcSecurityGroup :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcSecurityGroup = Lens.lens (securityGroup :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {securityGroup = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | Location of the user-data script to be executed when launching the server.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcUserData :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe UserData)
slcUserData = Lens.lens (userData :: ServerLaunchConfiguration -> Lude.Maybe UserData) (\s a -> s {userData = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The instance type to use when launching the server.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcInstanceType :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcInstanceType = Lens.lens (instanceType :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'configureScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcConfigureScript :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe S3Location)
slcConfigureScript = Lens.lens (configureScript :: ServerLaunchConfiguration -> Lude.Maybe S3Location) (\s a -> s {configureScript = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcConfigureScript "Use generic-lens or generic-optics with 'configureScript' instead." #-}

-- | The ID of the server with which the launch configuration is associated.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcServer :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Server)
slcServer = Lens.lens (server :: ServerLaunchConfiguration -> Lude.Maybe Server) (\s a -> s {server = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The ID of the VPC into which the server should be launched.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcVpc :: Lens.Lens' ServerLaunchConfiguration (Lude.Maybe Lude.Text)
slcVpc = Lens.lens (vpc :: ServerLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpc = a} :: ServerLaunchConfiguration)
{-# DEPRECATED slcVpc "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Lude.FromJSON ServerLaunchConfiguration where
  parseJSON =
    Lude.withObject
      "ServerLaunchConfiguration"
      ( \x ->
          ServerLaunchConfiguration'
            Lude.<$> (x Lude..:? "ec2KeyName")
            Lude.<*> (x Lude..:? "configureScriptType")
            Lude.<*> (x Lude..:? "associatePublicIpAddress")
            Lude.<*> (x Lude..:? "iamInstanceProfileName")
            Lude.<*> (x Lude..:? "subnet")
            Lude.<*> (x Lude..:? "logicalId")
            Lude.<*> (x Lude..:? "securityGroup")
            Lude.<*> (x Lude..:? "userData")
            Lude.<*> (x Lude..:? "instanceType")
            Lude.<*> (x Lude..:? "configureScript")
            Lude.<*> (x Lude..:? "server")
            Lude.<*> (x Lude..:? "vpc")
      )

instance Lude.ToJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ec2KeyName" Lude..=) Lude.<$> ec2KeyName,
            ("configureScriptType" Lude..=) Lude.<$> configureScriptType,
            ("associatePublicIpAddress" Lude..=)
              Lude.<$> associatePublicIPAddress,
            ("iamInstanceProfileName" Lude..=) Lude.<$> iamInstanceProfileName,
            ("subnet" Lude..=) Lude.<$> subnet,
            ("logicalId" Lude..=) Lude.<$> logicalId,
            ("securityGroup" Lude..=) Lude.<$> securityGroup,
            ("userData" Lude..=) Lude.<$> userData,
            ("instanceType" Lude..=) Lude.<$> instanceType,
            ("configureScript" Lude..=) Lude.<$> configureScript,
            ("server" Lude..=) Lude.<$> server,
            ("vpc" Lude..=) Lude.<$> vpc
          ]
      )
