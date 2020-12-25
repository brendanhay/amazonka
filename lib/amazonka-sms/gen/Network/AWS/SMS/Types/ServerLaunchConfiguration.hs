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
    slcAssociatePublicIpAddress,
    slcConfigureScript,
    slcConfigureScriptType,
    slcEc2KeyName,
    slcIamInstanceProfileName,
    slcInstanceType,
    slcLogicalId,
    slcSecurityGroup,
    slcServer,
    slcSubnet,
    slcUserData,
    slcVpc,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.EC2KeyName as Types
import qualified Network.AWS.SMS.Types.IamInstanceProfileName as Types
import qualified Network.AWS.SMS.Types.InstanceType as Types
import qualified Network.AWS.SMS.Types.LogicalId as Types
import qualified Network.AWS.SMS.Types.S3Location as Types
import qualified Network.AWS.SMS.Types.ScriptType as Types
import qualified Network.AWS.SMS.Types.SecurityGroup as Types
import qualified Network.AWS.SMS.Types.Server as Types
import qualified Network.AWS.SMS.Types.Subnet as Types
import qualified Network.AWS.SMS.Types.UserData as Types
import qualified Network.AWS.SMS.Types.Vpc as Types

-- | Launch configuration for a server.
--
-- /See:/ 'mkServerLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { -- | Indicates whether a publicly accessible IP address is created when launching the server.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    configureScript :: Core.Maybe Types.S3Location,
    -- | The type of configuration script.
    configureScriptType :: Core.Maybe Types.ScriptType,
    -- | The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
    ec2KeyName :: Core.Maybe Types.EC2KeyName,
    -- | The name of the IAM instance profile.
    iamInstanceProfileName :: Core.Maybe Types.IamInstanceProfileName,
    -- | The instance type to use when launching the server.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The logical ID of the server in the AWS CloudFormation template.
    logicalId :: Core.Maybe Types.LogicalId,
    -- | The ID of the security group that applies to the launched server.
    securityGroup :: Core.Maybe Types.SecurityGroup,
    -- | The ID of the server with which the launch configuration is associated.
    server :: Core.Maybe Types.Server,
    -- | The ID of the subnet the server should be launched into.
    subnet :: Core.Maybe Types.Subnet,
    -- | Location of the user-data script to be executed when launching the server.
    userData :: Core.Maybe Types.UserData,
    -- | The ID of the VPC into which the server should be launched.
    vpc :: Core.Maybe Types.Vpc
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerLaunchConfiguration' value with any optional fields omitted.
mkServerLaunchConfiguration ::
  ServerLaunchConfiguration
mkServerLaunchConfiguration =
  ServerLaunchConfiguration'
    { associatePublicIpAddress =
        Core.Nothing,
      configureScript = Core.Nothing,
      configureScriptType = Core.Nothing,
      ec2KeyName = Core.Nothing,
      iamInstanceProfileName = Core.Nothing,
      instanceType = Core.Nothing,
      logicalId = Core.Nothing,
      securityGroup = Core.Nothing,
      server = Core.Nothing,
      subnet = Core.Nothing,
      userData = Core.Nothing,
      vpc = Core.Nothing
    }

-- | Indicates whether a publicly accessible IP address is created when launching the server.
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcAssociatePublicIpAddress :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Core.Bool)
slcAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# DEPRECATED slcAssociatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'configureScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcConfigureScript :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.S3Location)
slcConfigureScript = Lens.field @"configureScript"
{-# DEPRECATED slcConfigureScript "Use generic-lens or generic-optics with 'configureScript' instead." #-}

-- | The type of configuration script.
--
-- /Note:/ Consider using 'configureScriptType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcConfigureScriptType :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.ScriptType)
slcConfigureScriptType = Lens.field @"configureScriptType"
{-# DEPRECATED slcConfigureScriptType "Use generic-lens or generic-optics with 'configureScriptType' instead." #-}

-- | The name of the Amazon EC2 SSH key to be used for connecting to the launched server.
--
-- /Note:/ Consider using 'ec2KeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEc2KeyName :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.EC2KeyName)
slcEc2KeyName = Lens.field @"ec2KeyName"
{-# DEPRECATED slcEc2KeyName "Use generic-lens or generic-optics with 'ec2KeyName' instead." #-}

-- | The name of the IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcIamInstanceProfileName :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.IamInstanceProfileName)
slcIamInstanceProfileName = Lens.field @"iamInstanceProfileName"
{-# DEPRECATED slcIamInstanceProfileName "Use generic-lens or generic-optics with 'iamInstanceProfileName' instead." #-}

-- | The instance type to use when launching the server.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcInstanceType :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.InstanceType)
slcInstanceType = Lens.field @"instanceType"
{-# DEPRECATED slcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The logical ID of the server in the AWS CloudFormation template.
--
-- /Note:/ Consider using 'logicalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcLogicalId :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.LogicalId)
slcLogicalId = Lens.field @"logicalId"
{-# DEPRECATED slcLogicalId "Use generic-lens or generic-optics with 'logicalId' instead." #-}

-- | The ID of the security group that applies to the launched server.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcSecurityGroup :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.SecurityGroup)
slcSecurityGroup = Lens.field @"securityGroup"
{-# DEPRECATED slcSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The ID of the server with which the launch configuration is associated.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcServer :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.Server)
slcServer = Lens.field @"server"
{-# DEPRECATED slcServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The ID of the subnet the server should be launched into.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcSubnet :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.Subnet)
slcSubnet = Lens.field @"subnet"
{-# DEPRECATED slcSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | Location of the user-data script to be executed when launching the server.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcUserData :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.UserData)
slcUserData = Lens.field @"userData"
{-# DEPRECATED slcUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The ID of the VPC into which the server should be launched.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcVpc :: Lens.Lens' ServerLaunchConfiguration (Core.Maybe Types.Vpc)
slcVpc = Lens.field @"vpc"
{-# DEPRECATED slcVpc "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.FromJSON ServerLaunchConfiguration where
  toJSON ServerLaunchConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("associatePublicIpAddress" Core..=)
              Core.<$> associatePublicIpAddress,
            ("configureScript" Core..=) Core.<$> configureScript,
            ("configureScriptType" Core..=) Core.<$> configureScriptType,
            ("ec2KeyName" Core..=) Core.<$> ec2KeyName,
            ("iamInstanceProfileName" Core..=) Core.<$> iamInstanceProfileName,
            ("instanceType" Core..=) Core.<$> instanceType,
            ("logicalId" Core..=) Core.<$> logicalId,
            ("securityGroup" Core..=) Core.<$> securityGroup,
            ("server" Core..=) Core.<$> server,
            ("subnet" Core..=) Core.<$> subnet,
            ("userData" Core..=) Core.<$> userData,
            ("vpc" Core..=) Core.<$> vpc
          ]
      )

instance Core.FromJSON ServerLaunchConfiguration where
  parseJSON =
    Core.withObject "ServerLaunchConfiguration" Core.$
      \x ->
        ServerLaunchConfiguration'
          Core.<$> (x Core..:? "associatePublicIpAddress")
          Core.<*> (x Core..:? "configureScript")
          Core.<*> (x Core..:? "configureScriptType")
          Core.<*> (x Core..:? "ec2KeyName")
          Core.<*> (x Core..:? "iamInstanceProfileName")
          Core.<*> (x Core..:? "instanceType")
          Core.<*> (x Core..:? "logicalId")
          Core.<*> (x Core..:? "securityGroup")
          Core.<*> (x Core..:? "server")
          Core.<*> (x Core..:? "subnet")
          Core.<*> (x Core..:? "userData")
          Core.<*> (x Core..:? "vpc")
