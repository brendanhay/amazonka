{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ImageBuilder
  ( ImageBuilder (..)
  -- * Smart constructor
  , mkImageBuilder
  -- * Lenses
  , ibName
  , ibAccessEndpoints
  , ibAppstreamAgentVersion
  , ibArn
  , ibCreatedTime
  , ibDescription
  , ibDisplayName
  , ibDomainJoinInfo
  , ibEnableDefaultInternetAccess
  , ibIamRoleArn
  , ibImageArn
  , ibImageBuilderErrors
  , ibInstanceType
  , ibNetworkAccessConfiguration
  , ibPlatform
  , ibState
  , ibStateChangeReason
  , ibVpcConfig
  ) where

import qualified Network.AWS.AppStream.Types.AccessEndpoint as Types
import qualified Network.AWS.AppStream.Types.AppstreamAgentVersion as Types
import qualified Network.AWS.AppStream.Types.Arn as Types
import qualified Network.AWS.AppStream.Types.DomainJoinInfo as Types
import qualified Network.AWS.AppStream.Types.ImageBuilderState as Types
import qualified Network.AWS.AppStream.Types.ImageBuilderStateChangeReason as Types
import qualified Network.AWS.AppStream.Types.NetworkAccessConfiguration as Types
import qualified Network.AWS.AppStream.Types.PlatformType as Types
import qualified Network.AWS.AppStream.Types.ResourceError as Types
import qualified Network.AWS.AppStream.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a virtual machine that is used to create an image. 
--
-- /See:/ 'mkImageBuilder' smart constructor.
data ImageBuilder = ImageBuilder'
  { name :: Core.Text
    -- ^ The name of the image builder.
  , accessEndpoints :: Core.Maybe (Core.NonEmpty Types.AccessEndpoint)
    -- ^ The list of virtual private cloud (VPC) interface endpoint objects. Administrators can connect to the image builder only through the specified endpoints.
  , appstreamAgentVersion :: Core.Maybe Types.AppstreamAgentVersion
    -- ^ The version of the AppStream 2.0 agent that is currently being used by the image builder. 
  , arn :: Core.Maybe Types.Arn
    -- ^ The ARN for the image builder.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp when the image builder was created.
  , description :: Core.Maybe Core.Text
    -- ^ The description to display.
  , displayName :: Core.Maybe Core.Text
    -- ^ The image builder name to display.
  , domainJoinInfo :: Core.Maybe Types.DomainJoinInfo
    -- ^ The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain. 
  , enableDefaultInternetAccess :: Core.Maybe Core.Bool
    -- ^ Enables or disables default internet access for the image builder.
  , iamRoleArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the IAM role that is applied to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
  , imageArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the image from which this builder was created.
  , imageBuilderErrors :: Core.Maybe [Types.ResourceError]
    -- ^ The image builder errors.
  , instanceType :: Core.Maybe Core.Text
    -- ^ The instance type for the image builder. The following instance types are available:
--
--
--     * stream.standard.medium
--
--
--     * stream.standard.large
--
--
--     * stream.compute.large
--
--
--     * stream.compute.xlarge
--
--
--     * stream.compute.2xlarge
--
--
--     * stream.compute.4xlarge
--
--
--     * stream.compute.8xlarge
--
--
--     * stream.memory.large
--
--
--     * stream.memory.xlarge
--
--
--     * stream.memory.2xlarge
--
--
--     * stream.memory.4xlarge
--
--
--     * stream.memory.8xlarge
--
--
--     * stream.memory.z1d.large
--
--
--     * stream.memory.z1d.xlarge
--
--
--     * stream.memory.z1d.2xlarge
--
--
--     * stream.memory.z1d.3xlarge
--
--
--     * stream.memory.z1d.6xlarge
--
--
--     * stream.memory.z1d.12xlarge
--
--
--     * stream.graphics-design.large
--
--
--     * stream.graphics-design.xlarge
--
--
--     * stream.graphics-design.2xlarge
--
--
--     * stream.graphics-design.4xlarge
--
--
--     * stream.graphics-desktop.2xlarge
--
--
--     * stream.graphics.g4dn.xlarge
--
--
--     * stream.graphics.g4dn.2xlarge
--
--
--     * stream.graphics.g4dn.4xlarge
--
--
--     * stream.graphics.g4dn.8xlarge
--
--
--     * stream.graphics.g4dn.12xlarge
--
--
--     * stream.graphics.g4dn.16xlarge
--
--
--     * stream.graphics-pro.4xlarge
--
--
--     * stream.graphics-pro.8xlarge
--
--
--     * stream.graphics-pro.16xlarge
--
--
  , networkAccessConfiguration :: Core.Maybe Types.NetworkAccessConfiguration
  , platform :: Core.Maybe Types.PlatformType
    -- ^ The operating system platform of the image builder.
  , state :: Core.Maybe Types.ImageBuilderState
    -- ^ The state of the image builder.
  , stateChangeReason :: Core.Maybe Types.ImageBuilderStateChangeReason
    -- ^ The reason why the last state change occurred.
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ The VPC configuration of the image builder.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImageBuilder' value with any optional fields omitted.
mkImageBuilder
    :: Core.Text -- ^ 'name'
    -> ImageBuilder
mkImageBuilder name
  = ImageBuilder'{name, accessEndpoints = Core.Nothing,
                  appstreamAgentVersion = Core.Nothing, arn = Core.Nothing,
                  createdTime = Core.Nothing, description = Core.Nothing,
                  displayName = Core.Nothing, domainJoinInfo = Core.Nothing,
                  enableDefaultInternetAccess = Core.Nothing,
                  iamRoleArn = Core.Nothing, imageArn = Core.Nothing,
                  imageBuilderErrors = Core.Nothing, instanceType = Core.Nothing,
                  networkAccessConfiguration = Core.Nothing, platform = Core.Nothing,
                  state = Core.Nothing, stateChangeReason = Core.Nothing,
                  vpcConfig = Core.Nothing}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibName :: Lens.Lens' ImageBuilder Core.Text
ibName = Lens.field @"name"
{-# INLINEABLE ibName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The list of virtual private cloud (VPC) interface endpoint objects. Administrators can connect to the image builder only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibAccessEndpoints :: Lens.Lens' ImageBuilder (Core.Maybe (Core.NonEmpty Types.AccessEndpoint))
ibAccessEndpoints = Lens.field @"accessEndpoints"
{-# INLINEABLE ibAccessEndpoints #-}
{-# DEPRECATED accessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead"  #-}

-- | The version of the AppStream 2.0 agent that is currently being used by the image builder. 
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibAppstreamAgentVersion :: Lens.Lens' ImageBuilder (Core.Maybe Types.AppstreamAgentVersion)
ibAppstreamAgentVersion = Lens.field @"appstreamAgentVersion"
{-# INLINEABLE ibAppstreamAgentVersion #-}
{-# DEPRECATED appstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead"  #-}

-- | The ARN for the image builder.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibArn :: Lens.Lens' ImageBuilder (Core.Maybe Types.Arn)
ibArn = Lens.field @"arn"
{-# INLINEABLE ibArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time stamp when the image builder was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibCreatedTime :: Lens.Lens' ImageBuilder (Core.Maybe Core.NominalDiffTime)
ibCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE ibCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDescription :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
ibDescription = Lens.field @"description"
{-# INLINEABLE ibDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The image builder name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDisplayName :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
ibDisplayName = Lens.field @"displayName"
{-# INLINEABLE ibDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain. 
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDomainJoinInfo :: Lens.Lens' ImageBuilder (Core.Maybe Types.DomainJoinInfo)
ibDomainJoinInfo = Lens.field @"domainJoinInfo"
{-# INLINEABLE ibDomainJoinInfo #-}
{-# DEPRECATED domainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead"  #-}

-- | Enables or disables default internet access for the image builder.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibEnableDefaultInternetAccess :: Lens.Lens' ImageBuilder (Core.Maybe Core.Bool)
ibEnableDefaultInternetAccess = Lens.field @"enableDefaultInternetAccess"
{-# INLINEABLE ibEnableDefaultInternetAccess #-}
{-# DEPRECATED enableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead"  #-}

-- | The ARN of the IAM role that is applied to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibIamRoleArn :: Lens.Lens' ImageBuilder (Core.Maybe Types.Arn)
ibIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE ibIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | The ARN of the image from which this builder was created.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibImageArn :: Lens.Lens' ImageBuilder (Core.Maybe Types.Arn)
ibImageArn = Lens.field @"imageArn"
{-# INLINEABLE ibImageArn #-}
{-# DEPRECATED imageArn "Use generic-lens or generic-optics with 'imageArn' instead"  #-}

-- | The image builder errors.
--
-- /Note:/ Consider using 'imageBuilderErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibImageBuilderErrors :: Lens.Lens' ImageBuilder (Core.Maybe [Types.ResourceError])
ibImageBuilderErrors = Lens.field @"imageBuilderErrors"
{-# INLINEABLE ibImageBuilderErrors #-}
{-# DEPRECATED imageBuilderErrors "Use generic-lens or generic-optics with 'imageBuilderErrors' instead"  #-}

-- | The instance type for the image builder. The following instance types are available:
--
--
--     * stream.standard.medium
--
--
--     * stream.standard.large
--
--
--     * stream.compute.large
--
--
--     * stream.compute.xlarge
--
--
--     * stream.compute.2xlarge
--
--
--     * stream.compute.4xlarge
--
--
--     * stream.compute.8xlarge
--
--
--     * stream.memory.large
--
--
--     * stream.memory.xlarge
--
--
--     * stream.memory.2xlarge
--
--
--     * stream.memory.4xlarge
--
--
--     * stream.memory.8xlarge
--
--
--     * stream.memory.z1d.large
--
--
--     * stream.memory.z1d.xlarge
--
--
--     * stream.memory.z1d.2xlarge
--
--
--     * stream.memory.z1d.3xlarge
--
--
--     * stream.memory.z1d.6xlarge
--
--
--     * stream.memory.z1d.12xlarge
--
--
--     * stream.graphics-design.large
--
--
--     * stream.graphics-design.xlarge
--
--
--     * stream.graphics-design.2xlarge
--
--
--     * stream.graphics-design.4xlarge
--
--
--     * stream.graphics-desktop.2xlarge
--
--
--     * stream.graphics.g4dn.xlarge
--
--
--     * stream.graphics.g4dn.2xlarge
--
--
--     * stream.graphics.g4dn.4xlarge
--
--
--     * stream.graphics.g4dn.8xlarge
--
--
--     * stream.graphics.g4dn.12xlarge
--
--
--     * stream.graphics.g4dn.16xlarge
--
--
--     * stream.graphics-pro.4xlarge
--
--
--     * stream.graphics-pro.8xlarge
--
--
--     * stream.graphics-pro.16xlarge
--
--
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibInstanceType :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
ibInstanceType = Lens.field @"instanceType"
{-# INLINEABLE ibInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibNetworkAccessConfiguration :: Lens.Lens' ImageBuilder (Core.Maybe Types.NetworkAccessConfiguration)
ibNetworkAccessConfiguration = Lens.field @"networkAccessConfiguration"
{-# INLINEABLE ibNetworkAccessConfiguration #-}
{-# DEPRECATED networkAccessConfiguration "Use generic-lens or generic-optics with 'networkAccessConfiguration' instead"  #-}

-- | The operating system platform of the image builder.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibPlatform :: Lens.Lens' ImageBuilder (Core.Maybe Types.PlatformType)
ibPlatform = Lens.field @"platform"
{-# INLINEABLE ibPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The state of the image builder.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibState :: Lens.Lens' ImageBuilder (Core.Maybe Types.ImageBuilderState)
ibState = Lens.field @"state"
{-# INLINEABLE ibState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason why the last state change occurred.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibStateChangeReason :: Lens.Lens' ImageBuilder (Core.Maybe Types.ImageBuilderStateChangeReason)
ibStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE ibStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | The VPC configuration of the image builder.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibVpcConfig :: Lens.Lens' ImageBuilder (Core.Maybe Types.VpcConfig)
ibVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE ibVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON ImageBuilder where
        parseJSON
          = Core.withObject "ImageBuilder" Core.$
              \ x ->
                ImageBuilder' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "AccessEndpoints" Core.<*>
                    x Core..:? "AppstreamAgentVersion"
                    Core.<*> x Core..:? "Arn"
                    Core.<*> x Core..:? "CreatedTime"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "DomainJoinInfo"
                    Core.<*> x Core..:? "EnableDefaultInternetAccess"
                    Core.<*> x Core..:? "IamRoleArn"
                    Core.<*> x Core..:? "ImageArn"
                    Core.<*> x Core..:? "ImageBuilderErrors"
                    Core.<*> x Core..:? "InstanceType"
                    Core.<*> x Core..:? "NetworkAccessConfiguration"
                    Core.<*> x Core..:? "Platform"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "VpcConfig"
