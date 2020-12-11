-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilder
  ( ImageBuilder (..),

    -- * Smart constructor
    mkImageBuilder,

    -- * Lenses
    ibDomainJoinInfo,
    ibIAMRoleARN,
    ibState,
    ibPlatform,
    ibNetworkAccessConfiguration,
    ibStateChangeReason,
    ibARN,
    ibCreatedTime,
    ibImageBuilderErrors,
    ibInstanceType,
    ibAccessEndpoints,
    ibVPCConfig,
    ibImageARN,
    ibDisplayName,
    ibEnableDefaultInternetAccess,
    ibDescription,
    ibAppstreamAgentVersion,
    ibName,
  )
where

import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.ImageBuilderState
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.PlatformType
import Network.AWS.AppStream.Types.ResourceError
import Network.AWS.AppStream.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a virtual machine that is used to create an image.
--
-- /See:/ 'mkImageBuilder' smart constructor.
data ImageBuilder = ImageBuilder'
  { domainJoinInfo ::
      Lude.Maybe DomainJoinInfo,
    iamRoleARN :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe ImageBuilderState,
    platform :: Lude.Maybe PlatformType,
    networkAccessConfiguration ::
      Lude.Maybe NetworkAccessConfiguration,
    stateChangeReason :: Lude.Maybe ImageBuilderStateChangeReason,
    arn :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Timestamp,
    imageBuilderErrors :: Lude.Maybe [ResourceError],
    instanceType :: Lude.Maybe Lude.Text,
    accessEndpoints :: Lude.Maybe (Lude.NonEmpty AccessEndpoint),
    vpcConfig :: Lude.Maybe VPCConfig,
    imageARN :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    enableDefaultInternetAccess :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    appstreamAgentVersion :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageBuilder' with the minimum fields required to make a request.
--
-- * 'accessEndpoints' - The list of virtual private cloud (VPC) interface endpoint objects. Administrators can connect to the image builder only through the specified endpoints.
-- * 'appstreamAgentVersion' - The version of the AppStream 2.0 agent that is currently being used by the image builder.
-- * 'arn' - The ARN for the image builder.
-- * 'createdTime' - The time stamp when the image builder was created.
-- * 'description' - The description to display.
-- * 'displayName' - The image builder name to display.
-- * 'domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
-- * 'enableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
-- * 'iamRoleARN' - The ARN of the IAM role that is applied to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'imageARN' - The ARN of the image from which this builder was created.
-- * 'imageBuilderErrors' - The image builder errors.
-- * 'instanceType' - The instance type for the image builder. The following instance types are available:
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
-- * 'name' - The name of the image builder.
-- * 'networkAccessConfiguration' - Undocumented field.
-- * 'platform' - The operating system platform of the image builder.
-- * 'state' - The state of the image builder.
-- * 'stateChangeReason' - The reason why the last state change occurred.
-- * 'vpcConfig' - The VPC configuration of the image builder.
mkImageBuilder ::
  -- | 'name'
  Lude.Text ->
  ImageBuilder
mkImageBuilder pName_ =
  ImageBuilder'
    { domainJoinInfo = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      state = Lude.Nothing,
      platform = Lude.Nothing,
      networkAccessConfiguration = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      imageBuilderErrors = Lude.Nothing,
      instanceType = Lude.Nothing,
      accessEndpoints = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      imageARN = Lude.Nothing,
      displayName = Lude.Nothing,
      enableDefaultInternetAccess = Lude.Nothing,
      description = Lude.Nothing,
      appstreamAgentVersion = Lude.Nothing,
      name = pName_
    }

-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDomainJoinInfo :: Lens.Lens' ImageBuilder (Lude.Maybe DomainJoinInfo)
ibDomainJoinInfo = Lens.lens (domainJoinInfo :: ImageBuilder -> Lude.Maybe DomainJoinInfo) (\s a -> s {domainJoinInfo = a} :: ImageBuilder)
{-# DEPRECATED ibDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | The ARN of the IAM role that is applied to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibIAMRoleARN :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibIAMRoleARN = Lens.lens (iamRoleARN :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: ImageBuilder)
{-# DEPRECATED ibIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The state of the image builder.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibState :: Lens.Lens' ImageBuilder (Lude.Maybe ImageBuilderState)
ibState = Lens.lens (state :: ImageBuilder -> Lude.Maybe ImageBuilderState) (\s a -> s {state = a} :: ImageBuilder)
{-# DEPRECATED ibState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The operating system platform of the image builder.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibPlatform :: Lens.Lens' ImageBuilder (Lude.Maybe PlatformType)
ibPlatform = Lens.lens (platform :: ImageBuilder -> Lude.Maybe PlatformType) (\s a -> s {platform = a} :: ImageBuilder)
{-# DEPRECATED ibPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibNetworkAccessConfiguration :: Lens.Lens' ImageBuilder (Lude.Maybe NetworkAccessConfiguration)
ibNetworkAccessConfiguration = Lens.lens (networkAccessConfiguration :: ImageBuilder -> Lude.Maybe NetworkAccessConfiguration) (\s a -> s {networkAccessConfiguration = a} :: ImageBuilder)
{-# DEPRECATED ibNetworkAccessConfiguration "Use generic-lens or generic-optics with 'networkAccessConfiguration' instead." #-}

-- | The reason why the last state change occurred.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibStateChangeReason :: Lens.Lens' ImageBuilder (Lude.Maybe ImageBuilderStateChangeReason)
ibStateChangeReason = Lens.lens (stateChangeReason :: ImageBuilder -> Lude.Maybe ImageBuilderStateChangeReason) (\s a -> s {stateChangeReason = a} :: ImageBuilder)
{-# DEPRECATED ibStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | The ARN for the image builder.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibARN :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibARN = Lens.lens (arn :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ImageBuilder)
{-# DEPRECATED ibARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time stamp when the image builder was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibCreatedTime :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Timestamp)
ibCreatedTime = Lens.lens (createdTime :: ImageBuilder -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ImageBuilder)
{-# DEPRECATED ibCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The image builder errors.
--
-- /Note:/ Consider using 'imageBuilderErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibImageBuilderErrors :: Lens.Lens' ImageBuilder (Lude.Maybe [ResourceError])
ibImageBuilderErrors = Lens.lens (imageBuilderErrors :: ImageBuilder -> Lude.Maybe [ResourceError]) (\s a -> s {imageBuilderErrors = a} :: ImageBuilder)
{-# DEPRECATED ibImageBuilderErrors "Use generic-lens or generic-optics with 'imageBuilderErrors' instead." #-}

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
ibInstanceType :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibInstanceType = Lens.lens (instanceType :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ImageBuilder)
{-# DEPRECATED ibInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The list of virtual private cloud (VPC) interface endpoint objects. Administrators can connect to the image builder only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibAccessEndpoints :: Lens.Lens' ImageBuilder (Lude.Maybe (Lude.NonEmpty AccessEndpoint))
ibAccessEndpoints = Lens.lens (accessEndpoints :: ImageBuilder -> Lude.Maybe (Lude.NonEmpty AccessEndpoint)) (\s a -> s {accessEndpoints = a} :: ImageBuilder)
{-# DEPRECATED ibAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The VPC configuration of the image builder.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibVPCConfig :: Lens.Lens' ImageBuilder (Lude.Maybe VPCConfig)
ibVPCConfig = Lens.lens (vpcConfig :: ImageBuilder -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: ImageBuilder)
{-# DEPRECATED ibVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The ARN of the image from which this builder was created.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibImageARN :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibImageARN = Lens.lens (imageARN :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: ImageBuilder)
{-# DEPRECATED ibImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The image builder name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDisplayName :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibDisplayName = Lens.lens (displayName :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: ImageBuilder)
{-# DEPRECATED ibDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Enables or disables default internet access for the image builder.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibEnableDefaultInternetAccess :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Bool)
ibEnableDefaultInternetAccess = Lens.lens (enableDefaultInternetAccess :: ImageBuilder -> Lude.Maybe Lude.Bool) (\s a -> s {enableDefaultInternetAccess = a} :: ImageBuilder)
{-# DEPRECATED ibEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDescription :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibDescription = Lens.lens (description :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImageBuilder)
{-# DEPRECATED ibDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The version of the AppStream 2.0 agent that is currently being used by the image builder.
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibAppstreamAgentVersion :: Lens.Lens' ImageBuilder (Lude.Maybe Lude.Text)
ibAppstreamAgentVersion = Lens.lens (appstreamAgentVersion :: ImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {appstreamAgentVersion = a} :: ImageBuilder)
{-# DEPRECATED ibAppstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead." #-}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibName :: Lens.Lens' ImageBuilder Lude.Text
ibName = Lens.lens (name :: ImageBuilder -> Lude.Text) (\s a -> s {name = a} :: ImageBuilder)
{-# DEPRECATED ibName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ImageBuilder where
  parseJSON =
    Lude.withObject
      "ImageBuilder"
      ( \x ->
          ImageBuilder'
            Lude.<$> (x Lude..:? "DomainJoinInfo")
            Lude.<*> (x Lude..:? "IamRoleArn")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "NetworkAccessConfiguration")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "ImageBuilderErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "AccessEndpoints")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "ImageArn")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "EnableDefaultInternetAccess")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "AppstreamAgentVersion")
            Lude.<*> (x Lude..: "Name")
      )
