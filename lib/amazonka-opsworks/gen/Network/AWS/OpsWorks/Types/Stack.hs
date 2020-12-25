{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Stack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Stack
  ( Stack (..),

    -- * Smart constructor
    mkStack,

    -- * Lenses
    sAgentVersion,
    sArn,
    sAttributes,
    sChefConfiguration,
    sConfigurationManager,
    sCreatedAt,
    sCustomCookbooksSource,
    sCustomJson,
    sDefaultAvailabilityZone,
    sDefaultInstanceProfileArn,
    sDefaultOs,
    sDefaultRootDeviceType,
    sDefaultSshKeyName,
    sDefaultSubnetId,
    sHostnameTheme,
    sName,
    sRegion,
    sServiceRoleArn,
    sStackId,
    sUseCustomCookbooks,
    sUseOpsworksSecurityGroups,
    sVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.ChefConfiguration as Types
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.OpsWorks.Types.RootDeviceType as Types
import qualified Network.AWS.OpsWorks.Types.Source as Types
import qualified Network.AWS.OpsWorks.Types.StackAttributesKeys as Types
import qualified Network.AWS.OpsWorks.Types.StackConfigurationManager as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a stack.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { -- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
    agentVersion :: Core.Maybe Types.String,
    -- | The stack's ARN.
    arn :: Core.Maybe Types.String,
    -- | The stack's attributes.
    attributes :: Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text),
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    chefConfiguration :: Core.Maybe Types.ChefConfiguration,
    -- | The configuration manager.
    configurationManager :: Core.Maybe Types.StackConfigurationManager,
    -- | The date when the stack was created.
    createdAt :: Core.Maybe Types.CreatedAt,
    -- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
    customCookbooksSource :: Core.Maybe Types.Source,
    -- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
    customJson :: Core.Maybe Types.String,
    -- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    defaultAvailabilityZone :: Core.Maybe Types.String,
    -- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    defaultInstanceProfileArn :: Core.Maybe Types.String,
    -- | The stack's default operating system.
    defaultOs :: Core.Maybe Types.String,
    -- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    defaultRootDeviceType :: Core.Maybe Types.RootDeviceType,
    -- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
    defaultSshKeyName :: Core.Maybe Types.String,
    -- | The default subnet ID; applicable only if the stack is running in a VPC.
    defaultSubnetId :: Core.Maybe Types.String,
    -- | The stack host name theme, with spaces replaced by underscores.
    hostnameTheme :: Core.Maybe Types.String,
    -- | The stack name.
    name :: Core.Maybe Types.String,
    -- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    region :: Core.Maybe Types.String,
    -- | The stack AWS Identity and Access Management (IAM) role.
    serviceRoleArn :: Core.Maybe Types.String,
    -- | The stack ID.
    stackId :: Core.Maybe Types.String,
    -- | Whether the stack uses custom cookbooks.
    useCustomCookbooks :: Core.Maybe Core.Bool,
    -- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
    useOpsworksSecurityGroups :: Core.Maybe Core.Bool,
    -- | The VPC ID; applicable only if the stack is running in a VPC.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Stack' value with any optional fields omitted.
mkStack ::
  Stack
mkStack =
  Stack'
    { agentVersion = Core.Nothing,
      arn = Core.Nothing,
      attributes = Core.Nothing,
      chefConfiguration = Core.Nothing,
      configurationManager = Core.Nothing,
      createdAt = Core.Nothing,
      customCookbooksSource = Core.Nothing,
      customJson = Core.Nothing,
      defaultAvailabilityZone = Core.Nothing,
      defaultInstanceProfileArn = Core.Nothing,
      defaultOs = Core.Nothing,
      defaultRootDeviceType = Core.Nothing,
      defaultSshKeyName = Core.Nothing,
      defaultSubnetId = Core.Nothing,
      hostnameTheme = Core.Nothing,
      name = Core.Nothing,
      region = Core.Nothing,
      serviceRoleArn = Core.Nothing,
      stackId = Core.Nothing,
      useCustomCookbooks = Core.Nothing,
      useOpsworksSecurityGroups = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAgentVersion :: Lens.Lens' Stack (Core.Maybe Types.String)
sAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED sAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The stack's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sArn :: Lens.Lens' Stack (Core.Maybe Types.String)
sArn = Lens.field @"arn"
{-# DEPRECATED sArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The stack's attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAttributes :: Lens.Lens' Stack (Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text))
sAttributes = Lens.field @"attributes"
{-# DEPRECATED sAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sChefConfiguration :: Lens.Lens' Stack (Core.Maybe Types.ChefConfiguration)
sChefConfiguration = Lens.field @"chefConfiguration"
{-# DEPRECATED sChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfigurationManager :: Lens.Lens' Stack (Core.Maybe Types.StackConfigurationManager)
sConfigurationManager = Lens.field @"configurationManager"
{-# DEPRECATED sConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | The date when the stack was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedAt :: Lens.Lens' Stack (Core.Maybe Types.CreatedAt)
sCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED sCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomCookbooksSource :: Lens.Lens' Stack (Core.Maybe Types.Source)
sCustomCookbooksSource = Lens.field @"customCookbooksSource"
{-# DEPRECATED sCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomJson :: Lens.Lens' Stack (Core.Maybe Types.String)
sCustomJson = Lens.field @"customJson"
{-# DEPRECATED sCustomJson "Use generic-lens or generic-optics with 'customJson' instead." #-}

-- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultAvailabilityZone :: Lens.Lens' Stack (Core.Maybe Types.String)
sDefaultAvailabilityZone = Lens.field @"defaultAvailabilityZone"
{-# DEPRECATED sDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultInstanceProfileArn :: Lens.Lens' Stack (Core.Maybe Types.String)
sDefaultInstanceProfileArn = Lens.field @"defaultInstanceProfileArn"
{-# DEPRECATED sDefaultInstanceProfileArn "Use generic-lens or generic-optics with 'defaultInstanceProfileArn' instead." #-}

-- | The stack's default operating system.
--
-- /Note:/ Consider using 'defaultOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultOs :: Lens.Lens' Stack (Core.Maybe Types.String)
sDefaultOs = Lens.field @"defaultOs"
{-# DEPRECATED sDefaultOs "Use generic-lens or generic-optics with 'defaultOs' instead." #-}

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultRootDeviceType :: Lens.Lens' Stack (Core.Maybe Types.RootDeviceType)
sDefaultRootDeviceType = Lens.field @"defaultRootDeviceType"
{-# DEPRECATED sDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

-- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
--
-- /Note:/ Consider using 'defaultSshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultSshKeyName :: Lens.Lens' Stack (Core.Maybe Types.String)
sDefaultSshKeyName = Lens.field @"defaultSshKeyName"
{-# DEPRECATED sDefaultSshKeyName "Use generic-lens or generic-optics with 'defaultSshKeyName' instead." #-}

-- | The default subnet ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultSubnetId :: Lens.Lens' Stack (Core.Maybe Types.String)
sDefaultSubnetId = Lens.field @"defaultSubnetId"
{-# DEPRECATED sDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The stack host name theme, with spaces replaced by underscores.
--
-- /Note:/ Consider using 'hostnameTheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHostnameTheme :: Lens.Lens' Stack (Core.Maybe Types.String)
sHostnameTheme = Lens.field @"hostnameTheme"
{-# DEPRECATED sHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Stack (Core.Maybe Types.String)
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRegion :: Lens.Lens' Stack (Core.Maybe Types.String)
sRegion = Lens.field @"region"
{-# DEPRECATED sRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The stack AWS Identity and Access Management (IAM) role.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServiceRoleArn :: Lens.Lens' Stack (Core.Maybe Types.String)
sServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED sServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackId :: Lens.Lens' Stack (Core.Maybe Types.String)
sStackId = Lens.field @"stackId"
{-# DEPRECATED sStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUseCustomCookbooks :: Lens.Lens' Stack (Core.Maybe Core.Bool)
sUseCustomCookbooks = Lens.field @"useCustomCookbooks"
{-# DEPRECATED sUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUseOpsworksSecurityGroups :: Lens.Lens' Stack (Core.Maybe Core.Bool)
sUseOpsworksSecurityGroups = Lens.field @"useOpsworksSecurityGroups"
{-# DEPRECATED sUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | The VPC ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVpcId :: Lens.Lens' Stack (Core.Maybe Types.String)
sVpcId = Lens.field @"vpcId"
{-# DEPRECATED sVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON Stack where
  parseJSON =
    Core.withObject "Stack" Core.$
      \x ->
        Stack'
          Core.<$> (x Core..:? "AgentVersion")
          Core.<*> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Attributes")
          Core.<*> (x Core..:? "ChefConfiguration")
          Core.<*> (x Core..:? "ConfigurationManager")
          Core.<*> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "CustomCookbooksSource")
          Core.<*> (x Core..:? "CustomJson")
          Core.<*> (x Core..:? "DefaultAvailabilityZone")
          Core.<*> (x Core..:? "DefaultInstanceProfileArn")
          Core.<*> (x Core..:? "DefaultOs")
          Core.<*> (x Core..:? "DefaultRootDeviceType")
          Core.<*> (x Core..:? "DefaultSshKeyName")
          Core.<*> (x Core..:? "DefaultSubnetId")
          Core.<*> (x Core..:? "HostnameTheme")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "ServiceRoleArn")
          Core.<*> (x Core..:? "StackId")
          Core.<*> (x Core..:? "UseCustomCookbooks")
          Core.<*> (x Core..:? "UseOpsworksSecurityGroups")
          Core.<*> (x Core..:? "VpcId")
