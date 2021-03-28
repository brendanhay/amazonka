{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Stack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Stack
  ( Stack (..)
  -- * Smart constructor
  , mkStack
  -- * Lenses
  , sAgentVersion
  , sArn
  , sAttributes
  , sChefConfiguration
  , sConfigurationManager
  , sCreatedAt
  , sCustomCookbooksSource
  , sCustomJson
  , sDefaultAvailabilityZone
  , sDefaultInstanceProfileArn
  , sDefaultOs
  , sDefaultRootDeviceType
  , sDefaultSshKeyName
  , sDefaultSubnetId
  , sHostnameTheme
  , sName
  , sRegion
  , sServiceRoleArn
  , sStackId
  , sUseCustomCookbooks
  , sUseOpsworksSecurityGroups
  , sVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.ChefConfiguration as Types
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.OpsWorks.Types.RootDeviceType as Types
import qualified Network.AWS.OpsWorks.Types.Source as Types
import qualified Network.AWS.OpsWorks.Types.StackAttributesKeys as Types
import qualified Network.AWS.OpsWorks.Types.StackConfigurationManager as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a stack.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { agentVersion :: Core.Maybe Core.Text
    -- ^ The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
  , arn :: Core.Maybe Core.Text
    -- ^ The stack's ARN.
  , attributes :: Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text)
    -- ^ The stack's attributes.
  , chefConfiguration :: Core.Maybe Types.ChefConfiguration
    -- ^ A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
  , configurationManager :: Core.Maybe Types.StackConfigurationManager
    -- ^ The configuration manager.
  , createdAt :: Core.Maybe Types.CreatedAt
    -- ^ The date when the stack was created.
  , customCookbooksSource :: Core.Maybe Types.Source
    -- ^ Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
  , customJson :: Core.Maybe Core.Text
    -- ^ A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
  , defaultAvailabilityZone :: Core.Maybe Core.Text
    -- ^ The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
  , defaultInstanceProfileArn :: Core.Maybe Core.Text
    -- ^ The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
  , defaultOs :: Core.Maybe Core.Text
    -- ^ The stack's default operating system.
  , defaultRootDeviceType :: Core.Maybe Types.RootDeviceType
    -- ^ The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
  , defaultSshKeyName :: Core.Maybe Core.Text
    -- ^ A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
  , defaultSubnetId :: Core.Maybe Core.Text
    -- ^ The default subnet ID; applicable only if the stack is running in a VPC.
  , hostnameTheme :: Core.Maybe Core.Text
    -- ^ The stack host name theme, with spaces replaced by underscores.
  , name :: Core.Maybe Core.Text
    -- ^ The stack name.
  , region :: Core.Maybe Core.Text
    -- ^ The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
  , serviceRoleArn :: Core.Maybe Core.Text
    -- ^ The stack AWS Identity and Access Management (IAM) role.
  , stackId :: Core.Maybe Core.Text
    -- ^ The stack ID.
  , useCustomCookbooks :: Core.Maybe Core.Bool
    -- ^ Whether the stack uses custom cookbooks.
  , useOpsworksSecurityGroups :: Core.Maybe Core.Bool
    -- ^ Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The VPC ID; applicable only if the stack is running in a VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Stack' value with any optional fields omitted.
mkStack
    :: Stack
mkStack
  = Stack'{agentVersion = Core.Nothing, arn = Core.Nothing,
           attributes = Core.Nothing, chefConfiguration = Core.Nothing,
           configurationManager = Core.Nothing, createdAt = Core.Nothing,
           customCookbooksSource = Core.Nothing, customJson = Core.Nothing,
           defaultAvailabilityZone = Core.Nothing,
           defaultInstanceProfileArn = Core.Nothing, defaultOs = Core.Nothing,
           defaultRootDeviceType = Core.Nothing,
           defaultSshKeyName = Core.Nothing, defaultSubnetId = Core.Nothing,
           hostnameTheme = Core.Nothing, name = Core.Nothing,
           region = Core.Nothing, serviceRoleArn = Core.Nothing,
           stackId = Core.Nothing, useCustomCookbooks = Core.Nothing,
           useOpsworksSecurityGroups = Core.Nothing, vpcId = Core.Nothing}

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAgentVersion :: Lens.Lens' Stack (Core.Maybe Core.Text)
sAgentVersion = Lens.field @"agentVersion"
{-# INLINEABLE sAgentVersion #-}
{-# DEPRECATED agentVersion "Use generic-lens or generic-optics with 'agentVersion' instead"  #-}

-- | The stack's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sArn :: Lens.Lens' Stack (Core.Maybe Core.Text)
sArn = Lens.field @"arn"
{-# INLINEABLE sArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The stack's attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAttributes :: Lens.Lens' Stack (Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text))
sAttributes = Lens.field @"attributes"
{-# INLINEABLE sAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sChefConfiguration :: Lens.Lens' Stack (Core.Maybe Types.ChefConfiguration)
sChefConfiguration = Lens.field @"chefConfiguration"
{-# INLINEABLE sChefConfiguration #-}
{-# DEPRECATED chefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead"  #-}

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfigurationManager :: Lens.Lens' Stack (Core.Maybe Types.StackConfigurationManager)
sConfigurationManager = Lens.field @"configurationManager"
{-# INLINEABLE sConfigurationManager #-}
{-# DEPRECATED configurationManager "Use generic-lens or generic-optics with 'configurationManager' instead"  #-}

-- | The date when the stack was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedAt :: Lens.Lens' Stack (Core.Maybe Types.CreatedAt)
sCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE sCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomCookbooksSource :: Lens.Lens' Stack (Core.Maybe Types.Source)
sCustomCookbooksSource = Lens.field @"customCookbooksSource"
{-# INLINEABLE sCustomCookbooksSource #-}
{-# DEPRECATED customCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead"  #-}

-- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomJson :: Lens.Lens' Stack (Core.Maybe Core.Text)
sCustomJson = Lens.field @"customJson"
{-# INLINEABLE sCustomJson #-}
{-# DEPRECATED customJson "Use generic-lens or generic-optics with 'customJson' instead"  #-}

-- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultAvailabilityZone :: Lens.Lens' Stack (Core.Maybe Core.Text)
sDefaultAvailabilityZone = Lens.field @"defaultAvailabilityZone"
{-# INLINEABLE sDefaultAvailabilityZone #-}
{-# DEPRECATED defaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead"  #-}

-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultInstanceProfileArn :: Lens.Lens' Stack (Core.Maybe Core.Text)
sDefaultInstanceProfileArn = Lens.field @"defaultInstanceProfileArn"
{-# INLINEABLE sDefaultInstanceProfileArn #-}
{-# DEPRECATED defaultInstanceProfileArn "Use generic-lens or generic-optics with 'defaultInstanceProfileArn' instead"  #-}

-- | The stack's default operating system.
--
-- /Note:/ Consider using 'defaultOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultOs :: Lens.Lens' Stack (Core.Maybe Core.Text)
sDefaultOs = Lens.field @"defaultOs"
{-# INLINEABLE sDefaultOs #-}
{-# DEPRECATED defaultOs "Use generic-lens or generic-optics with 'defaultOs' instead"  #-}

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultRootDeviceType :: Lens.Lens' Stack (Core.Maybe Types.RootDeviceType)
sDefaultRootDeviceType = Lens.field @"defaultRootDeviceType"
{-# INLINEABLE sDefaultRootDeviceType #-}
{-# DEPRECATED defaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead"  #-}

-- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
--
-- /Note:/ Consider using 'defaultSshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultSshKeyName :: Lens.Lens' Stack (Core.Maybe Core.Text)
sDefaultSshKeyName = Lens.field @"defaultSshKeyName"
{-# INLINEABLE sDefaultSshKeyName #-}
{-# DEPRECATED defaultSshKeyName "Use generic-lens or generic-optics with 'defaultSshKeyName' instead"  #-}

-- | The default subnet ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultSubnetId :: Lens.Lens' Stack (Core.Maybe Core.Text)
sDefaultSubnetId = Lens.field @"defaultSubnetId"
{-# INLINEABLE sDefaultSubnetId #-}
{-# DEPRECATED defaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead"  #-}

-- | The stack host name theme, with spaces replaced by underscores.
--
-- /Note:/ Consider using 'hostnameTheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHostnameTheme :: Lens.Lens' Stack (Core.Maybe Core.Text)
sHostnameTheme = Lens.field @"hostnameTheme"
{-# INLINEABLE sHostnameTheme #-}
{-# DEPRECATED hostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead"  #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Stack (Core.Maybe Core.Text)
sName = Lens.field @"name"
{-# INLINEABLE sName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRegion :: Lens.Lens' Stack (Core.Maybe Core.Text)
sRegion = Lens.field @"region"
{-# INLINEABLE sRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The stack AWS Identity and Access Management (IAM) role.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServiceRoleArn :: Lens.Lens' Stack (Core.Maybe Core.Text)
sServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE sServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackId :: Lens.Lens' Stack (Core.Maybe Core.Text)
sStackId = Lens.field @"stackId"
{-# INLINEABLE sStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUseCustomCookbooks :: Lens.Lens' Stack (Core.Maybe Core.Bool)
sUseCustomCookbooks = Lens.field @"useCustomCookbooks"
{-# INLINEABLE sUseCustomCookbooks #-}
{-# DEPRECATED useCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead"  #-}

-- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUseOpsworksSecurityGroups :: Lens.Lens' Stack (Core.Maybe Core.Bool)
sUseOpsworksSecurityGroups = Lens.field @"useOpsworksSecurityGroups"
{-# INLINEABLE sUseOpsworksSecurityGroups #-}
{-# DEPRECATED useOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead"  #-}

-- | The VPC ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVpcId :: Lens.Lens' Stack (Core.Maybe Core.Text)
sVpcId = Lens.field @"vpcId"
{-# INLINEABLE sVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON Stack where
        parseJSON
          = Core.withObject "Stack" Core.$
              \ x ->
                Stack' Core.<$>
                  (x Core..:? "AgentVersion") Core.<*> x Core..:? "Arn" Core.<*>
                    x Core..:? "Attributes"
                    Core.<*> x Core..:? "ChefConfiguration"
                    Core.<*> x Core..:? "ConfigurationManager"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "CustomCookbooksSource"
                    Core.<*> x Core..:? "CustomJson"
                    Core.<*> x Core..:? "DefaultAvailabilityZone"
                    Core.<*> x Core..:? "DefaultInstanceProfileArn"
                    Core.<*> x Core..:? "DefaultOs"
                    Core.<*> x Core..:? "DefaultRootDeviceType"
                    Core.<*> x Core..:? "DefaultSshKeyName"
                    Core.<*> x Core..:? "DefaultSubnetId"
                    Core.<*> x Core..:? "HostnameTheme"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Region"
                    Core.<*> x Core..:? "ServiceRoleArn"
                    Core.<*> x Core..:? "StackId"
                    Core.<*> x Core..:? "UseCustomCookbooks"
                    Core.<*> x Core..:? "UseOpsworksSecurityGroups"
                    Core.<*> x Core..:? "VpcId"
