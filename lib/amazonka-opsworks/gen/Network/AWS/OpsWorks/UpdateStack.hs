{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateStack
    (
    -- * Creating a request
      UpdateStack (..)
    , mkUpdateStack
    -- ** Request lenses
    , usStackId
    , usAgentVersion
    , usAttributes
    , usChefConfiguration
    , usConfigurationManager
    , usCustomCookbooksSource
    , usCustomJson
    , usDefaultAvailabilityZone
    , usDefaultInstanceProfileArn
    , usDefaultOs
    , usDefaultRootDeviceType
    , usDefaultSshKeyName
    , usDefaultSubnetId
    , usHostnameTheme
    , usName
    , usServiceRoleArn
    , usUseCustomCookbooks
    , usUseOpsworksSecurityGroups

    -- * Destructuring the response
    , UpdateStackResponse (..)
    , mkUpdateStackResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { stackId :: Core.Text
    -- ^ The stack ID.
  , agentVersion :: Core.Maybe Core.Text
    -- ^ The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.
--
--
--     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances.
--
--
-- The default setting is @LATEST@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
  , attributes :: Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text)
    -- ^ One or more user-defined key-value pairs to be added to the stack attributes.
  , chefConfiguration :: Core.Maybe Types.ChefConfiguration
    -- ^ A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
  , configurationManager :: Core.Maybe Types.StackConfigurationManager
    -- ^ The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
  , customCookbooksSource :: Core.Maybe Types.Source
    -- ^ Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
  , customJson :: Core.Maybe Core.Text
    -- ^ A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
  , defaultAvailabilityZone :: Core.Maybe Core.Text
    -- ^ The stack's default Availability Zone, which must be in the stack's region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' . 
  , defaultInstanceProfileArn :: Core.Maybe Core.Text
    -- ^ The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
  , defaultOs :: Core.Maybe Core.Text
    -- ^ The stack's operating system, which must be set to one of the following:
--
--
--     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2018.03@ , @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .
--
--
--     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .
--
--
--     * @CentOS Linux 7@ 
--
--
--     * @Red Hat Enterprise Linux 7@ 
--
--
--     * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
--
--
--     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information about how to use custom AMIs with OpsWorks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
--
-- The default option is the stack's current operating system. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
  , defaultRootDeviceType :: Core.Maybe Types.RootDeviceType
    -- ^ The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
  , defaultSshKeyName :: Core.Maybe Core.Text
    -- ^ A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> . 
  , defaultSubnetId :: Core.Maybe Core.Text
    -- ^ The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description. 
  , hostnameTheme :: Core.Maybe Core.Text
    -- ^ The stack's new host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
--
--
--     * @Baked_Goods@ 
--
--
--     * @Clouds@ 
--
--
--     * @Europe_Cities@ 
--
--
--     * @Fruits@ 
--
--
--     * @Greek_Deities_and_Titans@ 
--
--
--     * @Legendary_creatures_from_Japan@ 
--
--
--     * @Planets_and_Moons@ 
--
--
--     * @Roman_Deities@ 
--
--
--     * @Scottish_Islands@ 
--
--
--     * @US_Cities@ 
--
--
--     * @Wild_Cats@ 
--
--
-- To obtain a generated host name, call @GetHostNameSuggestion@ , which returns a host name based on the current theme.
  , name :: Core.Maybe Core.Text
    -- ^ The stack's new name.
  , serviceRoleArn :: Core.Maybe Core.Text
    -- ^ Do not use this parameter. You cannot update a stack's service role.
  , useCustomCookbooks :: Core.Maybe Core.Bool
    -- ^ Whether the stack uses custom cookbooks.
  , useOpsworksSecurityGroups :: Core.Maybe Core.Bool
    -- ^ Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. @UseOpsworksSecurityGroups@ allows you to provide your own custom security groups instead of using the built-in groups. @UseOpsworksSecurityGroups@ has the following settings: 
--
--     * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it, but you cannot delete the built-in security group.
--
--
--     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate EC2 security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on. Custom security groups are required only for those layers that need custom settings.
--
--
-- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStack' value with any optional fields omitted.
mkUpdateStack
    :: Core.Text -- ^ 'stackId'
    -> UpdateStack
mkUpdateStack stackId
  = UpdateStack'{stackId, agentVersion = Core.Nothing,
                 attributes = Core.Nothing, chefConfiguration = Core.Nothing,
                 configurationManager = Core.Nothing,
                 customCookbooksSource = Core.Nothing, customJson = Core.Nothing,
                 defaultAvailabilityZone = Core.Nothing,
                 defaultInstanceProfileArn = Core.Nothing, defaultOs = Core.Nothing,
                 defaultRootDeviceType = Core.Nothing,
                 defaultSshKeyName = Core.Nothing, defaultSubnetId = Core.Nothing,
                 hostnameTheme = Core.Nothing, name = Core.Nothing,
                 serviceRoleArn = Core.Nothing, useCustomCookbooks = Core.Nothing,
                 useOpsworksSecurityGroups = Core.Nothing}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackId :: Lens.Lens' UpdateStack Core.Text
usStackId = Lens.field @"stackId"
{-# INLINEABLE usStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.
--
--
--     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances.
--
--
-- The default setting is @LATEST@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAgentVersion :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usAgentVersion = Lens.field @"agentVersion"
{-# INLINEABLE usAgentVersion #-}
{-# DEPRECATED agentVersion "Use generic-lens or generic-optics with 'agentVersion' instead"  #-}

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAttributes :: Lens.Lens' UpdateStack (Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text))
usAttributes = Lens.field @"attributes"
{-# INLINEABLE usAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usChefConfiguration :: Lens.Lens' UpdateStack (Core.Maybe Types.ChefConfiguration)
usChefConfiguration = Lens.field @"chefConfiguration"
{-# INLINEABLE usChefConfiguration #-}
{-# DEPRECATED chefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead"  #-}

-- | The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usConfigurationManager :: Lens.Lens' UpdateStack (Core.Maybe Types.StackConfigurationManager)
usConfigurationManager = Lens.field @"configurationManager"
{-# INLINEABLE usConfigurationManager #-}
{-# DEPRECATED configurationManager "Use generic-lens or generic-optics with 'configurationManager' instead"  #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCustomCookbooksSource :: Lens.Lens' UpdateStack (Core.Maybe Types.Source)
usCustomCookbooksSource = Lens.field @"customCookbooksSource"
{-# INLINEABLE usCustomCookbooksSource #-}
{-# DEPRECATED customCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead"  #-}

-- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@ 
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCustomJson :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usCustomJson = Lens.field @"customJson"
{-# INLINEABLE usCustomJson #-}
{-# DEPRECATED customJson "Use generic-lens or generic-optics with 'customJson' instead"  #-}

-- | The stack's default Availability Zone, which must be in the stack's region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' . 
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultAvailabilityZone :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usDefaultAvailabilityZone = Lens.field @"defaultAvailabilityZone"
{-# INLINEABLE usDefaultAvailabilityZone #-}
{-# DEPRECATED defaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead"  #-}

-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultInstanceProfileArn :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usDefaultInstanceProfileArn = Lens.field @"defaultInstanceProfileArn"
{-# INLINEABLE usDefaultInstanceProfileArn #-}
{-# DEPRECATED defaultInstanceProfileArn "Use generic-lens or generic-optics with 'defaultInstanceProfileArn' instead"  #-}

-- | The stack's operating system, which must be set to one of the following:
--
--
--     * A supported Linux operating system: An Amazon Linux version, such as @Amazon Linux 2018.03@ , @Amazon Linux 2017.09@ , @Amazon Linux 2017.03@ , @Amazon Linux 2016.09@ , @Amazon Linux 2016.03@ , @Amazon Linux 2015.09@ , or @Amazon Linux 2015.03@ .
--
--
--     * A supported Ubuntu operating system, such as @Ubuntu 16.04 LTS@ , @Ubuntu 14.04 LTS@ , or @Ubuntu 12.04 LTS@ .
--
--
--     * @CentOS Linux 7@ 
--
--
--     * @Red Hat Enterprise Linux 7@ 
--
--
--     * A supported Windows operating system, such as @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
--
--
--     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information about how to use custom AMIs with OpsWorks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
--
-- The default option is the stack's current operating system. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
--
-- /Note:/ Consider using 'defaultOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultOs :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usDefaultOs = Lens.field @"defaultOs"
{-# INLINEABLE usDefaultOs #-}
{-# DEPRECATED defaultOs "Use generic-lens or generic-optics with 'defaultOs' instead"  #-}

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultRootDeviceType :: Lens.Lens' UpdateStack (Core.Maybe Types.RootDeviceType)
usDefaultRootDeviceType = Lens.field @"defaultRootDeviceType"
{-# INLINEABLE usDefaultRootDeviceType #-}
{-# DEPRECATED defaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead"  #-}

-- | A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> . 
--
-- /Note:/ Consider using 'defaultSshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultSshKeyName :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usDefaultSshKeyName = Lens.field @"defaultSshKeyName"
{-# INLINEABLE usDefaultSshKeyName #-}
{-# DEPRECATED defaultSshKeyName "Use generic-lens or generic-optics with 'defaultSshKeyName' instead"  #-}

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description. 
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultSubnetId :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usDefaultSubnetId = Lens.field @"defaultSubnetId"
{-# INLINEABLE usDefaultSubnetId #-}
{-# DEPRECATED defaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead"  #-}

-- | The stack's new host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
--
--
--     * @Baked_Goods@ 
--
--
--     * @Clouds@ 
--
--
--     * @Europe_Cities@ 
--
--
--     * @Fruits@ 
--
--
--     * @Greek_Deities_and_Titans@ 
--
--
--     * @Legendary_creatures_from_Japan@ 
--
--
--     * @Planets_and_Moons@ 
--
--
--     * @Roman_Deities@ 
--
--
--     * @Scottish_Islands@ 
--
--
--     * @US_Cities@ 
--
--
--     * @Wild_Cats@ 
--
--
-- To obtain a generated host name, call @GetHostNameSuggestion@ , which returns a host name based on the current theme.
--
-- /Note:/ Consider using 'hostnameTheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usHostnameTheme :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usHostnameTheme = Lens.field @"hostnameTheme"
{-# INLINEABLE usHostnameTheme #-}
{-# DEPRECATED hostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead"  #-}

-- | The stack's new name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usName = Lens.field @"name"
{-# INLINEABLE usName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Do not use this parameter. You cannot update a stack's service role.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usServiceRoleArn :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
usServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE usServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUseCustomCookbooks :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
usUseCustomCookbooks = Lens.field @"useCustomCookbooks"
{-# INLINEABLE usUseCustomCookbooks #-}
{-# DEPRECATED useCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead"  #-}

-- | Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. @UseOpsworksSecurityGroups@ allows you to provide your own custom security groups instead of using the built-in groups. @UseOpsworksSecurityGroups@ has the following settings: 
--
--     * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it, but you cannot delete the built-in security group.
--
--
--     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate EC2 security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on. Custom security groups are required only for those layers that need custom settings.
--
--
-- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUseOpsworksSecurityGroups :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
usUseOpsworksSecurityGroups = Lens.field @"useOpsworksSecurityGroups"
{-# INLINEABLE usUseOpsworksSecurityGroups #-}
{-# DEPRECATED useOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead"  #-}

instance Core.ToQuery UpdateStack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateStack where
        toHeaders UpdateStack{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateStack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateStack where
        toJSON UpdateStack{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackId" Core..= stackId),
                  ("AgentVersion" Core..=) Core.<$> agentVersion,
                  ("Attributes" Core..=) Core.<$> attributes,
                  ("ChefConfiguration" Core..=) Core.<$> chefConfiguration,
                  ("ConfigurationManager" Core..=) Core.<$> configurationManager,
                  ("CustomCookbooksSource" Core..=) Core.<$> customCookbooksSource,
                  ("CustomJson" Core..=) Core.<$> customJson,
                  ("DefaultAvailabilityZone" Core..=) Core.<$>
                    defaultAvailabilityZone,
                  ("DefaultInstanceProfileArn" Core..=) Core.<$>
                    defaultInstanceProfileArn,
                  ("DefaultOs" Core..=) Core.<$> defaultOs,
                  ("DefaultRootDeviceType" Core..=) Core.<$> defaultRootDeviceType,
                  ("DefaultSshKeyName" Core..=) Core.<$> defaultSshKeyName,
                  ("DefaultSubnetId" Core..=) Core.<$> defaultSubnetId,
                  ("HostnameTheme" Core..=) Core.<$> hostnameTheme,
                  ("Name" Core..=) Core.<$> name,
                  ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
                  ("UseCustomCookbooks" Core..=) Core.<$> useCustomCookbooks,
                  ("UseOpsworksSecurityGroups" Core..=) Core.<$>
                    useOpsworksSecurityGroups])

instance Core.AWSRequest UpdateStack where
        type Rs UpdateStack = UpdateStackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateStackResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStackResponse' value with any optional fields omitted.
mkUpdateStackResponse
    :: UpdateStackResponse
mkUpdateStackResponse = UpdateStackResponse'
