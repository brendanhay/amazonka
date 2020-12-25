{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CloneStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a clone of a specified stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-cloning.html Clone a Stack> . By default, all parameters are set to the values used by the parent stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CloneStack
  ( -- * Creating a request
    CloneStack (..),
    mkCloneStack,

    -- ** Request lenses
    cSourceStackId,
    cServiceRoleArn,
    cAgentVersion,
    cAttributes,
    cChefConfiguration,
    cCloneAppIds,
    cClonePermissions,
    cConfigurationManager,
    cCustomCookbooksSource,
    cCustomJson,
    cDefaultAvailabilityZone,
    cDefaultInstanceProfileArn,
    cDefaultOs,
    cDefaultRootDeviceType,
    cDefaultSshKeyName,
    cDefaultSubnetId,
    cHostnameTheme,
    cName,
    cRegion,
    cUseCustomCookbooks,
    cUseOpsworksSecurityGroups,
    cVpcId,

    -- * Destructuring the response
    CloneStackResponse (..),
    mkCloneStackResponse,

    -- ** Response lenses
    crsStackId,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCloneStack' smart constructor.
data CloneStack = CloneStack'
  { -- | The source stack ID.
    sourceStackId :: Types.String,
    -- | The stack AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. If you create a stack by using the AWS OpsWorks Stacks console, it creates the role for you. You can obtain an existing stack's IAM ARN programmatically by calling 'DescribePermissions' . For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    serviceRoleArn :: Types.String,
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
    agentVersion :: Core.Maybe Types.String,
    -- | A list of stack attributes and values as key/value pairs to be added to the cloned stack.
    attributes :: Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text),
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    chefConfiguration :: Core.Maybe Types.ChefConfiguration,
    -- | A list of source stack app IDs to be included in the cloned stack.
    cloneAppIds :: Core.Maybe [Types.String],
    -- | Whether to clone the source stack's permissions.
    clonePermissions :: Core.Maybe Core.Bool,
    -- | The configuration manager. When you clone a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
    configurationManager :: Core.Maybe Types.StackConfigurationManager,
    -- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
    customCookbooksSource :: Core.Maybe Types.Source,
    -- | A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
    customJson :: Core.Maybe Types.String,
    -- | The cloned stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
    defaultAvailabilityZone :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    defaultInstanceProfileArn :: Core.Maybe Types.String,
    -- | The stack's operating system, which must be set to one of the following.
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
    --     * @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
    --
    --
    --     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information about how to use custom AMIs with OpsWorks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
    --
    --
    -- The default option is the parent stack's operating system. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
    defaultOs :: Core.Maybe Types.String,
    -- | The default root device type. This value is used by default for all instances in the cloned stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    defaultRootDeviceType :: Core.Maybe Types.RootDeviceType,
    -- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
    defaultSshKeyName :: Core.Maybe Types.String,
    -- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
    defaultSubnetId :: Core.Maybe Types.String,
    -- | The stack's host name theme, with spaces are replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
    hostnameTheme :: Core.Maybe Types.String,
    -- | The cloned stack name.
    name :: Core.Maybe Types.String,
    -- | The cloned stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    region :: Core.Maybe Types.String,
    -- | Whether to use custom cookbooks.
    useCustomCookbooks :: Core.Maybe Core.Bool,
    -- | Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
    --
    -- AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. With @UseOpsworksSecurityGroups@ you can instead provide your own custom security groups. @UseOpsworksSecurityGroups@ has the following settings:
    --
    --     * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it but you cannot delete the built-in security group.
    --
    --
    --     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate Amazon Elastic Compute Cloud (Amazon EC2) security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on creation; custom security groups are required only for those layers that need custom settings.
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    useOpsworksSecurityGroups :: Core.Maybe Core.Bool,
    -- | The ID of the VPC that the cloned stack is to be launched into. It must be in the specified region. All instances are launched into this VPC, and you cannot change the ID later.
    --
    --
    --     * If your account supports EC2 Classic, the default value is no VPC.
    --
    --
    --     * If your account does not support EC2 Classic, the default value is the default VPC for the specified region.
    --
    --
    -- If the VPC ID corresponds to a default VPC and you have specified either the @DefaultAvailabilityZone@ or the @DefaultSubnetId@ parameter only, AWS OpsWorks Stacks infers the value of the other parameter. If you specify neither parameter, AWS OpsWorks Stacks sets these parameters to the first valid Availability Zone for the specified region and the corresponding default VPC subnet ID, respectively.
    -- If you specify a nondefault VPC ID, note the following:
    --
    --     * It must belong to a VPC in your account that is in the specified region.
    --
    --
    --     * You must specify a value for @DefaultSubnetId@ .
    --
    --
    -- For more information about how to use AWS OpsWorks Stacks with a VPC, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC> . For more information about default VPC and EC2 Classic, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloneStack' value with any optional fields omitted.
mkCloneStack ::
  -- | 'sourceStackId'
  Types.String ->
  -- | 'serviceRoleArn'
  Types.String ->
  CloneStack
mkCloneStack sourceStackId serviceRoleArn =
  CloneStack'
    { sourceStackId,
      serviceRoleArn,
      agentVersion = Core.Nothing,
      attributes = Core.Nothing,
      chefConfiguration = Core.Nothing,
      cloneAppIds = Core.Nothing,
      clonePermissions = Core.Nothing,
      configurationManager = Core.Nothing,
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
      useCustomCookbooks = Core.Nothing,
      useOpsworksSecurityGroups = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The source stack ID.
--
-- /Note:/ Consider using 'sourceStackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceStackId :: Lens.Lens' CloneStack Types.String
cSourceStackId = Lens.field @"sourceStackId"
{-# DEPRECATED cSourceStackId "Use generic-lens or generic-optics with 'sourceStackId' instead." #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. If you create a stack by using the AWS OpsWorks Stacks console, it creates the role for you. You can obtain an existing stack's IAM ARN programmatically by calling 'DescribePermissions' . For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cServiceRoleArn :: Lens.Lens' CloneStack Types.String
cServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED cServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

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
cAgentVersion :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED cAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A list of stack attributes and values as key/value pairs to be added to the cloned stack.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAttributes :: Lens.Lens' CloneStack (Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text))
cAttributes = Lens.field @"attributes"
{-# DEPRECATED cAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChefConfiguration :: Lens.Lens' CloneStack (Core.Maybe Types.ChefConfiguration)
cChefConfiguration = Lens.field @"chefConfiguration"
{-# DEPRECATED cChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

-- | A list of source stack app IDs to be included in the cloned stack.
--
-- /Note:/ Consider using 'cloneAppIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCloneAppIds :: Lens.Lens' CloneStack (Core.Maybe [Types.String])
cCloneAppIds = Lens.field @"cloneAppIds"
{-# DEPRECATED cCloneAppIds "Use generic-lens or generic-optics with 'cloneAppIds' instead." #-}

-- | Whether to clone the source stack's permissions.
--
-- /Note:/ Consider using 'clonePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClonePermissions :: Lens.Lens' CloneStack (Core.Maybe Core.Bool)
cClonePermissions = Lens.field @"clonePermissions"
{-# DEPRECATED cClonePermissions "Use generic-lens or generic-optics with 'clonePermissions' instead." #-}

-- | The configuration manager. When you clone a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConfigurationManager :: Lens.Lens' CloneStack (Core.Maybe Types.StackConfigurationManager)
cConfigurationManager = Lens.field @"configurationManager"
{-# DEPRECATED cConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomCookbooksSource :: Lens.Lens' CloneStack (Core.Maybe Types.Source)
cCustomCookbooksSource = Lens.field @"customCookbooksSource"
{-# DEPRECATED cCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomJson :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cCustomJson = Lens.field @"customJson"
{-# DEPRECATED cCustomJson "Use generic-lens or generic-optics with 'customJson' instead." #-}

-- | The cloned stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultAvailabilityZone :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cDefaultAvailabilityZone = Lens.field @"defaultAvailabilityZone"
{-# DEPRECATED cDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultInstanceProfileArn :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cDefaultInstanceProfileArn = Lens.field @"defaultInstanceProfileArn"
{-# DEPRECATED cDefaultInstanceProfileArn "Use generic-lens or generic-optics with 'defaultInstanceProfileArn' instead." #-}

-- | The stack's operating system, which must be set to one of the following.
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
--     * @Microsoft Windows Server 2012 R2 Base@ , @Microsoft Windows Server 2012 R2 with SQL Server Express@ , @Microsoft Windows Server 2012 R2 with SQL Server Standard@ , or @Microsoft Windows Server 2012 R2 with SQL Server Web@ .
--
--
--     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information about how to use custom AMIs with OpsWorks, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
--
-- The default option is the parent stack's operating system. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
--
-- /Note:/ Consider using 'defaultOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultOs :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cDefaultOs = Lens.field @"defaultOs"
{-# DEPRECATED cDefaultOs "Use generic-lens or generic-optics with 'defaultOs' instead." #-}

-- | The default root device type. This value is used by default for all instances in the cloned stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultRootDeviceType :: Lens.Lens' CloneStack (Core.Maybe Types.RootDeviceType)
cDefaultRootDeviceType = Lens.field @"defaultRootDeviceType"
{-# DEPRECATED cDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

-- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
--
-- /Note:/ Consider using 'defaultSshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultSshKeyName :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cDefaultSshKeyName = Lens.field @"defaultSshKeyName"
{-# DEPRECATED cDefaultSshKeyName "Use generic-lens or generic-optics with 'defaultSshKeyName' instead." #-}

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultSubnetId :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cDefaultSubnetId = Lens.field @"defaultSubnetId"
{-# DEPRECATED cDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The stack's host name theme, with spaces are replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
cHostnameTheme :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cHostnameTheme = Lens.field @"hostnameTheme"
{-# DEPRECATED cHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

-- | The cloned stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The cloned stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegion :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cRegion = Lens.field @"region"
{-# DEPRECATED cRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Whether to use custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cUseCustomCookbooks :: Lens.Lens' CloneStack (Core.Maybe Core.Bool)
cUseCustomCookbooks = Lens.field @"useCustomCookbooks"
{-# DEPRECATED cUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. With @UseOpsworksSecurityGroups@ you can instead provide your own custom security groups. @UseOpsworksSecurityGroups@ has the following settings:
--
--     * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it but you cannot delete the built-in security group.
--
--
--     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate Amazon Elastic Compute Cloud (Amazon EC2) security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on creation; custom security groups are required only for those layers that need custom settings.
--
--
-- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cUseOpsworksSecurityGroups :: Lens.Lens' CloneStack (Core.Maybe Core.Bool)
cUseOpsworksSecurityGroups = Lens.field @"useOpsworksSecurityGroups"
{-# DEPRECATED cUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | The ID of the VPC that the cloned stack is to be launched into. It must be in the specified region. All instances are launched into this VPC, and you cannot change the ID later.
--
--
--     * If your account supports EC2 Classic, the default value is no VPC.
--
--
--     * If your account does not support EC2 Classic, the default value is the default VPC for the specified region.
--
--
-- If the VPC ID corresponds to a default VPC and you have specified either the @DefaultAvailabilityZone@ or the @DefaultSubnetId@ parameter only, AWS OpsWorks Stacks infers the value of the other parameter. If you specify neither parameter, AWS OpsWorks Stacks sets these parameters to the first valid Availability Zone for the specified region and the corresponding default VPC subnet ID, respectively.
-- If you specify a nondefault VPC ID, note the following:
--
--     * It must belong to a VPC in your account that is in the specified region.
--
--
--     * You must specify a value for @DefaultSubnetId@ .
--
--
-- For more information about how to use AWS OpsWorks Stacks with a VPC, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC> . For more information about default VPC and EC2 Classic, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcId :: Lens.Lens' CloneStack (Core.Maybe Types.String)
cVpcId = Lens.field @"vpcId"
{-# DEPRECATED cVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON CloneStack where
  toJSON CloneStack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceStackId" Core..= sourceStackId),
            Core.Just ("ServiceRoleArn" Core..= serviceRoleArn),
            ("AgentVersion" Core..=) Core.<$> agentVersion,
            ("Attributes" Core..=) Core.<$> attributes,
            ("ChefConfiguration" Core..=) Core.<$> chefConfiguration,
            ("CloneAppIds" Core..=) Core.<$> cloneAppIds,
            ("ClonePermissions" Core..=) Core.<$> clonePermissions,
            ("ConfigurationManager" Core..=) Core.<$> configurationManager,
            ("CustomCookbooksSource" Core..=) Core.<$> customCookbooksSource,
            ("CustomJson" Core..=) Core.<$> customJson,
            ("DefaultAvailabilityZone" Core..=)
              Core.<$> defaultAvailabilityZone,
            ("DefaultInstanceProfileArn" Core..=)
              Core.<$> defaultInstanceProfileArn,
            ("DefaultOs" Core..=) Core.<$> defaultOs,
            ("DefaultRootDeviceType" Core..=) Core.<$> defaultRootDeviceType,
            ("DefaultSshKeyName" Core..=) Core.<$> defaultSshKeyName,
            ("DefaultSubnetId" Core..=) Core.<$> defaultSubnetId,
            ("HostnameTheme" Core..=) Core.<$> hostnameTheme,
            ("Name" Core..=) Core.<$> name,
            ("Region" Core..=) Core.<$> region,
            ("UseCustomCookbooks" Core..=) Core.<$> useCustomCookbooks,
            ("UseOpsworksSecurityGroups" Core..=)
              Core.<$> useOpsworksSecurityGroups,
            ("VpcId" Core..=) Core.<$> vpcId
          ]
      )

instance Core.AWSRequest CloneStack where
  type Rs CloneStack = CloneStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.CloneStack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CloneStackResponse'
            Core.<$> (x Core..:? "StackId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @CloneStack@ request.
--
-- /See:/ 'mkCloneStackResponse' smart constructor.
data CloneStackResponse = CloneStackResponse'
  { -- | The cloned stack ID.
    stackId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloneStackResponse' value with any optional fields omitted.
mkCloneStackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CloneStackResponse
mkCloneStackResponse responseStatus =
  CloneStackResponse' {stackId = Core.Nothing, responseStatus}

-- | The cloned stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsStackId :: Lens.Lens' CloneStackResponse (Core.Maybe Types.String)
crsStackId = Lens.field @"stackId"
{-# DEPRECATED crsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CloneStackResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
