{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-edit.html Create a New Stack> .
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateStack
  ( -- * Creating a request
    CreateStack (..),
    mkCreateStack,

    -- ** Request lenses
    csName,
    csRegion,
    csServiceRoleArn,
    csDefaultInstanceProfileArn,
    csAgentVersion,
    csAttributes,
    csChefConfiguration,
    csConfigurationManager,
    csCustomCookbooksSource,
    csCustomJson,
    csDefaultAvailabilityZone,
    csDefaultOs,
    csDefaultRootDeviceType,
    csDefaultSshKeyName,
    csDefaultSubnetId,
    csHostnameTheme,
    csUseCustomCookbooks,
    csUseOpsworksSecurityGroups,
    csVpcId,

    -- * Destructuring the response
    CreateStackResponse (..),
    mkCreateStackResponse,

    -- ** Response lenses
    csrrsStackId,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | The stack name.
    name :: Types.String,
    -- | The stack's AWS region, such as @ap-south-1@ . For more information about Amazon regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    region :: Types.String,
    -- | The stack's AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    serviceRoleArn :: Types.String,
    -- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    defaultInstanceProfileArn :: Types.String,
    -- | The default AWS OpsWorks Stacks agent version. You have the following options:
    --
    --
    --     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.
    --
    --
    --     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances.
    --
    --
    -- The default setting is the most recent release of the agent. To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
    agentVersion :: Core.Maybe Types.String,
    -- | One or more user-defined key-value pairs to be added to the stack attributes.
    attributes :: Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text),
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    chefConfiguration :: Core.Maybe Types.ChefConfiguration,
    -- | The configuration manager. When you create a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
    configurationManager :: Core.Maybe Types.StackConfigurationManager,
    -- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
    customCookbooksSource :: Core.Maybe Types.Source,
    -- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
    customJson :: Core.Maybe Types.String,
    -- | The stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
    defaultAvailabilityZone :: Core.Maybe Types.String,
    -- | The stack's default operating system, which is installed on every instance unless you specify a different operating system when you create the instance. You can specify one of the following.
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
    --     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
    --
    --
    -- The default option is the current Amazon Linux version. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
    defaultOs :: Core.Maybe Types.String,
    -- | The default root device type. This value is the default for all instances in the stack, but you can override it when you create an instance. The default option is @instance-store@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    defaultRootDeviceType :: Core.Maybe Types.RootDeviceType,
    -- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
    defaultSshKeyName :: Core.Maybe Types.String,
    -- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
    defaultSubnetId :: Core.Maybe Types.String,
    -- | The stack's host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
    -- | Whether the stack uses custom cookbooks.
    useCustomCookbooks :: Core.Maybe Core.Bool,
    -- | Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
    --
    -- AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. With @UseOpsworksSecurityGroups@ you can instead provide your own custom security groups. @UseOpsworksSecurityGroups@ has the following settings:
    --
    --     * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it, but you cannot delete the built-in security group.
    --
    --
    --     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate EC2 security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on creation; custom security groups are required only for those layers that need custom settings.
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    useOpsworksSecurityGroups :: Core.Maybe Core.Bool,
    -- | The ID of the VPC that the stack is to be launched into. The VPC must be in the stack's region. All instances are launched into this VPC. You cannot change the ID later.
    --
    --
    --     * If your account supports EC2-Classic, the default value is @no VPC@ .
    --
    --
    --     * If your account does not support EC2-Classic, the default value is the default VPC for the specified region.
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
    -- For more information about how to use AWS OpsWorks Stacks with a VPC, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC> . For more information about default VPC and EC2-Classic, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStack' value with any optional fields omitted.
mkCreateStack ::
  -- | 'name'
  Types.String ->
  -- | 'region'
  Types.String ->
  -- | 'serviceRoleArn'
  Types.String ->
  -- | 'defaultInstanceProfileArn'
  Types.String ->
  CreateStack
mkCreateStack name region serviceRoleArn defaultInstanceProfileArn =
  CreateStack'
    { name,
      region,
      serviceRoleArn,
      defaultInstanceProfileArn,
      agentVersion = Core.Nothing,
      attributes = Core.Nothing,
      chefConfiguration = Core.Nothing,
      configurationManager = Core.Nothing,
      customCookbooksSource = Core.Nothing,
      customJson = Core.Nothing,
      defaultAvailabilityZone = Core.Nothing,
      defaultOs = Core.Nothing,
      defaultRootDeviceType = Core.Nothing,
      defaultSshKeyName = Core.Nothing,
      defaultSubnetId = Core.Nothing,
      hostnameTheme = Core.Nothing,
      useCustomCookbooks = Core.Nothing,
      useOpsworksSecurityGroups = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateStack Types.String
csName = Lens.field @"name"
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack's AWS region, such as @ap-south-1@ . For more information about Amazon regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRegion :: Lens.Lens' CreateStack Types.String
csRegion = Lens.field @"region"
{-# DEPRECATED csRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The stack's AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceRoleArn :: Lens.Lens' CreateStack Types.String
csServiceRoleArn = Lens.field @"serviceRoleArn"
{-# DEPRECATED csServiceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultInstanceProfileArn :: Lens.Lens' CreateStack Types.String
csDefaultInstanceProfileArn = Lens.field @"defaultInstanceProfileArn"
{-# DEPRECATED csDefaultInstanceProfileArn "Use generic-lens or generic-optics with 'defaultInstanceProfileArn' instead." #-}

-- | The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.
--
--
--     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances.
--
--
-- The default setting is the most recent release of the agent. To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAgentVersion :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csAgentVersion = Lens.field @"agentVersion"
{-# DEPRECATED csAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAttributes :: Lens.Lens' CreateStack (Core.Maybe (Core.HashMap Types.StackAttributesKeys Types.Maybe Text))
csAttributes = Lens.field @"attributes"
{-# DEPRECATED csAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChefConfiguration :: Lens.Lens' CreateStack (Core.Maybe Types.ChefConfiguration)
csChefConfiguration = Lens.field @"chefConfiguration"
{-# DEPRECATED csChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

-- | The configuration manager. When you create a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csConfigurationManager :: Lens.Lens' CreateStack (Core.Maybe Types.StackConfigurationManager)
csConfigurationManager = Lens.field @"configurationManager"
{-# DEPRECATED csConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomCookbooksSource :: Lens.Lens' CreateStack (Core.Maybe Types.Source)
csCustomCookbooksSource = Lens.field @"customCookbooksSource"
{-# DEPRECATED csCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomJson :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csCustomJson = Lens.field @"customJson"
{-# DEPRECATED csCustomJson "Use generic-lens or generic-optics with 'customJson' instead." #-}

-- | The stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultAvailabilityZone :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csDefaultAvailabilityZone = Lens.field @"defaultAvailabilityZone"
{-# DEPRECATED csDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | The stack's default operating system, which is installed on every instance unless you specify a different operating system when you create the instance. You can specify one of the following.
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
--     * A custom AMI: @Custom@ . You specify the custom AMI you want to use when you create instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Using Custom AMIs> .
--
--
-- The default option is the current Amazon Linux version. For more information about supported operating systems, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-os.html AWS OpsWorks Stacks Operating Systems> .
--
-- /Note:/ Consider using 'defaultOs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultOs :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csDefaultOs = Lens.field @"defaultOs"
{-# DEPRECATED csDefaultOs "Use generic-lens or generic-optics with 'defaultOs' instead." #-}

-- | The default root device type. This value is the default for all instances in the stack, but you can override it when you create an instance. The default option is @instance-store@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultRootDeviceType :: Lens.Lens' CreateStack (Core.Maybe Types.RootDeviceType)
csDefaultRootDeviceType = Lens.field @"defaultRootDeviceType"
{-# DEPRECATED csDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

-- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
--
-- /Note:/ Consider using 'defaultSshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultSshKeyName :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csDefaultSshKeyName = Lens.field @"defaultSshKeyName"
{-# DEPRECATED csDefaultSshKeyName "Use generic-lens or generic-optics with 'defaultSshKeyName' instead." #-}

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultSubnetId :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csDefaultSubnetId = Lens.field @"defaultSubnetId"
{-# DEPRECATED csDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The stack's host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
csHostnameTheme :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csHostnameTheme = Lens.field @"hostnameTheme"
{-# DEPRECATED csHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseCustomCookbooks :: Lens.Lens' CreateStack (Core.Maybe Core.Bool)
csUseCustomCookbooks = Lens.field @"useCustomCookbooks"
{-# DEPRECATED csUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- AWS OpsWorks Stacks provides a standard set of built-in security groups, one for each layer, which are associated with layers by default. With @UseOpsworksSecurityGroups@ you can instead provide your own custom security groups. @UseOpsworksSecurityGroups@ has the following settings:
--
--     * True - AWS OpsWorks Stacks automatically associates the appropriate built-in security group with each layer (default setting). You can associate additional security groups with a layer after you create it, but you cannot delete the built-in security group.
--
--
--     * False - AWS OpsWorks Stacks does not associate built-in security groups with layers. You must create appropriate EC2 security groups and associate a security group with each layer that you create. However, you can still manually associate a built-in security group with a layer on creation; custom security groups are required only for those layers that need custom settings.
--
--
-- For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseOpsworksSecurityGroups :: Lens.Lens' CreateStack (Core.Maybe Core.Bool)
csUseOpsworksSecurityGroups = Lens.field @"useOpsworksSecurityGroups"
{-# DEPRECATED csUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | The ID of the VPC that the stack is to be launched into. The VPC must be in the stack's region. All instances are launched into this VPC. You cannot change the ID later.
--
--
--     * If your account supports EC2-Classic, the default value is @no VPC@ .
--
--
--     * If your account does not support EC2-Classic, the default value is the default VPC for the specified region.
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
-- For more information about how to use AWS OpsWorks Stacks with a VPC, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-vpc.html Running a Stack in a VPC> . For more information about default VPC and EC2-Classic, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> .
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csVpcId :: Lens.Lens' CreateStack (Core.Maybe Types.String)
csVpcId = Lens.field @"vpcId"
{-# DEPRECATED csVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON CreateStack where
  toJSON CreateStack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Region" Core..= region),
            Core.Just ("ServiceRoleArn" Core..= serviceRoleArn),
            Core.Just
              ("DefaultInstanceProfileArn" Core..= defaultInstanceProfileArn),
            ("AgentVersion" Core..=) Core.<$> agentVersion,
            ("Attributes" Core..=) Core.<$> attributes,
            ("ChefConfiguration" Core..=) Core.<$> chefConfiguration,
            ("ConfigurationManager" Core..=) Core.<$> configurationManager,
            ("CustomCookbooksSource" Core..=) Core.<$> customCookbooksSource,
            ("CustomJson" Core..=) Core.<$> customJson,
            ("DefaultAvailabilityZone" Core..=)
              Core.<$> defaultAvailabilityZone,
            ("DefaultOs" Core..=) Core.<$> defaultOs,
            ("DefaultRootDeviceType" Core..=) Core.<$> defaultRootDeviceType,
            ("DefaultSshKeyName" Core..=) Core.<$> defaultSshKeyName,
            ("DefaultSubnetId" Core..=) Core.<$> defaultSubnetId,
            ("HostnameTheme" Core..=) Core.<$> hostnameTheme,
            ("UseCustomCookbooks" Core..=) Core.<$> useCustomCookbooks,
            ("UseOpsworksSecurityGroups" Core..=)
              Core.<$> useOpsworksSecurityGroups,
            ("VpcId" Core..=) Core.<$> vpcId
          ]
      )

instance Core.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateStack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Core.<$> (x Core..:? "StackId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @CreateStack@ request.
--
-- /See:/ 'mkCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | The stack ID, which is an opaque string that you use to identify the stack when performing actions such as @DescribeStacks@ .
    stackId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackResponse' value with any optional fields omitted.
mkCreateStackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStackResponse
mkCreateStackResponse responseStatus =
  CreateStackResponse' {stackId = Core.Nothing, responseStatus}

-- | The stack ID, which is an opaque string that you use to identify the stack when performing actions such as @DescribeStacks@ .
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStackId :: Lens.Lens' CreateStackResponse (Core.Maybe Types.String)
csrrsStackId = Lens.field @"stackId"
{-# DEPRECATED csrrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStackResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
