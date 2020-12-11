{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csDefaultRootDeviceType,
    csVPCId,
    csChefConfiguration,
    csAgentVersion,
    csDefaultSSHKeyName,
    csCustomJSON,
    csCustomCookbooksSource,
    csDefaultAvailabilityZone,
    csAttributes,
    csDefaultOS,
    csUseOpsworksSecurityGroups,
    csUseCustomCookbooks,
    csDefaultSubnetId,
    csConfigurationManager,
    csHostnameTheme,
    csName,
    csRegion,
    csServiceRoleARN,
    csDefaultInstanceProfileARN,

    -- * Destructuring the response
    CreateStackResponse (..),
    mkCreateStackResponse,

    -- ** Response lenses
    crsStackId,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStack' smart constructor.
data CreateStack = CreateStack'
  { defaultRootDeviceType ::
      Lude.Maybe RootDeviceType,
    vpcId :: Lude.Maybe Lude.Text,
    chefConfiguration :: Lude.Maybe ChefConfiguration,
    agentVersion :: Lude.Maybe Lude.Text,
    defaultSSHKeyName :: Lude.Maybe Lude.Text,
    customJSON :: Lude.Maybe Lude.Text,
    customCookbooksSource :: Lude.Maybe Source,
    defaultAvailabilityZone :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)),
    defaultOS :: Lude.Maybe Lude.Text,
    useOpsworksSecurityGroups :: Lude.Maybe Lude.Bool,
    useCustomCookbooks :: Lude.Maybe Lude.Bool,
    defaultSubnetId :: Lude.Maybe Lude.Text,
    configurationManager :: Lude.Maybe StackConfigurationManager,
    hostnameTheme :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    region :: Lude.Text,
    serviceRoleARN :: Lude.Text,
    defaultInstanceProfileARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStack' with the minimum fields required to make a request.
--
-- * 'agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.
--
--
--     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances.
--
--
-- The default setting is the most recent release of the agent. To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
-- * 'attributes' - One or more user-defined key-value pairs to be added to the stack attributes.
-- * 'chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
-- * 'configurationManager' - The configuration manager. When you create a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
-- * 'customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
-- * 'customJSON' - A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
-- * 'defaultAvailabilityZone' - The stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
-- * 'defaultInstanceProfileARN' - The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'defaultOS' - The stack's default operating system, which is installed on every instance unless you specify a different operating system when you create the instance. You can specify one of the following.
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
-- * 'defaultRootDeviceType' - The default root device type. This value is the default for all instances in the stack, but you can override it when you create an instance. The default option is @instance-store@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
-- * 'defaultSSHKeyName' - A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
-- * 'defaultSubnetId' - The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
-- * 'hostnameTheme' - The stack's host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
-- * 'name' - The stack name.
-- * 'region' - The stack's AWS region, such as @ap-south-1@ . For more information about Amazon regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'serviceRoleARN' - The stack's AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'useCustomCookbooks' - Whether the stack uses custom cookbooks.
-- * 'useOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
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
-- * 'vpcId' - The ID of the VPC that the stack is to be launched into. The VPC must be in the stack's region. All instances are launched into this VPC. You cannot change the ID later.
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
mkCreateStack ::
  -- | 'name'
  Lude.Text ->
  -- | 'region'
  Lude.Text ->
  -- | 'serviceRoleARN'
  Lude.Text ->
  -- | 'defaultInstanceProfileARN'
  Lude.Text ->
  CreateStack
mkCreateStack
  pName_
  pRegion_
  pServiceRoleARN_
  pDefaultInstanceProfileARN_ =
    CreateStack'
      { defaultRootDeviceType = Lude.Nothing,
        vpcId = Lude.Nothing,
        chefConfiguration = Lude.Nothing,
        agentVersion = Lude.Nothing,
        defaultSSHKeyName = Lude.Nothing,
        customJSON = Lude.Nothing,
        customCookbooksSource = Lude.Nothing,
        defaultAvailabilityZone = Lude.Nothing,
        attributes = Lude.Nothing,
        defaultOS = Lude.Nothing,
        useOpsworksSecurityGroups = Lude.Nothing,
        useCustomCookbooks = Lude.Nothing,
        defaultSubnetId = Lude.Nothing,
        configurationManager = Lude.Nothing,
        hostnameTheme = Lude.Nothing,
        name = pName_,
        region = pRegion_,
        serviceRoleARN = pServiceRoleARN_,
        defaultInstanceProfileARN = pDefaultInstanceProfileARN_
      }

-- | The default root device type. This value is the default for all instances in the stack, but you can override it when you create an instance. The default option is @instance-store@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultRootDeviceType :: Lens.Lens' CreateStack (Lude.Maybe RootDeviceType)
csDefaultRootDeviceType = Lens.lens (defaultRootDeviceType :: CreateStack -> Lude.Maybe RootDeviceType) (\s a -> s {defaultRootDeviceType = a} :: CreateStack)
{-# DEPRECATED csDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

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
csVPCId :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csVPCId = Lens.lens (vpcId :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CreateStack)
{-# DEPRECATED csVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChefConfiguration :: Lens.Lens' CreateStack (Lude.Maybe ChefConfiguration)
csChefConfiguration = Lens.lens (chefConfiguration :: CreateStack -> Lude.Maybe ChefConfiguration) (\s a -> s {chefConfiguration = a} :: CreateStack)
{-# DEPRECATED csChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

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
csAgentVersion :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csAgentVersion = Lens.lens (agentVersion :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: CreateStack)
{-# DEPRECATED csAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
--
-- /Note:/ Consider using 'defaultSSHKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultSSHKeyName :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csDefaultSSHKeyName = Lens.lens (defaultSSHKeyName :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSSHKeyName = a} :: CreateStack)
{-# DEPRECATED csDefaultSSHKeyName "Use generic-lens or generic-optics with 'defaultSSHKeyName' instead." #-}

-- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomJSON :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csCustomJSON = Lens.lens (customJSON :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: CreateStack)
{-# DEPRECATED csCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomCookbooksSource :: Lens.Lens' CreateStack (Lude.Maybe Source)
csCustomCookbooksSource = Lens.lens (customCookbooksSource :: CreateStack -> Lude.Maybe Source) (\s a -> s {customCookbooksSource = a} :: CreateStack)
{-# DEPRECATED csCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | The stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultAvailabilityZone :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csDefaultAvailabilityZone = Lens.lens (defaultAvailabilityZone :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultAvailabilityZone = a} :: CreateStack)
{-# DEPRECATED csDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAttributes :: Lens.Lens' CreateStack (Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)))
csAttributes = Lens.lens (attributes :: CreateStack -> Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: CreateStack)
{-# DEPRECATED csAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

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
-- /Note:/ Consider using 'defaultOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultOS :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csDefaultOS = Lens.lens (defaultOS :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultOS = a} :: CreateStack)
{-# DEPRECATED csDefaultOS "Use generic-lens or generic-optics with 'defaultOS' instead." #-}

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
csUseOpsworksSecurityGroups :: Lens.Lens' CreateStack (Lude.Maybe Lude.Bool)
csUseOpsworksSecurityGroups = Lens.lens (useOpsworksSecurityGroups :: CreateStack -> Lude.Maybe Lude.Bool) (\s a -> s {useOpsworksSecurityGroups = a} :: CreateStack)
{-# DEPRECATED csUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseCustomCookbooks :: Lens.Lens' CreateStack (Lude.Maybe Lude.Bool)
csUseCustomCookbooks = Lens.lens (useCustomCookbooks :: CreateStack -> Lude.Maybe Lude.Bool) (\s a -> s {useCustomCookbooks = a} :: CreateStack)
{-# DEPRECATED csUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultSubnetId :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csDefaultSubnetId = Lens.lens (defaultSubnetId :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubnetId = a} :: CreateStack)
{-# DEPRECATED csDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The configuration manager. When you create a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csConfigurationManager :: Lens.Lens' CreateStack (Lude.Maybe StackConfigurationManager)
csConfigurationManager = Lens.lens (configurationManager :: CreateStack -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: CreateStack)
{-# DEPRECATED csConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

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
csHostnameTheme :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csHostnameTheme = Lens.lens (hostnameTheme :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {hostnameTheme = a} :: CreateStack)
{-# DEPRECATED csHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateStack Lude.Text
csName = Lens.lens (name :: CreateStack -> Lude.Text) (\s a -> s {name = a} :: CreateStack)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack's AWS region, such as @ap-south-1@ . For more information about Amazon regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRegion :: Lens.Lens' CreateStack Lude.Text
csRegion = Lens.lens (region :: CreateStack -> Lude.Text) (\s a -> s {region = a} :: CreateStack)
{-# DEPRECATED csRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The stack's AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceRoleARN :: Lens.Lens' CreateStack Lude.Text
csServiceRoleARN = Lens.lens (serviceRoleARN :: CreateStack -> Lude.Text) (\s a -> s {serviceRoleARN = a} :: CreateStack)
{-# DEPRECATED csServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultInstanceProfileARN :: Lens.Lens' CreateStack Lude.Text
csDefaultInstanceProfileARN = Lens.lens (defaultInstanceProfileARN :: CreateStack -> Lude.Text) (\s a -> s {defaultInstanceProfileARN = a} :: CreateStack)
{-# DEPRECATED csDefaultInstanceProfileARN "Use generic-lens or generic-optics with 'defaultInstanceProfileARN' instead." #-}

instance Lude.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Lude.<$> (x Lude..?> "StackId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CreateStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStack where
  toJSON CreateStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultRootDeviceType" Lude..=) Lude.<$> defaultRootDeviceType,
            ("VpcId" Lude..=) Lude.<$> vpcId,
            ("ChefConfiguration" Lude..=) Lude.<$> chefConfiguration,
            ("AgentVersion" Lude..=) Lude.<$> agentVersion,
            ("DefaultSshKeyName" Lude..=) Lude.<$> defaultSSHKeyName,
            ("CustomJson" Lude..=) Lude.<$> customJSON,
            ("CustomCookbooksSource" Lude..=) Lude.<$> customCookbooksSource,
            ("DefaultAvailabilityZone" Lude..=)
              Lude.<$> defaultAvailabilityZone,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("DefaultOs" Lude..=) Lude.<$> defaultOS,
            ("UseOpsworksSecurityGroups" Lude..=)
              Lude.<$> useOpsworksSecurityGroups,
            ("UseCustomCookbooks" Lude..=) Lude.<$> useCustomCookbooks,
            ("DefaultSubnetId" Lude..=) Lude.<$> defaultSubnetId,
            ("ConfigurationManager" Lude..=) Lude.<$> configurationManager,
            ("HostnameTheme" Lude..=) Lude.<$> hostnameTheme,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Region" Lude..= region),
            Lude.Just ("ServiceRoleArn" Lude..= serviceRoleARN),
            Lude.Just
              ("DefaultInstanceProfileArn" Lude..= defaultInstanceProfileARN)
          ]
      )

instance Lude.ToPath CreateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStack where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CreateStack@ request.
--
-- /See:/ 'mkCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { stackId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stackId' - The stack ID, which is an opaque string that you use to identify the stack when performing actions such as @DescribeStacks@ .
mkCreateStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStackResponse
mkCreateStackResponse pResponseStatus_ =
  CreateStackResponse'
    { stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The stack ID, which is an opaque string that you use to identify the stack when performing actions such as @DescribeStacks@ .
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsStackId :: Lens.Lens' CreateStackResponse (Lude.Maybe Lude.Text)
crsStackId = Lens.lens (stackId :: CreateStackResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: CreateStackResponse)
{-# DEPRECATED crsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateStackResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStackResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
