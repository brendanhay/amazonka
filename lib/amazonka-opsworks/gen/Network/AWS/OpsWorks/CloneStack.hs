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
    cDefaultInstanceProfileARN,
    cCloneAppIds,
    cServiceRoleARN,
    cDefaultRootDeviceType,
    cVPCId,
    cChefConfiguration,
    cAgentVersion,
    cDefaultSSHKeyName,
    cCustomJSON,
    cClonePermissions,
    cSourceStackId,
    cCustomCookbooksSource,
    cDefaultAvailabilityZone,
    cAttributes,
    cName,
    cDefaultOS,
    cUseOpsworksSecurityGroups,
    cUseCustomCookbooks,
    cDefaultSubnetId,
    cRegion,
    cConfigurationManager,
    cHostnameTheme,

    -- * Destructuring the response
    CloneStackResponse (..),
    mkCloneStackResponse,

    -- ** Response lenses
    csrsStackId,
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCloneStack' smart constructor.
data CloneStack = CloneStack'
  { -- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    defaultInstanceProfileARN :: Lude.Maybe Lude.Text,
    -- | A list of source stack app IDs to be included in the cloned stack.
    cloneAppIds :: Lude.Maybe [Lude.Text],
    -- | The stack AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. If you create a stack by using the AWS OpsWorks Stacks console, it creates the role for you. You can obtain an existing stack's IAM ARN programmatically by calling 'DescribePermissions' . For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    serviceRoleARN :: Lude.Text,
    -- | The default root device type. This value is used by default for all instances in the cloned stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    defaultRootDeviceType :: Lude.Maybe RootDeviceType,
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
    vpcId :: Lude.Maybe Lude.Text,
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    chefConfiguration :: Lude.Maybe ChefConfiguration,
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
    agentVersion :: Lude.Maybe Lude.Text,
    -- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
    defaultSSHKeyName :: Lude.Maybe Lude.Text,
    -- | A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
    customJSON :: Lude.Maybe Lude.Text,
    -- | Whether to clone the source stack's permissions.
    clonePermissions :: Lude.Maybe Lude.Bool,
    -- | The source stack ID.
    sourceStackId :: Lude.Text,
    -- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
    customCookbooksSource :: Lude.Maybe Source,
    -- | The cloned stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
    defaultAvailabilityZone :: Lude.Maybe Lude.Text,
    -- | A list of stack attributes and values as key/value pairs to be added to the cloned stack.
    attributes :: Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)),
    -- | The cloned stack name.
    name :: Lude.Maybe Lude.Text,
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
    defaultOS :: Lude.Maybe Lude.Text,
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
    useOpsworksSecurityGroups :: Lude.Maybe Lude.Bool,
    -- | Whether to use custom cookbooks.
    useCustomCookbooks :: Lude.Maybe Lude.Bool,
    -- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
    defaultSubnetId :: Lude.Maybe Lude.Text,
    -- | The cloned stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    region :: Lude.Maybe Lude.Text,
    -- | The configuration manager. When you clone a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
    configurationManager :: Lude.Maybe StackConfigurationManager,
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
    hostnameTheme :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloneStack' with the minimum fields required to make a request.
--
-- * 'defaultInstanceProfileARN' - The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'cloneAppIds' - A list of source stack app IDs to be included in the cloned stack.
-- * 'serviceRoleARN' - The stack AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. If you create a stack by using the AWS OpsWorks Stacks console, it creates the role for you. You can obtain an existing stack's IAM ARN programmatically by calling 'DescribePermissions' . For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'defaultRootDeviceType' - The default root device type. This value is used by default for all instances in the cloned stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
-- * 'vpcId' - The ID of the VPC that the cloned stack is to be launched into. It must be in the specified region. All instances are launched into this VPC, and you cannot change the ID later.
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
-- * 'chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
-- * 'agentVersion' - The default AWS OpsWorks Stacks agent version. You have the following options:
--
--
--     * Auto-update - Set this parameter to @LATEST@ . AWS OpsWorks Stacks automatically installs new agent versions on the stack's instances as soon as they are available.
--
--
--     * Fixed version - Set this parameter to your preferred agent version. To update the agent version, you must edit the stack configuration and specify a new version. AWS OpsWorks Stacks then automatically installs that version on the stack's instances.
--
--
-- The default setting is @LATEST@ . To specify an agent version, you must use the complete version number, not the abbreviated number shown on the console. For a list of available agent version numbers, call 'DescribeAgentVersions' . AgentVersion cannot be set to Chef 12.2.
-- * 'defaultSSHKeyName' - A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
-- * 'customJSON' - A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
-- * 'clonePermissions' - Whether to clone the source stack's permissions.
-- * 'sourceStackId' - The source stack ID.
-- * 'customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
-- * 'defaultAvailabilityZone' - The cloned stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
-- * 'attributes' - A list of stack attributes and values as key/value pairs to be added to the cloned stack.
-- * 'name' - The cloned stack name.
-- * 'defaultOS' - The stack's operating system, which must be set to one of the following.
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
-- * 'useOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
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
-- * 'useCustomCookbooks' - Whether to use custom cookbooks.
-- * 'defaultSubnetId' - The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
-- * 'region' - The cloned stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'configurationManager' - The configuration manager. When you clone a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
-- * 'hostnameTheme' - The stack's host name theme, with spaces are replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
mkCloneStack ::
  -- | 'serviceRoleARN'
  Lude.Text ->
  -- | 'sourceStackId'
  Lude.Text ->
  CloneStack
mkCloneStack pServiceRoleARN_ pSourceStackId_ =
  CloneStack'
    { defaultInstanceProfileARN = Lude.Nothing,
      cloneAppIds = Lude.Nothing,
      serviceRoleARN = pServiceRoleARN_,
      defaultRootDeviceType = Lude.Nothing,
      vpcId = Lude.Nothing,
      chefConfiguration = Lude.Nothing,
      agentVersion = Lude.Nothing,
      defaultSSHKeyName = Lude.Nothing,
      customJSON = Lude.Nothing,
      clonePermissions = Lude.Nothing,
      sourceStackId = pSourceStackId_,
      customCookbooksSource = Lude.Nothing,
      defaultAvailabilityZone = Lude.Nothing,
      attributes = Lude.Nothing,
      name = Lude.Nothing,
      defaultOS = Lude.Nothing,
      useOpsworksSecurityGroups = Lude.Nothing,
      useCustomCookbooks = Lude.Nothing,
      defaultSubnetId = Lude.Nothing,
      region = Lude.Nothing,
      configurationManager = Lude.Nothing,
      hostnameTheme = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultInstanceProfileARN :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cDefaultInstanceProfileARN = Lens.lens (defaultInstanceProfileARN :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultInstanceProfileARN = a} :: CloneStack)
{-# DEPRECATED cDefaultInstanceProfileARN "Use generic-lens or generic-optics with 'defaultInstanceProfileARN' instead." #-}

-- | A list of source stack app IDs to be included in the cloned stack.
--
-- /Note:/ Consider using 'cloneAppIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCloneAppIds :: Lens.Lens' CloneStack (Lude.Maybe [Lude.Text])
cCloneAppIds = Lens.lens (cloneAppIds :: CloneStack -> Lude.Maybe [Lude.Text]) (\s a -> s {cloneAppIds = a} :: CloneStack)
{-# DEPRECATED cCloneAppIds "Use generic-lens or generic-optics with 'cloneAppIds' instead." #-}

-- | The stack AWS Identity and Access Management (IAM) role, which allows AWS OpsWorks Stacks to work with AWS resources on your behalf. You must set this parameter to the Amazon Resource Name (ARN) for an existing IAM role. If you create a stack by using the AWS OpsWorks Stacks console, it creates the role for you. You can obtain an existing stack's IAM ARN programmatically by calling 'DescribePermissions' . For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cServiceRoleARN :: Lens.Lens' CloneStack Lude.Text
cServiceRoleARN = Lens.lens (serviceRoleARN :: CloneStack -> Lude.Text) (\s a -> s {serviceRoleARN = a} :: CloneStack)
{-# DEPRECATED cServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The default root device type. This value is used by default for all instances in the cloned stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultRootDeviceType :: Lens.Lens' CloneStack (Lude.Maybe RootDeviceType)
cDefaultRootDeviceType = Lens.lens (defaultRootDeviceType :: CloneStack -> Lude.Maybe RootDeviceType) (\s a -> s {defaultRootDeviceType = a} :: CloneStack)
{-# DEPRECATED cDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

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
cVPCId :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cVPCId = Lens.lens (vpcId :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CloneStack)
{-# DEPRECATED cVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChefConfiguration :: Lens.Lens' CloneStack (Lude.Maybe ChefConfiguration)
cChefConfiguration = Lens.lens (chefConfiguration :: CloneStack -> Lude.Maybe ChefConfiguration) (\s a -> s {chefConfiguration = a} :: CloneStack)
{-# DEPRECATED cChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

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
cAgentVersion :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cAgentVersion = Lens.lens (agentVersion :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: CloneStack)
{-# DEPRECATED cAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A default Amazon EC2 key pair name. The default value is none. If you specify a key pair name, AWS OpsWorks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
--
-- /Note:/ Consider using 'defaultSSHKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultSSHKeyName :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cDefaultSSHKeyName = Lens.lens (defaultSSHKeyName :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSSHKeyName = a} :: CloneStack)
{-# DEPRECATED cDefaultSSHKeyName "Use generic-lens or generic-optics with 'defaultSSHKeyName' instead." #-}

-- | A string that contains user-defined, custom JSON. It is used to override the corresponding default stack configuration JSON values. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomJSON :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cCustomJSON = Lens.lens (customJSON :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: CloneStack)
{-# DEPRECATED cCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | Whether to clone the source stack's permissions.
--
-- /Note:/ Consider using 'clonePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClonePermissions :: Lens.Lens' CloneStack (Lude.Maybe Lude.Bool)
cClonePermissions = Lens.lens (clonePermissions :: CloneStack -> Lude.Maybe Lude.Bool) (\s a -> s {clonePermissions = a} :: CloneStack)
{-# DEPRECATED cClonePermissions "Use generic-lens or generic-optics with 'clonePermissions' instead." #-}

-- | The source stack ID.
--
-- /Note:/ Consider using 'sourceStackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceStackId :: Lens.Lens' CloneStack Lude.Text
cSourceStackId = Lens.lens (sourceStackId :: CloneStack -> Lude.Text) (\s a -> s {sourceStackId = a} :: CloneStack)
{-# DEPRECATED cSourceStackId "Use generic-lens or generic-optics with 'sourceStackId' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomCookbooksSource :: Lens.Lens' CloneStack (Lude.Maybe Source)
cCustomCookbooksSource = Lens.lens (customCookbooksSource :: CloneStack -> Lude.Maybe Source) (\s a -> s {customCookbooksSource = a} :: CloneStack)
{-# DEPRECATED cCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | The cloned stack's default Availability Zone, which must be in the specified region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultAvailabilityZone :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cDefaultAvailabilityZone = Lens.lens (defaultAvailabilityZone :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultAvailabilityZone = a} :: CloneStack)
{-# DEPRECATED cDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | A list of stack attributes and values as key/value pairs to be added to the cloned stack.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAttributes :: Lens.Lens' CloneStack (Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)))
cAttributes = Lens.lens (attributes :: CloneStack -> Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: CloneStack)
{-# DEPRECATED cAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The cloned stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CloneStack)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

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
-- /Note:/ Consider using 'defaultOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultOS :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cDefaultOS = Lens.lens (defaultOS :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultOS = a} :: CloneStack)
{-# DEPRECATED cDefaultOS "Use generic-lens or generic-optics with 'defaultOS' instead." #-}

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
cUseOpsworksSecurityGroups :: Lens.Lens' CloneStack (Lude.Maybe Lude.Bool)
cUseOpsworksSecurityGroups = Lens.lens (useOpsworksSecurityGroups :: CloneStack -> Lude.Maybe Lude.Bool) (\s a -> s {useOpsworksSecurityGroups = a} :: CloneStack)
{-# DEPRECATED cUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | Whether to use custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cUseCustomCookbooks :: Lens.Lens' CloneStack (Lude.Maybe Lude.Bool)
cUseCustomCookbooks = Lens.lens (useCustomCookbooks :: CloneStack -> Lude.Maybe Lude.Bool) (\s a -> s {useCustomCookbooks = a} :: CloneStack)
{-# DEPRECATED cUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDefaultSubnetId :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cDefaultSubnetId = Lens.lens (defaultSubnetId :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubnetId = a} :: CloneStack)
{-# DEPRECATED cDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The cloned stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegion :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cRegion = Lens.lens (region :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: CloneStack)
{-# DEPRECATED cRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The configuration manager. When you clone a stack we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConfigurationManager :: Lens.Lens' CloneStack (Lude.Maybe StackConfigurationManager)
cConfigurationManager = Lens.lens (configurationManager :: CloneStack -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: CloneStack)
{-# DEPRECATED cConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

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
cHostnameTheme :: Lens.Lens' CloneStack (Lude.Maybe Lude.Text)
cHostnameTheme = Lens.lens (hostnameTheme :: CloneStack -> Lude.Maybe Lude.Text) (\s a -> s {hostnameTheme = a} :: CloneStack)
{-# DEPRECATED cHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

instance Lude.AWSRequest CloneStack where
  type Rs CloneStack = CloneStackResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CloneStackResponse'
            Lude.<$> (x Lude..?> "StackId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CloneStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CloneStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CloneStack where
  toJSON CloneStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultInstanceProfileArn" Lude..=)
              Lude.<$> defaultInstanceProfileARN,
            ("CloneAppIds" Lude..=) Lude.<$> cloneAppIds,
            Lude.Just ("ServiceRoleArn" Lude..= serviceRoleARN),
            ("DefaultRootDeviceType" Lude..=) Lude.<$> defaultRootDeviceType,
            ("VpcId" Lude..=) Lude.<$> vpcId,
            ("ChefConfiguration" Lude..=) Lude.<$> chefConfiguration,
            ("AgentVersion" Lude..=) Lude.<$> agentVersion,
            ("DefaultSshKeyName" Lude..=) Lude.<$> defaultSSHKeyName,
            ("CustomJson" Lude..=) Lude.<$> customJSON,
            ("ClonePermissions" Lude..=) Lude.<$> clonePermissions,
            Lude.Just ("SourceStackId" Lude..= sourceStackId),
            ("CustomCookbooksSource" Lude..=) Lude.<$> customCookbooksSource,
            ("DefaultAvailabilityZone" Lude..=)
              Lude.<$> defaultAvailabilityZone,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("Name" Lude..=) Lude.<$> name,
            ("DefaultOs" Lude..=) Lude.<$> defaultOS,
            ("UseOpsworksSecurityGroups" Lude..=)
              Lude.<$> useOpsworksSecurityGroups,
            ("UseCustomCookbooks" Lude..=) Lude.<$> useCustomCookbooks,
            ("DefaultSubnetId" Lude..=) Lude.<$> defaultSubnetId,
            ("Region" Lude..=) Lude.<$> region,
            ("ConfigurationManager" Lude..=) Lude.<$> configurationManager,
            ("HostnameTheme" Lude..=) Lude.<$> hostnameTheme
          ]
      )

instance Lude.ToPath CloneStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CloneStack where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CloneStack@ request.
--
-- /See:/ 'mkCloneStackResponse' smart constructor.
data CloneStackResponse = CloneStackResponse'
  { -- | The cloned stack ID.
    stackId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloneStackResponse' with the minimum fields required to make a request.
--
-- * 'stackId' - The cloned stack ID.
-- * 'responseStatus' - The response status code.
mkCloneStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CloneStackResponse
mkCloneStackResponse pResponseStatus_ =
  CloneStackResponse'
    { stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The cloned stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStackId :: Lens.Lens' CloneStackResponse (Lude.Maybe Lude.Text)
csrsStackId = Lens.lens (stackId :: CloneStackResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: CloneStackResponse)
{-# DEPRECATED csrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CloneStackResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CloneStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CloneStackResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
