{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateStack (..),
    mkUpdateStack,

    -- ** Request lenses
    usDefaultInstanceProfileARN,
    usServiceRoleARN,
    usDefaultRootDeviceType,
    usChefConfiguration,
    usAgentVersion,
    usDefaultSSHKeyName,
    usCustomJSON,
    usCustomCookbooksSource,
    usDefaultAvailabilityZone,
    usAttributes,
    usName,
    usDefaultOS,
    usUseOpsworksSecurityGroups,
    usUseCustomCookbooks,
    usDefaultSubnetId,
    usConfigurationManager,
    usStackId,
    usHostnameTheme,

    -- * Destructuring the response
    UpdateStackResponse (..),
    mkUpdateStackResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    defaultInstanceProfileARN :: Lude.Maybe Lude.Text,
    -- | Do not use this parameter. You cannot update a stack's service role.
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    defaultRootDeviceType :: Lude.Maybe RootDeviceType,
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
    -- | A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
    defaultSSHKeyName :: Lude.Maybe Lude.Text,
    -- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
    customJSON :: Lude.Maybe Lude.Text,
    -- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
    customCookbooksSource :: Lude.Maybe Source,
    -- | The stack's default Availability Zone, which must be in the stack's region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' .
    defaultAvailabilityZone :: Lude.Maybe Lude.Text,
    -- | One or more user-defined key-value pairs to be added to the stack attributes.
    attributes :: Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)),
    -- | The stack's new name.
    name :: Lude.Maybe Lude.Text,
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
    defaultOS :: Lude.Maybe Lude.Text,
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
    useOpsworksSecurityGroups :: Lude.Maybe Lude.Bool,
    -- | Whether the stack uses custom cookbooks.
    useCustomCookbooks :: Lude.Maybe Lude.Bool,
    -- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
    defaultSubnetId :: Lude.Maybe Lude.Text,
    -- | The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
    configurationManager :: Lude.Maybe StackConfigurationManager,
    -- | The stack ID.
    stackId :: Lude.Text,
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
    hostnameTheme :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- * 'defaultInstanceProfileARN' - The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'serviceRoleARN' - Do not use this parameter. You cannot update a stack's service role.
-- * 'defaultRootDeviceType' - The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
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
-- * 'defaultSSHKeyName' - A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
-- * 'customJSON' - A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
-- * 'customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
-- * 'defaultAvailabilityZone' - The stack's default Availability Zone, which must be in the stack's region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' .
-- * 'attributes' - One or more user-defined key-value pairs to be added to the stack attributes.
-- * 'name' - The stack's new name.
-- * 'defaultOS' - The stack's operating system, which must be set to one of the following:
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
-- * 'useOpsworksSecurityGroups' - Whether to associate the AWS OpsWorks Stacks built-in security groups with the stack's layers.
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
-- * 'useCustomCookbooks' - Whether the stack uses custom cookbooks.
-- * 'defaultSubnetId' - The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
-- * 'configurationManager' - The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
-- * 'stackId' - The stack ID.
-- * 'hostnameTheme' - The stack's new host name theme, with spaces replaced by underscores. The theme is used to generate host names for the stack's instances. By default, @HostnameTheme@ is set to @Layer_Dependent@ , which creates host names by appending integers to the layer's short name. The other themes are:
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
mkUpdateStack ::
  -- | 'stackId'
  Lude.Text ->
  UpdateStack
mkUpdateStack pStackId_ =
  UpdateStack'
    { defaultInstanceProfileARN = Lude.Nothing,
      serviceRoleARN = Lude.Nothing,
      defaultRootDeviceType = Lude.Nothing,
      chefConfiguration = Lude.Nothing,
      agentVersion = Lude.Nothing,
      defaultSSHKeyName = Lude.Nothing,
      customJSON = Lude.Nothing,
      customCookbooksSource = Lude.Nothing,
      defaultAvailabilityZone = Lude.Nothing,
      attributes = Lude.Nothing,
      name = Lude.Nothing,
      defaultOS = Lude.Nothing,
      useOpsworksSecurityGroups = Lude.Nothing,
      useCustomCookbooks = Lude.Nothing,
      defaultSubnetId = Lude.Nothing,
      configurationManager = Lude.Nothing,
      stackId = pStackId_,
      hostnameTheme = Lude.Nothing
    }

-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultInstanceProfileARN :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDefaultInstanceProfileARN = Lens.lens (defaultInstanceProfileARN :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultInstanceProfileARN = a} :: UpdateStack)
{-# DEPRECATED usDefaultInstanceProfileARN "Use generic-lens or generic-optics with 'defaultInstanceProfileARN' instead." #-}

-- | Do not use this parameter. You cannot update a stack's service role.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usServiceRoleARN :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usServiceRoleARN = Lens.lens (serviceRoleARN :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: UpdateStack)
{-# DEPRECATED usServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultRootDeviceType :: Lens.Lens' UpdateStack (Lude.Maybe RootDeviceType)
usDefaultRootDeviceType = Lens.lens (defaultRootDeviceType :: UpdateStack -> Lude.Maybe RootDeviceType) (\s a -> s {defaultRootDeviceType = a} :: UpdateStack)
{-# DEPRECATED usDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version on Chef 11.10 stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usChefConfiguration :: Lens.Lens' UpdateStack (Lude.Maybe ChefConfiguration)
usChefConfiguration = Lens.lens (chefConfiguration :: UpdateStack -> Lude.Maybe ChefConfiguration) (\s a -> s {chefConfiguration = a} :: UpdateStack)
{-# DEPRECATED usChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

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
usAgentVersion :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usAgentVersion = Lens.lens (agentVersion :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: UpdateStack)
{-# DEPRECATED usAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A default Amazon EC2 key-pair name. The default value is @none@ . If you specify a key-pair name, AWS OpsWorks Stacks installs the public key on the instance and you can use the private key with an SSH client to log in to the instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-ssh.html Using SSH to Communicate with an Instance> and <https://docs.aws.amazon.com/opsworks/latest/userguide/security-ssh-access.html Managing SSH Access> . You can override this setting by specifying a different key pair, or no key pair, when you <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html create an instance> .
--
-- /Note:/ Consider using 'defaultSSHKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultSSHKeyName :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDefaultSSHKeyName = Lens.lens (defaultSSHKeyName :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSSHKeyName = a} :: UpdateStack)
{-# DEPRECATED usDefaultSSHKeyName "Use generic-lens or generic-optics with 'defaultSSHKeyName' instead." #-}

-- | A string that contains user-defined, custom JSON. It can be used to override the corresponding default stack configuration JSON values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information about custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCustomJSON :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usCustomJSON = Lens.lens (customJSON :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: UpdateStack)
{-# DEPRECATED usCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCustomCookbooksSource :: Lens.Lens' UpdateStack (Lude.Maybe Source)
usCustomCookbooksSource = Lens.lens (customCookbooksSource :: UpdateStack -> Lude.Maybe Source) (\s a -> s {customCookbooksSource = a} :: UpdateStack)
{-# DEPRECATED usCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | The stack's default Availability Zone, which must be in the stack's region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> . If you also specify a value for @DefaultSubnetId@ , the subnet must be in the same zone. For more information, see 'CreateStack' .
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultAvailabilityZone :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDefaultAvailabilityZone = Lens.lens (defaultAvailabilityZone :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultAvailabilityZone = a} :: UpdateStack)
{-# DEPRECATED usDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAttributes :: Lens.Lens' UpdateStack (Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)))
usAttributes = Lens.lens (attributes :: UpdateStack -> Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: UpdateStack)
{-# DEPRECATED usAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The stack's new name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usName :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usName = Lens.lens (name :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateStack)
{-# DEPRECATED usName "Use generic-lens or generic-optics with 'name' instead." #-}

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
-- /Note:/ Consider using 'defaultOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultOS :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDefaultOS = Lens.lens (defaultOS :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultOS = a} :: UpdateStack)
{-# DEPRECATED usDefaultOS "Use generic-lens or generic-optics with 'defaultOS' instead." #-}

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
usUseOpsworksSecurityGroups :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Bool)
usUseOpsworksSecurityGroups = Lens.lens (useOpsworksSecurityGroups :: UpdateStack -> Lude.Maybe Lude.Bool) (\s a -> s {useOpsworksSecurityGroups = a} :: UpdateStack)
{-# DEPRECATED usUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUseCustomCookbooks :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Bool)
usUseCustomCookbooks = Lens.lens (useCustomCookbooks :: UpdateStack -> Lude.Maybe Lude.Bool) (\s a -> s {useCustomCookbooks = a} :: UpdateStack)
{-# DEPRECATED usUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | The stack's default VPC subnet ID. This parameter is required if you specify a value for the @VpcId@ parameter. All instances are launched into this subnet unless you specify otherwise when you create the instance. If you also specify a value for @DefaultAvailabilityZone@ , the subnet must be in that zone. For information on default values and when this parameter is required, see the @VpcId@ parameter description.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDefaultSubnetId :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usDefaultSubnetId = Lens.lens (defaultSubnetId :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubnetId = a} :: UpdateStack)
{-# DEPRECATED usDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The configuration manager. When you update a stack, we recommend that you use the configuration manager to specify the Chef version: 12, 11.10, or 11.4 for Linux stacks, or 12.2 for Windows stacks. The default value for Linux stacks is currently 12.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usConfigurationManager :: Lens.Lens' UpdateStack (Lude.Maybe StackConfigurationManager)
usConfigurationManager = Lens.lens (configurationManager :: UpdateStack -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: UpdateStack)
{-# DEPRECATED usConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackId :: Lens.Lens' UpdateStack Lude.Text
usStackId = Lens.lens (stackId :: UpdateStack -> Lude.Text) (\s a -> s {stackId = a} :: UpdateStack)
{-# DEPRECATED usStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

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
usHostnameTheme :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usHostnameTheme = Lens.lens (hostnameTheme :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {hostnameTheme = a} :: UpdateStack)
{-# DEPRECATED usHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

instance Lude.AWSRequest UpdateStack where
  type Rs UpdateStack = UpdateStackResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateStackResponse'

instance Lude.ToHeaders UpdateStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateStack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateStack where
  toJSON UpdateStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultInstanceProfileArn" Lude..=)
              Lude.<$> defaultInstanceProfileARN,
            ("ServiceRoleArn" Lude..=) Lude.<$> serviceRoleARN,
            ("DefaultRootDeviceType" Lude..=) Lude.<$> defaultRootDeviceType,
            ("ChefConfiguration" Lude..=) Lude.<$> chefConfiguration,
            ("AgentVersion" Lude..=) Lude.<$> agentVersion,
            ("DefaultSshKeyName" Lude..=) Lude.<$> defaultSSHKeyName,
            ("CustomJson" Lude..=) Lude.<$> customJSON,
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
            ("ConfigurationManager" Lude..=) Lude.<$> configurationManager,
            Lude.Just ("StackId" Lude..= stackId),
            ("HostnameTheme" Lude..=) Lude.<$> hostnameTheme
          ]
      )

instance Lude.ToPath UpdateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
mkUpdateStackResponse ::
  UpdateStackResponse
mkUpdateStackResponse = UpdateStackResponse'
