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
    sfDefaultInstanceProfileARN,
    sfServiceRoleARN,
    sfDefaultRootDeviceType,
    sfARN,
    sfCreatedAt,
    sfVPCId,
    sfChefConfiguration,
    sfAgentVersion,
    sfDefaultSSHKeyName,
    sfCustomJSON,
    sfCustomCookbooksSource,
    sfDefaultAvailabilityZone,
    sfAttributes,
    sfName,
    sfDefaultOS,
    sfUseOpsworksSecurityGroups,
    sfUseCustomCookbooks,
    sfDefaultSubnetId,
    sfRegion,
    sfConfigurationManager,
    sfStackId,
    sfHostnameTheme,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.RootDeviceType
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.StackAttributesKeys
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import qualified Network.AWS.Prelude as Lude

-- | Describes a stack.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { -- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    defaultInstanceProfileARN :: Lude.Maybe Lude.Text,
    -- | The stack AWS Identity and Access Management (IAM) role.
    serviceRoleARN :: Lude.Maybe Lude.Text,
    -- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
    defaultRootDeviceType :: Lude.Maybe RootDeviceType,
    -- | The stack's ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | The date when the stack was created.
    createdAt :: Lude.Maybe Lude.Text,
    -- | The VPC ID; applicable only if the stack is running in a VPC.
    vpcId :: Lude.Maybe Lude.Text,
    -- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
    chefConfiguration :: Lude.Maybe ChefConfiguration,
    -- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
    agentVersion :: Lude.Maybe Lude.Text,
    -- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
    defaultSSHKeyName :: Lude.Maybe Lude.Text,
    -- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
    --
    -- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
    -- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
    customJSON :: Lude.Maybe Lude.Text,
    -- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
    customCookbooksSource :: Lude.Maybe Source,
    -- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    defaultAvailabilityZone :: Lude.Maybe Lude.Text,
    -- | The stack's attributes.
    attributes :: Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)),
    -- | The stack name.
    name :: Lude.Maybe Lude.Text,
    -- | The stack's default operating system.
    defaultOS :: Lude.Maybe Lude.Text,
    -- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
    useOpsworksSecurityGroups :: Lude.Maybe Lude.Bool,
    -- | Whether the stack uses custom cookbooks.
    useCustomCookbooks :: Lude.Maybe Lude.Bool,
    -- | The default subnet ID; applicable only if the stack is running in a VPC.
    defaultSubnetId :: Lude.Maybe Lude.Text,
    -- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
    region :: Lude.Maybe Lude.Text,
    -- | The configuration manager.
    configurationManager :: Lude.Maybe StackConfigurationManager,
    -- | The stack ID.
    stackId :: Lude.Maybe Lude.Text,
    -- | The stack host name theme, with spaces replaced by underscores.
    hostnameTheme :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- * 'defaultInstanceProfileARN' - The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'serviceRoleARN' - The stack AWS Identity and Access Management (IAM) role.
-- * 'defaultRootDeviceType' - The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
-- * 'arn' - The stack's ARN.
-- * 'createdAt' - The date when the stack was created.
-- * 'vpcId' - The VPC ID; applicable only if the stack is running in a VPC.
-- * 'chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
-- * 'agentVersion' - The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
-- * 'defaultSSHKeyName' - A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
-- * 'customJSON' - A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
-- * 'customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
-- * 'defaultAvailabilityZone' - The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'attributes' - The stack's attributes.
-- * 'name' - The stack name.
-- * 'defaultOS' - The stack's default operating system.
-- * 'useOpsworksSecurityGroups' - Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
-- * 'useCustomCookbooks' - Whether the stack uses custom cookbooks.
-- * 'defaultSubnetId' - The default subnet ID; applicable only if the stack is running in a VPC.
-- * 'region' - The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'configurationManager' - The configuration manager.
-- * 'stackId' - The stack ID.
-- * 'hostnameTheme' - The stack host name theme, with spaces replaced by underscores.
mkStack ::
  Stack
mkStack =
  Stack'
    { defaultInstanceProfileARN = Lude.Nothing,
      serviceRoleARN = Lude.Nothing,
      defaultRootDeviceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      vpcId = Lude.Nothing,
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
      region = Lude.Nothing,
      configurationManager = Lude.Nothing,
      stackId = Lude.Nothing,
      hostnameTheme = Lude.Nothing
    }

-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'defaultInstanceProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultInstanceProfileARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfDefaultInstanceProfileARN = Lens.lens (defaultInstanceProfileARN :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultInstanceProfileARN = a} :: Stack)
{-# DEPRECATED sfDefaultInstanceProfileARN "Use generic-lens or generic-optics with 'defaultInstanceProfileARN' instead." #-}

-- | The stack AWS Identity and Access Management (IAM) role.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfServiceRoleARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfServiceRoleARN = Lens.lens (serviceRoleARN :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: Stack)
{-# DEPRECATED sfServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultRootDeviceType :: Lens.Lens' Stack (Lude.Maybe RootDeviceType)
sfDefaultRootDeviceType = Lens.lens (defaultRootDeviceType :: Stack -> Lude.Maybe RootDeviceType) (\s a -> s {defaultRootDeviceType = a} :: Stack)
{-# DEPRECATED sfDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

-- | The stack's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfARN = Lens.lens (arn :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Stack)
{-# DEPRECATED sfARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the stack was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCreatedAt :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfCreatedAt = Lens.lens (createdAt :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Stack)
{-# DEPRECATED sfCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The VPC ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfVPCId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfVPCId = Lens.lens (vpcId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Stack)
{-# DEPRECATED sfVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfChefConfiguration :: Lens.Lens' Stack (Lude.Maybe ChefConfiguration)
sfChefConfiguration = Lens.lens (chefConfiguration :: Stack -> Lude.Maybe ChefConfiguration) (\s a -> s {chefConfiguration = a} :: Stack)
{-# DEPRECATED sfChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAgentVersion :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfAgentVersion = Lens.lens (agentVersion :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: Stack)
{-# DEPRECATED sfAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
--
-- /Note:/ Consider using 'defaultSSHKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultSSHKeyName :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfDefaultSSHKeyName = Lens.lens (defaultSSHKeyName :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSSHKeyName = a} :: Stack)
{-# DEPRECATED sfDefaultSSHKeyName "Use generic-lens or generic-optics with 'defaultSSHKeyName' instead." #-}

-- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCustomJSON :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfCustomJSON = Lens.lens (customJSON :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: Stack)
{-# DEPRECATED sfCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCustomCookbooksSource :: Lens.Lens' Stack (Lude.Maybe Source)
sfCustomCookbooksSource = Lens.lens (customCookbooksSource :: Stack -> Lude.Maybe Source) (\s a -> s {customCookbooksSource = a} :: Stack)
{-# DEPRECATED sfCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultAvailabilityZone :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfDefaultAvailabilityZone = Lens.lens (defaultAvailabilityZone :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultAvailabilityZone = a} :: Stack)
{-# DEPRECATED sfDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | The stack's attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAttributes :: Lens.Lens' Stack (Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)))
sfAttributes = Lens.lens (attributes :: Stack -> Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: Stack)
{-# DEPRECATED sfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfName = Lens.lens (name :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Stack)
{-# DEPRECATED sfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack's default operating system.
--
-- /Note:/ Consider using 'defaultOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultOS :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfDefaultOS = Lens.lens (defaultOS :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultOS = a} :: Stack)
{-# DEPRECATED sfDefaultOS "Use generic-lens or generic-optics with 'defaultOS' instead." #-}

-- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfUseOpsworksSecurityGroups :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
sfUseOpsworksSecurityGroups = Lens.lens (useOpsworksSecurityGroups :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {useOpsworksSecurityGroups = a} :: Stack)
{-# DEPRECATED sfUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfUseCustomCookbooks :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
sfUseCustomCookbooks = Lens.lens (useCustomCookbooks :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {useCustomCookbooks = a} :: Stack)
{-# DEPRECATED sfUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | The default subnet ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultSubnetId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfDefaultSubnetId = Lens.lens (defaultSubnetId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubnetId = a} :: Stack)
{-# DEPRECATED sfDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRegion :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfRegion = Lens.lens (region :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Stack)
{-# DEPRECATED sfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfConfigurationManager :: Lens.Lens' Stack (Lude.Maybe StackConfigurationManager)
sfConfigurationManager = Lens.lens (configurationManager :: Stack -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: Stack)
{-# DEPRECATED sfConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfStackId = Lens.lens (stackId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Stack)
{-# DEPRECATED sfStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The stack host name theme, with spaces replaced by underscores.
--
-- /Note:/ Consider using 'hostnameTheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfHostnameTheme :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfHostnameTheme = Lens.lens (hostnameTheme :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {hostnameTheme = a} :: Stack)
{-# DEPRECATED sfHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

instance Lude.FromJSON Stack where
  parseJSON =
    Lude.withObject
      "Stack"
      ( \x ->
          Stack'
            Lude.<$> (x Lude..:? "DefaultInstanceProfileArn")
            Lude.<*> (x Lude..:? "ServiceRoleArn")
            Lude.<*> (x Lude..:? "DefaultRootDeviceType")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "ChefConfiguration")
            Lude.<*> (x Lude..:? "AgentVersion")
            Lude.<*> (x Lude..:? "DefaultSshKeyName")
            Lude.<*> (x Lude..:? "CustomJson")
            Lude.<*> (x Lude..:? "CustomCookbooksSource")
            Lude.<*> (x Lude..:? "DefaultAvailabilityZone")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DefaultOs")
            Lude.<*> (x Lude..:? "UseOpsworksSecurityGroups")
            Lude.<*> (x Lude..:? "UseCustomCookbooks")
            Lude.<*> (x Lude..:? "DefaultSubnetId")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "ConfigurationManager")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "HostnameTheme")
      )
