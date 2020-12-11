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
    sDefaultInstanceProfileARN,
    sServiceRoleARN,
    sDefaultRootDeviceType,
    sARN,
    sCreatedAt,
    sVPCId,
    sChefConfiguration,
    sAgentVersion,
    sDefaultSSHKeyName,
    sCustomJSON,
    sCustomCookbooksSource,
    sDefaultAvailabilityZone,
    sAttributes,
    sName,
    sDefaultOS,
    sUseOpsworksSecurityGroups,
    sUseCustomCookbooks,
    sDefaultSubnetId,
    sRegion,
    sConfigurationManager,
    sStackId,
    sHostnameTheme,
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
  { defaultInstanceProfileARN ::
      Lude.Maybe Lude.Text,
    serviceRoleARN :: Lude.Maybe Lude.Text,
    defaultRootDeviceType :: Lude.Maybe RootDeviceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    chefConfiguration :: Lude.Maybe ChefConfiguration,
    agentVersion :: Lude.Maybe Lude.Text,
    defaultSSHKeyName :: Lude.Maybe Lude.Text,
    customJSON :: Lude.Maybe Lude.Text,
    customCookbooksSource :: Lude.Maybe Source,
    defaultAvailabilityZone :: Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)),
    name :: Lude.Maybe Lude.Text,
    defaultOS :: Lude.Maybe Lude.Text,
    useOpsworksSecurityGroups :: Lude.Maybe Lude.Bool,
    useCustomCookbooks :: Lude.Maybe Lude.Bool,
    defaultSubnetId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    configurationManager :: Lude.Maybe StackConfigurationManager,
    stackId :: Lude.Maybe Lude.Text,
    hostnameTheme :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- * 'agentVersion' - The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
-- * 'arn' - The stack's ARN.
-- * 'attributes' - The stack's attributes.
-- * 'chefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
-- * 'configurationManager' - The configuration manager.
-- * 'createdAt' - The date when the stack was created.
-- * 'customCookbooksSource' - Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
-- * 'customJSON' - A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
-- * 'defaultAvailabilityZone' - The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'defaultInstanceProfileARN' - The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'defaultOS' - The stack's default operating system.
-- * 'defaultRootDeviceType' - The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
-- * 'defaultSSHKeyName' - A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
-- * 'defaultSubnetId' - The default subnet ID; applicable only if the stack is running in a VPC.
-- * 'hostnameTheme' - The stack host name theme, with spaces replaced by underscores.
-- * 'name' - The stack name.
-- * 'region' - The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
-- * 'serviceRoleARN' - The stack AWS Identity and Access Management (IAM) role.
-- * 'stackId' - The stack ID.
-- * 'useCustomCookbooks' - Whether the stack uses custom cookbooks.
-- * 'useOpsworksSecurityGroups' - Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
-- * 'vpcId' - The VPC ID; applicable only if the stack is running in a VPC.
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
sDefaultInstanceProfileARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDefaultInstanceProfileARN = Lens.lens (defaultInstanceProfileARN :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultInstanceProfileARN = a} :: Stack)
{-# DEPRECATED sDefaultInstanceProfileARN "Use generic-lens or generic-optics with 'defaultInstanceProfileARN' instead." #-}

-- | The stack AWS Identity and Access Management (IAM) role.
--
-- /Note:/ Consider using 'serviceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServiceRoleARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sServiceRoleARN = Lens.lens (serviceRoleARN :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleARN = a} :: Stack)
{-# DEPRECATED sServiceRoleARN "Use generic-lens or generic-optics with 'serviceRoleARN' instead." #-}

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- /Note:/ Consider using 'defaultRootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultRootDeviceType :: Lens.Lens' Stack (Lude.Maybe RootDeviceType)
sDefaultRootDeviceType = Lens.lens (defaultRootDeviceType :: Stack -> Lude.Maybe RootDeviceType) (\s a -> s {defaultRootDeviceType = a} :: Stack)
{-# DEPRECATED sDefaultRootDeviceType "Use generic-lens or generic-optics with 'defaultRootDeviceType' instead." #-}

-- | The stack's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sARN = Lens.lens (arn :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Stack)
{-# DEPRECATED sARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the stack was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedAt :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sCreatedAt = Lens.lens (createdAt :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: Stack)
{-# DEPRECATED sCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The VPC ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVPCId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sVPCId = Lens.lens (vpcId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: Stack)
{-# DEPRECATED sVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- /Note:/ Consider using 'chefConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sChefConfiguration :: Lens.Lens' Stack (Lude.Maybe ChefConfiguration)
sChefConfiguration = Lens.lens (chefConfiguration :: Stack -> Lude.Maybe ChefConfiguration) (\s a -> s {chefConfiguration = a} :: Stack)
{-# DEPRECATED sChefConfiguration "Use generic-lens or generic-optics with 'chefConfiguration' instead." #-}

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAgentVersion :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sAgentVersion = Lens.lens (agentVersion :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: Stack)
{-# DEPRECATED sAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
--
-- /Note:/ Consider using 'defaultSSHKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultSSHKeyName :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDefaultSSHKeyName = Lens.lens (defaultSSHKeyName :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSSHKeyName = a} :: Stack)
{-# DEPRECATED sDefaultSSHKeyName "Use generic-lens or generic-optics with 'defaultSSHKeyName' instead." #-}

-- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format:
--
-- @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@
-- For more information on custom JSON, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- /Note:/ Consider using 'customJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomJSON :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sCustomJSON = Lens.lens (customJSON :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {customJSON = a} :: Stack)
{-# DEPRECATED sCustomJSON "Use generic-lens or generic-optics with 'customJSON' instead." #-}

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Adding Apps> or <https://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Cookbooks and Recipes> .
--
-- /Note:/ Consider using 'customCookbooksSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomCookbooksSource :: Lens.Lens' Stack (Lude.Maybe Source)
sCustomCookbooksSource = Lens.lens (customCookbooksSource :: Stack -> Lude.Maybe Source) (\s a -> s {customCookbooksSource = a} :: Stack)
{-# DEPRECATED sCustomCookbooksSource "Use generic-lens or generic-optics with 'customCookbooksSource' instead." #-}

-- | The stack's default Availability Zone. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'defaultAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultAvailabilityZone :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDefaultAvailabilityZone = Lens.lens (defaultAvailabilityZone :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultAvailabilityZone = a} :: Stack)
{-# DEPRECATED sDefaultAvailabilityZone "Use generic-lens or generic-optics with 'defaultAvailabilityZone' instead." #-}

-- | The stack's attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAttributes :: Lens.Lens' Stack (Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text)))
sAttributes = Lens.lens (attributes :: Stack -> Lude.Maybe (Lude.HashMap StackAttributesKeys (Maybe Text))) (\s a -> s {attributes = a} :: Stack)
{-# DEPRECATED sAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Stack)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack's default operating system.
--
-- /Note:/ Consider using 'defaultOS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultOS :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDefaultOS = Lens.lens (defaultOS :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultOS = a} :: Stack)
{-# DEPRECATED sDefaultOS "Use generic-lens or generic-optics with 'defaultOS' instead." #-}

-- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- /Note:/ Consider using 'useOpsworksSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUseOpsworksSecurityGroups :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
sUseOpsworksSecurityGroups = Lens.lens (useOpsworksSecurityGroups :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {useOpsworksSecurityGroups = a} :: Stack)
{-# DEPRECATED sUseOpsworksSecurityGroups "Use generic-lens or generic-optics with 'useOpsworksSecurityGroups' instead." #-}

-- | Whether the stack uses custom cookbooks.
--
-- /Note:/ Consider using 'useCustomCookbooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUseCustomCookbooks :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
sUseCustomCookbooks = Lens.lens (useCustomCookbooks :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {useCustomCookbooks = a} :: Stack)
{-# DEPRECATED sUseCustomCookbooks "Use generic-lens or generic-optics with 'useCustomCookbooks' instead." #-}

-- | The default subnet ID; applicable only if the stack is running in a VPC.
--
-- /Note:/ Consider using 'defaultSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultSubnetId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sDefaultSubnetId = Lens.lens (defaultSubnetId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {defaultSubnetId = a} :: Stack)
{-# DEPRECATED sDefaultSubnetId "Use generic-lens or generic-optics with 'defaultSubnetId' instead." #-}

-- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRegion :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sRegion = Lens.lens (region :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Stack)
{-# DEPRECATED sRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfigurationManager :: Lens.Lens' Stack (Lude.Maybe StackConfigurationManager)
sConfigurationManager = Lens.lens (configurationManager :: Stack -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: Stack)
{-# DEPRECATED sConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sStackId = Lens.lens (stackId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Stack)
{-# DEPRECATED sStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The stack host name theme, with spaces replaced by underscores.
--
-- /Note:/ Consider using 'hostnameTheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sHostnameTheme :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sHostnameTheme = Lens.lens (hostnameTheme :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {hostnameTheme = a} :: Stack)
{-# DEPRECATED sHostnameTheme "Use generic-lens or generic-optics with 'hostnameTheme' instead." #-}

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
