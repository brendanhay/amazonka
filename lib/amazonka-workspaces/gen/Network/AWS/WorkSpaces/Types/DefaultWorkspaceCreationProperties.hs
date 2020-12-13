{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
  ( DefaultWorkspaceCreationProperties (..),

    -- * Smart constructor
    mkDefaultWorkspaceCreationProperties,

    -- * Lenses
    dwcpCustomSecurityGroupId,
    dwcpUserEnabledAsLocalAdministrator,
    dwcpEnableWorkDocs,
    dwcpEnableMaintenanceMode,
    dwcpEnableInternetAccess,
    dwcpDefaultOu,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the default values that are used to create WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> .
--
-- /See:/ 'mkDefaultWorkspaceCreationProperties' smart constructor.
data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'
  { -- | The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
    customSecurityGroupId :: Lude.Maybe Lude.Text,
    -- | Specifies whether WorkSpace users are local administrators on their WorkSpaces.
    userEnabledAsLocalAdministrator :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the directory is enabled for Amazon WorkDocs.
    enableWorkDocs :: Lude.Maybe Lude.Bool,
    -- | Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
    enableMaintenanceMode :: Lude.Maybe Lude.Bool,
    -- | Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
    enableInternetAccess :: Lude.Maybe Lude.Bool,
    -- | The organizational unit (OU) in the directory for the WorkSpace machine accounts.
    defaultOu :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefaultWorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- * 'customSecurityGroupId' - The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
-- * 'userEnabledAsLocalAdministrator' - Specifies whether WorkSpace users are local administrators on their WorkSpaces.
-- * 'enableWorkDocs' - Specifies whether the directory is enabled for Amazon WorkDocs.
-- * 'enableMaintenanceMode' - Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
-- * 'enableInternetAccess' - Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
-- * 'defaultOu' - The organizational unit (OU) in the directory for the WorkSpace machine accounts.
mkDefaultWorkspaceCreationProperties ::
  DefaultWorkspaceCreationProperties
mkDefaultWorkspaceCreationProperties =
  DefaultWorkspaceCreationProperties'
    { customSecurityGroupId =
        Lude.Nothing,
      userEnabledAsLocalAdministrator = Lude.Nothing,
      enableWorkDocs = Lude.Nothing,
      enableMaintenanceMode = Lude.Nothing,
      enableInternetAccess = Lude.Nothing,
      defaultOu = Lude.Nothing
    }

-- | The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
--
-- /Note:/ Consider using 'customSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpCustomSecurityGroupId :: Lens.Lens' DefaultWorkspaceCreationProperties (Lude.Maybe Lude.Text)
dwcpCustomSecurityGroupId = Lens.lens (customSecurityGroupId :: DefaultWorkspaceCreationProperties -> Lude.Maybe Lude.Text) (\s a -> s {customSecurityGroupId = a} :: DefaultWorkspaceCreationProperties)
{-# DEPRECATED dwcpCustomSecurityGroupId "Use generic-lens or generic-optics with 'customSecurityGroupId' instead." #-}

-- | Specifies whether WorkSpace users are local administrators on their WorkSpaces.
--
-- /Note:/ Consider using 'userEnabledAsLocalAdministrator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpUserEnabledAsLocalAdministrator :: Lens.Lens' DefaultWorkspaceCreationProperties (Lude.Maybe Lude.Bool)
dwcpUserEnabledAsLocalAdministrator = Lens.lens (userEnabledAsLocalAdministrator :: DefaultWorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {userEnabledAsLocalAdministrator = a} :: DefaultWorkspaceCreationProperties)
{-# DEPRECATED dwcpUserEnabledAsLocalAdministrator "Use generic-lens or generic-optics with 'userEnabledAsLocalAdministrator' instead." #-}

-- | Specifies whether the directory is enabled for Amazon WorkDocs.
--
-- /Note:/ Consider using 'enableWorkDocs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpEnableWorkDocs :: Lens.Lens' DefaultWorkspaceCreationProperties (Lude.Maybe Lude.Bool)
dwcpEnableWorkDocs = Lens.lens (enableWorkDocs :: DefaultWorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {enableWorkDocs = a} :: DefaultWorkspaceCreationProperties)
{-# DEPRECATED dwcpEnableWorkDocs "Use generic-lens or generic-optics with 'enableWorkDocs' instead." #-}

-- | Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
--
-- /Note:/ Consider using 'enableMaintenanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpEnableMaintenanceMode :: Lens.Lens' DefaultWorkspaceCreationProperties (Lude.Maybe Lude.Bool)
dwcpEnableMaintenanceMode = Lens.lens (enableMaintenanceMode :: DefaultWorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {enableMaintenanceMode = a} :: DefaultWorkspaceCreationProperties)
{-# DEPRECATED dwcpEnableMaintenanceMode "Use generic-lens or generic-optics with 'enableMaintenanceMode' instead." #-}

-- | Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
--
-- /Note:/ Consider using 'enableInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpEnableInternetAccess :: Lens.Lens' DefaultWorkspaceCreationProperties (Lude.Maybe Lude.Bool)
dwcpEnableInternetAccess = Lens.lens (enableInternetAccess :: DefaultWorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {enableInternetAccess = a} :: DefaultWorkspaceCreationProperties)
{-# DEPRECATED dwcpEnableInternetAccess "Use generic-lens or generic-optics with 'enableInternetAccess' instead." #-}

-- | The organizational unit (OU) in the directory for the WorkSpace machine accounts.
--
-- /Note:/ Consider using 'defaultOu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpDefaultOu :: Lens.Lens' DefaultWorkspaceCreationProperties (Lude.Maybe Lude.Text)
dwcpDefaultOu = Lens.lens (defaultOu :: DefaultWorkspaceCreationProperties -> Lude.Maybe Lude.Text) (\s a -> s {defaultOu = a} :: DefaultWorkspaceCreationProperties)
{-# DEPRECATED dwcpDefaultOu "Use generic-lens or generic-optics with 'defaultOu' instead." #-}

instance Lude.FromJSON DefaultWorkspaceCreationProperties where
  parseJSON =
    Lude.withObject
      "DefaultWorkspaceCreationProperties"
      ( \x ->
          DefaultWorkspaceCreationProperties'
            Lude.<$> (x Lude..:? "CustomSecurityGroupId")
            Lude.<*> (x Lude..:? "UserEnabledAsLocalAdministrator")
            Lude.<*> (x Lude..:? "EnableWorkDocs")
            Lude.<*> (x Lude..:? "EnableMaintenanceMode")
            Lude.<*> (x Lude..:? "EnableInternetAccess")
            Lude.<*> (x Lude..:? "DefaultOu")
      )
