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
    dwcpDefaultOu,
    dwcpEnableInternetAccess,
    dwcpEnableMaintenanceMode,
    dwcpEnableWorkDocs,
    dwcpUserEnabledAsLocalAdministrator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.DefaultOu as Types
import qualified Network.AWS.WorkSpaces.Types.SecurityGroupId as Types

-- | Describes the default values that are used to create WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> .
--
-- /See:/ 'mkDefaultWorkspaceCreationProperties' smart constructor.
data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'
  { -- | The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
    customSecurityGroupId :: Core.Maybe Types.SecurityGroupId,
    -- | The organizational unit (OU) in the directory for the WorkSpace machine accounts.
    defaultOu :: Core.Maybe Types.DefaultOu,
    -- | Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
    enableInternetAccess :: Core.Maybe Core.Bool,
    -- | Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
    enableMaintenanceMode :: Core.Maybe Core.Bool,
    -- | Specifies whether the directory is enabled for Amazon WorkDocs.
    enableWorkDocs :: Core.Maybe Core.Bool,
    -- | Specifies whether WorkSpace users are local administrators on their WorkSpaces.
    userEnabledAsLocalAdministrator :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultWorkspaceCreationProperties' value with any optional fields omitted.
mkDefaultWorkspaceCreationProperties ::
  DefaultWorkspaceCreationProperties
mkDefaultWorkspaceCreationProperties =
  DefaultWorkspaceCreationProperties'
    { customSecurityGroupId =
        Core.Nothing,
      defaultOu = Core.Nothing,
      enableInternetAccess = Core.Nothing,
      enableMaintenanceMode = Core.Nothing,
      enableWorkDocs = Core.Nothing,
      userEnabledAsLocalAdministrator = Core.Nothing
    }

-- | The identifier of the default security group to apply to WorkSpaces when they are created. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-security-groups.html Security Groups for Your WorkSpaces> .
--
-- /Note:/ Consider using 'customSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpCustomSecurityGroupId :: Lens.Lens' DefaultWorkspaceCreationProperties (Core.Maybe Types.SecurityGroupId)
dwcpCustomSecurityGroupId = Lens.field @"customSecurityGroupId"
{-# DEPRECATED dwcpCustomSecurityGroupId "Use generic-lens or generic-optics with 'customSecurityGroupId' instead." #-}

-- | The organizational unit (OU) in the directory for the WorkSpace machine accounts.
--
-- /Note:/ Consider using 'defaultOu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpDefaultOu :: Lens.Lens' DefaultWorkspaceCreationProperties (Core.Maybe Types.DefaultOu)
dwcpDefaultOu = Lens.field @"defaultOu"
{-# DEPRECATED dwcpDefaultOu "Use generic-lens or generic-optics with 'defaultOu' instead." #-}

-- | Specifies whether to automatically assign an Elastic public IP address to WorkSpaces in this directory by default. If enabled, the Elastic public IP address allows outbound internet access from your WorkSpaces when you’re using an internet gateway in the Amazon VPC in which your WorkSpaces are located. If you're using a Network Address Translation (NAT) gateway for outbound internet access from your VPC, or if your WorkSpaces are in public subnets and you manually assign them Elastic IP addresses, you should disable this setting. This setting applies to new WorkSpaces that you launch or to existing WorkSpaces that you rebuild. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
--
-- /Note:/ Consider using 'enableInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpEnableInternetAccess :: Lens.Lens' DefaultWorkspaceCreationProperties (Core.Maybe Core.Bool)
dwcpEnableInternetAccess = Lens.field @"enableInternetAccess"
{-# DEPRECATED dwcpEnableInternetAccess "Use generic-lens or generic-optics with 'enableInternetAccess' instead." #-}

-- | Specifies whether maintenance mode is enabled for WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
--
-- /Note:/ Consider using 'enableMaintenanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpEnableMaintenanceMode :: Lens.Lens' DefaultWorkspaceCreationProperties (Core.Maybe Core.Bool)
dwcpEnableMaintenanceMode = Lens.field @"enableMaintenanceMode"
{-# DEPRECATED dwcpEnableMaintenanceMode "Use generic-lens or generic-optics with 'enableMaintenanceMode' instead." #-}

-- | Specifies whether the directory is enabled for Amazon WorkDocs.
--
-- /Note:/ Consider using 'enableWorkDocs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpEnableWorkDocs :: Lens.Lens' DefaultWorkspaceCreationProperties (Core.Maybe Core.Bool)
dwcpEnableWorkDocs = Lens.field @"enableWorkDocs"
{-# DEPRECATED dwcpEnableWorkDocs "Use generic-lens or generic-optics with 'enableWorkDocs' instead." #-}

-- | Specifies whether WorkSpace users are local administrators on their WorkSpaces.
--
-- /Note:/ Consider using 'userEnabledAsLocalAdministrator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcpUserEnabledAsLocalAdministrator :: Lens.Lens' DefaultWorkspaceCreationProperties (Core.Maybe Core.Bool)
dwcpUserEnabledAsLocalAdministrator = Lens.field @"userEnabledAsLocalAdministrator"
{-# DEPRECATED dwcpUserEnabledAsLocalAdministrator "Use generic-lens or generic-optics with 'userEnabledAsLocalAdministrator' instead." #-}

instance Core.FromJSON DefaultWorkspaceCreationProperties where
  parseJSON =
    Core.withObject "DefaultWorkspaceCreationProperties" Core.$
      \x ->
        DefaultWorkspaceCreationProperties'
          Core.<$> (x Core..:? "CustomSecurityGroupId")
          Core.<*> (x Core..:? "DefaultOu")
          Core.<*> (x Core..:? "EnableInternetAccess")
          Core.<*> (x Core..:? "EnableMaintenanceMode")
          Core.<*> (x Core..:? "EnableWorkDocs")
          Core.<*> (x Core..:? "UserEnabledAsLocalAdministrator")
