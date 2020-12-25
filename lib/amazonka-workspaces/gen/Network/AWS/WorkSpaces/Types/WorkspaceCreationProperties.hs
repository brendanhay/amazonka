{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
  ( WorkspaceCreationProperties (..),

    -- * Smart constructor
    mkWorkspaceCreationProperties,

    -- * Lenses
    wcpCustomSecurityGroupId,
    wcpDefaultOu,
    wcpEnableInternetAccess,
    wcpEnableMaintenanceMode,
    wcpEnableWorkDocs,
    wcpUserEnabledAsLocalAdministrator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.DefaultOu as Types
import qualified Network.AWS.WorkSpaces.Types.SecurityGroupId as Types

-- | Describes the default properties that are used for creating WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> .
--
-- /See:/ 'mkWorkspaceCreationProperties' smart constructor.
data WorkspaceCreationProperties = WorkspaceCreationProperties'
  { -- | The identifier of your custom security group.
    customSecurityGroupId :: Core.Maybe Types.SecurityGroupId,
    -- | The default organizational unit (OU) for your WorkSpaces directories. This string must be the full Lightweight Directory Access Protocol (LDAP) distinguished name for the target domain and OU. It must be in the form @"OU=/value/ ,DC=/value/ ,DC=/value/ "@ , where /value/ is any string of characters, and the number of domain components (DCs) is two or more. For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@ .
    --
    -- /Important:/
    --     * To avoid errors, certain characters in the distinguished name must be escaped. For more information, see <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names> in the Microsoft documentation.
    --
    --
    --     * The API doesn't validate whether the OU exists.
    defaultOu :: Core.Maybe Types.DefaultOu,
    -- | Indicates whether internet access is enabled for your WorkSpaces.
    enableInternetAccess :: Core.Maybe Core.Bool,
    -- | Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
    enableMaintenanceMode :: Core.Maybe Core.Bool,
    -- | Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
    enableWorkDocs :: Core.Maybe Core.Bool,
    -- | Indicates whether users are local administrators of their WorkSpaces.
    userEnabledAsLocalAdministrator :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkspaceCreationProperties' value with any optional fields omitted.
mkWorkspaceCreationProperties ::
  WorkspaceCreationProperties
mkWorkspaceCreationProperties =
  WorkspaceCreationProperties'
    { customSecurityGroupId =
        Core.Nothing,
      defaultOu = Core.Nothing,
      enableInternetAccess = Core.Nothing,
      enableMaintenanceMode = Core.Nothing,
      enableWorkDocs = Core.Nothing,
      userEnabledAsLocalAdministrator = Core.Nothing
    }

-- | The identifier of your custom security group.
--
-- /Note:/ Consider using 'customSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpCustomSecurityGroupId :: Lens.Lens' WorkspaceCreationProperties (Core.Maybe Types.SecurityGroupId)
wcpCustomSecurityGroupId = Lens.field @"customSecurityGroupId"
{-# DEPRECATED wcpCustomSecurityGroupId "Use generic-lens or generic-optics with 'customSecurityGroupId' instead." #-}

-- | The default organizational unit (OU) for your WorkSpaces directories. This string must be the full Lightweight Directory Access Protocol (LDAP) distinguished name for the target domain and OU. It must be in the form @"OU=/value/ ,DC=/value/ ,DC=/value/ "@ , where /value/ is any string of characters, and the number of domain components (DCs) is two or more. For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@ .
--
-- /Important:/
--     * To avoid errors, certain characters in the distinguished name must be escaped. For more information, see <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names> in the Microsoft documentation.
--
--
--     * The API doesn't validate whether the OU exists.
--
--
--
-- /Note:/ Consider using 'defaultOu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpDefaultOu :: Lens.Lens' WorkspaceCreationProperties (Core.Maybe Types.DefaultOu)
wcpDefaultOu = Lens.field @"defaultOu"
{-# DEPRECATED wcpDefaultOu "Use generic-lens or generic-optics with 'defaultOu' instead." #-}

-- | Indicates whether internet access is enabled for your WorkSpaces.
--
-- /Note:/ Consider using 'enableInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpEnableInternetAccess :: Lens.Lens' WorkspaceCreationProperties (Core.Maybe Core.Bool)
wcpEnableInternetAccess = Lens.field @"enableInternetAccess"
{-# DEPRECATED wcpEnableInternetAccess "Use generic-lens or generic-optics with 'enableInternetAccess' instead." #-}

-- | Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
--
-- /Note:/ Consider using 'enableMaintenanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpEnableMaintenanceMode :: Lens.Lens' WorkspaceCreationProperties (Core.Maybe Core.Bool)
wcpEnableMaintenanceMode = Lens.field @"enableMaintenanceMode"
{-# DEPRECATED wcpEnableMaintenanceMode "Use generic-lens or generic-optics with 'enableMaintenanceMode' instead." #-}

-- | Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
--
-- /Note:/ Consider using 'enableWorkDocs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpEnableWorkDocs :: Lens.Lens' WorkspaceCreationProperties (Core.Maybe Core.Bool)
wcpEnableWorkDocs = Lens.field @"enableWorkDocs"
{-# DEPRECATED wcpEnableWorkDocs "Use generic-lens or generic-optics with 'enableWorkDocs' instead." #-}

-- | Indicates whether users are local administrators of their WorkSpaces.
--
-- /Note:/ Consider using 'userEnabledAsLocalAdministrator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpUserEnabledAsLocalAdministrator :: Lens.Lens' WorkspaceCreationProperties (Core.Maybe Core.Bool)
wcpUserEnabledAsLocalAdministrator = Lens.field @"userEnabledAsLocalAdministrator"
{-# DEPRECATED wcpUserEnabledAsLocalAdministrator "Use generic-lens or generic-optics with 'userEnabledAsLocalAdministrator' instead." #-}

instance Core.FromJSON WorkspaceCreationProperties where
  toJSON WorkspaceCreationProperties {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomSecurityGroupId" Core..=) Core.<$> customSecurityGroupId,
            ("DefaultOu" Core..=) Core.<$> defaultOu,
            ("EnableInternetAccess" Core..=) Core.<$> enableInternetAccess,
            ("EnableMaintenanceMode" Core..=) Core.<$> enableMaintenanceMode,
            ("EnableWorkDocs" Core..=) Core.<$> enableWorkDocs,
            ("UserEnabledAsLocalAdministrator" Core..=)
              Core.<$> userEnabledAsLocalAdministrator
          ]
      )
