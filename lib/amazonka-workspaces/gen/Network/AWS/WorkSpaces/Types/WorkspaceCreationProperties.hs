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
    wcpUserEnabledAsLocalAdministrator,
    wcpEnableWorkDocs,
    wcpEnableMaintenanceMode,
    wcpEnableInternetAccess,
    wcpDefaultOu,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the default properties that are used for creating WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces> .
--
-- /See:/ 'mkWorkspaceCreationProperties' smart constructor.
data WorkspaceCreationProperties = WorkspaceCreationProperties'
  { customSecurityGroupId ::
      Lude.Maybe Lude.Text,
    userEnabledAsLocalAdministrator ::
      Lude.Maybe Lude.Bool,
    enableWorkDocs ::
      Lude.Maybe Lude.Bool,
    enableMaintenanceMode ::
      Lude.Maybe Lude.Bool,
    enableInternetAccess ::
      Lude.Maybe Lude.Bool,
    defaultOu :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- * 'customSecurityGroupId' - The identifier of your custom security group.
-- * 'defaultOu' - The default organizational unit (OU) for your WorkSpaces directories. This string must be the full Lightweight Directory Access Protocol (LDAP) distinguished name for the target domain and OU. It must be in the form @"OU=/value/ ,DC=/value/ ,DC=/value/ "@ , where /value/ is any string of characters, and the number of domain components (DCs) is two or more. For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@ .
--
-- /Important:/
--     * To avoid errors, certain characters in the distinguished name must be escaped. For more information, see <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names> in the Microsoft documentation.
--
--
--     * The API doesn't validate whether the OU exists.
--
--
-- * 'enableInternetAccess' - Indicates whether internet access is enabled for your WorkSpaces.
-- * 'enableMaintenanceMode' - Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
-- * 'enableWorkDocs' - Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
-- * 'userEnabledAsLocalAdministrator' - Indicates whether users are local administrators of their WorkSpaces.
mkWorkspaceCreationProperties ::
  WorkspaceCreationProperties
mkWorkspaceCreationProperties =
  WorkspaceCreationProperties'
    { customSecurityGroupId =
        Lude.Nothing,
      userEnabledAsLocalAdministrator = Lude.Nothing,
      enableWorkDocs = Lude.Nothing,
      enableMaintenanceMode = Lude.Nothing,
      enableInternetAccess = Lude.Nothing,
      defaultOu = Lude.Nothing
    }

-- | The identifier of your custom security group.
--
-- /Note:/ Consider using 'customSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpCustomSecurityGroupId :: Lens.Lens' WorkspaceCreationProperties (Lude.Maybe Lude.Text)
wcpCustomSecurityGroupId = Lens.lens (customSecurityGroupId :: WorkspaceCreationProperties -> Lude.Maybe Lude.Text) (\s a -> s {customSecurityGroupId = a} :: WorkspaceCreationProperties)
{-# DEPRECATED wcpCustomSecurityGroupId "Use generic-lens or generic-optics with 'customSecurityGroupId' instead." #-}

-- | Indicates whether users are local administrators of their WorkSpaces.
--
-- /Note:/ Consider using 'userEnabledAsLocalAdministrator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpUserEnabledAsLocalAdministrator :: Lens.Lens' WorkspaceCreationProperties (Lude.Maybe Lude.Bool)
wcpUserEnabledAsLocalAdministrator = Lens.lens (userEnabledAsLocalAdministrator :: WorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {userEnabledAsLocalAdministrator = a} :: WorkspaceCreationProperties)
{-# DEPRECATED wcpUserEnabledAsLocalAdministrator "Use generic-lens or generic-optics with 'userEnabledAsLocalAdministrator' instead." #-}

-- | Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
--
-- /Note:/ Consider using 'enableWorkDocs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpEnableWorkDocs :: Lens.Lens' WorkspaceCreationProperties (Lude.Maybe Lude.Bool)
wcpEnableWorkDocs = Lens.lens (enableWorkDocs :: WorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {enableWorkDocs = a} :: WorkspaceCreationProperties)
{-# DEPRECATED wcpEnableWorkDocs "Use generic-lens or generic-optics with 'enableWorkDocs' instead." #-}

-- | Indicates whether maintenance mode is enabled for your WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance> .
--
-- /Note:/ Consider using 'enableMaintenanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpEnableMaintenanceMode :: Lens.Lens' WorkspaceCreationProperties (Lude.Maybe Lude.Bool)
wcpEnableMaintenanceMode = Lens.lens (enableMaintenanceMode :: WorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {enableMaintenanceMode = a} :: WorkspaceCreationProperties)
{-# DEPRECATED wcpEnableMaintenanceMode "Use generic-lens or generic-optics with 'enableMaintenanceMode' instead." #-}

-- | Indicates whether internet access is enabled for your WorkSpaces.
--
-- /Note:/ Consider using 'enableInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wcpEnableInternetAccess :: Lens.Lens' WorkspaceCreationProperties (Lude.Maybe Lude.Bool)
wcpEnableInternetAccess = Lens.lens (enableInternetAccess :: WorkspaceCreationProperties -> Lude.Maybe Lude.Bool) (\s a -> s {enableInternetAccess = a} :: WorkspaceCreationProperties)
{-# DEPRECATED wcpEnableInternetAccess "Use generic-lens or generic-optics with 'enableInternetAccess' instead." #-}

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
wcpDefaultOu :: Lens.Lens' WorkspaceCreationProperties (Lude.Maybe Lude.Text)
wcpDefaultOu = Lens.lens (defaultOu :: WorkspaceCreationProperties -> Lude.Maybe Lude.Text) (\s a -> s {defaultOu = a} :: WorkspaceCreationProperties)
{-# DEPRECATED wcpDefaultOu "Use generic-lens or generic-optics with 'defaultOu' instead." #-}

instance Lude.ToJSON WorkspaceCreationProperties where
  toJSON WorkspaceCreationProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomSecurityGroupId" Lude..=) Lude.<$> customSecurityGroupId,
            ("UserEnabledAsLocalAdministrator" Lude..=)
              Lude.<$> userEnabledAsLocalAdministrator,
            ("EnableWorkDocs" Lude..=) Lude.<$> enableWorkDocs,
            ("EnableMaintenanceMode" Lude..=) Lude.<$> enableMaintenanceMode,
            ("EnableInternetAccess" Lude..=) Lude.<$> enableInternetAccess,
            ("DefaultOu" Lude..=) Lude.<$> defaultOu
          ]
      )
