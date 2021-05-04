{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the default properties that are used for creating WorkSpaces.
-- For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html Update Directory Details for Your WorkSpaces>.
--
-- /See:/ 'newWorkspaceCreationProperties' smart constructor.
data WorkspaceCreationProperties = WorkspaceCreationProperties'
  { -- | Indicates whether maintenance mode is enabled for your WorkSpaces. For
    -- more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance>.
    enableMaintenanceMode :: Prelude.Maybe Prelude.Bool,
    -- | The default organizational unit (OU) for your WorkSpaces directories.
    -- This string must be the full Lightweight Directory Access Protocol
    -- (LDAP) distinguished name for the target domain and OU. It must be in
    -- the form @\"OU=value,DC=value,DC=value\"@, where /value/ is any string
    -- of characters, and the number of domain components (DCs) is two or more.
    -- For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@.
    --
    -- -   To avoid errors, certain characters in the distinguished name must
    --     be escaped. For more information, see
    --     <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names>
    --     in the Microsoft documentation.
    --
    -- -   The API doesn\'t validate whether the OU exists.
    defaultOu :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether internet access is enabled for your WorkSpaces.
    enableInternetAccess :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
    --
    -- If WorkDocs is already enabled for a WorkSpaces directory and you
    -- disable it, new WorkSpaces launched in the directory will not have
    -- WorkDocs enabled. However, WorkDocs remains enabled for any existing
    -- WorkSpaces, unless you either disable users\' access to WorkDocs or you
    -- delete the WorkDocs site. To disable users\' access to WorkDocs, see
    -- <https://docs.aws.amazon.com/workdocs/latest/adminguide/inactive-user.html Disabling Users>
    -- in the /Amazon WorkDocs Administration Guide/. To delete a WorkDocs
    -- site, see
    -- <https://docs.aws.amazon.com/workdocs/latest/adminguide/manage-sites.html Deleting a Site>
    -- in the /Amazon WorkDocs Administration Guide/.
    --
    -- If you enable WorkDocs on a directory that already has existing
    -- WorkSpaces, the existing WorkSpaces and any new WorkSpaces that are
    -- launched in the directory will have WorkDocs enabled.
    enableWorkDocs :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of your custom security group.
    customSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether users are local administrators of their WorkSpaces.
    userEnabledAsLocalAdministrator :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceCreationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableMaintenanceMode', 'workspaceCreationProperties_enableMaintenanceMode' - Indicates whether maintenance mode is enabled for your WorkSpaces. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance>.
--
-- 'defaultOu', 'workspaceCreationProperties_defaultOu' - The default organizational unit (OU) for your WorkSpaces directories.
-- This string must be the full Lightweight Directory Access Protocol
-- (LDAP) distinguished name for the target domain and OU. It must be in
-- the form @\"OU=value,DC=value,DC=value\"@, where /value/ is any string
-- of characters, and the number of domain components (DCs) is two or more.
-- For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@.
--
-- -   To avoid errors, certain characters in the distinguished name must
--     be escaped. For more information, see
--     <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names>
--     in the Microsoft documentation.
--
-- -   The API doesn\'t validate whether the OU exists.
--
-- 'enableInternetAccess', 'workspaceCreationProperties_enableInternetAccess' - Indicates whether internet access is enabled for your WorkSpaces.
--
-- 'enableWorkDocs', 'workspaceCreationProperties_enableWorkDocs' - Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
--
-- If WorkDocs is already enabled for a WorkSpaces directory and you
-- disable it, new WorkSpaces launched in the directory will not have
-- WorkDocs enabled. However, WorkDocs remains enabled for any existing
-- WorkSpaces, unless you either disable users\' access to WorkDocs or you
-- delete the WorkDocs site. To disable users\' access to WorkDocs, see
-- <https://docs.aws.amazon.com/workdocs/latest/adminguide/inactive-user.html Disabling Users>
-- in the /Amazon WorkDocs Administration Guide/. To delete a WorkDocs
-- site, see
-- <https://docs.aws.amazon.com/workdocs/latest/adminguide/manage-sites.html Deleting a Site>
-- in the /Amazon WorkDocs Administration Guide/.
--
-- If you enable WorkDocs on a directory that already has existing
-- WorkSpaces, the existing WorkSpaces and any new WorkSpaces that are
-- launched in the directory will have WorkDocs enabled.
--
-- 'customSecurityGroupId', 'workspaceCreationProperties_customSecurityGroupId' - The identifier of your custom security group.
--
-- 'userEnabledAsLocalAdministrator', 'workspaceCreationProperties_userEnabledAsLocalAdministrator' - Indicates whether users are local administrators of their WorkSpaces.
newWorkspaceCreationProperties ::
  WorkspaceCreationProperties
newWorkspaceCreationProperties =
  WorkspaceCreationProperties'
    { enableMaintenanceMode =
        Prelude.Nothing,
      defaultOu = Prelude.Nothing,
      enableInternetAccess = Prelude.Nothing,
      enableWorkDocs = Prelude.Nothing,
      customSecurityGroupId = Prelude.Nothing,
      userEnabledAsLocalAdministrator =
        Prelude.Nothing
    }

-- | Indicates whether maintenance mode is enabled for your WorkSpaces. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspace-maintenance.html WorkSpace Maintenance>.
workspaceCreationProperties_enableMaintenanceMode :: Lens.Lens' WorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
workspaceCreationProperties_enableMaintenanceMode = Lens.lens (\WorkspaceCreationProperties' {enableMaintenanceMode} -> enableMaintenanceMode) (\s@WorkspaceCreationProperties' {} a -> s {enableMaintenanceMode = a} :: WorkspaceCreationProperties)

-- | The default organizational unit (OU) for your WorkSpaces directories.
-- This string must be the full Lightweight Directory Access Protocol
-- (LDAP) distinguished name for the target domain and OU. It must be in
-- the form @\"OU=value,DC=value,DC=value\"@, where /value/ is any string
-- of characters, and the number of domain components (DCs) is two or more.
-- For example, @OU=WorkSpaces_machines,DC=machines,DC=example,DC=com@.
--
-- -   To avoid errors, certain characters in the distinguished name must
--     be escaped. For more information, see
--     <https://docs.microsoft.com/previous-versions/windows/desktop/ldap/distinguished-names Distinguished Names>
--     in the Microsoft documentation.
--
-- -   The API doesn\'t validate whether the OU exists.
workspaceCreationProperties_defaultOu :: Lens.Lens' WorkspaceCreationProperties (Prelude.Maybe Prelude.Text)
workspaceCreationProperties_defaultOu = Lens.lens (\WorkspaceCreationProperties' {defaultOu} -> defaultOu) (\s@WorkspaceCreationProperties' {} a -> s {defaultOu = a} :: WorkspaceCreationProperties)

-- | Indicates whether internet access is enabled for your WorkSpaces.
workspaceCreationProperties_enableInternetAccess :: Lens.Lens' WorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
workspaceCreationProperties_enableInternetAccess = Lens.lens (\WorkspaceCreationProperties' {enableInternetAccess} -> enableInternetAccess) (\s@WorkspaceCreationProperties' {} a -> s {enableInternetAccess = a} :: WorkspaceCreationProperties)

-- | Indicates whether Amazon WorkDocs is enabled for your WorkSpaces.
--
-- If WorkDocs is already enabled for a WorkSpaces directory and you
-- disable it, new WorkSpaces launched in the directory will not have
-- WorkDocs enabled. However, WorkDocs remains enabled for any existing
-- WorkSpaces, unless you either disable users\' access to WorkDocs or you
-- delete the WorkDocs site. To disable users\' access to WorkDocs, see
-- <https://docs.aws.amazon.com/workdocs/latest/adminguide/inactive-user.html Disabling Users>
-- in the /Amazon WorkDocs Administration Guide/. To delete a WorkDocs
-- site, see
-- <https://docs.aws.amazon.com/workdocs/latest/adminguide/manage-sites.html Deleting a Site>
-- in the /Amazon WorkDocs Administration Guide/.
--
-- If you enable WorkDocs on a directory that already has existing
-- WorkSpaces, the existing WorkSpaces and any new WorkSpaces that are
-- launched in the directory will have WorkDocs enabled.
workspaceCreationProperties_enableWorkDocs :: Lens.Lens' WorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
workspaceCreationProperties_enableWorkDocs = Lens.lens (\WorkspaceCreationProperties' {enableWorkDocs} -> enableWorkDocs) (\s@WorkspaceCreationProperties' {} a -> s {enableWorkDocs = a} :: WorkspaceCreationProperties)

-- | The identifier of your custom security group.
workspaceCreationProperties_customSecurityGroupId :: Lens.Lens' WorkspaceCreationProperties (Prelude.Maybe Prelude.Text)
workspaceCreationProperties_customSecurityGroupId = Lens.lens (\WorkspaceCreationProperties' {customSecurityGroupId} -> customSecurityGroupId) (\s@WorkspaceCreationProperties' {} a -> s {customSecurityGroupId = a} :: WorkspaceCreationProperties)

-- | Indicates whether users are local administrators of their WorkSpaces.
workspaceCreationProperties_userEnabledAsLocalAdministrator :: Lens.Lens' WorkspaceCreationProperties (Prelude.Maybe Prelude.Bool)
workspaceCreationProperties_userEnabledAsLocalAdministrator = Lens.lens (\WorkspaceCreationProperties' {userEnabledAsLocalAdministrator} -> userEnabledAsLocalAdministrator) (\s@WorkspaceCreationProperties' {} a -> s {userEnabledAsLocalAdministrator = a} :: WorkspaceCreationProperties)

instance Prelude.Hashable WorkspaceCreationProperties

instance Prelude.NFData WorkspaceCreationProperties

instance Prelude.ToJSON WorkspaceCreationProperties where
  toJSON WorkspaceCreationProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EnableMaintenanceMode" Prelude..=)
              Prelude.<$> enableMaintenanceMode,
            ("DefaultOu" Prelude..=) Prelude.<$> defaultOu,
            ("EnableInternetAccess" Prelude..=)
              Prelude.<$> enableInternetAccess,
            ("EnableWorkDocs" Prelude..=)
              Prelude.<$> enableWorkDocs,
            ("CustomSecurityGroupId" Prelude..=)
              Prelude.<$> customSecurityGroupId,
            ("UserEnabledAsLocalAdministrator" Prelude..=)
              Prelude.<$> userEnabledAsLocalAdministrator
          ]
      )
