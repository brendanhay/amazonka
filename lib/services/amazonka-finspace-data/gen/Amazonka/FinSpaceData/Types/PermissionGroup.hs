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
-- Module      : Amazonka.FinSpaceData.Types.PermissionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.PermissionGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ApplicationPermission
import Amazonka.FinSpaceData.Types.PermissionGroupMembershipStatus
import qualified Amazonka.Prelude as Prelude

-- | The structure for a permission group.
--
-- /See:/ 'newPermissionGroup' smart constructor.
data PermissionGroup = PermissionGroup'
  { -- | Indicates the permissions that are granted to a specific group for
    -- accessing the FinSpace application.
    --
    -- When assigning application permissions, be aware that the permission
    -- @ManageUsersAndGroups@ allows users to grant themselves or others access
    -- to any functionality in their FinSpace environment\'s application. It
    -- should only be granted to trusted users.
    --
    -- -   @CreateDataset@ – Group members can create new datasets.
    --
    -- -   @ManageClusters@ – Group members can manage Apache Spark clusters
    --     from FinSpace notebooks.
    --
    -- -   @ManageUsersAndGroups@ – Group members can manage users and
    --     permission groups. This is a privileged permission that allows users
    --     to grant themselves or others access to any functionality in the
    --     application. It should only be granted to trusted users.
    --
    -- -   @ManageAttributeSets@ – Group members can manage attribute sets.
    --
    -- -   @ViewAuditData@ – Group members can view audit data.
    --
    -- -   @AccessNotebooks@ – Group members will have access to FinSpace
    --     notebooks.
    --
    -- -   @GetTemporaryCredentials@ – Group members can get temporary API
    --     credentials.
    applicationPermissions :: Prelude.Maybe [ApplicationPermission],
    -- | The timestamp at which the group was created in FinSpace. The value is
    -- determined as epoch time in milliseconds.
    createTime :: Prelude.Maybe Prelude.Integer,
    -- | A brief description for the permission group.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Describes the last time the permission group was updated. The value is
    -- determined as epoch time in milliseconds.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | Indicates the status of the user account within a permission group.
    --
    -- -   @ADDITION_IN_PROGRESS@ – The user account is currently being added
    --     to the permission group.
    --
    -- -   @ADDITION_SUCCESS@ – The user account is successfully added to the
    --     permission group.
    --
    -- -   @REMOVAL_IN_PROGRESS@ – The user is currently being removed from the
    --     permission group.
    membershipStatus :: Prelude.Maybe PermissionGroupMembershipStatus,
    -- | The name of the permission group.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier for the permission group.
    permissionGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationPermissions', 'permissionGroup_applicationPermissions' - Indicates the permissions that are granted to a specific group for
-- accessing the FinSpace application.
--
-- When assigning application permissions, be aware that the permission
-- @ManageUsersAndGroups@ allows users to grant themselves or others access
-- to any functionality in their FinSpace environment\'s application. It
-- should only be granted to trusted users.
--
-- -   @CreateDataset@ – Group members can create new datasets.
--
-- -   @ManageClusters@ – Group members can manage Apache Spark clusters
--     from FinSpace notebooks.
--
-- -   @ManageUsersAndGroups@ – Group members can manage users and
--     permission groups. This is a privileged permission that allows users
--     to grant themselves or others access to any functionality in the
--     application. It should only be granted to trusted users.
--
-- -   @ManageAttributeSets@ – Group members can manage attribute sets.
--
-- -   @ViewAuditData@ – Group members can view audit data.
--
-- -   @AccessNotebooks@ – Group members will have access to FinSpace
--     notebooks.
--
-- -   @GetTemporaryCredentials@ – Group members can get temporary API
--     credentials.
--
-- 'createTime', 'permissionGroup_createTime' - The timestamp at which the group was created in FinSpace. The value is
-- determined as epoch time in milliseconds.
--
-- 'description', 'permissionGroup_description' - A brief description for the permission group.
--
-- 'lastModifiedTime', 'permissionGroup_lastModifiedTime' - Describes the last time the permission group was updated. The value is
-- determined as epoch time in milliseconds.
--
-- 'membershipStatus', 'permissionGroup_membershipStatus' - Indicates the status of the user account within a permission group.
--
-- -   @ADDITION_IN_PROGRESS@ – The user account is currently being added
--     to the permission group.
--
-- -   @ADDITION_SUCCESS@ – The user account is successfully added to the
--     permission group.
--
-- -   @REMOVAL_IN_PROGRESS@ – The user is currently being removed from the
--     permission group.
--
-- 'name', 'permissionGroup_name' - The name of the permission group.
--
-- 'permissionGroupId', 'permissionGroup_permissionGroupId' - The unique identifier for the permission group.
newPermissionGroup ::
  PermissionGroup
newPermissionGroup =
  PermissionGroup'
    { applicationPermissions =
        Prelude.Nothing,
      createTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      membershipStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      permissionGroupId = Prelude.Nothing
    }

-- | Indicates the permissions that are granted to a specific group for
-- accessing the FinSpace application.
--
-- When assigning application permissions, be aware that the permission
-- @ManageUsersAndGroups@ allows users to grant themselves or others access
-- to any functionality in their FinSpace environment\'s application. It
-- should only be granted to trusted users.
--
-- -   @CreateDataset@ – Group members can create new datasets.
--
-- -   @ManageClusters@ – Group members can manage Apache Spark clusters
--     from FinSpace notebooks.
--
-- -   @ManageUsersAndGroups@ – Group members can manage users and
--     permission groups. This is a privileged permission that allows users
--     to grant themselves or others access to any functionality in the
--     application. It should only be granted to trusted users.
--
-- -   @ManageAttributeSets@ – Group members can manage attribute sets.
--
-- -   @ViewAuditData@ – Group members can view audit data.
--
-- -   @AccessNotebooks@ – Group members will have access to FinSpace
--     notebooks.
--
-- -   @GetTemporaryCredentials@ – Group members can get temporary API
--     credentials.
permissionGroup_applicationPermissions :: Lens.Lens' PermissionGroup (Prelude.Maybe [ApplicationPermission])
permissionGroup_applicationPermissions = Lens.lens (\PermissionGroup' {applicationPermissions} -> applicationPermissions) (\s@PermissionGroup' {} a -> s {applicationPermissions = a} :: PermissionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp at which the group was created in FinSpace. The value is
-- determined as epoch time in milliseconds.
permissionGroup_createTime :: Lens.Lens' PermissionGroup (Prelude.Maybe Prelude.Integer)
permissionGroup_createTime = Lens.lens (\PermissionGroup' {createTime} -> createTime) (\s@PermissionGroup' {} a -> s {createTime = a} :: PermissionGroup)

-- | A brief description for the permission group.
permissionGroup_description :: Lens.Lens' PermissionGroup (Prelude.Maybe Prelude.Text)
permissionGroup_description = Lens.lens (\PermissionGroup' {description} -> description) (\s@PermissionGroup' {} a -> s {description = a} :: PermissionGroup) Prelude.. Lens.mapping Data._Sensitive

-- | Describes the last time the permission group was updated. The value is
-- determined as epoch time in milliseconds.
permissionGroup_lastModifiedTime :: Lens.Lens' PermissionGroup (Prelude.Maybe Prelude.Integer)
permissionGroup_lastModifiedTime = Lens.lens (\PermissionGroup' {lastModifiedTime} -> lastModifiedTime) (\s@PermissionGroup' {} a -> s {lastModifiedTime = a} :: PermissionGroup)

-- | Indicates the status of the user account within a permission group.
--
-- -   @ADDITION_IN_PROGRESS@ – The user account is currently being added
--     to the permission group.
--
-- -   @ADDITION_SUCCESS@ – The user account is successfully added to the
--     permission group.
--
-- -   @REMOVAL_IN_PROGRESS@ – The user is currently being removed from the
--     permission group.
permissionGroup_membershipStatus :: Lens.Lens' PermissionGroup (Prelude.Maybe PermissionGroupMembershipStatus)
permissionGroup_membershipStatus = Lens.lens (\PermissionGroup' {membershipStatus} -> membershipStatus) (\s@PermissionGroup' {} a -> s {membershipStatus = a} :: PermissionGroup)

-- | The name of the permission group.
permissionGroup_name :: Lens.Lens' PermissionGroup (Prelude.Maybe Prelude.Text)
permissionGroup_name = Lens.lens (\PermissionGroup' {name} -> name) (\s@PermissionGroup' {} a -> s {name = a} :: PermissionGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier for the permission group.
permissionGroup_permissionGroupId :: Lens.Lens' PermissionGroup (Prelude.Maybe Prelude.Text)
permissionGroup_permissionGroupId = Lens.lens (\PermissionGroup' {permissionGroupId} -> permissionGroupId) (\s@PermissionGroup' {} a -> s {permissionGroupId = a} :: PermissionGroup)

instance Data.FromJSON PermissionGroup where
  parseJSON =
    Data.withObject
      "PermissionGroup"
      ( \x ->
          PermissionGroup'
            Prelude.<$> ( x
                            Data..:? "applicationPermissions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastModifiedTime")
            Prelude.<*> (x Data..:? "membershipStatus")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "permissionGroupId")
      )

instance Prelude.Hashable PermissionGroup where
  hashWithSalt _salt PermissionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` applicationPermissions
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` membershipStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissionGroupId

instance Prelude.NFData PermissionGroup where
  rnf PermissionGroup' {..} =
    Prelude.rnf applicationPermissions `Prelude.seq`
      Prelude.rnf createTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf lastModifiedTime `Prelude.seq`
            Prelude.rnf membershipStatus `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf permissionGroupId
