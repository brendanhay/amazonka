{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.CreatePermissionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group of permissions for various actions that a user can
-- perform in FinSpace.
module Amazonka.FinSpaceData.CreatePermissionGroup
  ( -- * Creating a Request
    CreatePermissionGroup (..),
    newCreatePermissionGroup,

    -- * Request Lenses
    createPermissionGroup_clientToken,
    createPermissionGroup_description,
    createPermissionGroup_name,
    createPermissionGroup_applicationPermissions,

    -- * Destructuring the Response
    CreatePermissionGroupResponse (..),
    newCreatePermissionGroupResponse,

    -- * Response Lenses
    createPermissionGroupResponse_permissionGroupId,
    createPermissionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePermissionGroup' smart constructor.
data CreatePermissionGroup = CreatePermissionGroup'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A brief description for the permission group.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the permission group.
    name :: Data.Sensitive Prelude.Text,
    -- | The option to indicate FinSpace application permissions that are granted
    -- to a specific group.
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
    applicationPermissions :: [ApplicationPermission]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPermissionGroup_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'description', 'createPermissionGroup_description' - A brief description for the permission group.
--
-- 'name', 'createPermissionGroup_name' - The name of the permission group.
--
-- 'applicationPermissions', 'createPermissionGroup_applicationPermissions' - The option to indicate FinSpace application permissions that are granted
-- to a specific group.
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
newCreatePermissionGroup ::
  -- | 'name'
  Prelude.Text ->
  CreatePermissionGroup
newCreatePermissionGroup pName_ =
  CreatePermissionGroup'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Data._Sensitive Lens.# pName_,
      applicationPermissions = Prelude.mempty
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
createPermissionGroup_clientToken :: Lens.Lens' CreatePermissionGroup (Prelude.Maybe Prelude.Text)
createPermissionGroup_clientToken = Lens.lens (\CreatePermissionGroup' {clientToken} -> clientToken) (\s@CreatePermissionGroup' {} a -> s {clientToken = a} :: CreatePermissionGroup)

-- | A brief description for the permission group.
createPermissionGroup_description :: Lens.Lens' CreatePermissionGroup (Prelude.Maybe Prelude.Text)
createPermissionGroup_description = Lens.lens (\CreatePermissionGroup' {description} -> description) (\s@CreatePermissionGroup' {} a -> s {description = a} :: CreatePermissionGroup) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the permission group.
createPermissionGroup_name :: Lens.Lens' CreatePermissionGroup Prelude.Text
createPermissionGroup_name = Lens.lens (\CreatePermissionGroup' {name} -> name) (\s@CreatePermissionGroup' {} a -> s {name = a} :: CreatePermissionGroup) Prelude.. Data._Sensitive

-- | The option to indicate FinSpace application permissions that are granted
-- to a specific group.
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
createPermissionGroup_applicationPermissions :: Lens.Lens' CreatePermissionGroup [ApplicationPermission]
createPermissionGroup_applicationPermissions = Lens.lens (\CreatePermissionGroup' {applicationPermissions} -> applicationPermissions) (\s@CreatePermissionGroup' {} a -> s {applicationPermissions = a} :: CreatePermissionGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreatePermissionGroup where
  type
    AWSResponse CreatePermissionGroup =
      CreatePermissionGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePermissionGroupResponse'
            Prelude.<$> (x Data..?> "permissionGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePermissionGroup where
  hashWithSalt _salt CreatePermissionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` applicationPermissions

instance Prelude.NFData CreatePermissionGroup where
  rnf CreatePermissionGroup' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf applicationPermissions

instance Data.ToHeaders CreatePermissionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePermissionGroup where
  toJSON CreatePermissionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "applicationPermissions"
                  Data..= applicationPermissions
              )
          ]
      )

instance Data.ToPath CreatePermissionGroup where
  toPath = Prelude.const "/permission-group"

instance Data.ToQuery CreatePermissionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePermissionGroupResponse' smart constructor.
data CreatePermissionGroupResponse = CreatePermissionGroupResponse'
  { -- | The unique identifier for the permission group.
    permissionGroupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermissionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionGroupId', 'createPermissionGroupResponse_permissionGroupId' - The unique identifier for the permission group.
--
-- 'httpStatus', 'createPermissionGroupResponse_httpStatus' - The response's http status code.
newCreatePermissionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePermissionGroupResponse
newCreatePermissionGroupResponse pHttpStatus_ =
  CreatePermissionGroupResponse'
    { permissionGroupId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the permission group.
createPermissionGroupResponse_permissionGroupId :: Lens.Lens' CreatePermissionGroupResponse (Prelude.Maybe Prelude.Text)
createPermissionGroupResponse_permissionGroupId = Lens.lens (\CreatePermissionGroupResponse' {permissionGroupId} -> permissionGroupId) (\s@CreatePermissionGroupResponse' {} a -> s {permissionGroupId = a} :: CreatePermissionGroupResponse)

-- | The response's http status code.
createPermissionGroupResponse_httpStatus :: Lens.Lens' CreatePermissionGroupResponse Prelude.Int
createPermissionGroupResponse_httpStatus = Lens.lens (\CreatePermissionGroupResponse' {httpStatus} -> httpStatus) (\s@CreatePermissionGroupResponse' {} a -> s {httpStatus = a} :: CreatePermissionGroupResponse)

instance Prelude.NFData CreatePermissionGroupResponse where
  rnf CreatePermissionGroupResponse' {..} =
    Prelude.rnf permissionGroupId `Prelude.seq`
      Prelude.rnf httpStatus
