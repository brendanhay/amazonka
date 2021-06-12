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
-- Module      : Network.AWS.SSM.ModifyDocumentPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a Systems Manager document publicly or privately. If you share a
-- document privately, you must specify the AWS user account IDs for those
-- people who can use the document. If you share a document publicly, you
-- must specify /All/ as the account ID.
module Network.AWS.SSM.ModifyDocumentPermission
  ( -- * Creating a Request
    ModifyDocumentPermission (..),
    newModifyDocumentPermission,

    -- * Request Lenses
    modifyDocumentPermission_accountIdsToAdd,
    modifyDocumentPermission_sharedDocumentVersion,
    modifyDocumentPermission_accountIdsToRemove,
    modifyDocumentPermission_name,
    modifyDocumentPermission_permissionType,

    -- * Destructuring the Response
    ModifyDocumentPermissionResponse (..),
    newModifyDocumentPermissionResponse,

    -- * Response Lenses
    modifyDocumentPermissionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newModifyDocumentPermission' smart constructor.
data ModifyDocumentPermission = ModifyDocumentPermission'
  { -- | The AWS user accounts that should have access to the document. The
    -- account IDs can either be a group of account IDs or /All/.
    accountIdsToAdd :: Core.Maybe [Core.Text],
    -- | (Optional) The version of the document to share. If it\'s not specified,
    -- the system choose the @Default@ version to share.
    sharedDocumentVersion :: Core.Maybe Core.Text,
    -- | The AWS user accounts that should no longer have access to the document.
    -- The AWS user account can either be a group of account IDs or /All/. This
    -- action has a higher priority than /AccountIdsToAdd/. If you specify an
    -- account ID to add and the same ID to remove, the system removes access
    -- to the document.
    accountIdsToRemove :: Core.Maybe [Core.Text],
    -- | The name of the document that you want to share.
    name :: Core.Text,
    -- | The permission type for the document. The permission type can be
    -- /Share/.
    permissionType :: DocumentPermissionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDocumentPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIdsToAdd', 'modifyDocumentPermission_accountIdsToAdd' - The AWS user accounts that should have access to the document. The
-- account IDs can either be a group of account IDs or /All/.
--
-- 'sharedDocumentVersion', 'modifyDocumentPermission_sharedDocumentVersion' - (Optional) The version of the document to share. If it\'s not specified,
-- the system choose the @Default@ version to share.
--
-- 'accountIdsToRemove', 'modifyDocumentPermission_accountIdsToRemove' - The AWS user accounts that should no longer have access to the document.
-- The AWS user account can either be a group of account IDs or /All/. This
-- action has a higher priority than /AccountIdsToAdd/. If you specify an
-- account ID to add and the same ID to remove, the system removes access
-- to the document.
--
-- 'name', 'modifyDocumentPermission_name' - The name of the document that you want to share.
--
-- 'permissionType', 'modifyDocumentPermission_permissionType' - The permission type for the document. The permission type can be
-- /Share/.
newModifyDocumentPermission ::
  -- | 'name'
  Core.Text ->
  -- | 'permissionType'
  DocumentPermissionType ->
  ModifyDocumentPermission
newModifyDocumentPermission pName_ pPermissionType_ =
  ModifyDocumentPermission'
    { accountIdsToAdd =
        Core.Nothing,
      sharedDocumentVersion = Core.Nothing,
      accountIdsToRemove = Core.Nothing,
      name = pName_,
      permissionType = pPermissionType_
    }

-- | The AWS user accounts that should have access to the document. The
-- account IDs can either be a group of account IDs or /All/.
modifyDocumentPermission_accountIdsToAdd :: Lens.Lens' ModifyDocumentPermission (Core.Maybe [Core.Text])
modifyDocumentPermission_accountIdsToAdd = Lens.lens (\ModifyDocumentPermission' {accountIdsToAdd} -> accountIdsToAdd) (\s@ModifyDocumentPermission' {} a -> s {accountIdsToAdd = a} :: ModifyDocumentPermission) Core.. Lens.mapping Lens._Coerce

-- | (Optional) The version of the document to share. If it\'s not specified,
-- the system choose the @Default@ version to share.
modifyDocumentPermission_sharedDocumentVersion :: Lens.Lens' ModifyDocumentPermission (Core.Maybe Core.Text)
modifyDocumentPermission_sharedDocumentVersion = Lens.lens (\ModifyDocumentPermission' {sharedDocumentVersion} -> sharedDocumentVersion) (\s@ModifyDocumentPermission' {} a -> s {sharedDocumentVersion = a} :: ModifyDocumentPermission)

-- | The AWS user accounts that should no longer have access to the document.
-- The AWS user account can either be a group of account IDs or /All/. This
-- action has a higher priority than /AccountIdsToAdd/. If you specify an
-- account ID to add and the same ID to remove, the system removes access
-- to the document.
modifyDocumentPermission_accountIdsToRemove :: Lens.Lens' ModifyDocumentPermission (Core.Maybe [Core.Text])
modifyDocumentPermission_accountIdsToRemove = Lens.lens (\ModifyDocumentPermission' {accountIdsToRemove} -> accountIdsToRemove) (\s@ModifyDocumentPermission' {} a -> s {accountIdsToRemove = a} :: ModifyDocumentPermission) Core.. Lens.mapping Lens._Coerce

-- | The name of the document that you want to share.
modifyDocumentPermission_name :: Lens.Lens' ModifyDocumentPermission Core.Text
modifyDocumentPermission_name = Lens.lens (\ModifyDocumentPermission' {name} -> name) (\s@ModifyDocumentPermission' {} a -> s {name = a} :: ModifyDocumentPermission)

-- | The permission type for the document. The permission type can be
-- /Share/.
modifyDocumentPermission_permissionType :: Lens.Lens' ModifyDocumentPermission DocumentPermissionType
modifyDocumentPermission_permissionType = Lens.lens (\ModifyDocumentPermission' {permissionType} -> permissionType) (\s@ModifyDocumentPermission' {} a -> s {permissionType = a} :: ModifyDocumentPermission)

instance Core.AWSRequest ModifyDocumentPermission where
  type
    AWSResponse ModifyDocumentPermission =
      ModifyDocumentPermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyDocumentPermissionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyDocumentPermission

instance Core.NFData ModifyDocumentPermission

instance Core.ToHeaders ModifyDocumentPermission where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ModifyDocumentPermission" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyDocumentPermission where
  toJSON ModifyDocumentPermission' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountIdsToAdd" Core..=)
              Core.<$> accountIdsToAdd,
            ("SharedDocumentVersion" Core..=)
              Core.<$> sharedDocumentVersion,
            ("AccountIdsToRemove" Core..=)
              Core.<$> accountIdsToRemove,
            Core.Just ("Name" Core..= name),
            Core.Just ("PermissionType" Core..= permissionType)
          ]
      )

instance Core.ToPath ModifyDocumentPermission where
  toPath = Core.const "/"

instance Core.ToQuery ModifyDocumentPermission where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyDocumentPermissionResponse' smart constructor.
data ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDocumentPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyDocumentPermissionResponse_httpStatus' - The response's http status code.
newModifyDocumentPermissionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyDocumentPermissionResponse
newModifyDocumentPermissionResponse pHttpStatus_ =
  ModifyDocumentPermissionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyDocumentPermissionResponse_httpStatus :: Lens.Lens' ModifyDocumentPermissionResponse Core.Int
modifyDocumentPermissionResponse_httpStatus = Lens.lens (\ModifyDocumentPermissionResponse' {httpStatus} -> httpStatus) (\s@ModifyDocumentPermissionResponse' {} a -> s {httpStatus = a} :: ModifyDocumentPermissionResponse)

instance Core.NFData ModifyDocumentPermissionResponse
