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
-- Module      : Amazonka.SSM.ModifyDocumentPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a Amazon Web Services Systems Manager document (SSM
-- document)publicly or privately. If you share a document privately, you
-- must specify the Amazon Web Services user account IDs for those people
-- who can use the document. If you share a document publicly, you must
-- specify /All/ as the account ID.
module Amazonka.SSM.ModifyDocumentPermission
  ( -- * Creating a Request
    ModifyDocumentPermission (..),
    newModifyDocumentPermission,

    -- * Request Lenses
    modifyDocumentPermission_accountIdsToAdd,
    modifyDocumentPermission_accountIdsToRemove,
    modifyDocumentPermission_sharedDocumentVersion,
    modifyDocumentPermission_name,
    modifyDocumentPermission_permissionType,

    -- * Destructuring the Response
    ModifyDocumentPermissionResponse (..),
    newModifyDocumentPermissionResponse,

    -- * Response Lenses
    modifyDocumentPermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newModifyDocumentPermission' smart constructor.
data ModifyDocumentPermission = ModifyDocumentPermission'
  { -- | The Amazon Web Services user accounts that should have access to the
    -- document. The account IDs can either be a group of account IDs or /All/.
    accountIdsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services user accounts that should no longer have access
    -- to the document. The Amazon Web Services user account can either be a
    -- group of account IDs or /All/. This action has a higher priority than
    -- /AccountIdsToAdd/. If you specify an account ID to add and the same ID
    -- to remove, the system removes access to the document.
    accountIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | (Optional) The version of the document to share. If it isn\'t specified,
    -- the system choose the @Default@ version to share.
    sharedDocumentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the document that you want to share.
    name :: Prelude.Text,
    -- | The permission type for the document. The permission type can be
    -- /Share/.
    permissionType :: DocumentPermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDocumentPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIdsToAdd', 'modifyDocumentPermission_accountIdsToAdd' - The Amazon Web Services user accounts that should have access to the
-- document. The account IDs can either be a group of account IDs or /All/.
--
-- 'accountIdsToRemove', 'modifyDocumentPermission_accountIdsToRemove' - The Amazon Web Services user accounts that should no longer have access
-- to the document. The Amazon Web Services user account can either be a
-- group of account IDs or /All/. This action has a higher priority than
-- /AccountIdsToAdd/. If you specify an account ID to add and the same ID
-- to remove, the system removes access to the document.
--
-- 'sharedDocumentVersion', 'modifyDocumentPermission_sharedDocumentVersion' - (Optional) The version of the document to share. If it isn\'t specified,
-- the system choose the @Default@ version to share.
--
-- 'name', 'modifyDocumentPermission_name' - The name of the document that you want to share.
--
-- 'permissionType', 'modifyDocumentPermission_permissionType' - The permission type for the document. The permission type can be
-- /Share/.
newModifyDocumentPermission ::
  -- | 'name'
  Prelude.Text ->
  -- | 'permissionType'
  DocumentPermissionType ->
  ModifyDocumentPermission
newModifyDocumentPermission pName_ pPermissionType_ =
  ModifyDocumentPermission'
    { accountIdsToAdd =
        Prelude.Nothing,
      accountIdsToRemove = Prelude.Nothing,
      sharedDocumentVersion = Prelude.Nothing,
      name = pName_,
      permissionType = pPermissionType_
    }

-- | The Amazon Web Services user accounts that should have access to the
-- document. The account IDs can either be a group of account IDs or /All/.
modifyDocumentPermission_accountIdsToAdd :: Lens.Lens' ModifyDocumentPermission (Prelude.Maybe [Prelude.Text])
modifyDocumentPermission_accountIdsToAdd = Lens.lens (\ModifyDocumentPermission' {accountIdsToAdd} -> accountIdsToAdd) (\s@ModifyDocumentPermission' {} a -> s {accountIdsToAdd = a} :: ModifyDocumentPermission) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services user accounts that should no longer have access
-- to the document. The Amazon Web Services user account can either be a
-- group of account IDs or /All/. This action has a higher priority than
-- /AccountIdsToAdd/. If you specify an account ID to add and the same ID
-- to remove, the system removes access to the document.
modifyDocumentPermission_accountIdsToRemove :: Lens.Lens' ModifyDocumentPermission (Prelude.Maybe [Prelude.Text])
modifyDocumentPermission_accountIdsToRemove = Lens.lens (\ModifyDocumentPermission' {accountIdsToRemove} -> accountIdsToRemove) (\s@ModifyDocumentPermission' {} a -> s {accountIdsToRemove = a} :: ModifyDocumentPermission) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) The version of the document to share. If it isn\'t specified,
-- the system choose the @Default@ version to share.
modifyDocumentPermission_sharedDocumentVersion :: Lens.Lens' ModifyDocumentPermission (Prelude.Maybe Prelude.Text)
modifyDocumentPermission_sharedDocumentVersion = Lens.lens (\ModifyDocumentPermission' {sharedDocumentVersion} -> sharedDocumentVersion) (\s@ModifyDocumentPermission' {} a -> s {sharedDocumentVersion = a} :: ModifyDocumentPermission)

-- | The name of the document that you want to share.
modifyDocumentPermission_name :: Lens.Lens' ModifyDocumentPermission Prelude.Text
modifyDocumentPermission_name = Lens.lens (\ModifyDocumentPermission' {name} -> name) (\s@ModifyDocumentPermission' {} a -> s {name = a} :: ModifyDocumentPermission)

-- | The permission type for the document. The permission type can be
-- /Share/.
modifyDocumentPermission_permissionType :: Lens.Lens' ModifyDocumentPermission DocumentPermissionType
modifyDocumentPermission_permissionType = Lens.lens (\ModifyDocumentPermission' {permissionType} -> permissionType) (\s@ModifyDocumentPermission' {} a -> s {permissionType = a} :: ModifyDocumentPermission)

instance Core.AWSRequest ModifyDocumentPermission where
  type
    AWSResponse ModifyDocumentPermission =
      ModifyDocumentPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyDocumentPermissionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDocumentPermission where
  hashWithSalt _salt ModifyDocumentPermission' {..} =
    _salt `Prelude.hashWithSalt` accountIdsToAdd
      `Prelude.hashWithSalt` accountIdsToRemove
      `Prelude.hashWithSalt` sharedDocumentVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissionType

instance Prelude.NFData ModifyDocumentPermission where
  rnf ModifyDocumentPermission' {..} =
    Prelude.rnf accountIdsToAdd
      `Prelude.seq` Prelude.rnf accountIdsToRemove
      `Prelude.seq` Prelude.rnf sharedDocumentVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionType

instance Data.ToHeaders ModifyDocumentPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ModifyDocumentPermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyDocumentPermission where
  toJSON ModifyDocumentPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountIdsToAdd" Data..=)
              Prelude.<$> accountIdsToAdd,
            ("AccountIdsToRemove" Data..=)
              Prelude.<$> accountIdsToRemove,
            ("SharedDocumentVersion" Data..=)
              Prelude.<$> sharedDocumentVersion,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("PermissionType" Data..= permissionType)
          ]
      )

instance Data.ToPath ModifyDocumentPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDocumentPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyDocumentPermissionResponse' smart constructor.
data ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyDocumentPermissionResponse
newModifyDocumentPermissionResponse pHttpStatus_ =
  ModifyDocumentPermissionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyDocumentPermissionResponse_httpStatus :: Lens.Lens' ModifyDocumentPermissionResponse Prelude.Int
modifyDocumentPermissionResponse_httpStatus = Lens.lens (\ModifyDocumentPermissionResponse' {httpStatus} -> httpStatus) (\s@ModifyDocumentPermissionResponse' {} a -> s {httpStatus = a} :: ModifyDocumentPermissionResponse)

instance
  Prelude.NFData
    ModifyDocumentPermissionResponse
  where
  rnf ModifyDocumentPermissionResponse' {..} =
    Prelude.rnf httpStatus
