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
-- Module      : Amazonka.IdentityStore.UpdateGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified group in the specified identity store, updates the
-- group metadata and attributes.
module Amazonka.IdentityStore.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_identityStoreId,
    updateGroup_groupId,
    updateGroup_operations,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,

    -- * Response Lenses
    updateGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text,
    -- | A list of @AttributeOperation@ objects to apply to the requested group.
    -- These operations might add, replace, or remove an attribute.
    operations :: Prelude.NonEmpty AttributeOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'updateGroup_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'groupId', 'updateGroup_groupId' - The identifier for a group in the identity store.
--
-- 'operations', 'updateGroup_operations' - A list of @AttributeOperation@ objects to apply to the requested group.
-- These operations might add, replace, or remove an attribute.
newUpdateGroup ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'operations'
  Prelude.NonEmpty AttributeOperation ->
  UpdateGroup
newUpdateGroup
  pIdentityStoreId_
  pGroupId_
  pOperations_ =
    UpdateGroup'
      { identityStoreId = pIdentityStoreId_,
        groupId = pGroupId_,
        operations = Lens.coerced Lens.# pOperations_
      }

-- | The globally unique identifier for the identity store.
updateGroup_identityStoreId :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_identityStoreId = Lens.lens (\UpdateGroup' {identityStoreId} -> identityStoreId) (\s@UpdateGroup' {} a -> s {identityStoreId = a} :: UpdateGroup)

-- | The identifier for a group in the identity store.
updateGroup_groupId :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_groupId = Lens.lens (\UpdateGroup' {groupId} -> groupId) (\s@UpdateGroup' {} a -> s {groupId = a} :: UpdateGroup)

-- | A list of @AttributeOperation@ objects to apply to the requested group.
-- These operations might add, replace, or remove an attribute.
updateGroup_operations :: Lens.Lens' UpdateGroup (Prelude.NonEmpty AttributeOperation)
updateGroup_operations = Lens.lens (\UpdateGroup' {operations} -> operations) (\s@UpdateGroup' {} a -> s {operations = a} :: UpdateGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGroup where
  hashWithSalt _salt UpdateGroup' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` operations

instance Prelude.NFData UpdateGroup where
  rnf UpdateGroup' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf operations

instance Data.ToHeaders UpdateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.UpdateGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("Operations" Data..= operations)
          ]
      )

instance Data.ToPath UpdateGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGroupResponse_httpStatus' - The response's http status code.
newUpdateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Prelude.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Prelude.NFData UpdateGroupResponse where
  rnf UpdateGroupResponse' {..} = Prelude.rnf httpStatus
