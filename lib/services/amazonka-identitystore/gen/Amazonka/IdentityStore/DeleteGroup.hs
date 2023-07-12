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
-- Module      : Amazonka.IdentityStore.DeleteGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a group within an identity store given @GroupId@.
module Amazonka.IdentityStore.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_identityStoreId,
    deleteGroup_groupId,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,

    -- * Response Lenses
    deleteGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'deleteGroup_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'groupId', 'deleteGroup_groupId' - The identifier for a group in the identity store.
newDeleteGroup ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  DeleteGroup
newDeleteGroup pIdentityStoreId_ pGroupId_ =
  DeleteGroup'
    { identityStoreId = pIdentityStoreId_,
      groupId = pGroupId_
    }

-- | The globally unique identifier for the identity store.
deleteGroup_identityStoreId :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_identityStoreId = Lens.lens (\DeleteGroup' {identityStoreId} -> identityStoreId) (\s@DeleteGroup' {} a -> s {identityStoreId = a} :: DeleteGroup)

-- | The identifier for a group in the identity store.
deleteGroup_groupId :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_groupId = Lens.lens (\DeleteGroup' {groupId} -> groupId) (\s@DeleteGroup' {} a -> s {groupId = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGroup where
  hashWithSalt _salt DeleteGroup' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData DeleteGroup where
  rnf DeleteGroup' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders DeleteGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.DeleteGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("GroupId" Data..= groupId)
          ]
      )

instance Data.ToPath DeleteGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGroupResponse_httpStatus' - The response's http status code.
newDeleteGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGroupResponse
newDeleteGroupResponse pHttpStatus_ =
  DeleteGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteGroupResponse_httpStatus :: Lens.Lens' DeleteGroupResponse Prelude.Int
deleteGroupResponse_httpStatus = Lens.lens (\DeleteGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupResponse' {} a -> s {httpStatus = a} :: DeleteGroupResponse)

instance Prelude.NFData DeleteGroupResponse where
  rnf DeleteGroupResponse' {..} = Prelude.rnf httpStatus
