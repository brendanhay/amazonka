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
-- Module      : Amazonka.IdentityStore.DeleteGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a membership within a group given @MembershipId@.
module Amazonka.IdentityStore.DeleteGroupMembership
  ( -- * Creating a Request
    DeleteGroupMembership (..),
    newDeleteGroupMembership,

    -- * Request Lenses
    deleteGroupMembership_identityStoreId,
    deleteGroupMembership_membershipId,

    -- * Destructuring the Response
    DeleteGroupMembershipResponse (..),
    newDeleteGroupMembershipResponse,

    -- * Response Lenses
    deleteGroupMembershipResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGroupMembership' smart constructor.
data DeleteGroupMembership = DeleteGroupMembership'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a @GroupMembership@ in an identity store.
    membershipId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'deleteGroupMembership_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'membershipId', 'deleteGroupMembership_membershipId' - The identifier for a @GroupMembership@ in an identity store.
newDeleteGroupMembership ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  DeleteGroupMembership
newDeleteGroupMembership
  pIdentityStoreId_
  pMembershipId_ =
    DeleteGroupMembership'
      { identityStoreId =
          pIdentityStoreId_,
        membershipId = pMembershipId_
      }

-- | The globally unique identifier for the identity store.
deleteGroupMembership_identityStoreId :: Lens.Lens' DeleteGroupMembership Prelude.Text
deleteGroupMembership_identityStoreId = Lens.lens (\DeleteGroupMembership' {identityStoreId} -> identityStoreId) (\s@DeleteGroupMembership' {} a -> s {identityStoreId = a} :: DeleteGroupMembership)

-- | The identifier for a @GroupMembership@ in an identity store.
deleteGroupMembership_membershipId :: Lens.Lens' DeleteGroupMembership Prelude.Text
deleteGroupMembership_membershipId = Lens.lens (\DeleteGroupMembership' {membershipId} -> membershipId) (\s@DeleteGroupMembership' {} a -> s {membershipId = a} :: DeleteGroupMembership)

instance Core.AWSRequest DeleteGroupMembership where
  type
    AWSResponse DeleteGroupMembership =
      DeleteGroupMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGroupMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGroupMembership where
  hashWithSalt _salt DeleteGroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` membershipId

instance Prelude.NFData DeleteGroupMembership where
  rnf DeleteGroupMembership' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf membershipId

instance Data.ToHeaders DeleteGroupMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.DeleteGroupMembership" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGroupMembership where
  toJSON DeleteGroupMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("MembershipId" Data..= membershipId)
          ]
      )

instance Data.ToPath DeleteGroupMembership where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGroupMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupMembershipResponse' smart constructor.
data DeleteGroupMembershipResponse = DeleteGroupMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGroupMembershipResponse_httpStatus' - The response's http status code.
newDeleteGroupMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGroupMembershipResponse
newDeleteGroupMembershipResponse pHttpStatus_ =
  DeleteGroupMembershipResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGroupMembershipResponse_httpStatus :: Lens.Lens' DeleteGroupMembershipResponse Prelude.Int
deleteGroupMembershipResponse_httpStatus = Lens.lens (\DeleteGroupMembershipResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupMembershipResponse' {} a -> s {httpStatus = a} :: DeleteGroupMembershipResponse)

instance Prelude.NFData DeleteGroupMembershipResponse where
  rnf DeleteGroupMembershipResponse' {..} =
    Prelude.rnf httpStatus
