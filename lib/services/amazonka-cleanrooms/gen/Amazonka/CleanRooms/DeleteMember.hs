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
-- Module      : Amazonka.CleanRooms.DeleteMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member from a collaboration. The removed member is
-- placed in the Removed status and can\'t interact with the collaboration.
-- The removed member\'s data is inaccessible to active members of the
-- collaboration.
module Amazonka.CleanRooms.DeleteMember
  ( -- * Creating a Request
    DeleteMember (..),
    newDeleteMember,

    -- * Request Lenses
    deleteMember_collaborationIdentifier,
    deleteMember_accountId,

    -- * Destructuring the Response
    DeleteMemberResponse (..),
    newDeleteMemberResponse,

    -- * Response Lenses
    deleteMemberResponse_httpStatus,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMember' smart constructor.
data DeleteMember = DeleteMember'
  { -- | The unique identifier for the associated collaboration.
    collaborationIdentifier :: Prelude.Text,
    -- | The account ID of the member to remove.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationIdentifier', 'deleteMember_collaborationIdentifier' - The unique identifier for the associated collaboration.
--
-- 'accountId', 'deleteMember_accountId' - The account ID of the member to remove.
newDeleteMember ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  DeleteMember
newDeleteMember pCollaborationIdentifier_ pAccountId_ =
  DeleteMember'
    { collaborationIdentifier =
        pCollaborationIdentifier_,
      accountId = pAccountId_
    }

-- | The unique identifier for the associated collaboration.
deleteMember_collaborationIdentifier :: Lens.Lens' DeleteMember Prelude.Text
deleteMember_collaborationIdentifier = Lens.lens (\DeleteMember' {collaborationIdentifier} -> collaborationIdentifier) (\s@DeleteMember' {} a -> s {collaborationIdentifier = a} :: DeleteMember)

-- | The account ID of the member to remove.
deleteMember_accountId :: Lens.Lens' DeleteMember Prelude.Text
deleteMember_accountId = Lens.lens (\DeleteMember' {accountId} -> accountId) (\s@DeleteMember' {} a -> s {accountId = a} :: DeleteMember)

instance Core.AWSRequest DeleteMember where
  type AWSResponse DeleteMember = DeleteMemberResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMember where
  hashWithSalt _salt DeleteMember' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationIdentifier
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData DeleteMember where
  rnf DeleteMember' {..} =
    Prelude.rnf collaborationIdentifier
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders DeleteMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteMember where
  toPath DeleteMember' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier,
        "/member/",
        Data.toBS accountId
      ]

instance Data.ToQuery DeleteMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMemberResponse' smart constructor.
data DeleteMemberResponse = DeleteMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMemberResponse_httpStatus' - The response's http status code.
newDeleteMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMemberResponse
newDeleteMemberResponse pHttpStatus_ =
  DeleteMemberResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteMemberResponse_httpStatus :: Lens.Lens' DeleteMemberResponse Prelude.Int
deleteMemberResponse_httpStatus = Lens.lens (\DeleteMemberResponse' {httpStatus} -> httpStatus) (\s@DeleteMemberResponse' {} a -> s {httpStatus = a} :: DeleteMemberResponse)

instance Prelude.NFData DeleteMemberResponse where
  rnf DeleteMemberResponse' {..} =
    Prelude.rnf httpStatus
