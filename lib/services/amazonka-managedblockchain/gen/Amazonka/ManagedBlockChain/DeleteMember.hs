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
-- Module      : Amazonka.ManagedBlockChain.DeleteMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a member. Deleting a member removes the member and all
-- associated resources from the network. @DeleteMember@ can only be called
-- for a specified @MemberId@ if the principal performing the action is
-- associated with the Amazon Web Services account that owns the member. In
-- all other cases, the @DeleteMember@ action is carried out as the result
-- of an approved proposal to remove a member. If @MemberId@ is the last
-- member in a network specified by the last Amazon Web Services account,
-- the network is deleted also.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.DeleteMember
  ( -- * Creating a Request
    DeleteMember (..),
    newDeleteMember,

    -- * Request Lenses
    deleteMember_networkId,
    deleteMember_memberId,

    -- * Destructuring the Response
    DeleteMemberResponse (..),
    newDeleteMemberResponse,

    -- * Response Lenses
    deleteMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMember' smart constructor.
data DeleteMember = DeleteMember'
  { -- | The unique identifier of the network from which the member is removed.
    networkId :: Prelude.Text,
    -- | The unique identifier of the member to remove.
    memberId :: Prelude.Text
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
-- 'networkId', 'deleteMember_networkId' - The unique identifier of the network from which the member is removed.
--
-- 'memberId', 'deleteMember_memberId' - The unique identifier of the member to remove.
newDeleteMember ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  DeleteMember
newDeleteMember pNetworkId_ pMemberId_ =
  DeleteMember'
    { networkId = pNetworkId_,
      memberId = pMemberId_
    }

-- | The unique identifier of the network from which the member is removed.
deleteMember_networkId :: Lens.Lens' DeleteMember Prelude.Text
deleteMember_networkId = Lens.lens (\DeleteMember' {networkId} -> networkId) (\s@DeleteMember' {} a -> s {networkId = a} :: DeleteMember)

-- | The unique identifier of the member to remove.
deleteMember_memberId :: Lens.Lens' DeleteMember Prelude.Text
deleteMember_memberId = Lens.lens (\DeleteMember' {memberId} -> memberId) (\s@DeleteMember' {} a -> s {memberId = a} :: DeleteMember)

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
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData DeleteMember where
  rnf DeleteMember' {..} =
    Prelude.rnf networkId `Prelude.seq`
      Prelude.rnf memberId

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
      [ "/networks/",
        Data.toBS networkId,
        "/members/",
        Data.toBS memberId
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
