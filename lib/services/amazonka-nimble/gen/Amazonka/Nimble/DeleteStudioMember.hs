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
-- Module      : Amazonka.Nimble.DeleteStudioMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a user from studio membership.
module Amazonka.Nimble.DeleteStudioMember
  ( -- * Creating a Request
    DeleteStudioMember (..),
    newDeleteStudioMember,

    -- * Request Lenses
    deleteStudioMember_clientToken,
    deleteStudioMember_studioId,
    deleteStudioMember_principalId,

    -- * Destructuring the Response
    DeleteStudioMemberResponse (..),
    newDeleteStudioMemberResponse,

    -- * Response Lenses
    deleteStudioMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStudioMember' smart constructor.
data DeleteStudioMember = DeleteStudioMember'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The principal ID.
    principalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStudioMember_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'studioId', 'deleteStudioMember_studioId' - The studio ID.
--
-- 'principalId', 'deleteStudioMember_principalId' - The principal ID.
newDeleteStudioMember ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'principalId'
  Prelude.Text ->
  DeleteStudioMember
newDeleteStudioMember pStudioId_ pPrincipalId_ =
  DeleteStudioMember'
    { clientToken = Prelude.Nothing,
      studioId = pStudioId_,
      principalId = pPrincipalId_
    }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
deleteStudioMember_clientToken :: Lens.Lens' DeleteStudioMember (Prelude.Maybe Prelude.Text)
deleteStudioMember_clientToken = Lens.lens (\DeleteStudioMember' {clientToken} -> clientToken) (\s@DeleteStudioMember' {} a -> s {clientToken = a} :: DeleteStudioMember)

-- | The studio ID.
deleteStudioMember_studioId :: Lens.Lens' DeleteStudioMember Prelude.Text
deleteStudioMember_studioId = Lens.lens (\DeleteStudioMember' {studioId} -> studioId) (\s@DeleteStudioMember' {} a -> s {studioId = a} :: DeleteStudioMember)

-- | The principal ID.
deleteStudioMember_principalId :: Lens.Lens' DeleteStudioMember Prelude.Text
deleteStudioMember_principalId = Lens.lens (\DeleteStudioMember' {principalId} -> principalId) (\s@DeleteStudioMember' {} a -> s {principalId = a} :: DeleteStudioMember)

instance Core.AWSRequest DeleteStudioMember where
  type
    AWSResponse DeleteStudioMember =
      DeleteStudioMemberResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStudioMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStudioMember where
  hashWithSalt _salt DeleteStudioMember' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData DeleteStudioMember where
  rnf DeleteStudioMember' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf principalId

instance Core.ToHeaders DeleteStudioMember where
  toHeaders DeleteStudioMember' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DeleteStudioMember where
  toPath DeleteStudioMember' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/membership/",
        Core.toBS principalId
      ]

instance Core.ToQuery DeleteStudioMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioMemberResponse' smart constructor.
data DeleteStudioMemberResponse = DeleteStudioMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStudioMemberResponse_httpStatus' - The response's http status code.
newDeleteStudioMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStudioMemberResponse
newDeleteStudioMemberResponse pHttpStatus_ =
  DeleteStudioMemberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStudioMemberResponse_httpStatus :: Lens.Lens' DeleteStudioMemberResponse Prelude.Int
deleteStudioMemberResponse_httpStatus = Lens.lens (\DeleteStudioMemberResponse' {httpStatus} -> httpStatus) (\s@DeleteStudioMemberResponse' {} a -> s {httpStatus = a} :: DeleteStudioMemberResponse)

instance Prelude.NFData DeleteStudioMemberResponse where
  rnf DeleteStudioMemberResponse' {..} =
    Prelude.rnf httpStatus
