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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    deleteStudioMember_principalId,
    deleteStudioMember_studioId,

    -- * Destructuring the Response
    DeleteStudioMemberResponse (..),
    newDeleteStudioMemberResponse,

    -- * Response Lenses
    deleteStudioMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStudioMember' smart constructor.
data DeleteStudioMember = DeleteStudioMember'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The principal ID. This currently supports a IAM Identity Center UserId.
    principalId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
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
-- 'clientToken', 'deleteStudioMember_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'principalId', 'deleteStudioMember_principalId' - The principal ID. This currently supports a IAM Identity Center UserId.
--
-- 'studioId', 'deleteStudioMember_studioId' - The studio ID.
newDeleteStudioMember ::
  -- | 'principalId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  DeleteStudioMember
newDeleteStudioMember pPrincipalId_ pStudioId_ =
  DeleteStudioMember'
    { clientToken = Prelude.Nothing,
      principalId = pPrincipalId_,
      studioId = pStudioId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
deleteStudioMember_clientToken :: Lens.Lens' DeleteStudioMember (Prelude.Maybe Prelude.Text)
deleteStudioMember_clientToken = Lens.lens (\DeleteStudioMember' {clientToken} -> clientToken) (\s@DeleteStudioMember' {} a -> s {clientToken = a} :: DeleteStudioMember)

-- | The principal ID. This currently supports a IAM Identity Center UserId.
deleteStudioMember_principalId :: Lens.Lens' DeleteStudioMember Prelude.Text
deleteStudioMember_principalId = Lens.lens (\DeleteStudioMember' {principalId} -> principalId) (\s@DeleteStudioMember' {} a -> s {principalId = a} :: DeleteStudioMember)

-- | The studio ID.
deleteStudioMember_studioId :: Lens.Lens' DeleteStudioMember Prelude.Text
deleteStudioMember_studioId = Lens.lens (\DeleteStudioMember' {studioId} -> studioId) (\s@DeleteStudioMember' {} a -> s {studioId = a} :: DeleteStudioMember)

instance Core.AWSRequest DeleteStudioMember where
  type
    AWSResponse DeleteStudioMember =
      DeleteStudioMemberResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStudioMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStudioMember where
  hashWithSalt _salt DeleteStudioMember' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData DeleteStudioMember where
  rnf DeleteStudioMember' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders DeleteStudioMember where
  toHeaders DeleteStudioMember' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteStudioMember where
  toPath DeleteStudioMember' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/membership/",
        Data.toBS principalId
      ]

instance Data.ToQuery DeleteStudioMember where
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
