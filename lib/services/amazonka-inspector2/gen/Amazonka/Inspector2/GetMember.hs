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
-- Module      : Amazonka.Inspector2.GetMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets member information for your organization.
module Amazonka.Inspector2.GetMember
  ( -- * Creating a Request
    GetMember (..),
    newGetMember,

    -- * Request Lenses
    getMember_accountId,

    -- * Destructuring the Response
    GetMemberResponse (..),
    newGetMemberResponse,

    -- * Response Lenses
    getMemberResponse_member,
    getMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMember' smart constructor.
data GetMember = GetMember'
  { -- | The Amazon Web Services account ID of the member account to retrieve
    -- information on.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getMember_accountId' - The Amazon Web Services account ID of the member account to retrieve
-- information on.
newGetMember ::
  -- | 'accountId'
  Prelude.Text ->
  GetMember
newGetMember pAccountId_ =
  GetMember' {accountId = pAccountId_}

-- | The Amazon Web Services account ID of the member account to retrieve
-- information on.
getMember_accountId :: Lens.Lens' GetMember Prelude.Text
getMember_accountId = Lens.lens (\GetMember' {accountId} -> accountId) (\s@GetMember' {} a -> s {accountId = a} :: GetMember)

instance Core.AWSRequest GetMember where
  type AWSResponse GetMember = GetMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMemberResponse'
            Prelude.<$> (x Data..?> "member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMember where
  hashWithSalt _salt GetMember' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData GetMember where
  rnf GetMember' {..} = Prelude.rnf accountId

instance Data.ToHeaders GetMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMember where
  toJSON GetMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountId" Data..= accountId)]
      )

instance Data.ToPath GetMember where
  toPath = Prelude.const "/members/get"

instance Data.ToQuery GetMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMemberResponse' smart constructor.
data GetMemberResponse = GetMemberResponse'
  { -- | Details of the retrieved member account.
    member :: Prelude.Maybe Member,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'member', 'getMemberResponse_member' - Details of the retrieved member account.
--
-- 'httpStatus', 'getMemberResponse_httpStatus' - The response's http status code.
newGetMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMemberResponse
newGetMemberResponse pHttpStatus_ =
  GetMemberResponse'
    { member = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of the retrieved member account.
getMemberResponse_member :: Lens.Lens' GetMemberResponse (Prelude.Maybe Member)
getMemberResponse_member = Lens.lens (\GetMemberResponse' {member} -> member) (\s@GetMemberResponse' {} a -> s {member = a} :: GetMemberResponse)

-- | The response's http status code.
getMemberResponse_httpStatus :: Lens.Lens' GetMemberResponse Prelude.Int
getMemberResponse_httpStatus = Lens.lens (\GetMemberResponse' {httpStatus} -> httpStatus) (\s@GetMemberResponse' {} a -> s {httpStatus = a} :: GetMemberResponse)

instance Prelude.NFData GetMemberResponse where
  rnf GetMemberResponse' {..} =
    Prelude.rnf member
      `Prelude.seq` Prelude.rnf httpStatus
