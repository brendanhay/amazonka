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
-- Module      : Amazonka.Nimble.GetStudioMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a user\'s membership in a studio.
module Amazonka.Nimble.GetStudioMember
  ( -- * Creating a Request
    GetStudioMember (..),
    newGetStudioMember,

    -- * Request Lenses
    getStudioMember_principalId,
    getStudioMember_studioId,

    -- * Destructuring the Response
    GetStudioMemberResponse (..),
    newGetStudioMemberResponse,

    -- * Response Lenses
    getStudioMemberResponse_member,
    getStudioMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStudioMember' smart constructor.
data GetStudioMember = GetStudioMember'
  { -- | The principal ID. This currently supports a IAM Identity Center UserId.
    principalId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'getStudioMember_principalId' - The principal ID. This currently supports a IAM Identity Center UserId.
--
-- 'studioId', 'getStudioMember_studioId' - The studio ID.
newGetStudioMember ::
  -- | 'principalId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetStudioMember
newGetStudioMember pPrincipalId_ pStudioId_ =
  GetStudioMember'
    { principalId = pPrincipalId_,
      studioId = pStudioId_
    }

-- | The principal ID. This currently supports a IAM Identity Center UserId.
getStudioMember_principalId :: Lens.Lens' GetStudioMember Prelude.Text
getStudioMember_principalId = Lens.lens (\GetStudioMember' {principalId} -> principalId) (\s@GetStudioMember' {} a -> s {principalId = a} :: GetStudioMember)

-- | The studio ID.
getStudioMember_studioId :: Lens.Lens' GetStudioMember Prelude.Text
getStudioMember_studioId = Lens.lens (\GetStudioMember' {studioId} -> studioId) (\s@GetStudioMember' {} a -> s {studioId = a} :: GetStudioMember)

instance Core.AWSRequest GetStudioMember where
  type
    AWSResponse GetStudioMember =
      GetStudioMemberResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioMemberResponse'
            Prelude.<$> (x Data..?> "member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStudioMember where
  hashWithSalt _salt GetStudioMember' {..} =
    _salt
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetStudioMember where
  rnf GetStudioMember' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetStudioMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStudioMember where
  toPath GetStudioMember' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/membership/",
        Data.toBS principalId
      ]

instance Data.ToQuery GetStudioMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStudioMemberResponse' smart constructor.
data GetStudioMemberResponse = GetStudioMemberResponse'
  { -- | The member.
    member :: Prelude.Maybe StudioMembership,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'member', 'getStudioMemberResponse_member' - The member.
--
-- 'httpStatus', 'getStudioMemberResponse_httpStatus' - The response's http status code.
newGetStudioMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStudioMemberResponse
newGetStudioMemberResponse pHttpStatus_ =
  GetStudioMemberResponse'
    { member = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The member.
getStudioMemberResponse_member :: Lens.Lens' GetStudioMemberResponse (Prelude.Maybe StudioMembership)
getStudioMemberResponse_member = Lens.lens (\GetStudioMemberResponse' {member} -> member) (\s@GetStudioMemberResponse' {} a -> s {member = a} :: GetStudioMemberResponse)

-- | The response's http status code.
getStudioMemberResponse_httpStatus :: Lens.Lens' GetStudioMemberResponse Prelude.Int
getStudioMemberResponse_httpStatus = Lens.lens (\GetStudioMemberResponse' {httpStatus} -> httpStatus) (\s@GetStudioMemberResponse' {} a -> s {httpStatus = a} :: GetStudioMemberResponse)

instance Prelude.NFData GetStudioMemberResponse where
  rnf GetStudioMemberResponse' {..} =
    Prelude.rnf member
      `Prelude.seq` Prelude.rnf httpStatus
