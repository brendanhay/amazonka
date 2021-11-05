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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a user\'s membership in a studio.
module Amazonka.Nimble.GetStudioMember
  ( -- * Creating a Request
    GetStudioMember (..),
    newGetStudioMember,

    -- * Request Lenses
    getStudioMember_studioId,
    getStudioMember_principalId,

    -- * Destructuring the Response
    GetStudioMemberResponse (..),
    newGetStudioMemberResponse,

    -- * Response Lenses
    getStudioMemberResponse_member,
    getStudioMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStudioMember' smart constructor.
data GetStudioMember = GetStudioMember'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The principal ID.
    principalId :: Prelude.Text
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
-- 'studioId', 'getStudioMember_studioId' - The studio ID.
--
-- 'principalId', 'getStudioMember_principalId' - The principal ID.
newGetStudioMember ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'principalId'
  Prelude.Text ->
  GetStudioMember
newGetStudioMember pStudioId_ pPrincipalId_ =
  GetStudioMember'
    { studioId = pStudioId_,
      principalId = pPrincipalId_
    }

-- | The studio ID.
getStudioMember_studioId :: Lens.Lens' GetStudioMember Prelude.Text
getStudioMember_studioId = Lens.lens (\GetStudioMember' {studioId} -> studioId) (\s@GetStudioMember' {} a -> s {studioId = a} :: GetStudioMember)

-- | The principal ID.
getStudioMember_principalId :: Lens.Lens' GetStudioMember Prelude.Text
getStudioMember_principalId = Lens.lens (\GetStudioMember' {principalId} -> principalId) (\s@GetStudioMember' {} a -> s {principalId = a} :: GetStudioMember)

instance Core.AWSRequest GetStudioMember where
  type
    AWSResponse GetStudioMember =
      GetStudioMemberResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioMemberResponse'
            Prelude.<$> (x Core..?> "member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStudioMember

instance Prelude.NFData GetStudioMember

instance Core.ToHeaders GetStudioMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetStudioMember where
  toPath GetStudioMember' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/membership/",
        Core.toBS principalId
      ]

instance Core.ToQuery GetStudioMember where
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

instance Prelude.NFData GetStudioMemberResponse
