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
-- Module      : Amazonka.Nimble.GetLaunchProfileMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a user persona in launch profile membership.
module Amazonka.Nimble.GetLaunchProfileMember
  ( -- * Creating a Request
    GetLaunchProfileMember (..),
    newGetLaunchProfileMember,

    -- * Request Lenses
    getLaunchProfileMember_launchProfileId,
    getLaunchProfileMember_principalId,
    getLaunchProfileMember_studioId,

    -- * Destructuring the Response
    GetLaunchProfileMemberResponse (..),
    newGetLaunchProfileMemberResponse,

    -- * Response Lenses
    getLaunchProfileMemberResponse_member,
    getLaunchProfileMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfileMember' smart constructor.
data GetLaunchProfileMember = GetLaunchProfileMember'
  { -- | The ID of the launch profile used to control access from the streaming
    -- session.
    launchProfileId :: Prelude.Text,
    -- | The principal ID. This currently supports a IAM Identity Center UserId.
    principalId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfileId', 'getLaunchProfileMember_launchProfileId' - The ID of the launch profile used to control access from the streaming
-- session.
--
-- 'principalId', 'getLaunchProfileMember_principalId' - The principal ID. This currently supports a IAM Identity Center UserId.
--
-- 'studioId', 'getLaunchProfileMember_studioId' - The studio ID.
newGetLaunchProfileMember ::
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'principalId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetLaunchProfileMember
newGetLaunchProfileMember
  pLaunchProfileId_
  pPrincipalId_
  pStudioId_ =
    GetLaunchProfileMember'
      { launchProfileId =
          pLaunchProfileId_,
        principalId = pPrincipalId_,
        studioId = pStudioId_
      }

-- | The ID of the launch profile used to control access from the streaming
-- session.
getLaunchProfileMember_launchProfileId :: Lens.Lens' GetLaunchProfileMember Prelude.Text
getLaunchProfileMember_launchProfileId = Lens.lens (\GetLaunchProfileMember' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfileMember' {} a -> s {launchProfileId = a} :: GetLaunchProfileMember)

-- | The principal ID. This currently supports a IAM Identity Center UserId.
getLaunchProfileMember_principalId :: Lens.Lens' GetLaunchProfileMember Prelude.Text
getLaunchProfileMember_principalId = Lens.lens (\GetLaunchProfileMember' {principalId} -> principalId) (\s@GetLaunchProfileMember' {} a -> s {principalId = a} :: GetLaunchProfileMember)

-- | The studio ID.
getLaunchProfileMember_studioId :: Lens.Lens' GetLaunchProfileMember Prelude.Text
getLaunchProfileMember_studioId = Lens.lens (\GetLaunchProfileMember' {studioId} -> studioId) (\s@GetLaunchProfileMember' {} a -> s {studioId = a} :: GetLaunchProfileMember)

instance Core.AWSRequest GetLaunchProfileMember where
  type
    AWSResponse GetLaunchProfileMember =
      GetLaunchProfileMemberResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileMemberResponse'
            Prelude.<$> (x Data..?> "member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchProfileMember where
  hashWithSalt _salt GetLaunchProfileMember' {..} =
    _salt `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetLaunchProfileMember where
  rnf GetLaunchProfileMember' {..} =
    Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetLaunchProfileMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLaunchProfileMember where
  toPath GetLaunchProfileMember' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/launch-profiles/",
        Data.toBS launchProfileId,
        "/membership/",
        Data.toBS principalId
      ]

instance Data.ToQuery GetLaunchProfileMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLaunchProfileMemberResponse' smart constructor.
data GetLaunchProfileMemberResponse = GetLaunchProfileMemberResponse'
  { -- | The member.
    member :: Prelude.Maybe LaunchProfileMembership,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'member', 'getLaunchProfileMemberResponse_member' - The member.
--
-- 'httpStatus', 'getLaunchProfileMemberResponse_httpStatus' - The response's http status code.
newGetLaunchProfileMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLaunchProfileMemberResponse
newGetLaunchProfileMemberResponse pHttpStatus_ =
  GetLaunchProfileMemberResponse'
    { member =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The member.
getLaunchProfileMemberResponse_member :: Lens.Lens' GetLaunchProfileMemberResponse (Prelude.Maybe LaunchProfileMembership)
getLaunchProfileMemberResponse_member = Lens.lens (\GetLaunchProfileMemberResponse' {member} -> member) (\s@GetLaunchProfileMemberResponse' {} a -> s {member = a} :: GetLaunchProfileMemberResponse)

-- | The response's http status code.
getLaunchProfileMemberResponse_httpStatus :: Lens.Lens' GetLaunchProfileMemberResponse Prelude.Int
getLaunchProfileMemberResponse_httpStatus = Lens.lens (\GetLaunchProfileMemberResponse' {httpStatus} -> httpStatus) (\s@GetLaunchProfileMemberResponse' {} a -> s {httpStatus = a} :: GetLaunchProfileMemberResponse)

instance
  Prelude.NFData
    GetLaunchProfileMemberResponse
  where
  rnf GetLaunchProfileMemberResponse' {..} =
    Prelude.rnf member
      `Prelude.seq` Prelude.rnf httpStatus
