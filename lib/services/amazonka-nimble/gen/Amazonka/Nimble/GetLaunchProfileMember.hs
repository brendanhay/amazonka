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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getLaunchProfileMember_studioId,
    getLaunchProfileMember_principalId,
    getLaunchProfileMember_launchProfileId,

    -- * Destructuring the Response
    GetLaunchProfileMemberResponse (..),
    newGetLaunchProfileMemberResponse,

    -- * Response Lenses
    getLaunchProfileMemberResponse_member,
    getLaunchProfileMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfileMember' smart constructor.
data GetLaunchProfileMember = GetLaunchProfileMember'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The principal ID.
    principalId :: Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Text
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
-- 'studioId', 'getLaunchProfileMember_studioId' - The studio ID.
--
-- 'principalId', 'getLaunchProfileMember_principalId' - The principal ID.
--
-- 'launchProfileId', 'getLaunchProfileMember_launchProfileId' - The launch profile ID.
newGetLaunchProfileMember ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'principalId'
  Prelude.Text ->
  -- | 'launchProfileId'
  Prelude.Text ->
  GetLaunchProfileMember
newGetLaunchProfileMember
  pStudioId_
  pPrincipalId_
  pLaunchProfileId_ =
    GetLaunchProfileMember'
      { studioId = pStudioId_,
        principalId = pPrincipalId_,
        launchProfileId = pLaunchProfileId_
      }

-- | The studio ID.
getLaunchProfileMember_studioId :: Lens.Lens' GetLaunchProfileMember Prelude.Text
getLaunchProfileMember_studioId = Lens.lens (\GetLaunchProfileMember' {studioId} -> studioId) (\s@GetLaunchProfileMember' {} a -> s {studioId = a} :: GetLaunchProfileMember)

-- | The principal ID.
getLaunchProfileMember_principalId :: Lens.Lens' GetLaunchProfileMember Prelude.Text
getLaunchProfileMember_principalId = Lens.lens (\GetLaunchProfileMember' {principalId} -> principalId) (\s@GetLaunchProfileMember' {} a -> s {principalId = a} :: GetLaunchProfileMember)

-- | The launch profile ID.
getLaunchProfileMember_launchProfileId :: Lens.Lens' GetLaunchProfileMember Prelude.Text
getLaunchProfileMember_launchProfileId = Lens.lens (\GetLaunchProfileMember' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfileMember' {} a -> s {launchProfileId = a} :: GetLaunchProfileMember)

instance Core.AWSRequest GetLaunchProfileMember where
  type
    AWSResponse GetLaunchProfileMember =
      GetLaunchProfileMemberResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileMemberResponse'
            Prelude.<$> (x Core..?> "member")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchProfileMember

instance Prelude.NFData GetLaunchProfileMember

instance Core.ToHeaders GetLaunchProfileMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLaunchProfileMember where
  toPath GetLaunchProfileMember' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId,
        "/membership/",
        Core.toBS principalId
      ]

instance Core.ToQuery GetLaunchProfileMember where
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
