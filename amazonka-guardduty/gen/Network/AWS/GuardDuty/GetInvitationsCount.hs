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
-- Module      : Network.AWS.GuardDuty.GetInvitationsCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count of all GuardDuty membership invitations that were sent
-- to the current member account except the currently accepted invitation.
module Network.AWS.GuardDuty.GetInvitationsCount
  ( -- * Creating a Request
    GetInvitationsCount (..),
    newGetInvitationsCount,

    -- * Destructuring the Response
    GetInvitationsCountResponse (..),
    newGetInvitationsCountResponse,

    -- * Response Lenses
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInvitationsCount' smart constructor.
data GetInvitationsCount = GetInvitationsCount'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInvitationsCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetInvitationsCount ::
  GetInvitationsCount
newGetInvitationsCount = GetInvitationsCount'

instance Core.AWSRequest GetInvitationsCount where
  type
    AWSResponse GetInvitationsCount =
      GetInvitationsCountResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInvitationsCountResponse'
            Core.<$> (x Core..?> "invitationsCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInvitationsCount

instance Core.NFData GetInvitationsCount

instance Core.ToHeaders GetInvitationsCount where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetInvitationsCount where
  toPath = Core.const "/invitation/count"

instance Core.ToQuery GetInvitationsCount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInvitationsCountResponse' smart constructor.
data GetInvitationsCountResponse = GetInvitationsCountResponse'
  { -- | The number of received invitations.
    invitationsCount :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInvitationsCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitationsCount', 'getInvitationsCountResponse_invitationsCount' - The number of received invitations.
--
-- 'httpStatus', 'getInvitationsCountResponse_httpStatus' - The response's http status code.
newGetInvitationsCountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInvitationsCountResponse
newGetInvitationsCountResponse pHttpStatus_ =
  GetInvitationsCountResponse'
    { invitationsCount =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of received invitations.
getInvitationsCountResponse_invitationsCount :: Lens.Lens' GetInvitationsCountResponse (Core.Maybe Core.Int)
getInvitationsCountResponse_invitationsCount = Lens.lens (\GetInvitationsCountResponse' {invitationsCount} -> invitationsCount) (\s@GetInvitationsCountResponse' {} a -> s {invitationsCount = a} :: GetInvitationsCountResponse)

-- | The response's http status code.
getInvitationsCountResponse_httpStatus :: Lens.Lens' GetInvitationsCountResponse Core.Int
getInvitationsCountResponse_httpStatus = Lens.lens (\GetInvitationsCountResponse' {httpStatus} -> httpStatus) (\s@GetInvitationsCountResponse' {} a -> s {httpStatus = a} :: GetInvitationsCountResponse)

instance Core.NFData GetInvitationsCountResponse
