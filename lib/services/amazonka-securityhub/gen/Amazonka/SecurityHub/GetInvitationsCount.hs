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
-- Module      : Amazonka.SecurityHub.GetInvitationsCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count of all Security Hub membership invitations that were
-- sent to the current member account, not including the currently accepted
-- invitation.
module Amazonka.SecurityHub.GetInvitationsCount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newGetInvitationsCount' smart constructor.
data GetInvitationsCount = GetInvitationsCount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInvitationsCountResponse'
            Prelude.<$> (x Data..?> "InvitationsCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInvitationsCount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetInvitationsCount where
  rnf _ = ()

instance Data.ToHeaders GetInvitationsCount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInvitationsCount where
  toPath = Prelude.const "/invitations/count"

instance Data.ToQuery GetInvitationsCount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInvitationsCountResponse' smart constructor.
data GetInvitationsCountResponse = GetInvitationsCountResponse'
  { -- | The number of all membership invitations sent to this Security Hub
    -- member account, not including the currently accepted invitation.
    invitationsCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInvitationsCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitationsCount', 'getInvitationsCountResponse_invitationsCount' - The number of all membership invitations sent to this Security Hub
-- member account, not including the currently accepted invitation.
--
-- 'httpStatus', 'getInvitationsCountResponse_httpStatus' - The response's http status code.
newGetInvitationsCountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInvitationsCountResponse
newGetInvitationsCountResponse pHttpStatus_ =
  GetInvitationsCountResponse'
    { invitationsCount =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of all membership invitations sent to this Security Hub
-- member account, not including the currently accepted invitation.
getInvitationsCountResponse_invitationsCount :: Lens.Lens' GetInvitationsCountResponse (Prelude.Maybe Prelude.Int)
getInvitationsCountResponse_invitationsCount = Lens.lens (\GetInvitationsCountResponse' {invitationsCount} -> invitationsCount) (\s@GetInvitationsCountResponse' {} a -> s {invitationsCount = a} :: GetInvitationsCountResponse)

-- | The response's http status code.
getInvitationsCountResponse_httpStatus :: Lens.Lens' GetInvitationsCountResponse Prelude.Int
getInvitationsCountResponse_httpStatus = Lens.lens (\GetInvitationsCountResponse' {httpStatus} -> httpStatus) (\s@GetInvitationsCountResponse' {} a -> s {httpStatus = a} :: GetInvitationsCountResponse)

instance Prelude.NFData GetInvitationsCountResponse where
  rnf GetInvitationsCountResponse' {..} =
    Prelude.rnf invitationsCount
      `Prelude.seq` Prelude.rnf httpStatus
