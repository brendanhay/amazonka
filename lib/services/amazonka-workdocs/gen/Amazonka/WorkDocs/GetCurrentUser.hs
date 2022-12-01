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
-- Module      : Amazonka.WorkDocs.GetCurrentUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of the current user for whom the authentication token
-- was generated. This is not a valid action for SigV4 (administrative API)
-- clients.
--
-- This action requires an authentication token. To get an authentication
-- token, register an application with Amazon WorkDocs. For more
-- information, see
-- <https://docs.aws.amazon.com/workdocs/latest/developerguide/wd-auth-user.html Authentication and Access Control for User Applications>
-- in the /Amazon WorkDocs Developer Guide/.
module Amazonka.WorkDocs.GetCurrentUser
  ( -- * Creating a Request
    GetCurrentUser (..),
    newGetCurrentUser,

    -- * Request Lenses
    getCurrentUser_authenticationToken,

    -- * Destructuring the Response
    GetCurrentUserResponse (..),
    newGetCurrentUserResponse,

    -- * Response Lenses
    getCurrentUserResponse_user,
    getCurrentUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newGetCurrentUser' smart constructor.
data GetCurrentUser = GetCurrentUser'
  { -- | Amazon WorkDocs authentication token.
    authenticationToken :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCurrentUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'getCurrentUser_authenticationToken' - Amazon WorkDocs authentication token.
newGetCurrentUser ::
  -- | 'authenticationToken'
  Prelude.Text ->
  GetCurrentUser
newGetCurrentUser pAuthenticationToken_ =
  GetCurrentUser'
    { authenticationToken =
        Core._Sensitive Lens.# pAuthenticationToken_
    }

-- | Amazon WorkDocs authentication token.
getCurrentUser_authenticationToken :: Lens.Lens' GetCurrentUser Prelude.Text
getCurrentUser_authenticationToken = Lens.lens (\GetCurrentUser' {authenticationToken} -> authenticationToken) (\s@GetCurrentUser' {} a -> s {authenticationToken = a} :: GetCurrentUser) Prelude.. Core._Sensitive

instance Core.AWSRequest GetCurrentUser where
  type
    AWSResponse GetCurrentUser =
      GetCurrentUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCurrentUserResponse'
            Prelude.<$> (x Core..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCurrentUser where
  hashWithSalt _salt GetCurrentUser' {..} =
    _salt `Prelude.hashWithSalt` authenticationToken

instance Prelude.NFData GetCurrentUser where
  rnf GetCurrentUser' {..} =
    Prelude.rnf authenticationToken

instance Core.ToHeaders GetCurrentUser where
  toHeaders GetCurrentUser' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath GetCurrentUser where
  toPath = Prelude.const "/api/v1/me"

instance Core.ToQuery GetCurrentUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCurrentUserResponse' smart constructor.
data GetCurrentUserResponse = GetCurrentUserResponse'
  { -- | Metadata of the user.
    user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCurrentUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'getCurrentUserResponse_user' - Metadata of the user.
--
-- 'httpStatus', 'getCurrentUserResponse_httpStatus' - The response's http status code.
newGetCurrentUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCurrentUserResponse
newGetCurrentUserResponse pHttpStatus_ =
  GetCurrentUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata of the user.
getCurrentUserResponse_user :: Lens.Lens' GetCurrentUserResponse (Prelude.Maybe User)
getCurrentUserResponse_user = Lens.lens (\GetCurrentUserResponse' {user} -> user) (\s@GetCurrentUserResponse' {} a -> s {user = a} :: GetCurrentUserResponse)

-- | The response's http status code.
getCurrentUserResponse_httpStatus :: Lens.Lens' GetCurrentUserResponse Prelude.Int
getCurrentUserResponse_httpStatus = Lens.lens (\GetCurrentUserResponse' {httpStatus} -> httpStatus) (\s@GetCurrentUserResponse' {} a -> s {httpStatus = a} :: GetCurrentUserResponse)

instance Prelude.NFData GetCurrentUserResponse where
  rnf GetCurrentUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
