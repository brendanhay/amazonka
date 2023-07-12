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
-- Module      : Amazonka.Chime.GetUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details for the specified user ID, such as primary email
-- address, license type,and personal meeting PIN.
--
-- To retrieve user details with an email address instead of a user ID, use
-- the ListUsers action, and then filter by email address.
module Amazonka.Chime.GetUser
  ( -- * Creating a Request
    GetUser (..),
    newGetUser,

    -- * Request Lenses
    getUser_accountId,
    getUser_userId,

    -- * Destructuring the Response
    GetUserResponse (..),
    newGetUserResponse,

    -- * Response Lenses
    getUserResponse_user,
    getUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUser' smart constructor.
data GetUser = GetUser'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getUser_accountId' - The Amazon Chime account ID.
--
-- 'userId', 'getUser_userId' - The user ID.
newGetUser ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  GetUser
newGetUser pAccountId_ pUserId_ =
  GetUser'
    { accountId = pAccountId_,
      userId = pUserId_
    }

-- | The Amazon Chime account ID.
getUser_accountId :: Lens.Lens' GetUser Prelude.Text
getUser_accountId = Lens.lens (\GetUser' {accountId} -> accountId) (\s@GetUser' {} a -> s {accountId = a} :: GetUser)

-- | The user ID.
getUser_userId :: Lens.Lens' GetUser Prelude.Text
getUser_userId = Lens.lens (\GetUser' {userId} -> userId) (\s@GetUser' {} a -> s {userId = a} :: GetUser)

instance Core.AWSRequest GetUser where
  type AWSResponse GetUser = GetUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserResponse'
            Prelude.<$> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUser where
  hashWithSalt _salt GetUser' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData GetUser where
  rnf GetUser' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders GetUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetUser where
  toPath GetUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/users/",
        Data.toBS userId
      ]

instance Data.ToQuery GetUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | The user details.
    user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'getUserResponse_user' - The user details.
--
-- 'httpStatus', 'getUserResponse_httpStatus' - The response's http status code.
newGetUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUserResponse
newGetUserResponse pHttpStatus_ =
  GetUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user details.
getUserResponse_user :: Lens.Lens' GetUserResponse (Prelude.Maybe User)
getUserResponse_user = Lens.lens (\GetUserResponse' {user} -> user) (\s@GetUserResponse' {} a -> s {user = a} :: GetUserResponse)

-- | The response's http status code.
getUserResponse_httpStatus :: Lens.Lens' GetUserResponse Prelude.Int
getUserResponse_httpStatus = Lens.lens (\GetUserResponse' {httpStatus} -> httpStatus) (\s@GetUserResponse' {} a -> s {httpStatus = a} :: GetUserResponse)

instance Prelude.NFData GetUserResponse where
  rnf GetUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
